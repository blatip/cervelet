#pragma region // ================================================================== INCLUDES
    #include <stdio.h>
    #include <stdint.h>
    #include <cuda_runtime.h>
    #include "cervelet.h"

    uint8_t* d_data = NULL;
    cudaEvent_t evt_start, evt_stop;
    bool events_initialized = false;

    #define FAKE_WORK 15
    #define NBR_CRVPK NBR_CRVPS / 4
    size_t total_size = NBR_CRVPK * TAI_CRVPS;

#pragma endregion

#pragma region // ================================================================== UTILITAIRES
__device__ void gpu_sleep(clock_t delay) {
    clock_t start = clock();
    while (clock() - start < delay);
}

__device__ void fake_workload(int iterations = 10) {
    volatile int acc = 0;
    for (int i = 0; i < iterations; ++i) {
        acc = acc * 31 + i;  // instructions indépendantes, pas trop optimisées
        acc = acc / 31;
    }
}

int get_default_threads_per_block() {
    cudaDeviceProp prop;
    int device;
    cudaError_t err;

    // Récupérer le device actuel
    err = cudaGetDevice(&device);
    if (err != cudaSuccess) {
        fprintf(stderr, "Erreur cudaGetDevice: %s\n", cudaGetErrorString(err));
        return 256; // Valeur de secours
    }

    // Récupérer les propriétés de la carte
    err = cudaGetDeviceProperties(&prop, device);
    if (err != cudaSuccess) {
        fprintf(stderr, "Erreur cudaGetDeviceProperties: %s\n", cudaGetErrorString(err));
        return 256; // Valeur de secours
    }

    int max_threads = prop.maxThreadsPerBlock;

    // Choix conservateur basé sur les puissances de 2 inférieures ou égales à max_threads
    if (max_threads >= 1024) return 512;
    if (max_threads >= 512) return 256;
    if (max_threads >= 256) return 128;
    if (max_threads >= 128) return 64;

    return 32; // Pour les très vieilles cartes
}
#pragma endregion

#pragma region // ================================================================== VERROUILLAGES

// Table des locks
__device__ int locks[NBR_CRVPK]; // Un lock par portion

// Lock / Release simple
__device__ bool lock_acquire_timeout(int* lock, int max_attempts = 1000000) {
    int attempts = 0;
    while (atomicCAS(lock, 0, 1) != 0) {
        if (++attempts >= max_attempts) {
            printf("Timeout\n");
            return false;
        }
    }
    return true;
}
__device__ void lock_release(int* lock) {
    atomicExch(lock, 0);  // libère le verrou
}

// Lock / unlock three
__device__ bool lock_three_timeout(int* locks, int i) {
    bool ok = true;
    ok &= lock_acquire_timeout(&locks[i]);
    if (i > 0) {
        if ((i % 4) != 0) {
            // ok &= lock_acquire_timeout(&locks[i - 1]);
        }
    }
    if (i < NBR_CRVPK - 1) {
        if ((i % 4) != 0) {
            // ok &= lock_acquire_timeout(&locks[i + 1]);
        }
    }
    if (!ok) {
        if (i > 0) {
            // lock_release(&locks[i - 1]);
        }
        lock_release(&locks[i]);
        if (i < NBR_CRVPK - 1) {
            // lock_release(&locks[i + 1]);
        }
    }
    return ok;
}
__device__ void unlock_three(int* locks, int i) {
    if (i > 0) {
        // lock_release(&locks[i - 1]);
    }
    lock_release(&locks[i]);
    if (i < NBR_CRVPK - 1) {
        // lock_release(&locks[i + 1]);
    }
}

#pragma endregion

#pragma region // ================================================================== KERNELS CUDA

__global__ void kernel_count() {
    int bloc_id = blockIdx.x;
    int thread_id = threadIdx.x;
    printf("GPU - Bloc %d, Thread %d\n", bloc_id, thread_id);
}

__global__ void kernel(uint8_t* data) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i >= NBR_CRVPK) return;

    // if (lock_three_timeout(locks, i)) {
        data[i * 32] += 1;
        // gpu_sleep(1);
        fake_workload(FAKE_WORK);
        // unlock_three(locks, i);
    //}
    //else {
        // printf("Thread %d failed to lock.\n", i);
    //}
}

#pragma endregion

#pragma region // ================================================================== WRAPPERS
// Wrapper d'information sur le GPU
extern "C" void info_gpu() {
    int device;
    cudaGetDevice(&device);  // Obtenir l'ID du GPU utilisé
    cudaDeviceProp prop;
    cudaGetDeviceProperties(&prop, device);
    printf("Nom du GPU : %s\n", prop.name);
    printf("Capacite de calcul : %d.%d\n", prop.major, prop.minor);
    printf("Max grid size (x, y, z) : %d x %d x %d\n", prop.maxGridSize[0], prop.maxGridSize[1], prop.maxGridSize[2]);
    printf("Max blocs size (x, y, z) : %d x %d x %d\n", prop.maxThreadsDim[0], prop.maxThreadsDim[1], prop.maxThreadsDim[2]);
    printf("Max threads per bloc : %d\n", prop.maxThreadsPerBlock);
}
// Wrapper de test sommaire
extern "C" void basic_test_gpu() {
    kernel_count << < 2, 3 >> > ();
    cudaDeviceSynchronize();
}
// Fonction de début
extern "C" void init_cuda() {
    // Si l'allocation est déjà faite
    if (d_data != NULL) {
        cudaFree(d_data); // Au cas où tu réinitialises
    }
    // Allocation mémoire
    cudaError_t err = cudaMalloc(&d_data, total_size);
    if (err != cudaSuccess) {
        printf("cudaMalloc failed: %s\n", cudaGetErrorString(err));
    }
    else {
        printf("Allocation reussie (%zu octets)\n", total_size);
    }
    // Création des événements
    if (!events_initialized) {
        cudaEventCreate(&evt_start);
        cudaEventCreate(&evt_stop);
        events_initialized = true;
    }
}
// Fonction de test CPU -> GPU
extern "C" void run_test() {
    // Vérification de l'allocation
    if (d_data == NULL) {
        printf("Erreur : la mémoire n'est pas allouée\n");
        return;
    }
    // Initialisation des locks à 0
    int* d_locks;
    cudaGetSymbolAddress((void**)&d_locks, locks);
    cudaMemset(d_locks, 0, sizeof(int) * NBR_CRVPK);
    // Enregistrement du début de mesure
    cudaEventRecord(evt_start, 0);
    // déterminer le nombre de threads/blocks à utiliser
    int threads = get_default_threads_per_block();
    // Préparation du kernel
    int blocks = (NBR_CRVPK + threads - 1) / threads;
    // Lancement du kernel
    kernel <<<blocks, threads >>> (d_data);
    // Finition de tout le travail
    cudaDeviceSynchronize();
    // Enregistrement de la fin de mesure
    cudaEventRecord(evt_stop, 0);
    // Attente que l'enregistrement soit fait
    cudaEventSynchronize(evt_stop);
    // Mesure de la durée
    float milliseconds = 0;
    cudaEventElapsedTime(&milliseconds, evt_start, evt_stop);
    // Affichage de la durée
    printf("Duree : %.3f ms (avec %d blk de %d thr)\n", milliseconds, blocks, threads);
    // vérification si erreur
    cudaDeviceSynchronize();
    cudaError_t err = cudaGetLastError();
    if (err != cudaSuccess) {
        printf("CUDA kernel failed: %s\n", cudaGetErrorString(err));
    }
}
// Fonctionj de clôture du travail cuda
extern "C" void stop_cuda() {
    // Nettoyage des mesures
    if (events_initialized) {
        cudaEventDestroy(evt_start);
        cudaEventDestroy(evt_stop);
        events_initialized = false;
    }
    // Libération mémoire
    if (d_data != NULL) {
        cudaFree(d_data);
        d_data = NULL;
        printf("Memoire GPU liberee.\n");
    }
}
#pragma endregion
