#include <stdint.h>
#ifndef CERVELET_H
	#define CERVELET_H
	#ifdef __cplusplus
		extern "C" {
	#endif
		// Déclaration des fonction CUDA visible depuis C
		void basic_test_gpu();
		void info_gpu();
		void init_cuda();
		void run_test();
		void stop_cuda();
	#ifdef __cplusplus
		}
	#endif
#endif

// dimensionnement utilisateur (portions)
#define DIMENSION_X 512
#define DIMENSION_Y 256
#define DIMENSION_Z 32
#define NBR_CRVPS DIMENSION_X * DIMENSION_Y * DIMENSION_Z // max 2^32 = 4Gp / Limitation aussi par capacité de la carte
#define TAI_CRVPS 32

