// #------------------------------------------------------------------------------------ GPL LICENCE
    /*
    ; Copyright (C) 2024-2025 Philippe BLATIERE
    ;
    ; This program is free software: you can redistribute it and/or modify
    ; it under the terms of the GNU General Public License as published by
    ; the Free Software Foundation, either version 3 of the License, or
    ; (at your option) any later version.
    ;
    ; This program is distributed in the hope that it will be useful,
    ; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    ; GNU General Public License for more details.
    ;
    ; You should have received a copy of the GNU General Public License
    ; along with this program. If not, see <https://www.gnu.org/licenses/>.
    */
// #------------------------------------------------------------------------------------ A_FAIRE
    /*
    ; résoudre tous les points marqués A_FAIRE
    ; separer les numeros d'utilisateurs et les numeros de portions dans builder
    ; trouver une solution pour une mise en réseau
    ; coder les développements dendrites / axones
    */
// #------------------------------------------------------------------------------------ REFERENCES
    #include <stdio.h>
    #include <stdlib.h>

    #include <stdint.h>
    #include <string.h>
    #include <time.h>

    #include <windows.h>
    #include <winsock2.h>

    #include <ws2tcpip.h>  // Bibliothèque pour certaines fonctions réseau avancées
    #pragma comment(lib, "Ws2_32.lib")  // Linker automatiquement la librairie réseau


    /*
    bits 64
    ; default REL ?
    ; A_FAIRE : envisager l'utilisation du prefetch L1 des portions suivantes
    ; general
        extern GetModuleHandleA                 ; Recuperer le handle du module en cours
        extern GetLastError                     ; Recuperer la derniere erreur
        extern ExitProcess                      ; Fin du processus / kernel32
    ; pour console
        extern GetStdHandle                     ; Gestionnaire de peripheriques
        extern GetConsoleScreenBufferInfo
        extern SetConsoleCursorPosition         ; Positionnement du curseur
        extern WriteConsoleA                    ; Sortie en mode console (ANSI)
        extern ReadConsoleA                     ; Entree en mode console (ANSI)
    ; pour creation fenetre
        extern RegisterClassExA
        extern CreateWindowExA
        extern ShowWindow
        extern UpdateWindow
        extern InvalidateRect
        extern RedrawWindow
    ; pour traitement messages window
        extern PeekMessageA
        extern GetMessageA
        extern IsDialogMessageA
        extern TranslateMessage
        extern DispatchMessageA
    ; pour threads reseau
        extern CreateEventA
        ;
        extern WSAStartup
        extern WSACleanup
        extern socket
        ; extern connect
        ; extern send
        extern sendto
        extern closesocket
        ;
        extern bind
        extern listen ; NOT USED
        extern accept ; NOT USED
        extern recv ; NOT USED
        extern recvfrom

        extern WSAGetLastError

    ; pour gestion des threads
        extern CreateThread
        extern SetEvent
        extern WaitForSingleObject
        extern ResetEvent
        extern ExitThread
    ; pour window proc
        extern DefWindowProcA
        extern PostQuitMessage
        extern SetWindowTextA
    ; pour paint
        extern BeginPaint
        extern CreateCompatibleDC
        extern CreateDIBSection
        extern SelectObject
        extern GdiFlush
        extern BitBlt
        extern DeleteObject
        extern DeleteDC
        extern EndPaint
    ; pour acces fichier
        extern CreateFileA
        extern GetFileSizeEx
        extern ReadFile
        extern WriteFile
        extern CloseHandle
    ; pour lecture de flux video
        ; extern MFStartup, MFShutdown, MFCreateSourceReaderFromMediaSource, MFCreateMediaType
        ; extern IMFSourceReader_ReadSample, IMFSample_ConvertToContiguousBuffer, IMFMediaBuffer_Lock
    */
// #-------------------------------- c : macros ---------------------------------------- MACROS
    //---------- macros de recherche de donnees dans portions
        // extraction de donnees non signees
        #define U8_FROMP(port32, offset8) (((uint8_t *)crvDatas)[port32*TAILLE_DES_PORTIONS+offset8])
        #define U16_FROMP(port32, offset8) (*((uint16_t *)((uint8_t *)crvDatas + port32 * TAILLE_DES_PORTIONS + offset8)))
        #define U32_FROMP(port32, offset8) (*((uint32_t *)((uint8_t *)crvDatas + port32 * TAILLE_DES_PORTIONS + offset8)))
        #define U64_FROMP(port32, offset8) (*((uint64_t *)((uint8_t *)crvDatas + port32 * TAILLE_DES_PORTIONS + offset8)))
        // extraction de donnees signees
        #define S8_FROMP(port32, offset8) (((int8_t *)crvDatas)[port32*TAILLE_DES_PORTIONS+offset8])
        #define S16_FROMP(port32, offset8) (*((int16_t *)((uint8_t *)crvDatas + port32 * TAILLE_DES_PORTIONS + offset8)))
        // ecritures de donnees non signees
        #define FOR_U8_TOP(port32, offset8) *((uint8_t *)((uint8_t *)crvDatas + port32 * TAILLE_DES_PORTIONS + offset8))
        #define FOR_U16_TOP(port32, offset8) *((uint16_t *)((uint8_t *)crvDatas + port32 * TAILLE_DES_PORTIONS + offset8))
        #define FOR_U32_TOP(port32, offset8) *((uint32_t *)((uint8_t *)crvDatas + port32 * TAILLE_DES_PORTIONS + offset8))
        #define FOR_U64_TOP(port32, offset8) *((uint64_t *)((uint8_t *)crvDatas + port32 * TAILLE_DES_PORTIONS + offset8))
        // ecritures de donnees signees
        #define FOR_S8_TOP(port32, offset8) *((int8_t *)((uint8_t *)crvDatas + port32 * TAILLE_DES_PORTIONS + offset8))
        #define FOR_S16_TOP(port32, offset8) *((int16_t *)((uint8_t *)crvDatas + port32 * TAILLE_DES_PORTIONS + offset8))
    //---------- macros de recherche de donnees dans notes = msgStack
        // extraction de donnees non signees
        #define U32_FROMN(note64, offset8) (*((uint32_t *)((uint8_t *)msgStack + note64 * TAILLE_DES_NOTES + offset8)))
        // ecritures de donnees non signees
        #define FOR_U32_TON(note64, offset8) *((uint32_t *)((uint8_t *)msgStack + note64 * TAILLE_DES_NOTES + offset8))
        #define FOR_U64_TON(note64, offset8) *((uint64_t *)((uint8_t *)msgStack + note64 * TAILLE_DES_NOTES + offset8))
    //---------- macros de recherche de donnees dans msgToSnd
        // extraction de donnees non signees
        #define U16_FROMM(mesg64, offset8) (*((uint16_t *)((uint8_t *)msgToSnd + mesg64 * TAILLE_DES_MESSAGES + offset8)))
        #define U32_FROMM(mesg64, offset8) (*((uint32_t *)((uint8_t *)msgToSnd + mesg64 * TAILLE_DES_MESSAGES + offset8)))
        #define U64_FROMM(mesg64, offset8) (*((uint64_t *)((uint8_t *)msgToSnd + mesg64 * TAILLE_DES_MESSAGES + offset8)))
        // ecritures de donnees non signees
        #define FOR_U16_TOM(mesg64, offset8) *((uint16_t *)((uint8_t *)msgToSnd + mesg64 * TAILLE_DES_MESSAGES + offset8))
        #define FOR_U32_TOM(mesg64, offset8) *((uint32_t *)((uint8_t *)msgToSnd + mesg64 * TAILLE_DES_MESSAGES + offset8))
        #define FOR_U64_TOM(mesg64, offset8) *((uint64_t *)((uint8_t *)msgToSnd + mesg64 * TAILLE_DES_MESSAGES + offset8))
    //---------- macros de recherche de donnees dans users
        // extraction de donnees non signees
        #define U16_FROMU(util32, offset8) (*((uint16_t *)((uint8_t *)users + util32 * TAILLE_DES_UTILISATEURS + offset8)))
        #define U32_FROMU(util32, offset8) (*((uint32_t *)((uint8_t *)users + util32 * TAILLE_DES_UTILISATEURS + offset8)))
        #define U64_FROMU(util32, offset8) (*((uint64_t *)((uint8_t *)users + util32 * TAILLE_DES_UTILISATEURS + offset8)))
        // ecritures de donnees non signees
        #define FOR_U16_TOU(util32, offset8) *((uint16_t *)((uint8_t *)users + util32 * TAILLE_DES_UTILISATEURS + offset8))
        #define FOR_U32_TOU(util32, offset8) *((uint32_t *)((uint8_t *)users + util32 * TAILLE_DES_UTILISATEURS + offset8))
        #define FOR_U64_TOU(util32, offset8) *((uint64_t *)((uint8_t *)users + util32 * TAILLE_DES_UTILISATEURS + offset8))
    // # >>> entrees : FVTM / sorties : S / imperatifs : (ABCD 89012345 ou L/N)
    /*
    ; macros pour numeros de portions
        %macro defineMyPortion 1
            mov rbx, [myPackIdShifted]
            add rbx, %1
            %endmacro
        %macro defineMySource 1
            mov rcx, [myPackIdShifted]
            add rcx, %1
            %endmacro
        %macro defineMyDestin 1
            mov rdx, [myPackIdShifted]
            add rdx, %1
            %endmacro
    ; macros pour calculs d'adresses et longueurs
        %macro adresse_from_numero 2 ; calcul de l'adresse d'une portion (%1 <-- %2)
            ; a garder en coherence avec TAILLE_DES_PORTIONS
            ; utilisation pour traitement
            ; %1 REG mod = sortie adresse portion
            ; %2 REG mod = entree numero portion
            ; ----- recuperation adresse de base
            lea %1, [portions]
            ; ----- ajout de TAILLE_DES_PORTIONS fois le numero de portion
            shl %2, 5 ; %2 x 32
            add %1, %2 ; + 32 x TDP
            ; ----- fin
            %endmacro
        
        %macro adresse_from_user 2 ; calcul de l'adresse d'un utilisateur parmi les utilisateurs (%1 <-- %2)
            ; a garder en coherence avec TAILLE_DES_UTILISATEURS
            ; utilisation pour traitement
            ; %1 REG mod = sortie adresse utilisateur
            ; %2 REG mod = entree numero utilisateur
            ; ----- recuperation adresse de base
            lea %1, [users]
            ; ----- ajout de TAILLE_DES_UTILISATEURS fois le numero de portion
            shl %2, 3 ; %2 x 8
            add %1, %2 ; +8xTDU
            shl %2, 1 ; %2 x 2
            add %1, %2 ; +24xTDU
            ; ----- fin
            %endmacro

        %macro longueur_from_quantity 2 ; calcul de la longueur d'un paquet d'après le nombre de messages dans msgToSnd (%1 <-- %2)
            ; a garder en coherence avec TAILLE_DES_MESSAGES
            ; utilisation pour traitement
            ; %1 REG mod = sortie longueur paquet
            ; %2 REG mod = entree nombre de messages
            ; ----- calcul
            shl %2, 3 ; %2 x 8
            mov %1, %2 ; %2 x 8
            shl %2, 1 ; %2 x 16
            add %1, %2 ; %2 x 24 
            ; ----- fin
            %endmacro
    ; macros pour comparaisons en série
        %macro comparer_et_jump_si_egal 3 ; comparaison et jump si egalite
            ; %1 REG fix = valeur de referenceregistre 8 bits a comparer
            ; %2 REG/IMM fix = valeur a comparer (meme taille)
            ; %3 LABEL = label vers lequel sauter
            ; ----- comparaison et jump
            cmp %1, %2
            je %3
            ; ----- fin
            %endmacro

        %macro comparer_al_valeur_vers_dl_AVCD 4 ; comparaison pour jump unique après plusieurs comparaisons
            ; %1 REG fix = rax
            ; %2 REG/IMM fix = valeur a comparer
            ; %3 LABEL = rcx
            ; %4 LABEL = rdx
            ; ----- comparaison et attribution
            cmp al, %2
            sete cl
            or dl, cl
            ; ----- fin
            %endmacro

    ; macros pour évolutions (N/U - Pour mémoire)
        %macro faire_evoluer_dx_dy_SSABCDMMM 9 ; faire evoluer dx et dy de 1 dans une/deux directions pseudo-aleatoire
            ; %1 REG(N) mod = entree dx actuel / sortie nouveau dx
            ; %2 REG(N) mod = entree dy actuel / sortie nouveau dy
            ; %3 a %6 (rax), (rbx), (rcx), (rdx) mod = utilitaires
            ; %7 a % 9 REG mod = utilitaires (- = +)
            ; pseudo constantes
            mov %7, -1
            xor %8, %8
            mov %9, 1
            ; si %1 et %2 sont = 0
            mov rax, %1
            shl rax, 8
            add rax, %2
            test rax, rax
            ; alors mettre evo_ofs_x et evo_ofs_y a 1
            cmovz rcx, %9
            cmovz rdx, %9
            ; et passer aux limitations
            je %%fedd_rcx_et_rdx_deja_definis
            ; ---- calcul de la direction (0 a 4)
            ; direction globale a prendre -> rbx
            xor rbx, rbx
            cmp %1, %2
            cmovg rbx, %7 ; = -1
            cmovl rbx, %9 ; = 1
            ; complement de direction aleatoire -> rdx
            rdtsc ; -> edx:eax => ax = 0 a 65535
            xor dx, dx ; => dx = 0
            mov cx, 3 ; => cx = 3
            div cx ; -> division (65536*dx+ax)/cx ; reste dans dx => dx = 0 a 2
            ; A_FAIRE : Optimiser le temps d'execution
            dec dx ; -> dx = -1 a +1
            movsx rdx, dx
            ; total
            add rbx, rdx ; -> rbx = -2 a +2
            ; ----- calcul des nouveaux ofs_x et ofs_y
            ; nouveau evo_ofs_x -> rcx
            xor rcx, rcx
            cmp rbx, 1
            cmovg rcx, %7 ; = -1
            cmovl rcx, %9 ; = 1
            ; nouveau evo_ofs_y -> rdx
            xor rdx, rdx
            cmp rbx, -1
            cmovl rdx, %7 ; = -1
            cmovg rdx, %9 ; = 1
            ; nouveaux ofs_x et ofs_y
            %%fedd_rcx_et_rdx_deja_definis:
            add %1, rcx
            add %2, rdx
            ; ----- limitations ofs_x et ofs_y
            ; pseudo constantes
            mov %7, -63 ; mini -128
            mov %9, 127 ; maxi 127
            ; limitation de ofs_x
            cmp %1, %7 ; ? mini
            cmovl %1, %7 ; = mini
            cmp %1, %9 ; ? maxi
            cmovg %1, %9 ; = maxi
            ; limitation de ofs_y
            cmp %2, %7 ; ? mini
            cmovl %2, %7 ; = mini
            cmp %2, %9 ; ? maxi
            cmovg %2, %9 ; = maxi
            ; fin
            %endmacro
        %macro extraire_x_y_de_numero_FSS 3 ; extraction des coordonnees d'un numero de portion
            ; %1 REG/IMM fix = entree numero de portion
            ; %2 REG mod = sortie coordonnee x
            ; %3 REG mod = sortie coordonnee y
            ; nouvelle coordonnee x -> rcx
            mov %2, %1
            and %2, DIMENSION_X - 1
            ; nouvelle coordonnee y -> rdx
            mov %3, %1
            shr %3, BITS_POUR_X
            ; fin
            %endmacro
        %macro limiter_coordonnees_x_y_SSMMM 5 ; limitation des coordonnees x et y
            ; %1 REG mod = entree x a limiter
            ; %2 REG mod = entree y a limiter
            ; %3 %4 %5 : (r13), (r14), (r15)
            ; pseudo constantes
            xor r13, r13
            mov r14, DIMENSION_X
            mov r15, DIMENSION_Y
            ; limitations min/max sur x
            cmp %1, r13
            cmovl %1, r13
            cmp %1, r14
            cmovg %1, r14
            ; limitation min/max sur y
            cmp %2, r13
            cmovl %2, r13
            cmp %2, r15
            cmovg %2, r15
            ; fin
            %endmacro
        %macro construire_numero_de_x_y_FS 2 ; construction d'un numero de portion a partir des coordonnees x et y
            ; %1 REG/IMM fix = entree coordonnee x
            ; %2 REG mod = entree coordonnee y / sortie numero de portion
            ; construction
            shl %2, BITS_POUR_X
            add %2, %1
            ; fin
            %endmacro
    ; macros pour appels de fonctions (hors boucle principale)
        %macro call_datapush_style 1-*
                ; pre-calage eventuel de la pile
                %if %0 % 2 == 0
                sub rsp, 8
                %endif
                ; pushes
                %assign i %0-1
                %rotate 1
                %rep i
                push %1
                %rotate 1
                %endrep
                ; the call
                call %1
                ; re-calage des pushes de la pile
                %assign i (%0-1)*8
                add rsp, i
                ; re-calage eventuel de la pile
                %if %0 % 2 == 0
                add rsp, 8
                %endif
            %endmacro
        %macro call_winapi64_style 1-*
            ; sauvegarde des registres
            push rbx
            push rcx
            push rdx
            push r8
            push r9
            push r10
            push r11
            push r12
            push r13
            push r14
            push r15
            ; nom de la commande -> s
                %define s %1
            ; nombre de premiers parametres -> n
                %assign n %0-1
                %if n > 4
                %assign n 4
                %endif
            ; nombre de parametres au dela de 4 -> p
                %assign p %0-5
                %if p < 0
                %assign p 0
                %endif
            ; valeurs 4 premiers parametres éventuels
                %if n >= 1
                mov rcx, %2
                %endif
                %if n >= 2
                mov rdx, %3
                %endif
                %if n >= 3
                mov r8, %4
                %endif
                %if n >= 4
                mov r9, %5
                %endif
            ; pre-calage éventuel de la pile selon le nombre d'arguments au dela de 4
                %if p % 2 == 0
                sub rsp, 8
                %endif
            ; parametres en push en partant de la fin
                %if p > 0
                %rep p
                %rotate -1
                push %1
                %endrep
                %endif
            ; shadow space + call
                sub rsp, 32
                call s
                add rsp, 32
            ; reposition stack
                %assign i p*8
                add rsp, i
            ; recalage éventuel de la pile selon le nombre d'arguments au-delà de 4
                %if p % 2 == 0
                add rsp, 8
                %endif
            ; récupération des registres sauvegardés
                pop r15
                pop r14
                pop r13
                pop r12
                pop r11
                pop r10
                pop r9
                pop r8
                pop rdx
                pop rcx
                pop rbx
            ; fin
            %endmacro
    */
// #-------------------------------- c : defines --------------------------------------- CONSTANTES EQU + TESTS
    /*
    ; pour tests reseau
        MODE_RECEPTION: equ TRUE
        MODE_EXPEDITION: equ TRUE
        IP_0: equ IP_PH_ETH
        IP_1: equ IP_DO_ETH
        IP_2: equ IP_VA_WIFI
        IP_65000: equ IP_BOX_ETH

        IP_BOX_ETH: EQU 192*I1 + 168*I2 + 1*I3 + 1*I4 ; 192.168.1.1
        IP_PH_ETH: EQU 192*I1 + 168*I2 + 1*I3 + 124*I4 ; 192.168.1.124
        IP_DO_ETH: EQU 192*I1 + 168*I2 + 1*I3 + 21*I4 ; 192.168.1.21
        IP_VA_ETH: EQU 192*I1 + 168*I2 + 1*I3 + 34*I4 ; 192.168.1.34
        IP_A2I_WIFI: EQU 192*I1 + 168*I2 + 1*I3 + 26*I4 ; 192.168.1.26
        IP_VA_WIFI: EQU 192*I1 + 168*I2 + 1*I3 + 16*I4 ; 192.168.1.16
    ; booleens
        TRUE: equ 1
        FALSE: equ 0
    ; dimensionnement utilisateur (portions)
        */
        #define DIMENSION_X 512
        #define DIMENSION_Y 256
        #define DIMENSION_Z 32
        //
        #define FX 1
        #define FY DIMENSION_X
        #define FZ DIMENSION_X*DIMENSION_Y
        //
        #define USERLA_NB_P DIMENSION_X*DIMENSION_Y*DIMENSION_Z
        #define PACKID_NB_P (1ULL << 16)
        /*
        ; x
        BITS_POUR_X: equ 9
        DIMENSION_X: equ (1<<BITS_POUR_X) ; 512
        DECALAGE_X: equ 0
        MASQUE_X: equ (1<<DECALAGE_X)*((1<<BITS_POUR_X)-1)
        ; y
        BITS_POUR_Y: equ 8
        DIMENSION_Y: equ (1<<BITS_POUR_Y) ; 256
        DECALAGE_Y: equ BITS_POUR_X
        MASQUE_Y: equ (1<<DECALAGE_Y)*((1<<BITS_POUR_Y)-1)
        ; z
        BITS_POUR_Z: equ 5
        DIMENSION_Z: equ (1<<BITS_POUR_Z) ; 32
        DECALAGE_Z: equ BITS_POUR_X+BITS_POUR_Y
        MASQUE_Z: equ (1<<DECALAGE_Z)*((1<<BITS_POUR_Z)-1)
        ;
        FX: equ 1 ; non utilisé
        FY: equ DIMENSION_X
        FZ: equ DIMENSION_X*DIMENSION_Y
        FACTU: equ (1<<32)
        ;
    ; dimensionnement adressage complet (en bits)
        ; +----------------------------+
        ; |       FullGA(20+28)        | -> must be <= 48 bits (6 bytes) ; system fixed
        ; +-------------+--------------+
        ; | PackId(20/) | LocMax(/28)  | -> the two must be <= 32 bits (4 bytes) ; controller's decision
        ; |             |-----+--------+
        ; |             | N/U | UserLA | -> bits for x*y*z <= LocMax bits ; user's decision
        ; +-------------+-----+--------+
        ; 
        FULLGA_BITS: equ 48 ; 6 bytes ; same for all ; maxi = 64 - bits pour taille des portions ; 32bytes=>5bits=>64-5=59max
        FULLGA_NB_P: equ (1<<FULLGA_BITS)
        FULLGA_MASK: equ FULLGA_NB_P-1
        ;
        PACKID_BITS: equ FULLGA_BITS-LOCMAX_BITS
        PACKID_NB_P: equ (1<<PACKID_BITS)
        ;
        LOCMAX_BITS: equ 28 ; locally max / is shifted for packId / same for all / 
        LOCMAX_NB_P: equ (1<<LOCMAX_BITS)
        ;
        USERLA_BITS: equ BITS_POUR_X+BITS_POUR_Y+BITS_POUR_Z ; must be <= LOCMAX_BITS
        USERLA_NB_P: equ (1<<USERLA_BITS) ; = DIMENSION_X*DIMENSION_Y*DIMENSION_Z
        ;
    ; réseau
        WSA_VERSION: equ 0x202
        AF_INET_IPV4: equ 2
        ; SOCK_STREAM: equ 1 ; (par defaut en TCP)
        SOCK_DGRAM: equ 2 ; (par défaut en UDP)
        DEFAULT_PROTOCOL: equ 0
        SOCKET_ADDRESS_LEN: equ 16
    ; erreurs
        WSA_STARTUP_OK: equ 0
        INVALID_SOCKET: equ -1
        SOCKET_ERROR: equ -1
        CLOSE_SOCKET_OK: equ 0
    ; specifique application
        I1: equ 1
        I2: equ 256
        I3: equ 256*256
        I4: equ 256*256*256
        DEST_IP_PORT: equ 0x5000 ; port IP 80 (en big-endian)
        ; DEST_IP_ADDRESS: equ 0x2201A8C0 ; adresse en hexadécimal (192.168.1.34) (bswap eax) - portable Va
        DEST_IP_ADDRESS: equ 192*I1 + 168*I2 + 1*I3 + 1*I4  ; adresse en hexadécimal big-ended (192.168.1.1) (bswap eax) - pour tests
    ; codes de demandes (1 octet ?)
        */
        #define MC_SURCHARGE_DW 0x48435253 // 'SRCH' (little endian)
        /*
        ; MC_SURCHARGE_QW: equ 0x4843525300000000 ; '____SRCH'
        MC_SURCHARGE_QW: equ 0x0000545345540000 ; '__TEST__'
    ; reglages portions
        ; communs
        */
        #define INCREMENT_PULSEURS 100
        #define CHRONO_INITIAL 0
        #define CHRONO_MAXI 65535
        #define CHARGE_INVALIDE -32768
        #define CHARGE_MINI -32767
        #define CHARGE_MAXI +32767
        #define MINI_VALBB -127
        #define MAXI_VALBB +127
        #define MAXI_MASSEBB +255
        /*
        PORTION_ZERO: equ 0 ; portion zéro invalide = pas de portion
        DUREE_REFRACTAIRE: equ 10 ; maxi 255 / selon longueur temporelle maxi des dendrites apicales
        ; pulseurs / pour traitement
        INCREMENT_PULSEURS: equ 100 ; maxi 65535
        CHRONO_INITIAL: equ 0 ; maxi = CHRONO_MAXI
        CHRONO_MAXI: equ 65535
        ; neurones
        */
        #define VITESSE_DECHARGE 0
        /*
        CHARGE_INVALIDE: equ -32768 ; charge invalide (word)
        CHARGE_MINI: equ -32767 ; limite de charge basse (word) (exclusion de la charge invalide)
        CHARGE_MAXI: equ +32767 ; limite de charge haute (word)
        ; increment par surcharge
        VITESSE_DECHARGE: equ 0
    ; valeurs pour réseaux préconfigurés
        */
        #define VS_VALBB 127 // maxi 127
        #define VS_MASBB 255 // maxi 255
        #define VS_PSEUIL 8 // maxi 15
        #define VS_BREFRC 15 // mini = durée maxi de remontée réfractaire (cumul chaine apicale de segments le plus long)
        /*
        VS_VALBB: equ 127 ; maxi 127
        VS_MASBB: equ 255 ; maxi 255
        VS_PSEUIL: equ 8 ; maxi 15
        VS_BREFRC: equ 15 ; mini = durée maxi de remontée réfractaire (cumul chaine apicale de segments le plus long)
    ; réglages bouclages et écritures
        */
        #define DUREE_BOUCLE_MOYENNE_MS 100
        #define DUREE_BOUCLE_LENTE_MS 1000
        /*
        ; caractéristiques de l'ordi
        FREQUENCE_MHZ: equ 2710 ; million de cycles par seconde : en Mcycles/s = cycles/us (mon ordi : 2710 c/us)
        DUREE_DE_CYCLE_PS: equ 1024*1024/FREQUENCE_MHZ ; durée de cycle : en M.us/cycle = ps/cycle (mon ordi : 369 ps)
        ; besoin de bouclage
        DUREE_BOUCLE_MOYENNE_MS: equ 100 ; on sort de la boucle rapide quand on arrive à cette durée
        DUREE_BOUCLE_LENTE_MS: equ 1000 ; on sort de la boucle lente quand on arrive à cette durée
        ; valeurs utiles
        NBCYCLES_BOUCLE_MOYENNE: equ 1000 * DUREE_BOUCLE_MOYENNE_MS * FREQUENCE_MHZ
        NBCYCLES_BOUCLE_LENTE: equ 1000 * DUREE_BOUCLE_LENTE_MS * FREQUENCE_MHZ
        ; dans bouclage moyen : retraçage + messages fenêtre
        ; dans bouclage lent : ecritures console
        ; BOUCLAGE LENT : systematique sauf sur fermeture fenetre
    ; types de portions (0 = non utilisé, 255 = interdit/inutilisable/réservé)
        */
        #define TYPE_SOURCE 0b01001000 // 72 type source de données en mémoire
        #define TYPE_LECTEUR 0b10001000 // 136 type lecteur
        #define TYPE_PULSEUR 0b10010000 // 144 type pulseur

        #define TYPE_SEGMENT_GERME 0b11001000 // 200 type dendrite figé sans dendrite amont
        #define TYPE_SEGMENT_PARTIEL 0b11001001 // 201 type dendrite auquel il manque des dendrites
        #define TYPE_SEGMENT_COMPLET 0b11001010 // 202 type dendrite avec toutes dendrite amont

        #define TYPE_NEURONE_GERME 0b11011000 // 216 type neurone figé sans dendrite ni axone
        #define TYPE_NEURONE_PARTIEL 0b11011001 // 217 type neurone auquel il manque des dendrites et/ou l'axone
        #define TYPE_NEURONE_COMPLET 0b11011010 // 218 type neurone avec toutes dendrites et neurone

        #define TYPE_EXTENSION_GERME 0b11010000 // 208 type axone figé sans destination ni extension
        #define TYPE_EXTENSION_PARTIEL 0b11010001 // 209 type axone auquel il manque la destination ou l'extension
        #define TYPE_EXTENSION_COMPLET 0b11010010 // 210 type axone avec destination et extension
        /*
        ; type opérationnel spécial : 01
        TYPE_SOURCE: equ                0b01_001_000 ; 72 type source de données en mémoire
        ; type opérationnel pourvoyeur d'info : 10
        TYPE_LECTEUR: equ               0b10_001_000 ; 136 type lecteur
        TYPE_PULSEUR: equ               0b10_010_000 ; 144 type pulseur
        ; type opérationnel traitement de l'info : 11
        ; sous-type segment : 001
        TYPE_SEGMENT_GERME: equ         0b11_001_000 ; 200 type dendrite figé sans dendrite amont
        TYPE_SEGMENT_PARTIEL: equ       0b11_001_001 ; 201 type dendrite auquel il manque des dendrites
        TYPE_SEGMENT_COMPLET: equ       0b11_001_010 ; 202 type dendrite avec toutes dendrite amont
        ; sous-type neurone : 011
        TYPE_NEURONE_GERME: equ         0b11_011_000 ; 216 type neurone figé sans dendrite ni axone
        TYPE_NEURONE_PARTIEL: equ       0b11_011_001 ; 217 type neurone auquel il manque des dendrites et/ou l'axone
        TYPE_NEURONE_COMPLET: equ       0b11_011_010 ; 218 type neurone avec toutes dendrites et neurone
        ; sous-type extension : 010
        TYPE_EXTENSION_GERME: equ       0b11_010_000 ; 208 type axone figé sans destination ni extension
        TYPE_EXTENSION_PARTIEL: equ     0b11_010_001 ; 209 type axone auquel il manque la destination ou l'extension
        TYPE_EXTENSION_COMPLET: equ     0b11_010_010 ; 210 type axone avec destination et extension
    ; masques or, and, xor T3 E2 S2 A1
        MASQUE_TYPE_TSx: equ            0b11_111_000
    ; taille et offsets dans portions : OS + ./D/L/P/S/N/E
            */
            #define TAILLE_DES_PORTIONS 32        
            /*
            TAILLE_DES_PORTIONS: equ 32
        ; ----- offsets communs impératifs
            */
            #define OS_TYPESTEV_1 0
            #define OS_NIVACTMP_1 4
            #define OS_ACTIVITE_1 5
            #define OS_CHARG0_2 8
            /*
            OS_TYPESTEV_1:  equ 0 ; offset du type/sous-type/evolution
            OS_NIVACTMP_1:  equ 4 ; offset niveau activité temporelle
            OS_ACTIVITE_1:  equ 5 ; offset de l'activité
            OS_CHARG0_2:    equ 8 ; offset charge 0 en cours
        ; ----- offsets accès mémoire
            */
            #define OSD_LGSEG_1 4
            #define OSD_NBSEG_2 8
            #define OSD_ADDRS_8 16
            /*
            ; commun typese : 1_0
            ; N/D :           3_1
            OSD_LGSEG_1:    equ 4 ; offset de la largeur des segments (en octets / 8 maxi)
            ; N/D :           3_5
            OSD_NBSEG_2:    equ 8 ; offset du nombre de segments
            ; N/D :           6_10
            OSD_ADDRS_8:    equ 16 ; offset de l'adresse de base
            ; N/D :           8_24
        ; ----- offsets lecteurs
            */
            #define OSL_PERSA_1 2
            #define OSL_PERSB_1 3
            #define OSL_INDEX_2 4
            #define OSL_NMBIT_1 6
            #define OSL_BSYNV_1 7
            #define OSL_PAM_N_4 8
            #define OSL_PAM_U_4 12
            #define OSL_DST_N_4 16
            #define OSL_DST_U_4 20
            /*
            ; commun typese : 1_0
            ; N/D :           1_1
            OSL_PERSA_1:    equ 2 ; offset de la persistance actuelle (2)
            OSL_PERSB_1:    equ 3 ; offset de la base de persistance (3)
            OSL_INDEX_2:    equ 4 ; offset de l'index du segment à lire (4)
            OSL_NMBIT_1:    equ 6 ; offset de numéro de bit à lire (6)
            OSL_BSYNV_1:    equ 7 ; offset valeur du bloc de boutons (7)
            OSL_PAM_UN_8:    equ 8 ; offset du numéro complet de portion des paramètres d'accès mémoire
                OSL_PAM_N_4:    equ OSL_PAM_UN_8 ; offset du numéro de portion des paramètres d'accès mémoire / num portion
                OSL_PAM_U_4:    equ OSL_PAM_UN_8 + 4 ; offset du numéro de portion des paramètres d'accès mémoire / utilisateur
            OSL_DST_UN_8:    equ 16 ; offset du numero complet de portion de destination
                OSL_DST_N_4:    equ OSL_DST_UN_8 ; offset du numero de portion de destination / num portion
                OSL_DST_U_4:    equ OSL_DST_UN_8 + 4 ; offset du numero de portion de destination / utilisateur
            ; N/D :           8_24
        ; ----- offsets pulseurs
            */
            #define OSP_CHARGI_2 8
            #define OSP_SEUIL_1 10
            #define OSP_BSYNV_1 11
            // #define OSP_DST_UN_8 16
            #define OSP_DST_N_4 16
            #define OSP_DST_U_4 20
            /*
            ; commun typese : 1_0
            ; N/D :           7_1
            OSP_CHARGI_2:   equ 8 ; offset charge interne
            OSP_SEUIL_1:    equ 10 ; offset puissance de seuil
            OSP_BSYNV_1:    equ 11 ; offset valeur du bloc de boutons
            ; N/D :           4_12
            OSP_DST_UN_8:    equ 16 ; offset du numero complet de portion de destination
                OSP_DST_N_4:    equ OSP_DST_UN_8 ; offset du numero de portion de destination / num portion
                OSP_DST_U_4:    equ OSP_DST_UN_8 + 4 ; offset du numero de portion de destination / utilisateur
            ; N/D :           8_24
        ; ----- offsets segments dendritiques
            */
            #define OSS_TEVOS_1 1
            #define OSS_PSYNA_2 2
            #define OSS_NBRCH_1 6
            #define OSS_CHARG1_2 10
            #define OSS_CHARG2_2 12
            #define OSS_CHARG3_2 14
            #define OSS_SVT_N_4 16
            #define OSS_SVT_U_4 20
            #define OSS_PRSEG_1 24
            /*
            ; commun typese : 1<0
            OSS_TEVOS_1:    equ 1 ; offset de temporisation d'evolution de segment
            OSS_PSYNA_2:    equ 2 ; offset potentiel synaptique restant
            ; Niv act temp    1_4
            ; Rétro act       1_5
            OSS_NBRCH_1:    equ 6 ; offset du nombre de charges internes utilisées
            ; N/D             1_7
            ; commun chrg0 :  2_8
            OSS_CHARG1_2:   equ 10 ; offset charge 1 stockée
            OSS_CHARG2_2:   equ 12 ; offset charge 2 stockée
            OSS_CHARG3_2:   equ 14 ; offset charge 3 stockée
            OSS_SVT_UN_8:    equ 16 ; offset du numero complet de segment/neurone suivant
                OSS_SVT_N_4:    equ OSS_SVT_UN_8 ; offset du numero de segment/neurone suivant / num portion
                OSS_SVT_U_4:    equ OSS_SVT_UN_8 + 4 ; offset du numero de segment/neurone suivant / utilisateur
            OSS_PRSEG_1:    equ 24 ; offset potentiel de segments restants
            ; N/D             7_25
        ; ----- offsets neurones
            */
            #define OSN_CREFR_1 6
            #define OSN_BREFR_1 7
            #define OSN_SEUIL_1 10
            #define OSN_PRAYO_1 12
            #define OSN_PPLAN_1 13
            #define OSN_PAPIC_1 14
            #define OSN_PPANI_1 15
            #define OSN_PAXON_1 22
            #define OSN_ORIENT_1 23
            /*
            ; commun typese : 1_0
            ; N/D             3_1
            ; Niv act temp    1_4
            ; Activité        1_5
            OSN_CREFR_1:    equ 6 ; offset du (dé-)compteur réfractaire
            OSN_BREFR_1:    equ 7 ; offset de la base réfractaire
            ; commun chrg0 :  2_8
            OSN_SEUIL_1:    equ 10 ; offset de la puissance du seuil de charge
            ; N/D             1_11
            OSN_PRAYO_1:    equ 12 ; offset puissance de segments dendritiques rayonnants
            OSN_PPLAN_1:    equ 13 ; offset puissance de segments dendritiques planaires
            OSN_PAPIC_1:    equ 14 ; offset puissance de segments dendritiques apicaux
            OSN_PPANI_1:    equ 15 ; offset puissance de segments dendritiques paniers
            ; N/D             6_16
            OSN_PAXON_1:    equ 22 ; offset puissance de potentiel d'extensions axonales
            OSN_ORIENT_1:   equ 23 ; offset orientation des développements
            ; N/D             8_24
        ; ----- offsets extensions axonales
            */
            // commun type se
            #define OSE_TEVOE_1 1
            // N/D_2 2
            #define OSE_CACTT_1 4
            // Activite 1 5
            #define OSE_BSYNV_1 6
            #define OSE_BSYNM_1 7
            #define OSE_DST_N_4 8
            #define OSE_DST_U_4 12
            #define OSE_ANT_N_4 16
            #define OSE_ANT_U_4 20
            #define OSE_PREXT_1 24
            // N/D_7 25
            /*
            ; commun typese : 1_0
            OSE_TEVOE_1:    equ 1 ; offset de temporisation d'evolution d'extension
            ; N/D             2_2
            OSE_CACTT_1:    equ 4 ; offset compteur d'activité temporelle extension
            ; Activité        1_5
            OSE_BSYNV_1:    equ 6 ; offset de la valeur du bloc de boutons synaptiques
            OSE_BSYNM_1:    equ 7 ; offset de la masse du bloc de boutons synaptiques
            OSE_DST_UN_8:   equ 8 ; offset du numero de portion de destination / num portion
                OSE_DST_N_4:    equ OSE_DST_UN_8 ; offset du numero de portion de destination / num portion
                OSE_DST_U_4:    equ OSE_DST_UN_8 + 4 ; offset du numero de portion de destination / utilisateur
            OSE_ANT_UN_8:    equ 16 ; offset du numero complet de portion antécédente
                OSE_ANT_N_4:    equ OSE_ANT_UN_8 ; offset du numero de portion antécédente / num portion
                OSE_ANT_U_4:    equ OSE_ANT_UN_8 + 4 ; offset du numero de portion antécédente / utilisateur
            OSE_PREXT_1:    equ 24 ; offset potentiel d'extensions restantes
            ; N/D             7_25
    ; taille et offsets dans liste de notes msgStack : ON_
        */
        #define TAILLE_DES_NOTES 32
        #define ON_EMMP_4 0
        #define ON_EMMU_4 4
        #define ON_DESP_4 8
        #define ON_DESU_4 12
        #define ON_DEMC_4 16
        #define ON_DEMP_4 20
        // #define ON_ADRD_8 24 // A transformer en position de l'utilisateur dans users !!!
        /*
        TAILLE_DES_NOTES: equ 32
        ON_EMMT_8:  equ 0 ; numero complet d'émetteur (U+N)
        ON_DEST_8:  equ 8 ; numero complet de destinataire (U+N)
        ON_DEMD_8:  equ 16 ; demande complete (code + paramètres)
        ON_ADRD_8:  equ 24 ; adresse memoire du destinataire (dans users)
        ; taille et offsets dans liste des utilisateurs users : OU_
        */
        #define TAILLE_DES_UTILISATEURS 24
        #define OU_IPAD_4 0
        #define OU_PORT_2 4
        #define OU_VIDE_2 6
        #define OU_NMES_8 8
        #define OU_ADRP_8 16
        /*
        TAILLE_DES_UTILISATEURS: equ 24
        OU_IPAD_4:  equ 0 ; offset de l'adresse IP de l'utilisateur
        OU_PORT_2:  equ 4 ; port ouvert
        OU_VIDE_2:  equ 6 ; à garder vide
        OU_NMES_8:  equ 8 ; nombre de messages pour ce destinataire
        OU_ADRP_8:  equ 16 ; adresse glissante (début) plage de messages dans msgToSnd pour ce destinataire (hors pilote)
    ; taille et offsets dans liste de messages dans msgToSnd : OM_ + ./P/M
        */
        #define TAILLE_DES_MESSAGES 24
        #define OMP_IPAD_4 0
        #define OMP_PORT_2 4
        #define OMP_VIDE_2 6
        #define OMP_ADPS_8 8
        #define OMP_TAIP_8 16
        //
        #define OMM_EMMP_4 0
        #define OMM_EMMU_4 4
        #define OMM_DESP_4 8
        #define OMM_DESU_4 12
        #define OMM_DEMC_4 16
        #define OMM_DEMP_4 20
        /*
            TAILLE_DES_MESSAGES: equ 24
        ; offset dans unité pilote
            OMP_IPAD_4:  equ 0 ; offset de l'adresse IP de l'utilisateur
            OMP_PORT_2:  equ 4 ; port ouvert
            OMP_VIDE_2:  equ 6 ; à garder vide
            OMP_ADPS_8:  equ 8 ; adresse du pilote de la plage suivante
            OMP_TAIP_8:  equ 16 ; taille de la plage utile (en octets)
        ; offset dans unité message
            OMM_EMMT_8:  equ 0 ; numero complet d'émetteur (U+N)
            OMM_DEST_8:  equ 8 ; numero complet de destinataire (U+N)
            OMM_DEMD_8:  equ 16 ; demande complete (code + paramètres)
    ; paramètres graphiques
        */
        #define LARGEUR_FENETRE DIMENSION_X
        #define HAUTEUR_FENETRE DIMENSION_Y
        #define NOMBRE_OCTET_PAR_POINT 3
        #define COMPLEMENT_LIGNE_DWORD (((LARGEUR_FENETRE * NOMBRE_OCTET_PAR_POINT * 8 + 7) / 8) % 4)
        /*
        ; dimensions fenêtre
        LARGEUR_FENETRE: equ DIMENSION_X
        HAUTEUR_FENETRE: equ DIMENSION_Y
        ; codage mémoire / graphique
        NOMBRE_OCTET_PAR_POINT: equ 3
        COMPLEMENT_LIGNE_DWORD: equ ((LARGEUR_FENETRE*NOMBRE_OCTET_PAR_POINT*8+7)/8) % 4
        ; offsets couleurs
        */
        #define ROUG 2
        #define VERT 1
        #define BLEU 0
        /*
        ROUG:   equ 2
        VERT:   equ 1
        BLEU:   equ 0
        ; dimensions viseur
        VISEUR_INT: equ 3
        VISEUR_EXT: equ 10
    ; pour console
        */
        #define POSITION_TABLE_X 50
        #define HAUTEUR_TABLE 23
        #define LONGUEUR_LIGNES 45
        #define NOMBRE_CARS_AVANT 2
        #define NOMBRE_CARS_APRES 2
        /*
        POSITION_TABLE_X: equ 50
        POSITION_TABLE_Y: equ 5
        LONGUEUR_LIGNES: equ 45 ; NOMBRE_CARS_AVANT+NOMBRE_CARS_APRES+20 mini pour accepter les nombres jusqu'à 64 bits / 254 maxi au total
        HAUTEUR_TABLE: equ 19
        NOMBRE_CARS_AVANT: equ 2
        NOMBRE_CARS_APRES: equ 2
        LONGUEUR_NOMBRES: equ 20 + 1 ; 1 pour marge
        REPONSE_LONG_MAX: equ 12
    ; pour lecture de fichier
        */
        #define NOMBRE_MAX_SEGMENTS_TEXTE 65535
        #define LARGEUR_DE_SEGMENT_DE_TEXTE 1 // Number of bytes per segment
        /*
        NOMBRE_MAX_SEGMENTS_TEXTE: equ 65535
        LARGEUR_DE_SEGMENT_DE_TEXTE: equ 1 ; en octets limité à 8 ?
    ; fenetre winapi
        ; general
        SHADOW_SPACE_SIZE: equ 32
        ; constantes winapi pour interface
        STD_INPUT_HANDLE: equ -10 ; dword = 4294967286
        STD_OUTPUT_HANDLE: equ -11 ; dword
        ; constantes winapi pour classe de fenetre
        STYLE_WINDOW: equ CS_BYTEALIGNWINDOW | CS_HREDRAW | CS_VREDRAW
            CS_OWNDC: equ 0x0020
            CS_BYTEALIGNWINDOW: equ 0x2000
            CS_HREDRAW: equ 0x0002
            CS_VREDRAW: equ 0x0001
        COLOR_WINDOW: equ 5
        ; constantes winapi pour creation de la fenetre
        WS_EX_COMPOSITED: equ 0x02000000
        WS_VISIBLE: equ 0x10000000 ; long
        WS_OVERLAPPEDWINDOW: equ WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX
            WS_OVERLAPPED: equ 0x00000000   ; long
            WS_CAPTION: equ 0x00C00000      ; long
            WS_SYSMENU: equ 0x00080000      ; long
            WS_THICKFRAME: equ 0x00040000   ; long
            WS_MINIMIZEBOX: equ 0x00020000  ; long
            WS_MAXIMIZEBOX: equ 0x00010000  ; long
        ; constantes winapi pour gestion fenetre
        SW_SHOW: equ 0x00000005             ; long
        SW_SHOWNORMAL: equ 0x00000001       ; long
        PM_NOREMOVE: equ 0x00000000         ; long
        RDW_INTERNALPAINT: equ 0x0002       ; long
        ; constantes winapi pour lecture de fichier
        GENERIC_READ: equ 0x80000000
        GENERIC_WRITE: equ 0x40000000
        OPEN_EXISTING: equ 3
        CREATE_ALWAYS: equ 2
        FILE_ATTRIBUTE_NORMAL: equ 0x80
        ; constantes winapi pour procedure de fenetre
        ; WM_CREATE: equ 0x0001
        WM_KEYDOWN: equ 0x0100
        WM_SIZE: equ 0x0005
        WM_PAINT: equ 0x000F
        ; WM_COMMAND: equ 0x0111
        ; WM_CLOSE: equ 0x0010
        WM_DESTROY: equ 0x0002
        ; constantes winapi dans procedure de fenetre : dessin
        PS_SOLID: equ 0x0000
        ROP: equ 0x00C000CA
            SRCCOPY: equ -1 ; a utiliser en theorie
            MERGECOPY: equ 0x00C000CA ; fonctionne
            BLACKNESS: equ 0x00000000
        DIB_RGB_COLORS: equ 0x00000000
        BI_RGB: equ 0x00000000
        ; constantes winapi dans procedure de fenetre : clavier
        VK_ESCAPE: equ 0x1B
    */
// section .rodata ; --------------------------------------------------------------------- TEXTES ET TABLES
        /*
        ; lecture du fichier
            */
            #define INPUT_FILE_NAME "textealire.txt"
            #define CERVELET_FILE_NAME "structure.crv"
            /*
            nom_de_fichier_texte: db '.\textealire.txt', 0
        ; fichier de sauvegarde
            nom_de_fichier_sauvegarde: db '.\structure.crv', 0
        ; winapi pour application
            app_instance: db 'Handle process winapi : ', 0
        ; winapi pour fenetre
            windowClassName: db 'Classe de fenetre', 0
            windowName: db 'Fenetre de tracage', 0
            NOM_FENETRE_A0: db 'Types de portions', 0
            NOM_FENETRE_Z1: db 'Valeurs du bloc de boutons', 0
            NOM_FENETRE_E2: db 'Charges et chronos', 0
            NOM_FENETRE_R3: db 'Activation et retro-activation', 0
        ; winapi pour tracage
            drawing_handle: db 'Handle de drawing : ', 0
            DIB_handle: db 'Handle de DIB : ', 0
            DIB_address: db 'Adresse de DIB : ', 0
        ; winapi pour threads reseau
            sendThreadEventName: db 'sendThreadEvent', 0
        ; pseudo-textes
            */
            const char *ligne_de_cadre = "####################################################################################################";
            const char *ligne_vide = "";
            /*
            ligne_de_cadre: db LONGUEUR_LIGNES dup ('#'), 0
            ligne_vide: db '', 0
            interparagraphes_text: db LONGUEUR_LIGNES dup ('#'), 0
            interligne_text: db '----------', 0
        ; textes communs
            */
            const char *texte_numero_de_boucle = "Numero de boucle longue : ";
            const char *texte_duree_de_boucle = "Duree de boucle sur portions : ";
            const char *texte_duree_de_posttrait = "Duree de post traitement : ";
            const char *texte_nombre_d_utilisateurs = "Nombre d utilisateurs : ";
            const char *texte_nombre_de_messages_a_emettre = "Nombre de message to send : ";
            const char *texte_nombre_de_messages_recus = "Nombre de message recus : ";
            const char *texte_nombre_de_cibles_nulles = "Nombre de cible nulles : ";
            const char *texte_nombre_de_cibles_incoherentes = "Nombre de cibles incoherentes : ";
            const char *texte_valeur_de_test = "------ Valeur de test : ";
            const char *texte_erreur_a_voir = "Erreur a voir : ";
            /*
            texte_numero_de_boucle: db 'Numero de boucle longue : ', 0
            texte_duree_de_boucle: db 'Duree de boucle sur portions : ', 0
            texte_duree_de_posttrait: db 'Duree de post traitement : ', 0
            texte_nombre_d_utilisateurs: db 'Nombre d utilisateurs : ', 0
            texte_nombre_de_messages_a_emettre: db 'Nombre de message to send : ', 0
            texte_nombre_de_messages_recus: db 'Nombre de message recus : ', 0
            texte_nombre_de_cibles_nulles: db 'Nombre de cible nulles : ', 0
            texte_nombre_de_cibles_incoherentes: db 'Nombre de cibles incoherentes : ', 0
            texte_valeur_de_test: db '------ Valeur de test : ', 0
            texte_erreur_a_voir: db 'Erreur a voir : ', 0
        ; texte d'erreurs
            texte_erreur_WsaStartup: db 'ERROR : Echec init reseau', 0
            texte_erreur_creationSocket: db 'ERROR : Echec creation socket', 0
            texte_erreur_sendTo: db 'ERROR : Echec envoi message', 0
            texte_erreur_closeSocket: db 'ERROR : Echec cloture socket', 0
        ; textes divers
            texte_chargement: db 'Chargement', 0
            texte_sauvegarde: db 'Sauvegarde', 0
            texte_erreur_longueur: db 'Erreur : longueur differente', 0
            texte_chargement_ok: db 'Ok : chargement effectue', 0
            texte_sauvegarde_ok: db 'Ok : sauvegarde effectue', 0
        ; textes de détail des portions
            */
            const char *portion_nonu_text = "Non utilise : ";
            const char *portion_nnux_text = "Non utilise ........................ x ";
            const char *portion_ind1_text = "Indefini 1 octet : ";
            const char *portion_ind2_text = "Indefini 2 octets : ";
            const char *portion_ind4_text = "Indefini 4 octets : ";
            const char *portion_ind8_text = "Indefini 8 octets : ";
            const char *portion_numr_text = "Numero de la portion : ";
            const char *texte_position_x = "Abscisse x : ";
            const char *texte_position_y = "Ordonnee y : ";
            const char *texte_position_z = "Profondeur z : ";
            /*
            portion_nonu_text: db 'Non utilise : ', 0
            ; textes commun de détails
            portion_numr_text: db 'Numero de la portion : ', 0
            texte_position_x: db 'Abscisse x : ', 0
            texte_position_y: db 'Ordonnee y : ', 0
            texte_position_z: db 'Profondeur z : ', 0
            ; detail portions
            */
            const char *portion_type_text = "Type de portion : ";
                const char *portion_typeD_text = "Type de portion Data : ";
                const char *portion_typeL_text = "Type de portion Lecteur : ";
                const char *portion_typeP_text = "Type de portion Pulseur : ";
                const char *portion_typeS_text = "Type de portion Segment : ";
                const char *portion_typeN_text = "Type de portion Neurone : ";
                const char *portion_typeE_text = "Type de portion Extension : ";
            const char *portion_octt_text = "Octet simple : ";
            const char *portion_actv_text = "Activite propre : ";
            const char *portion_chrg_text = "Charge actuelle : ";
            const char *portion_chrs_text = "Seuil charge : ";
            const char *portion_cref_text = "Compteur refractaire : ";
            const char *portion_bref_text = "Base refractaire : ";
            const char *portion_blcv_text = "Valeur du bloc de boutons : ";
            const char *portion_blcp_text = "Masse du bloc de boutons : ";
            const char *portion_dstn_text = "Portion de destination : ";
            const char *portion_dstu_text = "Utilisateur destination : ";
            const char *portion_ctac_text = "Compteur temporel d activite : ";
            const char *portion_ntac_text = "Niveau temporel d activite : ";
            const char *portion_devo_text = "Decompte d evolution : ";
            /*
            portion_type_text: db 'Type de portion : ', 0
            portion_octt_text: db 'Octet simple : ', 0
            portion_actv_text: db 'Activité propre : ', 0
            portion_chrg_text: db 'Charge actuelle : ', 0
            portion_chrs_text: db 'Seuil charge : ', 0
            portion_cref_text: db 'Compteur refractaire : ', 0
            portion_bref_text: db 'Base refractaire : ', 0
            portion_blcv_text: db 'Valeur du bloc de boutons : ', 0
            portion_blcp_text: db 'Masse du bloc de boutons : ', 0
            portion_dest_text: db 'Portion de destination : ', 0
            portion_dstu_text: db 'Utilisateur destinataire : ', 0
            portion_dstn_text: db 'Num portion destinataire : ', 0
            portion_lias_text: db 'Portion de liaison : ', 0
            portion_ctac_text: db 'Compteur temporel d activite : ', 0
            portion_ntac_text: db 'Niveau temporel d activite : ', 0
            portion_devo_text: db 'Decompte d evolution : ', 0
            ; specifiques accès mémoire
            */
            const char *portion_adrs_text = "Adresse source : ";
            const char *portion_tseg_text = "Taille des segments : ";
            const char *portion_nseg_text = "Nombre de segments : ";
            /*
            portion_adrs_text: db 'Adresse source : ', 0
            portion_tseg_text: db 'Taille des segments : ', 0
            portion_nseg_text: db 'Nombre de segments : ', 0
            ; specifiques lecteurs
            */
            const char *portion_idxl_text = "Index de lecture : ";
            const char *portion_bitl_text = "Numero de bit a lire : ";
            const char *portion_prsa_text = "Persistance actuelle : ";
            const char *portion_prsb_text = "Persistance de base : ";
            const char *portion_pamm_text = "Portion d acces memoire : ";
            /*
            portion_idxl_text: db 'Index de lecture : ', 0
            portion_bitl_text: db 'Numero de bit a lire : ', 0
            portion_prsa_text: db 'Persistance actuelle : ', 0
            portion_prsb_text: db 'Persistance de base : ', 0
            portion_pamm_text: db 'Portion d acces memoire : ', 0
            ; specifiques pulseurs
            */
            const char *portion_chro_text = "Chrono actuel : ";
            const char *portion_chrl_text = "Limite chrono : ";
            const char *portion_psyn_text = "Potentiel synaptique restant : ";
            const char *portion_pseg_text = "Potentiel segments restants : ";
            const char *portion_pext_text = "Potentiel extensions restantes : ";
            /*
            portion_chro_text: db 'Chrono actuel : ', 0
            portion_chrl_text: db 'Limite chrono : ', 0
            ; specifiques segments / extensions
            portion_segs_text: db 'Segment suivant : ', 0
            portion_psyn_text: db 'Potentiel synaptique restant : ', 0
            portion_pseg_text: db 'Potentiel segments restants : ', 0
            portion_pext_text: db 'Potentiel extensions restantes : ', 0
            ; spécifiques neurones
            */
            const char *portion_ornt_text = "Orientation developpements : ";
            const char *portion_pray_text = "Potentiel rayonnant : ";
            const char *portion_ppla_text = "Potentiel planaire : ";
            const char *portion_papi_text = "Potentiel apical : ";
            const char *portion_ppan_text = "Potentiel panier : ";
            const char *portion_paxo_text = "Potentiel axonal : ";
            /*
            portion_ornt_text: db 'Orientation developpements : ', 0
            portion_pray_text: db 'Potentiel rayonnant : ', 0
            portion_ppla_text: db 'Potentiel planaire : ', 0
            portion_papi_text: db 'Potentiel apical : ', 0
            portion_ppan_text: db 'Potentiel panier : ', 0
            portion_paxo_text: db 'Potentiel axonal : ', 0
            ; spécifiques extensions
            */
            const char *portion_antn_text = "Antecedant numero portion : ";
            const char *portion_antu_text = "Antecedant utilisateur : ";
            /*
            portion_eaxo_text: db 'Extension axonale : ', 0
        ; textes menu d'aides général
            */
            const char *help_00 = "All controls with focus on gfx window";
            const char *help_01 = "";
            const char *help_10 = "H : Console writing (rotate)";
            const char *help_11 = "  - Main help ";
            const char *help_12 = "  - Point help";
            const char *help_13 = "  - Timers & chronos";
            const char *help_14 = "  - Details";
            const char *help_15 = "  - Silent";
            const char *help_20 = "Flow control";
            const char *help_21 = "  B : Step by step mode (toggle)";
            const char *help_22 = "  N : Next step (on step by step mode)";
            const char *help_23 = "  P : Quit";
            const char *help_30 = "Load and save";
            const char *help_31 = "  I : Load from structure.crv";
            const char *help_32 = "  O : Save to structure.crv";
            /*
            help_00: db 'All controls with focus on gfx window', 0
            help_01: db '', 0
            help_10: db 'H : Console writing (rotate)', 0
            help_11: db '  - Main help ', 0
            help_12: db '  - Point help', 0
            help_13: db '  - Timers & chronos', 0
            help_14: db '  - Details', 0
            help_15: db '  - Silent', 0
            help_20: db 'Flow control', 0
            help_21: db '  B : Step by step mode (toggle)', 0
            help_22: db '  N : Next step (on step by step mode)', 0
            help_23: db '  P : Quit', 0
            help_30: db 'Load and save', 0
            help_31: db '  I : Load from structure.crv', 0
            help_32: db '  O : Save to structure.crv', 0
        ; textes menu d'aides détails
            */
            const char *help_50 = "Target positionning";
            const char *help_51 = "  V : X plus";
            const char *help_52 = "  X : X minus";
            const char *help_53 = "  C : Y plus";
            const char *help_54 = "  D/F : Y minus";
            const char *help_55 = "  Q/S : Z plus";
            const char *help_56 = "  W : Z minus";
            const char *help_60 = "Graphical windows";
            const char *help_61 = "  A : Draw type of portions";
            const char *help_62 = "  Z : Draw value of synpases blocs";
            const char *help_63 = "  E : Draw load of portions";
            const char *help_64 = "  R : Draw retro / activity";
            /*
            help_50: db 'Target positionning', 0
            help_51: db '  V : X plus', 0
            help_52: db '  X : X minus', 0
            help_53: db '  C : Y plus', 0
            help_54: db '  D/F : Y minus', 0
            help_55: db '  Q/S : Z plus', 0
            help_56: db '  W : Z minus', 0
            help_60: db 'Graphical windows', 0
            help_61: db '  A : Draw type of portions', 0
            help_62: db '  Z : Draw value of synpases blocs', 0
            help_63: db '  E : Draw load of portions', 0
            help_64: db '  R : Draw retro / activity', 0
        ; table d'exploration
            explorT0_x: db   0
            explorT1_x: db   1,  1,  0, -1, -1, -1,  0,  1
            explorT2_x: db   2,  2,  2,  1,  0, -1, -2, -2, -2, -2, -2, -1,  0,  1,  2,  2
            
            explorT0_y: db   0
            explorT1_y: db   0,  1,  1,  1,  0, -1, -1, -1
            explorT2_y: db   0,  1,  2,  2,  2,  2,  2,  1,  0, -1, -2, -2, -2, -2, -2, -1
    */
// section .data ; ----------------- c : variables globales ------------------------------ VARIABLES INITALISEES
    // Type de variables
    typedef struct {
        uint16_t x;
        uint16_t y;
        uint16_t z;
    } id_prt;
    // Variables globales
    id_prt p_port;
    id_prt p_srce;
    id_prt p_dest;
    id_prt p_suiv;
    id_prt p_antc;
    id_prt null_port = {0, 0, 0};
    // gestion
    uint8_t mode_pas_a_pas = 1;
    uint8_t affichageConsole = 1;
    // fenetre
    HANDLE instanceHandle = NULL;
    const char *className = "GraphicWindowClass";
    HWND gWindowHandle = NULL;
    // bitmap
    BITMAPINFO bitmapInfo;
    uint8_t *bitmapData = NULL;
    // lecture fichier texte
    uint8_t *rawData_Texte = NULL;
    size_t longueur_Texte = 0;
    // ecritures console
    char ligneAEcrire[LONGUEUR_LIGNES + 1];
    // decompte du temps
    clock_t timer_avant_rapides;
    clock_t timer_en_fin_de_rapides;
    double duree_cumul_des_rapides;
    // double nombre_de_cycles;
    clock_t timer_avant_moyennes;
    clock_t timer_en_fin_de_moyennes;
    double duree_cumul_des_moyennes;
    clock_t debut_cycle;
    clock_t fin_cycle;
    double duree_cycle;
    // portions
    // alignas(8) uint8_t crvDatas[DIMENSION_Z][DIMENSION_Y][DIMENSION_X][TAILLE_DES_PORTIONS]; // C11 compiler
    uint8_t crvDatas[DIMENSION_Z][DIMENSION_Y][DIMENSION_X][TAILLE_DES_PORTIONS] __attribute__((aligned(8))) = {0}; // GCC & Clang
    // uint8_t *portions = NULL; // Memory for portions
    /* Assembly
            ouiReception: db MODE_RECEPTION
            ouiExpedition: db MODE_EXPEDITION
        ; systeme
            ErrorsBreak: db 1
        */
    // ; interface graphique
    uint8_t modeTracage = 2;
    uint16_t visualize_x = 100;
    uint16_t visualize_y = 100;
    uint16_t visualize_z = 0;
    /* Assembly
            modeTracage: db 2
            visualize_x: dq 5
            visualize_y: dq 5
            visualize_z: dq 31
        ; interface console
            affichageConsole: db 1
            positionDeRetour_xCurseur: dw 0
            positionDeRetour_yCurseur: dw 0
            affichageGraphique: db 1
            mode_pas_a_pas: db 1
        ; plages de textes modifiables
            texte_a_ecrire: db LONGUEUR_LIGNES dup ('?'), 0x0A
            nombre_text: db LONGUEUR_NOMBRES dup ('-') ; pourrait être mis en resb
        ; pour reseau
            sendThreadEnded: db 0
            messageDeTest: db 'CECI EST UN TEST'
            MESSAGE_DE_TEST_LEN: equ $ - messageDeTest
        */
// --------------------------------- c : prototypes de fonctions
    // Fonctions prototypes
        // fichiers et donnees
        void ReadInputFile();
        void InitializeData();
        void ProcessPortions();
        // console
        void SetupConsole();
        void ecrire_ligne_4(HANDLE consoleHandle, const char *texte, int64_t nombre, SHORT positionX, SHORT positionY);
        void ecrire_messages_console();
        void CleanupConsole();
        // creation intiale de reseaux
        void sub_creer_portion_datasource_4(id_prt port, uint64_t addr, uint16_t nmbr, uint8_t larg);
        // void sub_creer_portion_lecteur_7(id_prt port, id_prt srce, uint16_t idxlect, uint8_t bitlect, uint8_t persisb, int8_t valblocb, id_prt dest);
        // void sub_creer_portion_pulseur_4(id_prt port, uint8_t seuil, uint8_t valblocb, id_prt dest);
        // void sub_creer_segment_dendritique_6(id_prt port, uint8_t typeSegm, uint8_t nivTempA, uint8_t potSegR, uint16_t synDispo, id_prt dest);
        // sub_creer_portion_neurone_10 --- pourquoi pas nécessaire ???
        // void sub_creer_extension_axonale_8(id_prt port, uint8_t typeExtA, id_prt ante, uint8_t nivTempA, uint8_t potAxoR, uint8_t masBlocB, int8_t valBlocB, uint32_t udest, id_prt dext);
        void sub_creer_reseau(id_prt port, uint8_t type, id_prt ante, uint32_t usucc, id_prt succ, uint16_t nb_x, uint16_t nb_y, uint16_t nb_z, char pas);
        // pour portions
        void verifier_portions();
        void ajouter_note(uint32_t dmdr_usr, uint32_t dmdr_prt, uint32_t demande, uint32_t parametre, uint32_t dest_usr, uint32_t dest_prt);
        // fenetre et graphisme
        int RegisterWindowClass(HINSTANCE hInstance);
        void CreateGraphicWindow(HINSTANCE hInstance);
        // communication reseau
        DWORD WINAPI sendThread(LPVOID param);
        DWORD WINAPI recvThread(LPVOID param);
        // fenetre et tracage
        LRESULT CALLBACK WindowProc(HWND windowHandle, UINT uMsg, WPARAM wParam, LPARAM lParam);
        void InitializeBitmap();
        void retracer_la_fenetre();
        // conversion
        uint32_t xyz_to_u32(id_prt id);
        id_prt u32_to_xyz(uint32_t num);
// section .bss ; ------------------------------------------------------------------------ RESERVATIONS MEMOIRE
    char users[PACKID_NB_P][TAILLE_DES_UTILISATEURS];
    char msgStack[USERLA_NB_P + 1][TAILLE_DES_NOTES];
    char msgToSnd[USERLA_NB_P + 1][TAILLE_DES_MESSAGES];
    /* Assembly
        ; moteur
            align 64
            ; cerveau
            portions: times USERLA_NB_P resb TAILLE_DES_PORTIONS
            ; notes pour messages
            msgStack: times USERLA_NB_P + 1 resb TAILLE_DES_NOTES
            ; reseau
            users: times PACKID_NB_P resb TAILLE_DES_UTILISATEURS
            ; messages
            msgToSnd: times USERLA_NB_P + 1 resb TAILLE_DES_MESSAGES
        */
    uint32_t myPackId;
    uint32_t PackId_1;
    uint32_t PackId_2;
    uint64_t prochaine_note;
    uint64_t nombreDUtilisateurs;
    uint64_t notesATraiter;
    uint64_t nombreDePaquetsAEmettre;
    uint64_t pointeurDUnite;
    uint32_t destinataireNote;
    uint64_t emplacementMessage;
    uint64_t nombreDeNotes;
    uint64_t valeurDeTest;
    /* Assembly
        ; utilisateur
            myPackId: resd 1
            myPackIdShifted: resq 1
        ; sauvegarde de registres (fast)
            outputHandle: resq 1
            r10_backup: resq 1
            r11_backup: resq 1
            r13_backup: resq 1
        ; sauvegarde de tous les registres (slow)
            registersMemory: resq 16
        ; gestion des threads
            etatSendThread: resb 1
        ; pour messages
            nombreDUtilisateurs: resq 1
            valeurDeTest: resq 1
            erreurAVoir: resq 1
        */
    uint64_t numero_de_boucle_lente;
    uint64_t dureeDePostTraitement;
    uint64_t erreurCibleNulle;
    uint64_t erreurCibleIncoherente;
    /* Assembly
        ; pour deroulement et duree des boucles
            numero_de_boucle_lente: resq 1
            procCycleCntr_avant_bouclage_moyen: resq 1
            procCycleCntr_avant_bouclage_rapide: resq 1
            procCycleCntr_avant_bouclage_portions: resq 1
            procCycleCntr_apres_bouclage_portions: resq 1
            ; vitesse
            dureeDeTraitement: resq 1
            dureeDePostTraitement: resq 1
            ; erreurs
            erreurCibleNulle: resq 1
            erreurCibleIncoherente: resq 1
            erreurReception: resq 1
            ; communication
            nombreDeMessageAEmettre: resq 1
            nombreDeMessagesRecus: resq 1
        ; pour lecture fichiers
            Filehandle: resq 1
            longueurFichier: resq 1
            ; fichier texte
            rawData_Texte: resb NOMBRE_MAX_SEGMENTS_TEXTE * LARGEUR_DE_SEGMENT_DE_TEXTE
            longueur_Texte: resw 1
        ; interface
            ; positionnement console A_FAIRE
            positionCurseur: resw 2
            csbi: resb 22 ; 2+2/2+2/2/2+2+2+2/2+2
            ; dwSize(4)/dwCursorPosition(4)/wAttributes(2)/srWindow(8)/dwMaximumWindowSize(4)
            written: resd 1
            ; pour utilitaires et console
            reponse_text: resb REPONSE_LONG_MAX
            input_record: resw 64 ; 2/...
            reponse_long_ret: resd 1 ; nombre de caracteres ecrits/lus ou d'evenements console/clavier
        ; fenetre windows
            ; process principal
            Instance: resq 1
            ; pour classe de fenetre
            OurWindowclass: resb 80 ; 4/4/8/4/4/8/8/8/8/8/8/8 (in)
            ; OurWindowclass=cbSize(4)/style(4)/lpfnWndProc(8)/cbClsExtra(4)/cbWndExtra(4)
            ; hInstance(8)/hIcon(8)/hCursor(8)/hbrBackground(8)/lpszMenuName(8)/lpszClassName(8)/hIconSm(8)
            ClassAtom: resq 1
            ; pour fenetre
            Windowhandle: resq 1
            WindowMessage: resq 44 ; 8/4/8/8/4/4+4/4
            ; WindowMessage=hwnd(8)/message(4)/wParam(8)/lParam(8)/time(4)/pt(4+4)/lPrivate(4)
            rectDef: resd 4 ;
            ; pour draw context
            PaintStruct: resb 72 ; 8/4/4+4+4+4/4/4/8+8+8+8/4? (out)
            ; PaintStruct=hdc(8)/fErase(4)/rcPaintx1(4)/rcPainty1(4)/rcPaintx2(4)/rcPainty2(4)
            ; fRestore(4)/fIncUpdate(4)/rgbReserved(8)/rgbReserved(8)/rgbReserved(8)/rgbReserved(8)/Padding?(4)
            DrawingCtxHandle: resq 1
            DrawingCtxHandle2: resq 1
            ; pour tracage
            BitmapHandle2: resq 1
            OldBitmapHandle2: resq 1
            bitMapInfos: resb 40 ; 4/4/4/2/2/4/4/4/4/4/4 (in)
            pBitDataAdress: resq 1
            pBitDataHandle: resq 1
            */
    #define BUFFERINLEN 512
    #define SOCKET_ADDRESS_LEN sizeof(struct sockaddr_in)
    #define WSA_VERSION MAKEWORD(2, 2)
    #define AF_INET_IPV4 AF_INET
    #define DEFAULT_PROTOCOL 0
    // reseau
    WSADATA wsaDataStruct;
    // pour envoi
    struct sockaddr_in toDestSocket = {0};
    SOCKET sendSocketHandle;
    HANDLE sendThreadEventHandle;
    int sendThreadEnded = FALSE;
    // pour reception
    // SOCKET recvSocketHandle;
    // char bufferIn[BUFFERINLEN];
    /* Assembly
        ; pour threads reseau
            wsaDataStruct: resb 400

            sendThreadEventHandle: resq 1
            sendSocketFileDescriptor: resq 1
            toDestSocket: resb SOCKET_ADDRESS_LEN

            recvSocketHandle: resq 1
            localInSocket: resb SOCKET_ADDRESS_LEN
            BUFFERINLEN: equ 512
            bufferIn: resb BUFFERINLEN

            localInSocketR:
                family: resw 1
                port: resw 1
                address: resd 1
                zeros: resb SOCKET_ADDRESS_LEN-8

        ; for TCP - NOT USED
            conxSocketFileDescriptor: resq 1
        */
int main() {
    myPackId = 0; // utilisateur
    PackId_1 = 65000; // destinataire
    PackId_2 = 2; // destinataire
    instanceHandle = GetModuleHandleA(NULL); // récupération du handle d'instance
    /* Assembly
        // section .text ; ------------------------------------------------------------------------------- DEBUT DU CODE        global main
        main:
            ; alignement de la pile
            sub rsp, 8
            ; call_winapi64_style GetStdHandle, STD_OUTPUT_HANDLE
            ; mov [outputHandle], rax
            ; utilisateur
            mov eax, 0
            mov [myPackId], eax
            shl rax, 32
            mov [myPackIdShifted], rax
        */
    
    InitializeData(); // Initialiser la structure de portions
// #-------------------------------------------------------------------------------------------- CREATION FENETRE ET ASSOCIES
    // Creer et enregistrer la classe de fenetre
    if (!RegisterWindowClass(instanceHandle)) {
        return -1;
    }
    /* Assembly
        ; recuperation de l'instance actuelle
            xor rcx, rcx
            sub rsp, SHADOW_SPACE_SIZE
            call GetModuleHandleA
            add rsp, SHADOW_SPACE_SIZE
            mov qword [Instance], rax
        ; construction de la classe de fenetre a l'adresse [OurWindowclass]
            mov dword [OurWindowclass], 80                      ; cbSize (4)
            mov dword [OurWindowclass+4], STYLE_WINDOW          ; style (4)
            lea rax, [WindowProc]
            mov qword [OurWindowclass+8], rax                   ; lpfnWndProc (8)
            mov dword [OurWindowclass+16], 0                    ; cbClsExtra (4)
            mov dword [OurWindowclass+20], 0                    ; cbWndExtra (4)
            mov rax, qword [Instance]
            mov qword [OurWindowclass+24], rax                  ; hInstance (8)
            mov qword [OurWindowclass+32], 0                    ; hIcon (8)
            mov qword [OurWindowclass+40], 0                    ; hCursor (8)
            mov qword [OurWindowclass+48], COLOR_WINDOW         ; hbrBackground (8)
            mov qword [OurWindowclass+56], 0                    ; lpszMenuName (8)
            lea rax, [windowClassName]
            mov qword [OurWindowclass+64], rax                  ; lpszClassName (8)
            mov qword [OurWindowclass+72], 0                    ; hIconSm (8)
        ; enregistrement de la classe de fenetre
            lea rcx, [OurWindowclass]
            sub rsp, SHADOW_SPACE_SIZE
            call RegisterClassExA
            add rsp, SHADOW_SPACE_SIZE
            mov qword [ClassAtom], rax
        */
    // Creer la fenetre graphique
    CreateGraphicWindow(instanceHandle);
    /* Assembly
        ; creation de la fenetre
            ; 4 premiers parametres
            mov ecx, WS_EX_COMPOSITED                           ; dwExStyle
            lea rdx, [windowClassName]                          ; lpClassName (atome de classe)
            lea r8, [windowName]                                ; lpWindowName
            mov r9d, WS_OVERLAPPEDWINDOW                        ; dwStyle
            ; parametres suivants en commencant par la fin
            push qword 0                                        ; lpParam
            push qword [Instance]                               ; hInstance
            push qword 0                                        ; hMenu
            push qword 0                                        ; hWndParent
            push qword HAUTEUR_FENETRE                          ; nHeight
            push qword LARGEUR_FENETRE                          ; nWidth
            push qword 100                                      ; Y
            push qword 100                                      ; X
            ; appel
            sub rsp, SHADOW_SPACE_SIZE
            call CreateWindowExA
            add rsp, SHADOW_SPACE_SIZE
            add rsp, 64
            mov qword [Windowhandle], rax
        */    // afficher la fenetre
    ShowWindow(gWindowHandle, SW_SHOWNORMAL);
    /* Assembly
        ; affichage de la fenetre
            mov   rcx, qword [Windowhandle]                     ; hWnd
            mov   rdx, SW_SHOW                                  ; nCmdShow
            sub   rsp, SHADOW_SPACE_SIZE
            call  ShowWindow
            add   rsp, SHADOW_SPACE_SIZE
        */    // Mettre a jour la fenetre
    UpdateWindow(gWindowHandle);
    /* Assembly
            ; mise a jour de la fenetre
            mov   rcx, qword [Windowhandle]                     ; hWnd
            sub   rsp, SHADOW_SPACE_SIZE
            call  UpdateWindow
            add   rsp, SHADOW_SPACE_SIZE
        */
    // Initialiser le bitmap pour le tracage
    InitializeBitmap();
    /* Assembly
        ; limites de tracage initiales
            mov dword [rectDef + 0], 0                          ; left
            mov dword [rectDef + 4], HAUTEUR_FENETRE            ; top
            mov dword [rectDef + 8], LARGEUR_FENETRE            ; right
            mov dword [rectDef + 12], 0                         ; bottom
        ; structure bitMapInfo (40)
            ; bmiHeader (40) :
            mov dword [bitMapInfos + 0 ], 40                        ; biSize            DWORD / 4
            mov dword [bitMapInfos + 4 ], LARGEUR_FENETRE           ; biWidth           LONG / 4
            mov dword [bitMapInfos + 8 ], -HAUTEUR_FENETRE          ; biHeight          LONG / 4
            mov word [bitMapInfos + 12 ], 1                         ; biPlanes          WORD / 2
            mov word [bitMapInfos + 14 ], NOMBRE_OCTET_PAR_POINT*8  ; biBitCount        WORD / 2
            mov dword [bitMapInfos + 16 ], BI_RGB                   ; biCompression     DWORD / 4
            mov dword [bitMapInfos + 20 ], 0                        ; biSizeImage       DWORD / 4 = 1250x3=3750=>3752x800
            mov dword [bitMapInfos + 24 ], 0                        ; biXPelsPerMeter   LONG / 4
            mov dword [bitMapInfos + 28 ], 0                        ; biYPelsPerMeter   LONG / 4
            mov dword [bitMapInfos + 32 ], 0                        ; biClrUsed         DWORD / 4
            mov dword [bitMapInfos + 36 ], 0                        ; biClrImportant    DWORD / 4
        */
// #-------------------------------------------------------------------------------------------- LECTURE DU FICHIER D'ENTREES
    // Read input file
    ReadInputFile();
    /* Assembly
        ; ouverture du fichier
            lea rcx, [nom_de_fichier_texte] ;       LPCSTR      lpFileName
            call_winapi64_style CreateFileA, rcx, GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL
            mov [Filehandle], rax
        ; recuperation de la taille du fichier
            mov rcx, [Filehandle] ;                 HANDLE      hFile,
            lea rdx, [longueurFichier] ;            PLARGE_INTEGER lpFileSize
            call_winapi64_style GetFileSizeEx, rcx, rdx
            mov rbx, [longueurFichier]
        ; limitation de la longueur à lire
            cmp rbx, NOMBRE_MAX_SEGMENTS_TEXTE * LARGEUR_DE_SEGMENT_DE_TEXTE
            jbe suite_lecture_fichier_texte
                mov rbx, NOMBRE_MAX_SEGMENTS_TEXTE * LARGEUR_DE_SEGMENT_DE_TEXTE
            suite_lecture_fichier_texte:
        ; calculer et enregistrer le nombre de caracteres
            mov rax, rbx
            ; A_FAIRE : diviser la longueur par LARGEUR_DE_SEGMENT_DE_TEXTE
            ;           au lieu de limiter à NOMBRE_MAX_SEGMENTS_TEXTE
            cmp rax, NOMBRE_MAX_SEGMENTS_TEXTE
            jbe suite_lecture_fichier_texte_2
                mov rax, NOMBRE_MAX_SEGMENTS_TEXTE
            suite_lecture_fichier_texte_2:
            mov [longueur_Texte], ax
        ; lecture du contenu
            mov rcx, [Filehandle] ;                 HANDLE      hFile,
            lea rdx, [rawData_Texte] ;              LPVOID      lpBuffer,
            call_winapi64_style ReadFile, rcx, rdx, rbx, 0, 0
        ; fermeture du fichier
            mov rcx, [Filehandle] ;                 HANDLE      hObject
            call_winapi64_style CloseHandle, rcx
        */
// #-------------------------------------------------------------------------------------------- INITIALISATION DU CERVEAU
    // =============================================== RAZ PORTIONS
    // RAZ Portions already done at creation
    /* Assembly
        lea rdx, [portions]
        mov rax, USERLA_NB_P*TAILLE_DES_PORTIONS
        boucle_init_portions:
            mov [rdx], byte 0
            inc rdx
            sub rax, 1
        jnz boucle_init_portions
        */
    // ; =============================================== COUCHE 0 (Data, Lecteurs, Pulseurs)
        // ; --------------- source de données texte en 1,0,0 POSITION IMPERATIVE => pour chargement de structure.crv
            p_port = (id_prt){1, 0, 0};
            uint64_t adresse = (uint64_t)(uintptr_t)rawData_Texte;
            sub_creer_portion_datasource_4(p_port, adresse, longueur_Texte, 1);
            /* Assembly
                lea rdx, [rawData_Texte]
                movzx rax, word [longueur_Texte]
                call_datapush_style sub_creer_portion_datasource_4, \
                1+0*FY+0*FZ, rdx,     rax,             1
                ; Numero,      Adresse, NombreDElements, LongueurElement
                */
        // ; --------------- réseau de lecteurs à gauche ->1
            p_port = (id_prt){5, 5, 0};
            p_srce = (id_prt){1, 0, 0};
            p_dest = (id_prt){5, 5, 1};
            sub_creer_reseau(p_port, TYPE_LECTEUR, p_srce, myPackId, p_dest, 250, 200, 1, 1);
            /* Assembly
                ; creer macro : params_lecteurs_pnsd myPackId, 2+2*FY+0*FZ, 1+0*FY+0*FZ, 2+2*FY+1*FZ
                defineMySource 1+0*FY+0*FZ ; numero (1ère ?) source
                defineMyPortion 5+5*FY+0*FZ ; numero 1ère portion
                defineMyDestin 5+5*FY+1*FZ ; numero 1ère destination
                call_datapush_style sub_creer_reseau, \
                rcx, rdx, TYPE_LECTEUR, rbx, 250, 200, 1,  1
                ;Src Dst  Type          NumP n/X  n/Y  n/Z Pas
                */
        // ; --------------- réseau de pulseurs à droite ->1
            p_port = (id_prt){260, 5, 0};
            p_dest = (id_prt){260, 5, 1};
            sub_creer_reseau(p_port, TYPE_PULSEUR, null_port, myPackId, p_dest, 250, 250, 1, 1);
            /* Assembly
                defineMyPortion 260+5*FY+0*FZ
                defineMyDestin 260+5*FY+1*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, TYPE_PULSEUR, rbx, 250, 250, 1,  1 
                ;Dst Type          NumP n/X  n/Y  n/Z Pas
                */
    // ; =============================================== CREATIONS PAR RESEAUX (couches 1 à 31)
        // ; -------------------------------------------------------------------------------- Pile simple
        // ; 1 --------------- réseau de neurones
            p_port = (id_prt){5, 5, 1};
            sub_creer_reseau(p_port, TYPE_NEURONE_COMPLET, null_port, myPackId, null_port, 505, 250, 1, 1);
            /* Assembly
                defineMyPortion 5+5*FY+1*FZ
                call_datapush_style sub_creer_reseau, \
                TYPE_NEURONE_COMPLET, rbx, 505, 250, 1,  1
                ; Type                NumP n/X  n/Y  n/Z Pas
                */
        // ; 2 --------------- réseau d'extensions axonales 1-> ->3
            p_port = (id_prt){5, 5, 2};
            p_srce = (id_prt){5, 5, 1};
            p_dest = (id_prt){5, 5, 3};
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, myPackId, p_dest, 505, 250, 1, 1);
            /* Assembly
                defineMySource 5+5*FY+1*FZ
                defineMyPortion 5+5*FY+2*FZ
                defineMyDestin 5+5*FY+3*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 505, 250, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; 3 --------------- réseau de segments dendritiques ->4
            p_port = (id_prt){5, 5, 3};
            p_dest = (id_prt){5, 5, 4};
            sub_creer_reseau(p_port, TYPE_SEGMENT_COMPLET, null_port, myPackId, p_dest, 505, 250, 1, 1);
            /* Assembly
                defineMyPortion 5+5*FY+3*FZ
                defineMyDestin 5+5*FY+4*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, TYPE_SEGMENT_COMPLET, rbx, 505, 250, 1,  1
                ;Svt Type                  NumP n/X  n/Y  n/Z Pas
                */
        // ; 4 --------------- réseau de neurones
            p_port = (id_prt){5, 5, 4};
            sub_creer_reseau(p_port, TYPE_NEURONE_COMPLET, null_port, myPackId, null_port, 505, 250, 1, 1);
            /* Assembly
                defineMyPortion 5+5*FY+4*FZ
                call_datapush_style sub_creer_reseau, \
                TYPE_NEURONE_COMPLET, rbx, 505, 250, 1,  1
                ; Type                NumP n/X  n/Y  n/Z Pas
                */
        // ; -------------------------------------------------------------------------------- Diffusion 2x2
        // ; 5 --------------- réseau d'extensions axonales 4-> ->9
            p_port = (id_prt){5, 5, 5};
            p_srce = (id_prt){5, 5, 4};
            p_dest = (id_prt){5, 5, 9};
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, myPackId, p_dest, 505, 250, 1, 1);
            /* Assembly
                defineMySource 5+5*FY+4*FZ
                defineMyPortion 5+5*FY+5*FZ
                defineMyDestin 5+5*FY+9*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 505, 250, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; 6 --------------- réseau d'extensions axonales 4-> ->9
            p_port = (id_prt){5, 5, 6};
            p_srce = (id_prt){5, 5, 4};
            p_dest = (id_prt){5, 5, 9};
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, myPackId, p_dest, 505, 250, 1, 1);
            /* Assembly
                defineMySource 5+5*FY+4*FZ
                defineMyPortion 5+5*FY+6*FZ
                defineMyDestin 6+5*FY+9*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 504, 250, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; 7 --------------- réseau d'extensions axonales 4-> ->9
            p_port = (id_prt){5, 5, 7};
            p_srce = (id_prt){5, 5, 4};
            p_dest = (id_prt){5, 5, 9};
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, myPackId, p_dest, 505, 250, 1, 1);
            /* Assembly
                defineMySource 5+5*FY+4*FZ
                defineMyPortion 5+5*FY+7*FZ
                defineMyDestin 6+6*FY+9*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 504, 249, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; 8 --------------- réseau d'extensions axonales 4-> ->9
            p_port = (id_prt){5, 5, 8};
            p_srce = (id_prt){5, 5, 4};
            p_dest = (id_prt){5, 5, 9};
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, myPackId, p_dest, 505, 250, 1, 1);
            /* Assembly
                defineMySource 5+5*FY+4*FZ
                defineMyPortion 5+5*FY+8*FZ
                defineMyDestin 5+6*FY+9*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 505, 249, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; -------------------------------------------------------------------------------- Neurones simples
        // ; 9 --------------- réseau de neurones
            p_port = (id_prt){5, 5, 9};
            sub_creer_reseau(p_port, TYPE_NEURONE_COMPLET, null_port, myPackId, null_port, 505, 250, 1, 1);
            /* Assembly
                defineMyPortion 5+5*FY+9*FZ
                call_datapush_style sub_creer_reseau, \
                TYPE_NEURONE_COMPLET, rbx, 505, 250, 1,  1
                ; Type                NumP n/X  n/Y  n/Z Pas
                */
        // ; -------------------------------------------------------------------------------- Diffusion 3x3
        // ; 10 --------------- réseau d'extensions axonales 9-> ->19
            p_port = (id_prt){5, 5, 10};
            p_srce = (id_prt){5, 5, 9};
            p_dest = (id_prt){5, 5, 19};
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, myPackId, p_dest, 505, 250, 1, 1);
            /* Assembly
                defineMySource 5+5*FY+9*FZ
                defineMyPortion 5+5*FY+10*FZ
                defineMyDestin 5+5*FY+19*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 505, 250, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; 11 --------------- réseau d'extensions axonales 9-> ->19
            p_port = (id_prt){5, 5, 11};
            p_srce = (id_prt){5, 5, 9};
            p_dest = (id_prt){5, 5, 19};
            /* Assembly
                defineMySource 5+5*FY+9*FZ
                defineMyPortion 5+5*FY+11*FZ
                defineMyDestin 6+5*FY+19*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 504, 250, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; 12 --------------- réseau d'extensions axonales 9-> ->19
            p_port = (id_prt){5, 5, 12};
            p_srce = (id_prt){5, 5, 9};
            p_dest = (id_prt){5, 5, 19};
            /* Assembly
                defineMySource 5+5*FY+9*FZ
                defineMyPortion 5+5*FY+12*FZ
                defineMyDestin 7+5*FY+19*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 503, 250, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; 13 --------------- réseau d'extensions axonales 9-> ->19
            p_port = (id_prt){5, 5, 13};
            p_srce = (id_prt){5, 5, 9};
            p_dest = (id_prt){5, 5, 19};
            /* Assembly
                defineMySource 5+5*FY+9*FZ
                defineMyPortion 5+5*FY+13*FZ
                defineMyDestin 5+6*FY+19*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 505, 249, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; 14 --------------- réseau d'extensions axonales 9-> ->19
            p_port = (id_prt){5, 5, 14};
            p_srce = (id_prt){5, 5, 9};
            p_dest = (id_prt){5, 5, 19};
            /* Assembly
                defineMySource 5+5*FY+9*FZ
                defineMyPortion 5+5*FY+14*FZ
                defineMyDestin 6+6*FY+19*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 504, 249, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; 15 --------------- réseau d'extensions axonales 9-> ->19
            p_port = (id_prt){5, 5, 15};
            p_srce = (id_prt){5, 5, 9};
            p_dest = (id_prt){5, 5, 19};
            /* Assembly
                defineMySource 5+5*FY+9*FZ
                defineMyPortion 5+5*FY+15*FZ
                defineMyDestin 7+6*FY+19*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 503, 249, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; 16 --------------- réseau d'extensions axonales 9-> ->19
            p_port = (id_prt){5, 5, 16};
            p_srce = (id_prt){5, 5, 9};
            p_dest = (id_prt){5, 5, 19};
            /* Assembly
                defineMySource 5+5*FY+9*FZ
                defineMyPortion 5+5*FY+16*FZ
                defineMyDestin 5+7*FY+19*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 505, 248, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; 17 --------------- réseau d'extensions axonales 9-> ->19
            p_port = (id_prt){5, 5, 17};
            p_srce = (id_prt){5, 5, 9};
            p_dest = (id_prt){5, 5, 19};
            /* Assembly
                defineMySource 5+5*FY+9*FZ
                defineMyPortion 5+5*FY+17*FZ
                defineMyDestin 6+7*FY+19*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 504, 248, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; 18 --------------- réseau d'extensions axonales 9-> ->19
            p_port = (id_prt){5, 5, 18};
            p_srce = (id_prt){5, 5, 9};
            p_dest = (id_prt){5, 5, 19};
            /* Assembly
                defineMySource 5+5*FY+9*FZ
                defineMyPortion 5+5*FY+18*FZ
                defineMyDestin 7+7*FY+19*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 503, 248, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; -------------------------------------------------------------------------------- Pile minimale
        // ; 19 --------------- réseau de neurones
            p_port = (id_prt){5, 5, 19};
            sub_creer_reseau(p_port, TYPE_NEURONE_COMPLET, null_port, myPackId, null_port, 505, 250, 1, 1);
            /* Assembly
                    defineMyPortion 5+5*FY+19*FZ
                    call_datapush_style sub_creer_reseau, \
                    TYPE_NEURONE_COMPLET, rbx, 505, 250, 1,  1
                    ; Type                NumP n/X  n/Y  n/Z Pas
                    */
        // ; 20 --------------- réseau d'extensions axonales 19-> ->21
            p_port = (id_prt){5, 5, 20};
            p_srce = (id_prt){5, 5, 19};
            p_dest = (id_prt){5, 5, 21};
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, myPackId, p_dest, 505, 250, 1, 1);
            /* Assembly
                defineMySource 5+5*FY+19*FZ
                defineMyPortion 5+5*FY+20*FZ
                defineMyDestin 5+5*FY+21*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 505, 250, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; -------------------------------------------------------------------------------- Chaine dendritique
        // ; 21 --------------- réseau de segments dendritiques ->22
            p_port = (id_prt){5, 5, 21};
            p_dest = (id_prt){5, 5, 22};
            sub_creer_reseau(p_port, TYPE_SEGMENT_COMPLET, null_port, myPackId, p_dest, 505, 250, 1, 1);
            /* Assembly
                defineMyPortion 5+5*FY+21*FZ
                defineMyDestin 5+5*FY+22*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, TYPE_SEGMENT_COMPLET, rbx, 505, 250, 1,  1
                ;Svt Type                  NumP n/X  n/Y  n/Z Pas
                */
        // ; 22 --------------- réseau de segments dendritiques ->23
            p_port = (id_prt){5, 5, 22};
            p_dest = (id_prt){5, 5, 23};
            sub_creer_reseau(p_port, TYPE_SEGMENT_COMPLET, null_port, myPackId, p_dest, 505, 250, 1, 1);
            /* Assembly
                defineMyPortion 5+5*FY+22*FZ
                defineMyDestin 5+5*FY+23*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, TYPE_SEGMENT_COMPLET, rbx, 505, 250, 1,  1
                ;Svt Type                  NumP n/X  n/Y  n/Z Pas
                */
        // ; 23 --------------- réseau de segments dendritiques ->24
            p_port = (id_prt){5, 5, 23};
            p_dest = (id_prt){5, 5, 24};
            sub_creer_reseau(p_port, TYPE_SEGMENT_COMPLET, null_port, myPackId, p_dest, 505, 250, 1, 1);
            /* Assembly
                defineMyPortion 5+5*FY+23*FZ
                defineMyDestin 5+5*FY+24*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, TYPE_SEGMENT_COMPLET, rbx, 505, 250, 1,  1
                ;Svt Type                  NumP n/X  n/Y  n/Z Pas
                */
        // ; -------------------------------------------------------------------------------- Neurones simples
        // ; 24 --------------- réseau de neurones
            p_port = (id_prt){5, 5, 24};
            sub_creer_reseau(p_port, TYPE_NEURONE_COMPLET, null_port, myPackId, null_port, 505, 250, 1, 1);
            /* Assembly
                defineMyPortion 5+5*FY+24*FZ
                call_datapush_style sub_creer_reseau, \
                TYPE_NEURONE_COMPLET, rbx, 505, 250, 1,  1
                ; Type                NumP n/X  n/Y  n/Z Pas
                */
        // ; -------------------------------------------------------------------------------- Concentration axonale
        // ; 25 --------------- réseau d'extensions axonales 4-> ->28
            p_port = (id_prt){5, 5, 25};
            p_srce = (id_prt){5, 5, 4};
            p_dest = (id_prt){5, 5, 28};
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, myPackId, p_dest, 505, 250, 1, 1);
            /* Assembly
                defineMySource 5+5*FY+4*FZ
                defineMyPortion 5+5*FY+25*FZ
                defineMyDestin 5+5*FY+28*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 505, 250, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; 26 --------------- réseau d'extensions axonales 9-> ->29
            p_port = (id_prt){5, 5, 26};
            p_srce = (id_prt){5, 5, 9};
            p_dest = (id_prt){5, 5, 28};
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, myPackId, p_dest, 505, 250, 1, 1);
            /* Assembly
                defineMySource 5+5*FY+9*FZ
                defineMyPortion 5+5*FY+26*FZ
                defineMyDestin 5+5*FY+28*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 505, 250, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; 27 --------------- réseau d'extensions axonales 19-> ->29
            p_port = (id_prt){5, 5, 27};
            p_srce = (id_prt){5, 5, 19};
            p_dest = (id_prt){5, 5, 28};
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, myPackId, p_dest, 505, 250, 1, 1);
            /* Assembly
                defineMySource 5+5*FY+19*FZ
                defineMyPortion 5+5*FY+27*FZ
                defineMyDestin 5+5*FY+28*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 505, 250, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y  n/Z Pas
                */
        // ; -------------------------------------------------------------------------------- Neurones simples
        // ; 28 --------------- réseau de neurones
            p_port = (id_prt){5, 5, 28};
            sub_creer_reseau(p_port, TYPE_NEURONE_COMPLET, null_port, myPackId, null_port, 505, 250, 1, 1);
            /* Assembly
                defineMyPortion 5+5*FY+28*FZ
                call_datapush_style sub_creer_reseau, \
                TYPE_NEURONE_COMPLET, rbx, 505, 250, 1,  1
                ; Type                NumP n/X  n/Y  n/Z Pas
                */
        // ; -------------------------------------------------------------------------------- Rétro-influx divers
        // ; 29 --------------- réseau d'extensions axonales 28-> ->1
            p_port = (id_prt){100, 50, 29};
            p_srce = (id_prt){100, 50, 28};
            p_dest = (id_prt){100, 50, 1};
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, myPackId, p_dest, 300, 40, 1, 1);
            /* Assembly
                defineMySource 100+50*FY+28*FZ
                defineMyPortion 100+50*FY+29*FZ
                defineMyDestin 100+50*FY+1*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 300, 40, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y n/Z Pas
                */
        // ; 30 --------------- réseau d'extensions axonales 28-> ->9
            p_port = (id_prt){100, 150, 30};
            p_srce = (id_prt){100, 150, 28};
            p_dest = (id_prt){100, 150, 9};
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, myPackId, p_dest, 300, 40, 1, 1);
            /* Assembly
                defineMySource 100+150*FY+28*FZ
                defineMyPortion 100+150*FY+30*FZ
                defineMyDestin 100+150*FY+9*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 300, 40, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y n/Z Pas
                */
        // ; 31 --------------- réseau d'extensions axonales 28-> ->19
            p_port = (id_prt){100, 150, 31};
            p_srce = (id_prt){100, 150, 28};
            p_dest = (id_prt){100, 150, 19};
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, myPackId, p_dest, 300, 40, 1, 1);
            /* Assembly
                defineMySource 100+150*FY+28*FZ
                defineMyPortion 100+150*FY+31*FZ
                defineMyDestin 100+150*FY+19*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 300, 40, 1,  1
                ;Dst Ant  Type                    NumP n/X  n/Y n/Z Pas
                */
    // ; =============================================== CREATIONS VERS L'EXTERIEUR
        // Extension vers PackId_1
            p_srce = (id_prt){5, 5, 1};
            p_port = (id_prt){5, 5, 31};
            p_dest = (id_prt){5, 5, 0};
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, PackId_1, p_dest, 1, 1, 1, 1);
        // Pulseur vers PackId_1
            p_port = (id_prt){5, 0, 0};
            p_dest = (id_prt){10, 0, 0};
            sub_creer_reseau(p_port, TYPE_PULSEUR, null_port, PackId_1, p_dest, 1, 1, 1, 1);    
        // Pulseur vers PackId_2
            p_port = (id_prt){6, 0, 0};
            p_dest = (id_prt){20, 0, 0};
            sub_creer_reseau(p_port, TYPE_PULSEUR, null_port, PackId_2, p_dest, 1, 1, 1, 1);    
        /* Assembly
            ; 31 --------------- extension axonale 1-> ->Extérieur
                ; une extension seule vers l'extérieur
                defineMySource 300+5*FY+1*FZ
                defineMyPortion 5+5*FY+31*FZ
                mov rdx, 1<<32 ; destinataire = utilisateur 1
                add rdx, 5+5*FY+0*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 1,  1,  1,  1
                ;Dst Ant  Type                    NumP n/X n/Y n/Z Pas
            ; 31 --------------- extension axonale 1-> ->Extérieur
                ; une extension seule vers l'extérieur
                defineMySource 300+5*FY+1*FZ
                defineMyPortion 10+5*FY+31*FZ
                mov rdx, 2<<32 ; destinataire = utilisateur 2
                add rdx, 5+5*FY+0*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 2,  1,  1,  1
                ;Dst Ant  Type                    NumP n/X n/Y n/Z Pas
            ; 31 --------------- extension axonale 1-> ->Extérieur
                ; une extension seule vers l'extérieur
                defineMySource 300+5*FY+1*FZ
                defineMyPortion 15+5*FY+31*FZ
                mov rdx, 65000<<32 ; destinataire = utilisateur 65000            
                add rdx, 10+5*FY+0*FZ
                call_datapush_style sub_creer_reseau, \
                rdx, rcx, TYPE_EXTENSION_COMPLET, rbx, 3,  1,  1,  1
                ;Dst Ant  Type                    NumP n/X n/Y n/Z Pas
            jmp fin_modeles
    ; =============================================== MODELES
        ; =============== A l'unité
        ; --------------- lecteur
            call_datapush_style sub_creer_portion_lecteur_7, \
            10+150*FY+0*FZ, 1+0*FY+0*FZ,   0,         0,      1,       127,     20+150*FY+1*FZ
            ; Numero,       PortionSource, IndexLect, NumBit, Persist, ValBloc, Destination
        ; --------------- segment dendritique
            call_datapush_style sub_creer_segment_dendritique_6, \
            20+150*FY+0*FZ, TYPE_SEGMENT_COMPLET, 0,       0,             0,           50+150*FY+0*FZ
            ; Numero,       Type,                 NivTemp, PotSegRestant, SynapsDispo, SegSuivant
        ; --------------- pulseur
            call_datapush_style sub_creer_portion_pulseur_4, \
            30+150*FY+0*FZ, 11,         127,     40+150*FY+0*FZ
            ; Numero,       PuissSeuil, ValBloc, Dest
        ; --------------- segment dendritique
            call_datapush_style sub_creer_segment_dendritique_6, \
            40+150*FY+0*FZ, TYPE_SEGMENT_COMPLET, 0,       0,             0,           50+150*FY+0*FZ
            ; Numero,       Type,                 NivTemp, PotSegRestant, SynapsDispo, SegSuivant
        ; --------------- neurone
            call_datapush_style sub_creer_portion_neurone_10, \
            50+150*FY+0*FZ, TYPE_NEURONE_COMPLET, 0,      0,    0,     0,    0,    0,    10,         10
            ; Numero,       Type,                 Orient, PRay, PPlan, PApi, PPan, PAxo, PuissSeuil, BaseRef
        ; --------------- extension axonale
            call_datapush_style sub_creer_extension_axonale_8, \
            60+150*FY+0*FZ, TYPE_EXTENSION_COMPLET, 0,       0,             0,     0,     70+150*FY+0*FZ, 50+150*FY+0*FZ
            ; Numero,       Type,                   NivTemp, PotAxoRestant, ValBB, MasBB, Destination,    Antecedent
        ; --------------- segment dendritique
            call_datapush_style sub_creer_segment_dendritique_6, \
            70+150*FY+0*FZ, TYPE_SEGMENT_COMPLET, 0,       0,             0,           80+150*FY+0*FZ
            ; Numero,       Type,                 NivTemp, PotSegRestant, SynapsDispo, SegSuivant
        ; --------------- neurone
            call_datapush_style sub_creer_portion_neurone_10, \
            80+150*FY+0*FZ, TYPE_NEURONE_COMPLET, 0,      0,    0,     0,    0,    0,    10,         10
            ; Numero,       Type,                 Orient, PRay, PPlan, PApi, PPan, PAxo, PuissSeuil, BaseRef

        ; =============== par réseaux
        ; --------------- réseau de segments dendritiques
            call_datapush_style sub_creer_reseau, \
            1+1*FY+2*FZ, TYPE_SEGMENT_COMPLET, 1+1*FY+1*FZ, 490, 210, 1, 1
            ; Suivant(N) Type                  NumP         n/X  n/Y n/Z Pas
        ; --------------- réseau de neurones
            call_datapush_style sub_creer_reseau, \
            1+1*FY+3*FZ, TYPE_NEURONE_COMPLET, 1+1*FY+2*FZ, 490, 210, 1, 1
            ; suivant(E) Type                  NumP         n/X  n/Y n/Z Pas
        ; --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            2+2*FY+1*FZ, 1+1*FY+4*FZ, TYPE_EXTENSION_COMPLET, 1+1*FY+3*FZ, 490, 210, 1, 1
            ; Dest       Suivant(E)   Type                    NumP         n/X  n/Y n/Z Pas

        ; =============== Réseaux
        ; --------------- réseau de pulseurs
            call_datapush_style sub_creer_reseau, \
            250+1*FY+1*FZ, TYPE_PULSEUR, 250+1*FY+0*FZ, 240, 210, 1, 1 
            ; Dest       Type          NumP         n/X  n/Y n/Z Pas
        ; --------------------------------------------------------------------------------
        ; --------------- réseau de lecteurs
            call_datapush_style sub_creer_reseau, \
            1+0*FY+0*FZ, 1+1*FY+1*FZ, TYPE_LECTEUR, 1+1*FY+0*FZ, 240, 210, 1, 1
            ; Source,    Dest         Type          NumP         n/X  n/Y n/Z Pas
        ; --------------------------------------------------------------------------------
        ; --------------- réseau de segments dendritiques
            call_datapush_style sub_creer_reseau, \
            1+1*FY+2*FZ, TYPE_SEGMENT_COMPLET, 1+1*FY+1*FZ, 490, 210, 1, 1
            ; Suivant(N) Type                  NumP         n/X  n/Y n/Z Pas
        ; --------------- réseau de neurones
            call_datapush_style sub_creer_reseau, \
            1+1*FY+3*FZ, TYPE_NEURONE_COMPLET, 1+1*FY+2*FZ, 490, 210, 1, 1
            ; suivant(E) Type                  NumP         n/X  n/Y n/Z Pas
        ; --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            2+2*FY+1*FZ, 1+1*FY+4*FZ, TYPE_EXTENSION_COMPLET, 1+1*FY+3*FZ, 490, 210, 1, 1
            ; Dest       Suivant(E)   Type                    NumP         n/X  n/Y n/Z Pas
        ; =============================================== FIN CREATIONS
        fin_modeles:
    */
// #-------------------------------------------------------------------------------------------- SOCKETS THREADS EVENEMENTS
    // Initialisation de WinSock
    if (WSAStartup(WSA_VERSION, &wsaDataStruct) != 0) {
        printf("Échec de l'initialisation de WinSock\n");
        return EXIT_FAILURE;
    }
    /* Assembly
        ; ======================================== support accès réseau
            ; initialisation WinSock
                mov rcx, WSA_VERSION
                lea rdx, [wsaDataStruct]
                sub rsp, SHADOW_SPACE_SIZE
                call WSAStartup
                add rsp, SHADOW_SPACE_SIZE
                ; cmp rax, WSA_STARTUP_OK
                ; je no_erreur_wsaStartup
                ;     call erreur_wsaStartup
                ; no_erreur_wsaStartup
        */
    // Pour reception
    // Création du socket de réception
    // recvSocketHandle = socket(AF_INET_IPV4, SOCK_DGRAM, DEFAULT_PROTOCOL);
    // if (recvSocketHandle == INVALID_SOCKET) {
    //    printf("Échec de la création du socket de réception\n");
    //    WSACleanup();
    //    return EXIT_FAILURE;
    // }
    /* Assembly
        ; ======================================== reception
                cmp byte [ouiReception], TRUE
                jne suite_sockets
            ; jmp suite_sockets
            ; creation socket de réception
                mov rcx, AF_INET_IPV4 ; famille d'adresses (IPV4)
                mov rdx, SOCK_DGRAM ; type de socket (datagramme)
                mov r8, DEFAULT_PROTOCOL ; protocole (par défaut => UDP car datagramme)
                sub rsp, SHADOW_SPACE_SIZE
                call socket
                add rsp, SHADOW_SPACE_SIZE
                ; cmp rax, INVALID_SOCKET
                ; je erreur_creationSocket
                mov [recvSocketHandle], rax
        */
    // Configuration de l'adresse locale
    // localInSocket.sin_family = AF_INET_IPV4;
    // localInSocket.sin_port = htons(8080);
    // localInSocket.sin_addr.s_addr = htonl(INADDR_ANY);
    // if (bind(recvSocketHandle, (struct sockaddr*)&localInSocket, SOCKET_ADDRESS_LEN) == SOCKET_ERROR) {
    //     printf("Échec du bind\n");
    //     closesocket(recvSocketHandle);
    //     WSACleanup();
    //     return EXIT_FAILURE;
    // }
    /* Assembly
        ; associer le socket de réception au port
        mov word [localInSocket], AF_INET_IPV4 ; ----------> type d'adresse
        mov eax, 0x7C01A8C0
        mov [localInSocket + 4], eax ; --------------------> adresse ip locale
        mov ax, 0x8080
        mov [localInSocket + 2], ax ; ---------------------> port ip de réception
        ; appel de bind
        lea rcx, [recvSocketHandle]
        lea rdx, [localInSocket]
        mov r8, SOCKET_ADDRESS_LEN
        sub rsp, 32
        call bind
        add rsp, 32
        ; mov [valeurDeTest], rax
        ; cmp rax, 0
        ; jne erreur_bindSocket
        */
    // Création des threads
    // HANDLE hRecvThread = CreateThread(NULL, 0, recvThread, NULL, 0, NULL);
    // if (hRecvThread == NULL) {
    //     printf("Echec de la creation du thread de reception\n");
    //     closesocket(recvSocketHandle);
    //     WSACleanup();
    //     return EXIT_FAILURE;
    // }
    /* Assembly
        ; thread de réception
            mov rcx, 0                          ; [in, optional] lpThreadAttributes
            mov rdx, 0                          ; [in] dwStackSize
            lea r8, [recvThread]                ; [in] lpStartAddress
            mov r9, 0                           ; [in, optional] lpParameter
            push 0                              ; [out, optional] lpThreadId
            push 0                              ;  [in] dwCreationFlags
            sub rsp, 32
            call CreateThread
            add rsp, 32
        suite_sockets:
        */
    // Pour expedition
    sendSocketHandle = socket(AF_INET_IPV4, SOCK_DGRAM, DEFAULT_PROTOCOL);
    if (sendSocketHandle == INVALID_SOCKET) {
        printf("Échec de la création du socket d envoi\n");
        WSACleanup();
        return EXIT_FAILURE;
    }
    /* Assembly
        cmp byte [ouiExpedition], TRUE
        jne fin_socketse
        ; creation socket d'envoi
            mov rcx, AF_INET_IPV4 ; famille d'adresses (IPV4)
            mov rdx, SOCK_DGRAM ; type de socket (datagramme)
            mov r8, DEFAULT_PROTOCOL ; DEFAULT_PROTOCOL ; protocole (par défaut => UDP car datagramme)
            sub rsp, SHADOW_SPACE_SIZE
            call socket
            add rsp, SHADOW_SPACE_SIZE
            ; cmp rax, INVALID_SOCKET
            ; je erreur_creationSocket
            mov [sendSocketFileDescriptor], rax
        */
    HANDLE hSendThread = CreateThread(NULL, 0, sendThread, NULL, 0, NULL);
    if (hSendThread == NULL) {
        printf("Echec de la creation du thread d envoi\n");
        closesocket(sendSocketHandle);
        WSACleanup();
        return EXIT_FAILURE;
    }
    /* Assembly
        ; creer le thread d'envoi
            mov rcx, 0                          ; [in, optional] lpThreadAttributes
            mov rdx, 0                          ; [in] dwStackSize
            lea r8, [sendThread]                ; [in] lpStartAddress
            mov r9, 0                           ; [in, optional] lpParameter
            push 0                              ; [out, optional] lpThreadId
            push 0                              ;  [in] dwCreationFlags
            sub rsp, 32
            call CreateThread
            add rsp, 32
        */
    // goto suite;
    //                            // Attente de la fin des threads (boucle infinie ici)
    //                            WaitForSingleObject(hRecvThread, INFINITE);
    //                            WaitForSingleObject(hSendThread, INFINITE);
    //suite:
    // Creation de levenement de declenchement de l'envoi
    const char sendThreadEventName[] = "sendThreadEvent";
    sendThreadEventHandle = CreateEventA(
        NULL,                // lpEventAttributes = NULL
        TRUE,                // bManualReset = TRUE (événement manuel)
        FALSE,               // bInitialState = FALSE (non signalé)
        sendThreadEventName  // lpName = adresse du nom (facultatif)
    );
    if (sendThreadEventHandle == NULL) {
        printf("Échec de la creation de l'evenement d'envoi : %lu\n", GetLastError());
        return 1;
    }
    printf("Handle de l'evenement : %p\n", (void*)sendThreadEventHandle);
    /* Assembly
        ; évenement de declenchement d'envoi
            xor rcx, rcx                        ; lpEventAttributes = NULL
            mov rdx, 1                          ; bManualReset = TRUE (événement manuel)
            xor r8, r8                          ; bInitialState = FALSE (non signalé)
            lea r9, [sendThreadEventName]       ; lpName = adresse du nom (facultatif)
            call CreateEventA                   ; Créer l'événement
            mov [sendThreadEventHandle], rax    ; Sauvegarde du handle
        fin_sockets:
        */
// #-------------------------------------------------------------------------------------------- ANNONCES
    /*
        call_winapi64_style GetStdHandle, STD_OUTPUT_HANDLE
        mov r15, rax
        call_datapush_style ecrire_ligne_4, r15, interparagraphes_text, -1, -1 ;            interligne
        call_datapush_style ecrire_ligne_4, r15, rawData_Texte, -1, -1 ;                    texte de lecture
        call_datapush_style ecrire_ligne_4, r15, interligne_text, -1, -1 ;                  interligne
        call_datapush_style ecrire_ligne_4, r15, app_instance, qword [Instance], -1 ;       instance de l'application
        call_datapush_style ecrire_ligne_4, r15, windowClassName, qword [ClassAtom], -1 ;   atome de classe de fenetre
        call_datapush_style ecrire_ligne_4, r15, windowName, qword [Windowhandle], -1 ;     handle de la fenetre 
        call_datapush_style ecrire_ligne_4, r15, interligne_text, -1, -1 ;                  interligne
    */
// #-------------------------------------------------------------------------------------------- PREPARATIFS RESEAU
    /* Assembly
        ; A_FAIRE : initialiser la plage complète des utilisateurs
        ; forçage de quelques utilisateurs
        */
    nombreDUtilisateurs = 0;
    FOR_U32_TOU(nombreDUtilisateurs, OU_IPAD_4) = inet_addr("192 168 1 124"); // voir pour utiliser inet_pton()
    FOR_U16_TOU(nombreDUtilisateurs, OU_PORT_2) = htons(8080);
    FOR_U16_TOU(nombreDUtilisateurs, OU_VIDE_2) = 0;
    FOR_U64_TOU(nombreDUtilisateurs, OU_NMES_8) = 0;
    FOR_U64_TOU(nombreDUtilisateurs, OU_ADRP_8) = 0;
    /* Assembly
        ; ----------------------- utilisateur 0 (maitre)
        lea rax, [users]
        mov ecx, IP_0
        mov dword [rax+OU_IPAD_4], ecx
        userPort0: equ 8080
        mov word [rax+OU_PORT_2], userPort0
        */
    nombreDUtilisateurs = 1;
    FOR_U32_TOU(nombreDUtilisateurs, OU_IPAD_4) = inet_addr("192.168.1.21"); // Dorian
    FOR_U16_TOU(nombreDUtilisateurs, OU_PORT_2) = htons(8080);
    FOR_U16_TOU(nombreDUtilisateurs, OU_VIDE_2) = 0;
    FOR_U64_TOU(nombreDUtilisateurs, OU_NMES_8) = 0;
    FOR_U64_TOU(nombreDUtilisateurs, OU_ADRP_8) = 0;
    /* Assembly
        ; ----------------------- utilisateur 1 Va Eth
        add rax, TAILLE_DES_UTILISATEURS
        mov ecx, IP_1
        mov dword [rax+OU_IPAD_4], ecx
        userPort1: equ 8080
        mov word [rax+OU_PORT_2], userPort1
        */
    nombreDUtilisateurs = 2;
    FOR_U32_TOU(nombreDUtilisateurs, OU_IPAD_4) = inet_addr("192.168.1.16"); // Va wifi
    FOR_U16_TOU(nombreDUtilisateurs, OU_PORT_2) = htons(8080);
    FOR_U16_TOU(nombreDUtilisateurs, OU_VIDE_2) = 0;
    FOR_U64_TOU(nombreDUtilisateurs, OU_NMES_8) = 0;
    FOR_U64_TOU(nombreDUtilisateurs, OU_ADRP_8) = 0;
    /* Assembly
        ; ----------------------- utilisateur 2 Va Wifi
        add rax, TAILLE_DES_UTILISATEURS
        mov ecx, IP_2
        mov dword [rax+OU_IPAD_4], ecx
        userPort2: equ 8080
        mov word [rax+OU_PORT_2], userPort2
        */
    nombreDUtilisateurs = 65000;
    FOR_U32_TOU(nombreDUtilisateurs, OU_IPAD_4) = inet_addr("192.168.1.1"); // Box Eth
    FOR_U16_TOU(nombreDUtilisateurs, OU_PORT_2) = htons(8080);
    FOR_U16_TOU(nombreDUtilisateurs, OU_VIDE_2) = 0;
    FOR_U64_TOU(nombreDUtilisateurs, OU_NMES_8) = 0;
    FOR_U64_TOU(nombreDUtilisateurs, OU_ADRP_8) = 0;
    /* Assembly
        ; ----------------------- utilisateur dernier X Do Eth / box
        lea rax, [users]
        add rax, TAILLE_DES_UTILISATEURS*(65000) ; pour tests réalistes
        ; add rax, TAILLE_DES_UTILISATEURS*(PACKID_NB_P-1) ; pour tests au maximum de charge
        mov ecx, IP_65000
        mov dword [rax+OU_IPAD_4], ecx
        userPortX: equ 8080
        mov word [rax+OU_PORT_2], userPortX
        */
    nombreDUtilisateurs += 1;
    /* Assembly
        ; identification du dernier utilisateur enregistre r11
        lea rdx, [users] ; adresse en analyse
        xor r10, r10 ; dernier utilisateur
        mov rcx, 0 ; compteur
        boucle_dernier_utilisateur:
            mov eax, [rdx+OU_IPAD_4]
            test rax, rax
            cmovnz r10, rcx
            add rdx, TAILLE_DES_UTILISATEURS
            inc rcx
            cmp rcx, PACKID_NB_P
        jne boucle_dernier_utilisateur
        mov [nombreDUtilisateurs], r10
    ; erreurs non bloquantes
        mov byte [ErrorsBreak], 0
    ; compteurs
        mov qword [nombreDeMessageAEmettre], 0
        mov qword [nombreDeMessagesRecus], 0
    */
// #------------------------------------------------------------------------------------ BOUCLAGES DIVERS
    verifier_portions();
    numero_de_boucle_lente = 0;
    while (1) {
        /* Assembly
            ; bouclage lent
            mov qword [numero_de_boucle_lente], 0
            bouclage_lent: ; boucle lente pour ecrire des infos et choisir de sortir
            ; initialisation boucle rapide
            xor rax, rax
            rdtsc ; -> edx:eax
            shl rdx, 32
            or rdx, rax ; procCycleCntr condensé
            call verifier_portions
            mov [procCycleCntr_avant_bouclage_moyen], rdx ; stockage procCycleCntr pour durée boucles moyennes
            */
        timer_avant_moyennes = clock(); // Start time measurement
        do {                
            /* Assembly
                bouclage_moyen: ; boucle moyenne pour redessiner regulierement
                ; initialisation serie de bouclages rapides
                xor rax, rax
                rdtsc ; -> edx:eax
                shl rdx, 32
                or rdx, rax ; procCycleCntr condensé
                mov [procCycleCntr_avant_bouclage_rapide], rdx ; stockage procCycleCntr pour durée boucle rapide
                */
            timer_avant_rapides = clock(); // Start time measurement
            // nombre_de_cycles = 0;
            do {                
                /* Assembly
                    bouclage_rapide:
                    */
                debut_cycle = clock(); // Start time measurement
                /* Assembly
                    ; initialisation calcul de la duree du bouclage sur portions
                    xor rax, rax
                    rdtsc ; -> edx:eax
                    shl rdx, 32
                    or rdx, rax ; procCycleCntr 64 bits
                    mov [procCycleCntr_avant_bouclage_portions], rdx
                    */
// #---------------------------------------------------------------------------------------- BOUCLAGE CYCLE
                // initialisation du premier message a envoyer apres la boucle sur portions !
                /* Assembly
                    ; initialisation messages à envoyer (premier message à 0)
                    lea rdx, [msgToSnd]
                    mov dword [rdx+OMP_IPAD_4], 0
                    mov word [rdx+OMP_PORT_2], 0
                    mov word [rdx+OMP_VIDE_2], 0
                    mov qword [rdx+OMP_ADPS_8], 0
                    mov qword [rdx+OMP_TAIP_8], 0
                    */
                notesATraiter = 0;
                /* Assembly
                    ; pointeur pour premier message
                    lea r11, [msgStack]
                    */
                for (uint32_t portion = 0; portion < DIMENSION_Z*DIMENSION_Y*DIMENSION_X; portion++) {
                    /* Assembly
                        ; initialisation boucle portions
                        mov r8, 0 ; premiere portion
                        adresse_from_numero r9, r8 ; calcul adresse de la première portion
                        mov r8, 0 ; initialisation numéro de la première portion
                        bouclage_portions:
                            ; registre constant sur toutes les boucles :
                            ;   r10 = dernier utilisateur/packId enregistré
                            ; registres constant dans la boucle sur portions :
                            ;   r8 = numero de la portion en traitement
                            ;   r9 = pointeur/adresse de la portion en traitement
                            ;   r11 = pointeur de la pile de notes
                            ; registres internes à chaque type de portion traitée ou etape traitée
                            ;   rax, rbx, rcx, rdx = variables de travail
                            ;   r13 = adresse origine
                            ;   r14 = adresse source
                            ;   r15 = adresse destination
                        */
// #-------------------------------------------------------------------------------------------- TRAITEMENT PORTION (r8/r9)
                    // NOTE POUR L'ACCES AUX PORTIONS DIRECTEMENT :
                    // portion NP
                    // offset OS

                    // Pour acceder a un octet :
                    // uint8_t *ptr = (uint8_t *)crvDatas; // Cast du tableau en pointeur
                    // uint8_t valeur = ptr[NP*TAILLE_DES_PORTIONS+OS]; // Acces direct a l'octet
                    // >> et en une ligne : uint8_t valeur = ((uint8_t *)crvDatas)[NP * TAILLE_DES_PORTIONS + OS];

                    // Pour acceder a un dword (valable aussi pour un u_int8)
                    // uint32_t *ptr32 = (uint32_t *)((uint8_t *)crvDatas + NP*TAILLE_DES_PORTIONS+OS);
                    // uint32_t valeur = *ptr32;
                    // >> et en une seule ligne : uint32_t valeur = *(uint32_t *)((uint8_t *)crvDatas + NP * TAILLE_DES_PORTIONS + OS);

                    /* Assembly
                        ; aiguillage vers traitements concernés
                        */
                    uint8_t typestev = U8_FROMP(portion, OS_TYPESTEV_1);
                    switch (typestev) {
                        /* Assembly
                            ; A_FAIRE : réorganiser les tests selon la fréquence d'apparition du cas
                            ; Sauts pour Segments-Neurones-Extensions
                            mov al, [r9+OS_TYPESTEV_1] ; récuperation du type de portion
                            comparer_et_jump_si_egal al, TYPE_SEGMENT_COMPLET,                  portion_segment_complet
                            comparer_et_jump_si_egal al, TYPE_NEURONE_COMPLET,                  portion_neurone_complet
                            comparer_et_jump_si_egal al, TYPE_EXTENSION_COMPLET,                portion_extension_complet
                            comparer_et_jump_si_egal al, TYPE_SEGMENT_PARTIEL,                  portion_segment_partiel
                            comparer_et_jump_si_egal al, TYPE_NEURONE_PARTIEL,                  portion_neurone_partiel
                            comparer_et_jump_si_egal al, TYPE_EXTENSION_PARTIEL,                portion_extension_partiel
                            comparer_et_jump_si_egal al, TYPE_SEGMENT_GERME,                    portion_segment_germe
                            comparer_et_jump_si_egal al, TYPE_NEURONE_GERME,                    portion_neurone_germe
                            comparer_et_jump_si_egal al, TYPE_EXTENSION_GERME,                  portion_extension_germe
                            ; Sauts pour autres
                            comparer_et_jump_si_egal al, TYPE_LECTEUR,                          portion_lecteur
                            comparer_et_jump_si_egal al, TYPE_PULSEUR,                          portion_pulseur
                            ; ne pas traiter les portions de type : 0 et TYPE_SOURCE et autres
                            jmp fin_traitement
                            ; portions speciales ---------------------------------------------------
                            */
                        case TYPE_LECTEUR: {
                            /* Assembly
                                portion_lecteur: ; (rax/rbx/rcx/rdx/r13/r14/r15)
                                */
                            uint32_t sourceU = U32_FROMP(portion, OSL_PAM_U_4);
                            uint32_t sourceN = U32_FROMP(portion, OSL_PAM_N_4);
                            if (sourceU != myPackId) {
                                ajouter_note(myPackId, portion, 0, 0, sourceU, sourceN);
                                /* Assembly
                                    mov eax, [r9+OSL_PAM_U_4]
                                    cmp eax, [myPackId]
                                    jne lecteur_demande_datainfo
                                    */
                            }
                            else {
                                if (sourceN != 0) {
                                    /* Assembly
                                        mov eax, [r9+OSL_PAM_N_4] ; portion des paramètres de l'accès mémoire
                                        test eax, eax ; PORTION_ZERO ?
                                        jz fin_traitement
                                        */
                                    uint32_t destinU = U32_FROMP(portion, OSL_DST_U_4);
                                    uint32_t destinN = U32_FROMP(portion, OSL_DST_N_4);
                                    if (destinU != myPackId) {
                                        ajouter_note(myPackId, portion, 0, 0, destinU, destinN);
                                        /* Assembly
                                            adresse_from_numero r13, rax ; r13 = adresse des paramètres des données à lire
                                            mov eax, [r9+OSL_DST_U_4]
                                            cmp eax, [myPackId]
                                            jne lecteur_demande_charge
                                            */
                                    }
                                    else {
                                        if (sourceN != 0) {
                                            /* Assembly
                                                mov eax, [r9+OSL_DST_N_4] ; portion destination / num portion
                                                test eax, eax ; PORTION_ZERO ?
                                                jz fin_traitement
                                                */
                                            // parametres de lecture
                                            uint8_t largSegment8 = U8_FROMP(sourceN, OSD_LGSEG_1);
                                            uint16_t indexDeLecture16 = U16_FROMP(portion, OSL_INDEX_2);
                                            uint8_t bitALire8 = U8_FROMP(portion, OSL_NMBIT_1);
                                            // adresse de lecture
                                            uint64_t adresse64 = U64_FROMP(sourceN, OSD_ADDRS_8);
                                            uint64_t adresseALire = adresse64 + indexDeLecture16 * largSegment8;
                                            // lecture du bit
                                            uint64_t valeur64;
                                            memcpy(&valeur64, (void *)(uintptr_t)adresseALire, sizeof(uint64_t));  
                                            uint8_t bit = (valeur64 >> bitALire8) & 1;
                                            // bit = 0; // pour bit constant
                                            /* Assembly
                                                adresse_from_numero r15, rax ; r15 = adresse de la destination
                                                mov r14, [r13+OSD_ADDRS_8] ; r14 = adresse de base des données
                                                movzx rdx, byte [r13+OSD_LGSEG_1] ; largeur des segments en octets (maxi 8)
                                                mov bx, [r9+OSL_INDEX_2] ; bx = index de lecture
                                                movzx eax, bx
                                                ; calcul de l'adresse de base à lire
                                                ; A_FAIRE ? ajouter une variable de position de tête relatif et boucler sur maxi nombre à lire
                                                ; A_FAIRE ? peut-on eviter la multiplication ?
                                                mul edx ; => edx:eax (?)
                                                and rax, 0x00ffffff ; effacement de la partie haute de rax (car maxi possible = 65535*8)
                                                add r14, rax ; -> adresse de base à lire
                                                ; lecture du bit de donnee
                                                mov rax, qword [r14] ; contenu du segment (plus le suite jusqu'a 8 octets)
                                                mov cl, byte [r9+OSL_NMBIT_1] ; numero de bit a lire (0 à 63)
                                                shr rax, cl ; décalage du nombre de bits nécessaire
                                                shr rax, 1 ; sortie sur le carry flag
                                                */
                                            if (bit) {
                                                // lire la charge de la destination
                                                int16_t chargeD16 = S16_FROMP(destinN, OS_CHARG0_2);
                                                // surcharge
                                                int8_t surcharge8 = S8_FROMP(portion, OSL_BSYNV_1);
                                                // surcharger_destination();
                                                int32_t chargeD32 = (int32_t)(chargeD16) + surcharge8;
                                                chargeD32 = (chargeD32 > CHARGE_MINI) ? chargeD32 : CHARGE_MINI;
                                                chargeD32 = (chargeD32 < CHARGE_MAXI) ? chargeD32 : CHARGE_MAXI;
                                                FOR_S16_TOP(destinN, OS_CHARG0_2) = (int16_t)chargeD32;
                                                /* Assembly
                                                    ; ----- surcharge destination
                                                    jnc pl_pas_d_activation ; on n'active pas la destination si le bit lu est 0
                                                        ; surcharge segment destination
                                                        movsx ecx, word [r15+OS_CHARG0_2] ; charge actuelle destination (word signé -> dword signé)
                                                        movsx eax, byte [r9+OSL_BSYNV_1] ; valeur du bloc de boutons (byte signé -> dword signé)
                                                        add ecx, eax ; ajouter les charges du bloc de boutons
                                                        ; limitation de la charge avant enregistrement
                                                        mov eax, CHARGE_MINI
                                                        cmp ecx, eax
                                                        cmovl ecx, eax ; limiter ecx à la valeur minimale d'un word signé (excepté -32768 charge non valide)
                                                        mov eax, CHARGE_MAXI
                                                        cmp ecx, eax
                                                        cmovg ecx, eax ; limiter ecx à la valeur maximale d'un word signé
                                                        ; enregistrer la nouvelle charge
                                                        mov [r15+OS_CHARG0_2], cx
                                                    pl_pas_d_activation:
                                                    */
                                            }
                                            // lire la persistance actuelle
                                            uint8_t persistActu8 = U8_FROMP(portion, OSL_PERSA_1);
                                            if (persistActu8 == 0) {
                                                /* Assembly
                                                    ; persistance
                                                    mov cl, [r9+OSL_PERSA_1] ; persistance restante actuelle
                                                    test cl, cl
                                                    jnz pl_persistance_encore_active
                                                    */
                                                // remplacer la persistance actuelle par la base
                                                persistActu8 = U8_FROMP(portion, OSL_PERSB_1);
                                                // lire l'index de lecture actuel et l'augmenter
                                                indexDeLecture16 = indexDeLecture16 + 1;
                                                // s'il depasse le nombre de segments, le remettre a 0
                                                uint16_t nbrSeg16 = U16_FROMP(sourceN, OSD_NBSEG_2);
                                                indexDeLecture16 = (indexDeLecture16 < nbrSeg16 ? indexDeLecture16 : 0);
                                                FOR_U16_TOP(portion, OSL_INDEX_2) = indexDeLecture16;
                                            }
                                            FOR_U8_TOP(portion, OSL_PERSA_1) = persistActu8 - 1;
                                                /* Assembly
                                                        mov cl, [r9+OSL_PERSB_1] ; persistance de base (remplace actuelle)
                                                        inc bx ; augmenter l'index de lecture
                                                        cmp bx, [r13+OSD_NBSEG_2] ; comparer au nombre de segments à lire
                                                        jne pl_fin_pas_atteinte
                                                            xor bx, bx ; reprendre la lecture au début
                                                        pl_fin_pas_atteinte:
                                                        mov [r9+OSL_INDEX_2], bx ; enregistrer l'index de lecture
                                                    pl_persistance_encore_active:
                                                    dec cl
                                                    mov [r9+OSL_PERSA_1], cl ; persistance restante
                                                    jmp fin_traitement
                                                    */
                                        }
                                    }
                                }
                            }
                            break;
                        }
                        case TYPE_PULSEUR: {
                            /* Assembly
                                portion_pulseur: ; (rax/rbx/rcx/rdx)
                                */
                            // chrono = charge actuelle
                            uint16_t chrono16 = U16_FROMP(portion, OSP_CHARGI_2);
                            // surcharge
                            uint32_t chrono32 = (uint32_t)chrono16 + INCREMENT_PULSEURS;
                            // limitation word
                            chrono32 = (chrono32 < CHRONO_MAXI) ? chrono32 : CHRONO_MAXI;
                            // seuil
                            uint8_t pSeuil8 = U8_FROMP(portion, OSP_SEUIL_1);
                            uint32_t seuil32 = (1 << pSeuil8) - 1;
                            // test
                            if (chrono32 < seuil32) {
                                FOR_U16_TOP(portion, OSP_CHARGI_2) = (uint16_t)chrono32;
                            }
                            /* Assembly
                                ; augmentation chrono
                                movzx ebx, word [r9+OSP_CHARGI_2] ; chrono actuel (word non signé)
                                add ebx, INCREMENT_PULSEURS ; incrementation chrono
                                mov eax, CHRONO_MAXI
                                cmp ebx, eax
                                cmova ebx, eax ; limiter ecx à un word non signé
                                ; seuil
                                mov cl, [r9+OSP_SEUIL_1] ; puissance de seuil avec décalage (maxi 16)
                                mov eax, 1
                                shl eax, cl
                                dec eax ; seuil calculé (0 à 65535)
                                ; conséquences
                                mov cx, CHRONO_INITIAL
                                cmp bx, ax ; tester si le seuil n'est pas atteint
                                cmovae bx, cx ; réinitialiser le compteur si le seuil a été atteint
                                mov [r9+OSP_CHARGI_2], bx ; enregistrer le nouveau chrono (0 ou valeur incrémentée)
                                jb fin_traitement ; ne rien faire
                                */
                            else {
                                // reinit charge + enregistrement
                                FOR_U16_TOP(portion, OSP_CHARGI_2) = CHRONO_INITIAL;
                                // surcharge signee destination
                                int8_t surcharge8 = S8_FROMP(portion, OSP_BSYNV_1);
                                // user destinataire
                                uint32_t destinU = U32_FROMP(portion, OSP_DST_U_4);
                                uint32_t destinN = U32_FROMP(portion, OSP_DST_N_4);
                                if (destinU != myPackId) {
                                    ajouter_note(myPackId, portion, MC_SURCHARGE_DW, (int32_t)surcharge8, destinU, destinN);
                                }
                                else {
                                    if (destinN != 0) {
                                        int16_t chargeD16 = S16_FROMP(destinN, OS_CHARG0_2);
                                        // surcharger_destination();
                                        int32_t chargeD32 = (int32_t)(chargeD16) + surcharge8;
                                        chargeD32 = (chargeD32 > CHARGE_MINI) ? chargeD32 : CHARGE_MINI;
                                        chargeD32 = (chargeD32 < CHARGE_MAXI) ? chargeD32 : CHARGE_MAXI;
                                        FOR_S16_TOP(destinN, OS_CHARG0_2) = (int16_t)chargeD32;
                                    }
                                }
                            }
                            /* Assembly
                                    mov eax, [r9+OSP_DST_U_4]
                                    cmp eax, [myPackId]
                                    jne pulseur_demande_charge
                                        mov eax, [r9+OSP_DST_N_4] ; neurone de destination / num portion
                                        test rax, rax ; NEURONE_ZERO ?
                                        jz fin_traitement ; si pas de destination, on ne fait rien
                                            adresse_from_numero rdx, rax ; adresse neurone de destination
                                            movsx ecx, word [rdx+OS_CHARG0_2] ; charge actuelle destination (word signé -> dword signé)
                                            movsx eax, byte [r9+OSP_BSYNV_1] ; valeur du bloc de boutons (byte signé -> dword signé)
                                            add ecx, eax ; ajouter les charges du bloc de boutons
                                            ; limitation de la charge avant enregistrement
                                            mov eax, CHARGE_MINI
                                            cmp ecx, eax
                                            cmovl ecx, eax ; limiter ecx à la valeur minimale d'un word signé (excepté -32768 charge non valide)
                                            mov eax, CHARGE_MAXI
                                            cmp ecx, eax
                                            cmovg ecx, eax ; limiter ecx à la valeur maximale d'un word signé
                                            ; enregistrer la nouvelle charge
                                            mov [rdx+OS_CHARG0_2], cx
                                            jmp fin_traitement
                                            */
                            break;
                        }
                        case TYPE_SEGMENT_GERME:
                            /* Assembly
                                ; portions segments (dendrites) ---------------------------------------------------
                                portion_segment_germe:
                                    ; A_FAIRE : coder le passage en mode partiel éventuel
                                    jmp fin_traitement
                                */
                        case TYPE_SEGMENT_PARTIEL:
                            /* Assembly
                                portion_segment_partiel:
                                    ; A_FAIRE : coder la création de la dendrite antecedente (même type)
                                    ; A_FAIRE : conditionner tout ça à la vitesse de développement (?)
                                    ; A_FAIRE : coder le passage en mode complet
                                */
                        case TYPE_SEGMENT_COMPLET: {
                            /* Assembly
                                portion_segment_complet: ; (rax/rbx/rbx/rdx)
                                */
                            // mise a 0 de l'activite
                            FOR_U8_TOP(portion, OS_ACTIVITE_1) = 0;
                            uint32_t suivanU = U32_FROMP(portion, OSS_SVT_U_4);
                            uint32_t suivanN = U32_FROMP(portion, OSS_SVT_N_4);
                            /* Assembly
                                mov byte [r9+OS_ACTIVITE_1], 0 ; enlever la rétro-activité le cas échéant
                                ; extraire d'abord la surcharge, glissement inclus
                                mov eax, [r9+OSS_SVT_U_4]
                                */
                            if (suivanU != myPackId) {
                                ajouter_note(myPackId, portion, 0, 0, suivanU, suivanN);
                                /* Assembly
                                    cmp eax, [myPackId]
                                    jne segment_demande_charge
                                    */
                            }
                            else {
                                if (suivanN != 0) {
                                    // transfert de l'activite du suivant vers l'actuel
                                    uint8_t actSuiv8 = U8_FROMP(suivanN, OS_ACTIVITE_1);
                                    FOR_U8_TOP(portion, OS_ACTIVITE_1) = actSuiv8;
                                    /* Assembly
                                        mov eax, [r9+OSS_SVT_N_4] ; numero de segment/neurone suivant (eax=>rax)
                                        test eax, eax ; PORTION_ZERO ?
                                        jz psc_isole ; aller au cas d'un segment qui a perdu son segment/neurone aval
                                            adresse_from_numero rdx, rax ; adresse du suivant
                                            mov al, [rdx+OS_ACTIVITE_1] ; activité du suivant
                                            mov [r9+OS_ACTIVITE_1], al ; l'appliquer à la portion en traitement
                                        */
                                    // extraction de la charge 0 + mise a 0
                                    int16_t chargeAUt16 = S16_FROMP(portion, OS_CHARG0_2);
                                    FOR_S16_TOP(portion, OS_CHARG0_2) = 0;
                                    // extraction de la charge 1 et sortie si invalide
                                    int16_t charge116 = S16_FROMP(portion, OSS_CHARG1_2);
                                    if (charge116 != CHARGE_INVALIDE) {
                                        // transfert charge 0 vers position 1 + charge a utiliser
                                        FOR_S16_TOP(portion, OSS_CHARG1_2) = chargeAUt16;
                                        chargeAUt16 = charge116;
                                        // extraction de la charge 2 et sortie si invalide
                                        int16_t charge216 = S16_FROMP(portion, OSS_CHARG2_2);
                                        if (charge216 != CHARGE_INVALIDE) {
                                            // transfert charge 1 vers position 2 + charge a utiliser
                                            FOR_S16_TOP(portion, OSS_CHARG2_2) = chargeAUt16;
                                            chargeAUt16 = charge216;
                                            // extraction de la charge 3 et sortie si invalide
                                            int16_t charge316 = S16_FROMP(portion, OSS_CHARG3_2);
                                            if (charge316 != CHARGE_INVALIDE) {
                                                // transfert charge 2 vers position 3 + charge a utiliser
                                                FOR_S16_TOP(portion, OSS_CHARG3_2) = chargeAUt16;
                                                chargeAUt16 = charge316;
                                                /* Assembly
                                                    ; A_FAIRE : Mettre les cherges en file sur un qword (4 maxi)
                                                    ;           Utiliser celle qui nous intéresse
                                                    ;           Puis faire glisser de 16 bits
                                                                        ; movzx rcx, byte [r9+OSS_...] ; = 0, 2, 4 ou 6
                                                                        ; add rcx, OS_CHARG0_2
                                                                        ; add rcx, r9
                                                                        ; mov bx, [rcx]
                                                                        ; shl qword [rcx+OS_CHARG0_2], 16
                                                    ; glissement des charges vers bx
                                                    mov bx, [r9+OS_CHARG0_2] ; charge en cours
                                                    mov word [r9+OS_CHARG0_2], 0 ; charge en cours = 0
                                                    mov ax, [r9+OSS_CHARG1_2] ; charge 1
                                                    cmp ax, CHARGE_INVALIDE
                                                    je psc_appliquer_surcharge ; bx = charge 0 ; charge 0 = 0 ; pas de glissement
                                                        mov [r9+OSS_CHARG1_2], bx ; glissement charge 0 -> charge 1
                                                        mov bx, ax
                                                        mov ax, [r9+OSS_CHARG2_2] ; charge 2
                                                        cmp ax, CHARGE_INVALIDE
                                                        je psc_appliquer_surcharge ; bx = charge 1 ; glissement 0->1 fait ; charge 0 = 0
                                                            mov [r9+OSS_CHARG2_2], bx ; glissement charge 1 -> charge 2
                                                            mov bx, ax
                                                            ; ici : bx = charge 2 ; glissement 1->2 fait ; glissement 0->1 fait ; charge 0 = 0
                                                    */
                                            }
                                        }
                                    }
                                    // utilisation de chargeAUt16
                                    int16_t chargeD16 = S16_FROMP(suivanN, OS_CHARG0_2);
                                    // surcharger_destination();
                                    int32_t chargeD32 = (int32_t)(chargeD16) + chargeAUt16;
                                    chargeD32 = (chargeD32 > CHARGE_MINI) ? chargeD32 : CHARGE_MINI;
                                    chargeD32 = (chargeD32 < CHARGE_MAXI) ? chargeD32 : CHARGE_MAXI;
                                    FOR_S16_TOP(suivanN, OS_CHARG0_2) = (int16_t)chargeD32;
                                    /* Assembly
                                        psc_appliquer_surcharge:
                                        ; application de la surcharge bx
                                        movsx ebx, bx ; charge à appliquer (word signé -> dword signé)
                                        movsx ecx, word [rdx+OS_CHARG0_2] ; charge actuelle du suivant (word signé -> dword signé)
                                        add ecx, ebx ; ajouter les charges
                                        ; limitation de la charge avant enregistrement
                                        mov eax, CHARGE_MINI
                                        cmp ecx, eax
                                        cmovl ecx, eax ; limiter ecx à la valeur minimale d'un word signé (excepté -32768 charge non valide)
                                        mov eax, CHARGE_MAXI
                                        cmp ecx, eax
                                        cmovg ecx, eax ; limiter ecx à la valeur maximale d'un word signé
                                        ; enregistrer la nouvelle charge
                                        mov [rdx+OS_CHARG0_2], cx
                                        ; fin
                                        jmp fin_traitement
                                        */
                                }
                                else {
                                    /* Assembly
                                        psc_isole:
                                            ; A_FAIRE : cas d'un segment qui n'a pas de neurone à alimenter : détruire ?
                                            jmp fin_traitement
                                        */
                                }
                            }
                            break;
                        }
                        case TYPE_NEURONE_GERME:
                        case TYPE_NEURONE_PARTIEL:
                        case TYPE_NEURONE_COMPLET: {
                            FOR_U8_TOP(portion, OS_ACTIVITE_1) = 0;
                            uint8_t decpteRef8 = U8_FROMP(portion, OSN_CREFR_1);
                            /* Assembly
                                ; portions neurone ---------------------------------------------------
                                portion_neurone_germe:
                                    ; A_FAIRE : coder le passage en mode partiel éventuel
                                    jmp fin_traitement
                                portion_neurone_partiel:
                                    ; A_FAIRE : coder la création des dendrites initiales
                                    ; A_FAIRE : coder la création de l'axone initiale
                                    ; A_FAIRE : conditionner tout ça à la vitesse de développement (?)
                                    ; A_FAIRE : coder le passage en mode complet
                                portion_neurone_complet: ; (rax/rbx/rcx/rdx)
                                    mov byte [r9+OS_ACTIVITE_1], 0 ; enlever la rétro-activité le cas échéant
                                    mov al, [r9+OSN_CREFR_1] ; decompte réfractaire en cours
                                    test al, al ; tester si al<>0
                                    jnz pnc_refractaire ; aller au traitement du cas réfractaire
                                */
                            if (decpteRef8 == 0) { // le neurone n'est plus refractaire
                                int16_t chargeD16 = S16_FROMP(portion, OS_CHARG0_2);
                                uint8_t pSeuil8 = U8_FROMP(portion, OSN_SEUIL_1);
                                uint32_t seuil32 = (1 << pSeuil8) - 1;
                                if (chargeD16 >= seuil32) { // il y a declenchement
                                    FOR_U8_TOP(portion, OS_ACTIVITE_1) = 1; // auto-activation
                                    uint8_t baseRefr8 = U8_FROMP(portion, OSN_BREFR_1);
                                    FOR_U8_TOP(portion, OSN_CREFR_1) = baseRefr8; // lancement compteur refractaire
                                    /* Assembly
                                        mov ax, [r9+OS_CHARG0_2] ; charge actuelle (word signé -32767 à 32767)
                                        mov cl, [r9+OSN_SEUIL_1] ; puissance de seuil avec décalage (maxi 15)
                                        mov edx, 1
                                        shl edx, cl
                                        dec edx ; seuil calculé (0 à 32767)
                                        cmp ax, dx ; tester si le seuil n'est pas atteint
                                        jl pnc_seuil_non_atteint ; aller au traitement du cas du seuil non atteint
                                            mov byte [r9+OS_ACTIVITE_1], 1 ; s'auto-activer / réctro-activer
                                            mov al, [r9+OSN_BREFR_1] ; récupérer la base réfractaire
                                            mov [r9+OSN_CREFR_1], al ; lancer la refractarité
                                            jmp fin_traitement
                                        */
                                }
                                else { // le seuil n'est pas encore atteint
                                    chargeD16 -= VITESSE_DECHARGE;
                                    chargeD16 = (chargeD16 < 0 ? 0 : chargeD16);
                                    FOR_S16_TOP(portion, OS_CHARG0_2) = chargeD16;
                                }
                            }
                            else { // le neurone est encore refractaire
                                decpteRef8 -= 1;
                                FOR_U8_TOP(portion, OSN_CREFR_1) = decpteRef8;
                                if (decpteRef8 == 0) {
                                    FOR_U16_TOP(portion, OS_CHARG0_2) = 0;
                                }
                            }
                            /* Assembly
                                pnc_refractaire:
                                    ; al est forcément positif en arrivant
                                    dec al
                                    mov [r9+OSN_CREFR_1], al
                                    cmp al, 0
                                    jne fin_traitement
                                        mov word [r9+OS_CHARG0_2], 0
                                        jmp fin_traitement
                                pnc_seuil_non_atteint:
                                    ; ax est la charge actuelle = word signé de -32767 à 32767
                                    sub ax, VITESSE_DECHARGE
                                    mov bx, 0
                                    cmp ax, 0
                                    cmovl ax, bx
                                    IL FAUT ENREGISTRER CETTE NOUVELLE VALEUR A OS_CHARG0_2
                                    jmp fin_traitement
                                */
                            break;
                        }
                        case TYPE_EXTENSION_GERME:
                        case TYPE_EXTENSION_PARTIEL:
                        case TYPE_EXTENSION_COMPLET: {
                            uint32_t antecedU = U32_FROMP(portion, OSE_ANT_U_4);
                            uint32_t antecedN = U32_FROMP(portion, OSE_ANT_N_4);
                            if (antecedU != myPackId) {
                                ajouter_note(myPackId, portion, 0, 0, antecedU, antecedN);
                                /* Assembly
                                    ; portions extensions (axonales) ---------------------------------------------------
                                    portion_extension_germe:
                                        ; A_FAIRE : coder le passage en mode partiel éventuel
                                        jmp fin_traitement
                                    portion_extension_partiel:
                                        ; A_FAIRE : coder la recherche de destination + réalisation du lien
                                        ; A_FAIRE : coder la création de l'extension suivante
                                        ; A_FAIRE : conditionner tout ça à la vitesse de développement (?)
                                        ; A_FAIRE : coder le passage en mode complet
                                    portion_extension_complet: ; (rax/rbx/rcx/rdx)
                                        mov eax, [r9+OSE_ANT_U_4]
                                        cmp eax, [myPackId]
                                        jne extension_demande_activite
                                    */
                            }
                            else {
                                if (antecedN == 0) {
                                    // auto-detruire ?
                                    /* Assembly
                                        mov eax, [r9+OSE_ANT_N_4] ; portion antécédente / num portion (eax=>rax)
                                        test eax, eax ; PORTION_ZERO ?
                                        jz pec_autodestruction
                                        */
                                }
                                else {
                                    uint32_t destinN = 0; // initialisation destination pour le cas "retroactivite seulement"
                                    FOR_U8_TOP(portion, OS_ACTIVITE_1) = 0; // auto-desactivation
                                    uint8_t activAntec8 = U8_FROMP(antecedN, OS_ACTIVITE_1); // activite de l'antecedent
                                    /* Assembly
                                        xor rdx, rdx ; initialiser l'adresse de la destination à 0 pour cas "retroactvité seulement"
                                        mov byte [r9+OS_ACTIVITE_1], 0 ; s'auto-desactiver (pour éventuel suivant)
                                        adresse_from_numero rbx, rax ; calculer l'adresse de l'antécédent
                                        mov al, [rbx+OS_ACTIVITE_1]
                                        */
                                    if (activAntec8 != 0) { // si l'antecedent est actif
                                        FOR_U8_TOP(portion, OS_ACTIVITE_1) = 1; // s'auto-activer (de nouveau)
                                        uint32_t destinU = U32_FROMP(portion, OSE_DST_U_4);
                                        /* Assembly
                                            test al, al
                                            jz pec_retrocompteur ; cas le plus fréquent => avant test d'existance de destination
                                                mov byte [r9+OS_ACTIVITE_1], 1 ; s'auto-activer (pour éventuel suivant)
                                                mov eax, [r9+OSE_DST_U_4] ; utilisateur destinataire
                                            */
                                        if (destinU != myPackId) {
                                            ajouter_note(myPackId, portion, 0, 0, destinU, destinN);
                                            /* Assembly
                                                    cmp eax, [myPackId]
                                                    jne extension_demande_charge ; r9 = adresse portion demandeuse / eax = destinataire
                                                */
                                        }
                                        else {
                                            uint32_t destinNtemporaire = U32_FROMP(portion, OSE_DST_N_4);
                                            if (destinNtemporaire != 0) {
                                                /* Assembly
                                                    mov eax, [r9+OSE_DST_N_4] ; portion destinataire
                                                    test eax, eax ; NEURONE_ZERO ?
                                                    jz fin_traitement ; pas de destination donc rien a faire
                                                    */
                                                destinN = destinNtemporaire; // forcage du vrai destinataire
                                                FOR_U8_TOP(portion, OSE_CACTT_1) = 1; // initier le compteur
                                                int16_t chargeD16 = S16_FROMP(destinN, OS_CHARG0_2);
                                                int8_t surcharge8 = S8_FROMP(portion, OSE_BSYNV_1);
                                                int32_t chargeD32 = (int32_t)(chargeD16) + surcharge8;
                                                chargeD32 = (chargeD32 > CHARGE_MINI) ? chargeD32 : CHARGE_MINI;
                                                chargeD32 = (chargeD32 < CHARGE_MAXI) ? chargeD32 : CHARGE_MAXI;
                                                FOR_S16_TOP(destinN, OS_CHARG0_2) = (int16_t)chargeD32;
                                                goto pec_retroaction; // BY-PASS VERS LA MISE EN OEUVRE DE LA RETROACTION AVAL
                                                /* Assembly
                                                    adresse_from_numero rdx, rax ; adresse de la destination -> rdx
                                                    mov byte [r9+OSE_CACTT_1], 1 ; initier le compteur
                                                    ; surcharge segment destination
                                                    movsx ecx, word [rdx+OS_CHARG0_2] ; charge actuelle destination (word signé -> dword signé)
                                                    movsx eax, byte [r9+OSE_BSYNV_1] ; valeur du bloc de boutons (byte signé -> dword signé)
                                                    add ecx, eax ; ajouter les charges du bloc de boutons
                                                    ; limitation de la charge avant enregistrement
                                                    mov eax, CHARGE_MINI
                                                    cmp ecx, eax
                                                    cmovl ecx, eax ; limiter ecx à la valeur minimale d'un word signé (excepté -32768 charge non valide)
                                                    mov eax, CHARGE_MAXI
                                                    cmp ecx, eax
                                                    cmovg ecx, eax ; limiter ecx à la valeur maximale d'un word signé
                                                    ; enregistrer la nouvelle charge
                                                    mov [rdx+OS_CHARG0_2], cx
                                                    jmp pec_retroaction
                                                    */
                                            }
                                        }
                                    }
                                    else { // gestion de l'evolution du compteur d'activite
                                        uint8_t comptAct8 = U8_FROMP(portion, OSE_CACTT_1);
                                        if (comptAct8 != 0) {
                                            comptAct8 += 1;
                                        }
                                        /* Assembly
                                            pec_retrocompteur: ; evolution ou pas du compteur d'activité
                                                mov cl, [r9+OSE_CACTT_1]
                                                test cl, cl
                                                jz pec_retroaction
                                                    inc cl
                                            */
                                        pec_retroaction: // POINT D ARRIVEE APRES SURCHARGE DE LA DESTINATION
                                        // tester si destination exterieure ?
                                        if (destinN != 0) {
                                            uint8_t actDest8 = U8_FROMP(destinN, OS_ACTIVITE_1);
                                            if (actDest8 != 0) {
                                                int8_t valblocb8 = S8_FROMP(portion, OSE_BSYNV_1);
                                                /* Assembly
                                                    pec_retroaction: ; retroaction aval sur bloc de boutons
                                                        ; A_FAIRE : tester si destination exterieure ?
                                                        test rdx, rdx ; la destination n'a pas été définie / pas de destination ?
                                                        jz fin_traitement
                                                        test byte [rdx+OS_ACTIVITE_1], 1 ; destination non rétro-active ?
                                                        jz fin_traitement
                                                        ; A_FAIRE : variation si test aléatoire au-dessus de la masse
                                                        ; jxx pec_desactiver_cpt
                                                            mov bl, [r9+OSE_BSYNV_1] ; valeur du bloc de boutons (byte signé)
                                                            mov cx, bx ; mise en réserve de la valeur du bloc de boutons
                                                    */
                                                uint8_t comptAct8 = U8_FROMP(portion, OSE_CACTT_1);
                                                uint8_t nivActivDest = U8_FROMP(destinN, OS_NIVACTMP_1);
                                                // modification de la valeur du bloc de boutons
                                                if (comptAct8 != nivActivDest) {
                                                    valblocb8 = (valblocb8 <= MINI_VALBB ? MINI_VALBB : valblocb8 - 1);
                                                }
                                                else {
                                                    valblocb8 = (valblocb8 >= MAXI_VALBB ? MAXI_VALBB : valblocb8 + 1);
                                                }
                                                FOR_U8_TOP(portion, OSE_BSYNV_1) = valblocb8;
                                                // modification de la masse de bloc de boutons
                                                uint8_t masblocb8 = U8_FROMP(portion, OSE_BSYNM_1);
                                                masblocb8 = (masblocb8 >= MAXI_MASSEBB ? MAXI_MASSEBB : masblocb8 + 1);
                                                FOR_U8_TOP(portion, OSE_BSYNM_1) = masblocb8;
                                                FOR_U8_TOP(portion, OSE_CACTT_1) = 0;
                                                /* Assembly
                                                            mov al, [r9+OSE_CACTT_1] ; compteur d'activité actuel
                                                            test byte [rdx+OS_NIVACTMP_1], al ; différence avec niveau d'activité de la destination
                                                            jz pec_negativer
                                                                add bl, 1 ; incrémenter la valeur du bloc de boutons
                                                                jmp pec_affectation_et_masse
                                                            pec_negativer:
                                                            sub bl, 1 ; décrémenter la valeur du bloc de boutons
                                                            pec_affectation_et_masse:
                                                            cmovo bx, cx ; bornage à la valeur précédente si on dépasse la capacité
                                                            mov [r9+OSE_BSYNV_1], bl ; enregistrer résultat
                                                            ; modification de la masse du bloc de boutons
                                                            mov bl, [r9+OSE_BSYNM_1] ; masse du bloc de boutons (byte non signé)
                                                            mov cl, bl ; mise en réserve de la masse du bloc de boutons
                                                            add bl, 1 ; incrémenter la valeur du bloc de boutons
                                                            cmovc bx, cx ; plafonnement à la valeur précédente
                                                            mov [r9+OSE_BSYNM_1], bl
                                                        pec_desactiver_cpt:
                                                        mov byte [r9+OSE_CACTT_1], 0 ; desactiver le compteur
                                                        jmp fin_traitement
                                                    */
                                            }
                                        }
                                    }
                                    // }
                                }
                            }
                            /* Assembly
                                    pec_autodestruction:
                                        ; A_FAIRE : auto-destruction
                                        jmp fin_traitement

                                */
                            break;
                        }
                    }
// #-------------------------------------------------------------------------------------------- MESSAGE A EMETTRE
                    /*
                    ; création des messages
                    lecteur_demande_datainfo:
                        jmp fin_ajouter_note
                    lecteur_demande_charge:
                        jmp fin_ajouter_note
                    pulseur_demande_charge:
                        jmp fin_ajouter_note
                    segment_demande_charge:
                        jmp fin_ajouter_note
                    extension_demande_activite:
                        jmp fin_ajouter_note
                    extension_demande_charge:
                        ; r11 -------------------------------> adresse de la prochaine note à écrire
                        ; r8 = portion demandeuse
                        ; rax/eax ---------------------------> utilisateur destinataire seulement
                        mov rbx, [myPackIdShifted]
                        add rbx, r8 ; -----------------------> portion demandeuse complète
                        mov r15, [r9+OSE_DST_UN_8] ; --------> portion destinataire complète
                        mov rdx, MC_SURCHARGE_QW
                        mov dl, [r9+OSE_BSYNV_1] ; ----------> demande complétée
                        jmp ajouter_note
                    ; enregistrement de la note
                    ajouter_note:
                        ; rbx ---------------------------> portion demandeuse complète
                        ; r15 ---------------------------> portion destinataire complete
                        ; eax/rax -----------------------> utilisateur destinataire seulement
                        ; r11 ---------------------------> adresse de la prochaine note à écrire
                        ; rdx ---------------------------> demande complète
                        adresse_from_user rcx, rax ; ----> adresse du destinataire dans users
                        ; insertion note
                        mov [r11+ON_EMMT_8], rbx
                        mov [r11+ON_DEST_8], r15
                        mov [r11+ON_DEMD_8], rdx
                        mov [r11+ON_ADRD_8], rcx
                        ; incrementation compteur de messages de l'utilisateur
                        add qword [rcx+OU_NMES_8], 1 ; compte nombre de messages pour cet utilisateur (partie 1)
                        ; calage pour suivante
                        add r11, TAILLE_DES_NOTES ; pointeur pour note suivante
                        ; add dword [valeurDeTest], 10000
                    fin_ajouter_note:
                    */
// #---------------------------------------------------------------------------------------- BOUCLAGE CYCLE + POST-TRAITEMENTS
                }
                /* Assembly
                    fin_traitement:
                    add r9, TAILLE_DES_PORTIONS
                    inc r8
                    cmp r8, USERLA_NB_P
                    jne bouclage_portions
                    */
                fin_cycle = clock(); // End time measurement
                duree_cycle = (double)(fin_cycle - debut_cycle) * 1000 / CLOCKS_PER_SEC;
                // Calcul plus precis (reactiver les occurences de nombre_de_cycles) :
                // nombre_de_cycles += 1;
                // duree_cycle = (double)((fin_cycle - timer_avant_rapides) / nombre_de_cycles) * 1000 / CLOCKS_PER_SEC;
                /* Assembly
                    ; Calcul durée de cycle
                    ; recuperation du procCycleCntr
                    xor rax, rax
                    rdtsc ; -> edx:eax
                    shl rdx, 32
                    or rdx, rax ; procCycleCntr condensé
                    mov [procCycleCntr_apres_bouclage_portions], rdx
                    ; durée en nombre de cycles processeur
                    mov rax, [procCycleCntr_avant_bouclage_portions]
                    sub rdx, rax ; durée boucle de traitement en cycles (environ 30_000_000 à vide)
                    ; calcul de la duree en nanoSecondes
                    shr rdx, 10 ; durée en kiloCycles (environ 30_000 à vide)
                    mov rax, DUREE_DE_CYCLE_PS ; en ps/cycle (369 sur mon ordi)
                    mul rdx ; durée en kilocycle*ps/cycle => ns/cycle (11_000_000 sur mon ordi à vide)
                    shr rax, 10 ; durée en us/cycle (environ 11_000 sur mon ordi à vide)
                    mov [dureeDeTraitement], rax ; stocker le resultat
                    */
// #---------------------------------------------------------------------------------------- EXPEDITION / RECEPTION MESSAGES RESEAU
                // verifier_portions();
                // Retraitement et preparation des paquets de messages a envoyer
                /* Assembly
                    ; fermeture de la pile de notes (derniere demande en r11 à 0)
                        mov qword [r11+ON_DEMD_8], 0
                    ; récupération du nombre d'utilisateurs
                        ; A_FAIRE : déplacer juste avant réception des messges ?
                        ; ou éventuellement lancer la réception dès ici !
                        mov r10, [nombreDUtilisateurs]
                    */
                nombreDePaquetsAEmettre = 0;
                pointeurDUnite = 0; // position de depart dans msgToSnd
                for (int util = 0; util < nombreDUtilisateurs; util++) {
                    nombreDeNotes = U64_FROMU(util, OU_NMES_8); // Nombre de notes pour ce paquet
                    if (nombreDeNotes != 0) {
                        /* Assembly
                            ; calcul des zones de messages dans msgToSend + initialisation des entêtes dans msgToSend
                            mov rbx, 0 ; ----------------------------------> compteur nombre total de notes/messages
                            lea r13, [users] ; ----------------------------> pointeur adresse premier utilisateur
                            lea r12, [msgToSnd] ; -------------------------> pointeur adresse premier paquet de messages à envoyer
                            mov r11, r10 ; dernier numero d'utilisateur
                            inc r11 ; -------------------------------------> nombre d'utilisateurs total
                            boucle_sur_utilisateurs:
                                mov rcx, [r13+OU_NMES_8] ; ----------------> nombre de notes/messages pour l'utilisateur
                                test rcx, rcx
                                jz fin_calcul_zone
                            */
                        // remplissage de l'en-tete
                        FOR_U32_TOM(pointeurDUnite, OMP_IPAD_4) = U32_FROMU(util, OU_IPAD_4); // Adresse IP
                        FOR_U16_TOM(pointeurDUnite, OMP_PORT_2) = U16_FROMU(util, OU_PORT_2); // Port IP
                        FOR_U16_TOM(pointeurDUnite, OMP_VIDE_2) = U16_FROMU(util, OU_VIDE_2); // Vide
                        FOR_U64_TOM(pointeurDUnite, OMP_TAIP_8) = nombreDeNotes; // Nombre de notes pour ce paquet
                        pointeurDUnite++; // Position pour la premiere note de ce paquet
                        FOR_U64_TOU(util, OU_ADRP_8) = pointeurDUnite; // Position ou ecrire la premiere note
                        pointeurDUnite += nombreDeNotes; // Position du debut du prochain paquet
                        FOR_U64_TOU(util, OU_NMES_8) = 0; // mise a zero du nombre de notes (pour la fois suivante)
                        nombreDePaquetsAEmettre += 1;
                        /* Assembly
                            ; Cumul du nombre notes/messages
                            add rbx, rcx
                            ; Dans msgToSend : IP + port destinataire
                            mov rax, [r13+OU_IPAD_4]
                            mov [r12+OMP_IPAD_4], rax
                            ; Dans msgToSend : Longueur de la plage utile pour protocole UDP
                            longueur_from_quantity rdx, rcx
                            mov [r12+OMP_TAIP_8], rdx
                            ; Dans users : Pointeur de debut de plage utile dans msgToSend
                            mov rax, r12
                            add rax, TAILLE_DES_MESSAGES
                            mov [r13+OU_ADRP_8], rax
                            ; Dans users : Effacement du nombre de notes
                            mov qword [r13+OU_NMES_8], 0
                            ; Dans msgToSend : Adresse mémoire du pilote de la plage suivante
                            add rax, rdx
                            mov [r12+OMP_ADPS_8], rax
                            ; Pointage sur début de plage suivante dans msgToSend
                            mov r12, rax
                            */
                    }
                }
                /* Assembly
                        fin_calcul_zone:
                        ; compteurs de boucle
                        add r13, TAILLE_DES_UTILISATEURS ; adresse utilisateur suivante
                        sub r11, 1 ; compteur utilisateurs
                    jnz boucle_sur_utilisateurs
                    */
                // cloture de msgToSend
                // FOR_U32_TOM(pointeurDUnite, OMP_IPAD_4) = 0; // Adresse IP du message suivant a 0 pour cloture de msgToSnd
                /* Assembly
                        ; cloture des mesToSend
                        mov qword [r12+OMP_IPAD_4], 0
                        ; enregistrement du nombre de messages cumulé
                        add [nombreDeMessageAEmettre], rbx
                    ; add qword [valeurDeTest], 1
                    */
                // Initialiser l'emplacement du premier message a envoyer
                // (deplacement de l'assembleur avant bouclage cycle)
                /* A utiliser ?
                    FOR_U32_TOM(0, OMP_IPAD_4) = 0;
                    FOR_U16_TOM(0, OMP_PORT_2) = 0;
                    FOR_U16_TOM(0, OMP_VIDE_2) = 0;
                    FOR_U64_TOM(0, OMP_ADPS_8) = 0;
                    FOR_U64_TOM(0, OMP_TAIP_8) = 0;
                    */
                /* A TRANSFORMER DIRECTEMENT EN C ?
                    ; initialisation messages à envoyer (premier message à 0)
                    lea rdx, [msgToSnd]
                    mov dword [rdx+OMP_IPAD_4], 0
                    mov word [rdx+OMP_PORT_2], 0
                    mov word [rdx+OMP_VIDE_2], 0
                    mov qword [rdx+OMP_ADPS_8], 0
                    mov qword [rdx+OMP_TAIP_8], 0
                    */
                if (nombreDePaquetsAEmettre != 0) {
                    for (int note = 0; note < notesATraiter; note++) {
                        destinataireNote = U32_FROMN(note, ON_DESU_4);
                        // numero de message en cours pour cet utilisateur
                        emplacementMessage = U64_FROMU(destinataireNote, OU_ADRP_8);
                        // enregistrement du message
                        FOR_U32_TOM(emplacementMessage, OMM_EMMP_4) = U32_FROMN(note, ON_EMMP_4);
                        FOR_U32_TOM(emplacementMessage, OMM_EMMU_4) = U32_FROMN(note, ON_EMMU_4);
                        FOR_U32_TOM(emplacementMessage, OMM_DESP_4) = U32_FROMN(note, ON_DESP_4);
                        FOR_U32_TOM(emplacementMessage, OMM_DESU_4) = U32_FROMN(note, ON_DESU_4);
                        FOR_U32_TOM(emplacementMessage, OMM_DEMC_4) = U32_FROMN(note, ON_DEMC_4);
                        FOR_U32_TOM(emplacementMessage, OMM_DEMP_4) = U32_FROMN(note, ON_DEMP_4);
                        // calage sur suivant pour cet utilisateur
                        FOR_U64_TOU(destinataireNote, OU_ADRP_8) = emplacementMessage + 1;
                        valeurDeTest += 1;
                    }
                }
                /* Assembly
                    ; transfert des notes vers msgToSend
                        cmp rbx, 0
                        je fin_expedition_messages 
                        ; adresse première note à copier
                        lea r11, [msgStack]
                        boucle_sur_messages:
                            ; récupération des données du message
                            mov rax, [r11+ON_EMMT_8] ; emetteur complet
                            mov rdx, [r11+ON_DEST_8] ; destinataire complet
                            mov rcx, [r11+ON_DEMD_8] ; demande complète
                            mov r12, [r11+ON_ADRD_8] ; adresse mémoire destinataire/user
                            ; récupération de l'adresse d'enregistrement du message
                            mov r13, [r12+OU_ADRP_8] ; adresse message à envoyer en cours
                            ; enregistrement du message
                            mov [r13+OMM_EMMT_8], rax ; emetteur complet complèt
                            mov [r13+OMM_DEST_8], rdx ; destinataire complète
                            mov [r13+OMM_DEMD_8], rcx ; demande complète
                            ; suivant dans users
                            add qword [r12+OU_ADRP_8], TAILLE_DES_MESSAGES
                            ; bouclage
                            add r11, TAILLE_DES_NOTES
                            sub rbx, 1
                        jnz boucle_sur_messages
                    */
                // Expedition des paquets de messages
                if (!SetEvent(sendThreadEventHandle)) {
                    printf("Erreur lors de l'activation de l'evenement d'envoi. Code : %lu\n", GetLastError());
                    CloseHandle(sendThreadEventHandle);
                    return 1;
                }
                while (!sendThreadEnded) {
                    Sleep(1); // Pour éviter un spinlock trop agressif
                }
                sendThreadEnded = FALSE;
                /* Assembly
                    ; thread expédition
                        ; jmp fin_expedition_messages
                        ; sauvegarde des variables
                        mov [r10_backup], r10
                        ; declenchement envoi messages
                        mov rcx, [sendThreadEventHandle]
                        sub rsp, SHADOW_SPACE_SIZE
                        call SetEvent
                        add rsp, SHADOW_SPACE_SIZE
                        ; validation fin d'envoi
                        attente_expe:
                            mov cl, [sendThreadEnded]
                            cmp cl, 1
                        jne attente_expe
                        mov byte [sendThreadEnded], 0
                        ; récupération des variables
                        mov r10, [r10_backup]
                        fin_expedition_messages:
                    */
                // Reception des messages (a faire)
                /* Assembly
                    ; réception / traitement messages
                        ; réceptionner les messages
                        ; en faire un seul paquet
                        ; pour chaque message, le traiter et créer eventuellement le message de réponse
                        reponse_a_demande_de_charge:
                    ; ré-enregistrer le nombre d'utilisateurs eventuellement modifié
                        mov qword [nombreDUtilisateurs], r10
                    */
// #------------------------------------------------------------------------------------ BOUCLAGES DIVERS
                if (mode_pas_a_pas != 0) {
                    break;
                }
                /* Assembly
                    ; forcage sortie si mode pas à pas
                        mov bl, [mode_pas_a_pas]
                        test bl, bl
                        jnz boucle_moyenne_suite
                    */
                timer_en_fin_de_rapides = clock();
                dureeDePostTraitement = (double)(timer_en_fin_de_rapides - fin_cycle) * 1000 / CLOCKS_PER_SEC;
                duree_cumul_des_rapides = (double)(timer_en_fin_de_rapides - timer_avant_rapides) * 1000 / CLOCKS_PER_SEC;
                /* Assembly
                    ; durée de post traitement
                        ; recuperation du procCycleCntr
                        xor rax, rax
                        rdtsc ; -> edx:eax
                        shl rdx, 32
                        or rdx, rax ; procCycleCntr condensé
                        ; durée en nombre de cycles processeur
                        mov rax, [procCycleCntr_apres_bouclage_portions]
                        sub rdx, rax ; durée du post traitement en cycles processeur
                        ; calcul de la duree en nanoSecondes
                        shr rdx, 10 ; durée en kiloCycles
                        mov rax, DUREE_DE_CYCLE_PS ; en ps
                        mul rdx ; durée en kilocycle*ps/cycle => ns/cycle
                        shr rax, 10 ; durée en us/cycle
                        mov [dureeDePostTraitement], rax ; stocker le resultat
                    */
            } while (duree_cumul_des_rapides < DUREE_BOUCLE_MOYENNE_MS);
            /* Assembly
                    ; conditionnement de sortie bouclage rapide
                    mov rax, [procCycleCntr_avant_bouclage_rapide] ; récupération début de boucle
                    mov rcx, [procCycleCntr_apres_bouclage_portions] ; recupération fin de cycle
                    sub rcx, rax ; durée de la boucle rapide
                    cmp rcx, NBCYCLES_BOUCLE_MOYENNE
                jb bouclage_rapide
                boucle_moyenne_suite:
                */
            retracer_la_fenetre();
            /* Assembly
                ; masque + demande de retraçage
                    call retracer_la_fenetre
                */
            if (mode_pas_a_pas != 0) {
                verifier_portions();
                ecrire_messages_console();
            }
            /* Assembly
                ; ecritures si pas à pas
                mov bl, [mode_pas_a_pas]
                test bl, bl
                jz pas_d_ecriture_apres_boucle_rapide
                    call ecrire_messages_console
                pas_d_ecriture_apres_boucle_rapide:
                */
            BOOL there_is_message;
            MSG msg = {0};
            while (1) {
                /* Assembly
                    ; ----- liberation / traitement des messages fenetre en attente
                    messagesLoop:
                    */
                messagesLoop:
                there_is_message = PeekMessageA(&msg, NULL, 0, 0, PM_NOREMOVE);
                /* Assembly
                    ; lecture des messages windows un par un
                        lea   rcx, [WindowMessage]              ; lpMsg
                        xor   edx, edx                          ; hWnd
                        xor   r8d, r8d                          ; wMsgFilterMin
                        xor   r9d, r9d                          ; wMsgFilterMax
                        sub   rsp, 8
                        push  PM_NOREMOVE                       ; wRemoveMsg
                        sub   rsp, SHADOW_SPACE_SIZE
                        call  PeekMessageA
                        add   rsp, SHADOW_SPACE_SIZE
                        add   rsp, 16
                    ; tests pour sorties
                    */
                if (!there_is_message) {
                    if (mode_pas_a_pas == 0) {
                        break; // sortie si plus de message + pas mode pas-a-pas
                    }
                }
                /* Assembly
                    test rax, rax
                    jnz continue_MLoop_1 ; on continue la boucle s'il y a un message à traiter
                        test bl, bl ; (mode pas à pas)
                        jz traitementMessagesFini ; on sort si pas mode pas-à-pas (et pas de message à traiter)
                    continue_MLoop_1:
                    */
                if (mode_pas_a_pas == 2) {
                    mode_pas_a_pas = 1;
                    break; // sortie si N appuye en mode pas-a-pas
                }
                /* Assembly
                    mov bl, [mode_pas_a_pas]
                    cmp bl, 2
                    jne continue_MLoop_2 ; on continue si pas de demande d'avance d'un pas en pas-à-pas (mode_pas_a_pas <> 2)
                        mov bl, 1
                        mov [mode_pas_a_pas], bl
                        jmp traitementMessagesFini ; on sort si demande d'avance d'un pas (donc mode pas-à-pas)
                    continue_MLoop_2:
                    */
                if (there_is_message) {
                    if (mode_pas_a_pas != 0) {
                        retracer_la_fenetre();
                        verifier_portions();
                        ecrire_messages_console();
                    }
                }
                /* Assembly
                    ; demande de retraçage
                    test rax, rax
                    jz continue_MLoop_3 ; on ne fait rien si pas de message (donc mode pas-à-pas)
                        test bl, bl
                        jz continue_MLoop_3 ; on retrace si mode pas à pas + message
                            call retracer_la_fenetre
                            call ecrire_messages_console
                    continue_MLoop_3:
                    */
                there_is_message = GetMessageA(&msg, NULL, 0, 0);
                /* Assembly
                    ; recuperation du message
                        lea   rcx, [WindowMessage]              ; lpMsg
                        xor   edx, edx                          ; hWnd
                        xor   r8d, r8d                          ; wMsgFilterMin
                        xor   r9d, r9d                          ; wMsgFilterMax
                        sub   rsp, SHADOW_SPACE_SIZE
                        call  GetMessageA
                        add   rsp, SHADOW_SPACE_SIZE
                    */
                if (there_is_message == 0) {
                    goto sortieComplete;
                }
                /* Assembly
                    ; sortie si c'est une demande de fermeture WM_QUIT
                        cmp   rax, 0
                        je    sortieComplete                    ; on arrete tout = sortie toutes boucles
                    */
                if (!IsDialogMessageA(gWindowHandle, &msg)) {
                    /* Assembly
                        ; analyse du message
                            mov   rcx, qword [Windowhandle]         ; hDlg
                            lea   rdx, [WindowMessage]                ; lpMsg
                            sub   rsp, SHADOW_SPACE_SIZE
                            call  IsDialogMessageA                  ; For keyboard strokes (?)
                            add   rsp, SHADOW_SPACE_SIZE
                        ; bouclage si autre chose d'une touche clavier
                            cmp   rax, 0
                            jne   messagesLoop                 ; le message a ete traite par IsDialogMessageA => on reboucle
                        */
                    TranslateMessage(&msg);
                    /* Assembly
                        ; traduction du message
                            lea   rcx, [WindowMessage]                ; lpMsg
                            sub   rsp, SHADOW_SPACE_SIZE
                            call  TranslateMessage
                            add   rsp, SHADOW_SPACE_SIZE
                        ; diffusion du message
                        */
                    DispatchMessageA(&msg);
                    /* Assembly
                        lea   rcx, [WindowMessage]            ; lpMsg
                        sub   rsp, SHADOW_SPACE_SIZE
                        call  DispatchMessageA
                        add   rsp, SHADOW_SPACE_SIZE
                        ; c'est ici qu'il faudrait retracer la fenêtre ?
                        */
                }
            }
            /* Assembly
                ; bouclage messages
                jmp messagesLoop
                ; sortie de la boucle des messages
                traitementMessagesFini:
                */
            // Sortie uniquement sur un break :
            // - soit pas mode pas-a-pas + tous les messages ont ete traites
            // - mode pas-a-pas + soit appui sur N
            timer_en_fin_de_moyennes = clock(); // End time measurement
            duree_cumul_des_moyennes = (double)(timer_en_fin_de_moyennes - timer_avant_moyennes) * 1000 / CLOCKS_PER_SEC;
            /* Assembly
                xor rax, rax
                rdtsc ; -> edx:eax
                shl rdx, 32
                or rdx, rax ; procCycleCntr condensé
                */
        } while (duree_cumul_des_moyennes < DUREE_BOUCLE_LENTE_MS);
        /* Assembly
            ; conditionnement de sortie bouclage moyen
                mov rax, [procCycleCntr_avant_bouclage_moyen] ; récupération début de boucle
                sub rdx, rax ; durée de la boucle moyenne
                mov rax, NBCYCLES_BOUCLE_LENTE
                cmp rdx, rax
            ; bouclage moyen
            jb bouclage_moyen
            */
        numero_de_boucle_lente += 1;
        /* Assembly
            apres_boucle_moyenne:
            mov rax, qword [numero_de_boucle_lente]
            add rax, 1
            mov qword [numero_de_boucle_lente], rax
            */
        if (mode_pas_a_pas == 0) {
            verifier_portions();
            ecrire_messages_console();
        }
        /* Assembly
            ; écritures si pas mode pas-à-pas
            mov bl, [mode_pas_a_pas]
            test bl, bl
            jnz pas_d_ecriture_dans_boucle_lente
                call ecrire_messages_console
            pas_d_ecriture_dans_boucle_lente:
            ; bouclage lent
            jmp bouclage_lent
            */
    }
// #------------------------------------------------------------------------------------ SORTIE
    sortieComplete:
    // Nettoyage
    // closesocket(recvSocketHandle);
    closesocket(sendSocketHandle);
    WSACleanup();
    /* Assembly
        sortieComplete:
        ; cloture reseau
            ; ----- fermer la socket de contrôle
            mov rcx, [sendSocketFileDescriptor]
            sub rsp, SHADOW_SPACE_SIZE
            call closesocket
            add rsp, SHADOW_SPACE_SIZE
            ; cmp rax, CLOSE_SOCKET_OK
            ; jne erreur_closeSocket
            ; ----- nettoyer WinSock
            sub rsp, SHADOW_SPACE_SIZE
            call WSACleanup
            add rsp, SHADOW_SPACE_SIZE
        ; Terminer le thread de force après un délai
            ; pas de solution de forçage propre depuis l'exterieur du thread
            ; a priori automatique à la fermeture du processus
        ; finalisation ----------------------------------------------
            call_winapi64_style GetStdHandle, STD_OUTPUT_HANDLE
            mov r15, rax
            call_datapush_style ecrire_ligne_4, r15, interligne_text, r15, -1
        */
    // Liberation de la memoire
    free(bitmapData);
    free(rawData_Texte);
    // Cleanup console
    CleanupConsole();
    return 0;
    /* Assembly
        ; fin du programme
        ; add rsp, 8
        xor rcx, rcx
        call ExitProcess
        */
}
// #------------------------------------------------------------------------------------ TRAITEMENT DES ERREUR
    /*
    ; erreur_wsaStartup:
    ;     call_datapush_style ecrire_ligne_4, r15, texte_erreur_WsaStartup, 0, -1
    ; jmp continue_or_not
    ; erreur_creationSocket:
    ;     call_datapush_style ecrire_ligne_4, r15, texte_erreur_creationSocket, 0, -1
    ; jmp continue_or_not
    ; erreur_sendTo:
    ;     call_datapush_style ecrire_ligne_4, r15, texte_erreur_sendTo, 0, -1
    ; jmp continue_or_not
    ; erreur_closeSocket:
    ;     call_datapush_style ecrire_ligne_4, r15, texte_erreur_closeSocket, 0, -1
    ; jmp continue_or_not
    ; continue_or_not:
    ;     mov rax, [ErrorsBreak]
    ;     cmp rax, 0

    ; jmp sortieComplete
    */
// #-------------------------------- c : definition des fonctions
    uint32_t xyz_to_u32(id_prt id) {
        return id.x + id.y*FY + id.z*FZ;
    }
    id_prt u32_to_xyz(uint32_t num) {
        uint16_t z = (uint16_t)(num / FZ);
        uint16_t y = (uint16_t)((num - z*FZ) / FY);
        uint16_t x = (uint16_t)(num - z*FZ - y*FY);
        return (id_prt){x, y, z};
    }
    void CleanupConsole() {
            FreeConsole();
            printf("Console released successfully.\n");
    }
    int RegisterWindowClass(HINSTANCE hInstance) {
        WNDCLASSEXA windowClass = {0};
        windowClass.cbSize = sizeof(WNDCLASSEXA);
        windowClass.style = CS_HREDRAW | CS_VREDRAW;
        windowClass.lpfnWndProc = WindowProc;
        windowClass.hInstance = hInstance;
        windowClass.hCursor = LoadCursor(NULL, IDC_ARROW);
        windowClass.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
        windowClass.lpszClassName = className;
        if (!RegisterClassExA(&windowClass)) {
            printf("Failed to register window class\n");
            return 0;
        }
        return 1;
    }
    void CreateGraphicWindow(HINSTANCE hInstance) {
        gWindowHandle = CreateWindowExA(
            0,                                    // Extended window style
            "GraphicWindowClass",                  // Window class name
            "Main Application Window",         // Window name
            WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_VISIBLE, // Window style
            100, 100,                            // Position (x, y)
            LARGEUR_FENETRE, HAUTEUR_FENETRE,    // Size (width, height)
            NULL,                                // Parent window
            NULL,                                // Menu
            hInstance,                           // Instance handle
            NULL                                 // Additional application data
        );
        if (!gWindowHandle) {
            printf("Failed to create main window!");
            exit(-1);
        }
    }
    void InitializeBitmap() {
        // Set up the bitmap information header
        bitmapInfo.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
        bitmapInfo.bmiHeader.biWidth = LARGEUR_FENETRE;
        bitmapInfo.bmiHeader.biHeight = -HAUTEUR_FENETRE; // Top-down bitmap
        bitmapInfo.bmiHeader.biPlanes = 1;
        bitmapInfo.bmiHeader.biBitCount = NOMBRE_OCTET_PAR_POINT * 8;
        bitmapInfo.bmiHeader.biCompression = BI_RGB;
        bitmapInfo.bmiHeader.biSizeImage = 0;
        bitmapInfo.bmiHeader.biXPelsPerMeter = 0;
        bitmapInfo.bmiHeader.biYPelsPerMeter = 0;
        bitmapInfo.bmiHeader.biClrUsed = 0;
        bitmapInfo.bmiHeader.biClrImportant = 0;

        // Allocate memory for the bitmap data
        size_t bitmapSize = LARGEUR_FENETRE * HAUTEUR_FENETRE * NOMBRE_OCTET_PAR_POINT;
        bitmapData = (uint8_t *)malloc(bitmapSize);
        if (!bitmapData) {
            printf("Failed to allocate memory for bitmap");
            exit(-1);
        }
        // Clear the bitmap data
        memset(bitmapData, 0, bitmapSize);
    }
    void ReadInputFile() {
        HANDLE fileHandle = CreateFileA(
            INPUT_FILE_NAME, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
        if (fileHandle == INVALID_HANDLE_VALUE) {
            printf("Failed to open input text file");
            exit(-1);
        }
        LARGE_INTEGER fileSize;
        if (!GetFileSizeEx(fileHandle, &fileSize)) {
            printf("Failed to get file size");
            CloseHandle(fileHandle);
            exit(-1);
        }
        size_t maxSize = NOMBRE_MAX_SEGMENTS_TEXTE * LARGEUR_DE_SEGMENT_DE_TEXTE;
        longueur_Texte = (size_t)fileSize.QuadPart;
        if (longueur_Texte > maxSize) {
            longueur_Texte = maxSize;
        }
        rawData_Texte = (uint8_t *)malloc(longueur_Texte);
        if (!rawData_Texte) {
            printf("Failed to allocate memory for text data");
            CloseHandle(fileHandle);
            exit(-1);
        }
        DWORD bytesRead;
        if (!ReadFile(fileHandle, rawData_Texte, (DWORD)longueur_Texte, &bytesRead, NULL) || bytesRead != longueur_Texte) {
            printf("Failed to read input file.");
            free(rawData_Texte);
            CloseHandle(fileHandle);
            exit(-1);
        }
        CloseHandle(fileHandle);
    }
    void InitializeData() {
        for (int z = 0; z < DIMENSION_Z; z++) {
            for (int y = 0; y < DIMENSION_Y; y++) {
                for (int x = 0; x < DIMENSION_X; x++) {
                    memset(crvDatas[z][y][x], 0, TAILLE_DES_PORTIONS); // Zero initialize each element
                }
            }
        }
        printf("3D data array initialized with 32 bytes per element.\n");
    }
    void LoadCervelet() {
        // Ouverture du fichier
        HANDLE Filehandle = CreateFileA(
            CERVELET_FILE_NAME, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
        if (Filehandle == INVALID_HANDLE_VALUE) {
            printf("Erreur ouverture fichier");
            exit(-1);
        }
        LARGE_INTEGER longueurFichier;
        if (!GetFileSizeEx(Filehandle, &longueurFichier)) {
            printf("Erreur obtention taille fichier");
            CloseHandle(Filehandle);
            exit(-1);
        }
        // Vérification cohérence longueur
        if (longueurFichier.QuadPart != USERLA_NB_P * TAILLE_DES_PORTIONS) {
            printf("Erreur: taille fichier incorrecte\n");
            CloseHandle(Filehandle);
            exit(-1);
        }
        // Lecture du contenu dans portions
        if (!ReadFile(Filehandle, crvDatas, (DWORD)longueurFichier.QuadPart, NULL, NULL)) {
            printf("Erreur lecture fichier");
            CloseHandle(Filehandle);
            exit(-1);
        }
        // Fermeture du fichier
        CloseHandle(Filehandle);
        // Recherche des portions sources et écriture des paramètres
        uint64_t adresse = (uint64_t)(uintptr_t)rawData_Texte;
        for (uint32_t numPort = 0; numPort < USERLA_NB_P; numPort++) {
            uint8_t TypeDePortion = U8_FROMP(numPort, OS_TYPESTEV_1);
            if (TypeDePortion == TYPE_SOURCE) {
                FOR_U64_TOP(numPort, OSD_ADDRS_8) = adresse;
            }
        }
    }
    void SaveCervelet() {
        // Ouverture du fichier en mode écriture
        HANDLE Filehandle = CreateFileA(
            CERVELET_FILE_NAME, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
        if (Filehandle == INVALID_HANDLE_VALUE) {
            printf("Erreur ouverture fichier pour sauvegarde");
            exit(-1);
        }
        // Écriture du contenu des portions dans le fichier
        DWORD bytesWritten;
        if (!WriteFile(Filehandle, crvDatas, USERLA_NB_P * TAILLE_DES_PORTIONS, &bytesWritten, NULL)) {
            printf("Erreur écriture fichier");
            CloseHandle(Filehandle);
            exit(-1);
        }
        if (bytesWritten != USERLA_NB_P * TAILLE_DES_PORTIONS) {
            printf("Erreur: nombre d'octets écrits incorrect\n");
            CloseHandle(Filehandle);
            exit(-1);
        }
        // Fermeture du fichier
        CloseHandle(Filehandle);
    }
    void ajouter_note(uint32_t dmdr_usr, uint32_t dmdr_prt, uint32_t demande, uint32_t parametre, uint32_t dest_usr, uint32_t dest_prt) {
        // ajouter la note
        FOR_U32_TON(notesATraiter, ON_EMMP_4) = dmdr_prt;
        FOR_U32_TON(notesATraiter, ON_EMMU_4) = dmdr_usr;
        FOR_U32_TON(notesATraiter, ON_DESP_4) = dest_prt;
        FOR_U32_TON(notesATraiter, ON_DESU_4) = dest_usr;
        FOR_U32_TON(notesATraiter, ON_DEMC_4) = demande;
        FOR_U32_TON(notesATraiter, ON_DEMP_4) = parametre;
            // uint64_t adresse = 0;
            // FOR_U64_TON(notesATraiter, ON_ADRD_8) = adresse;
        // incrementation du nombre de messages pour cet utilisateur
        uint64_t nbrMess = U64_FROMU(dest_usr, OU_NMES_8);
        FOR_U64_TOU(dest_usr, OU_NMES_8) = nbrMess + 1;
        notesATraiter += 1;
        //
    }
// #---------------------------------------------------------------------------------------- OUTILS
    // ; Outils de portions
    void verifier_portions() {
        erreurCibleNulle = 0;
        erreurCibleIncoherente = 0;
        /* Assembly
            verifier_portions:
            ; sub rsp, 8
            push r8
            push r9
            push r10
            push r11
            push r13
            push rax
            push rdx
            ;
            mov qword [erreurCibleNulle], 0
            mov qword [erreurCibleIncoherente], 0
            mov r10, 0
            mov r11, 0
            ; initialisation boucle portions
            mov r8, 0 ; premiere portion
            adresse_from_numero r9, r8 ; calcul adresse de la première portion
            */
        for (uint32_t portion = 0; portion < DIMENSION_Z*DIMENSION_Y*DIMENSION_X; portion++) {
            /* Assembly
                mov r8, 0 ; initialisation numéro de la première portion
                vp_boucle:
                */
            uint8_t typestev = U8_FROMP(portion, OS_TYPESTEV_1);
            switch (typestev) {
                /* Assembly
                    ; aiguillage vers traitements concernés
                        mov al, [r9+OS_TYPESTEV_1] ; récuperation du type de portion
                        comparer_et_jump_si_egal al, TYPE_LECTEUR,                          vp_lecteur
                        comparer_et_jump_si_egal al, TYPE_PULSEUR,                          vp_pulseur
                        and al, MASQUE_TYPE_TSx
                        comparer_et_jump_si_egal al, TYPE_SEGMENT_GERME,                    vp_segment
                        comparer_et_jump_si_egal al, TYPE_EXTENSION_GERME,                  vp_extension
                        ; vérifier TYPE_SOURCE ?
                        jmp vp_fin_traitement
                    */
                case TYPE_LECTEUR: {
                    uint32_t portAccesMemU = U32_FROMP(portion, OSL_PAM_U_4);
                    if (portAccesMemU = myPackId) {
                        uint32_t portAccesMemN = U32_FROMP(portion, OSL_PAM_N_4);
                        if (portAccesMemN = 0) {
                            erreurCibleNulle += 1;
                        }
                        else {
                            uint8_t typeSource = U8_FROMP(portAccesMemN, OS_TYPESTEV_1);
                            if (typeSource != TYPE_SOURCE) {
                                erreurCibleIncoherente += 1;
                            }
                        }
                    }
                    /* Assembly
                        vp_lecteur:
                            ; CIBLE PORTION MEMOIRE
                            mov eax, [r9+OSL_PAM_U_4]
                            cmp eax, [myPackId]
                            jne vp_lecteur_suite
                                mov eax, [r9+OSL_PAM_N_4] ; portion des paramètres de l'accès mémoire
                                test eax, eax ; PORTION_ZERO ?
                                jz vp_lecteur_suite
                                    adresse_from_numero r13, rax ; r13 = adresse des paramètres de l'accès mémoire
                                    movzx rdx, byte [r13+OS_TYPESTEV_1] ; type de cible
                                    ; bloc de verifications propres
                                    cmp rdx, TYPE_SOURCE
                                    je vp_lecteur_suite
                                        inc r10 ; -> cible nulle ou incohérente
                                        cmp rdx, 0
                                        je vp_lecteur_suite
                                            inc r11 ; -> cible incohérente
                                            ; FAIRE ICI LA CORRECTION EVENTUELLE
                                            ; je vp_lecteur_suite
                        */
                    uint32_t portDestinationU = U32_FROMP(portion, OSL_DST_U_4);
                    if (portDestinationU = myPackId) {
                        uint32_t portDestinationN = U32_FROMP(portion, OSL_DST_N_4);
                        if (portDestinationN = 0) {
                            erreurCibleNulle += 1;
                        }
                        else {
                            uint8_t typeDestin = U8_FROMP(portDestinationN, OS_TYPESTEV_1);
                            if (typeDestin != TYPE_SEGMENT_COMPLET && typeDestin != TYPE_NEURONE_COMPLET) {
                                erreurCibleIncoherente += 1;
                            }
                        }
                    }
                    /* Assembly
                        vp_lecteur_suite:
                            ; CIBLE DESTINATION SEGMENT OU NEURONE
                            mov eax, [r9+OSL_DST_U_4]
                            cmp eax, [myPackId]
                            jne vp_fin_traitement
                                mov eax, [r9+OSL_DST_N_4] ; portion destination / num portion
                                test eax, eax ; PORTION_ZERO ?
                                jz vp_fin_traitement
                                    adresse_from_numero r13, rax ; r13 = adresse de la destination
                                    movzx rdx, byte [r13+OS_TYPESTEV_1] ; type de cible
                                    and rdx, MASQUE_TYPE_TSx
                                    ; bloc de verifications propres
                                    cmp rdx, TYPE_SEGMENT_GERME
                                    je vp_fin_traitement
                                    cmp rdx, TYPE_NEURONE_GERME
                                    je vp_fin_traitement
                                        inc r10 ; -> cible nulle ou incohérente
                                        cmp rdx, 0
                                        je vp_fin_traitement
                                            inc r11 ; -> cible incohérente
                                            ; FAIRE ICI LA CORRECTION EVENTUELLE
                                            jmp vp_fin_traitement
                        */
                    break;
                }
                case TYPE_PULSEUR: {
                    uint32_t portDestinationU = U32_FROMP(portion, OSP_DST_U_4);
                    if (portDestinationU = myPackId) {
                        uint32_t portDestinationN = U32_FROMP(portion, OSP_DST_N_4);
                        if (portDestinationN = 0) {
                            erreurCibleNulle += 1;
                        }
                        else {
                            uint8_t typeDestin = U8_FROMP(portDestinationN, OS_TYPESTEV_1);
                            if (typeDestin != TYPE_SEGMENT_COMPLET && typeDestin != TYPE_NEURONE_COMPLET) {
                                erreurCibleIncoherente += 1;
                            }
                        }
                    }
                    /* Assembly
                        vp_pulseur:
                            ; CIBLE DESTINATION SEGMENT OU NEURONE
                            mov eax, [r9+OSP_DST_U_4]
                            cmp eax, [myPackId]
                            jne vp_fin_traitement
                                mov eax, [r9+OSP_DST_N_4] ; portion destination / num portion
                                test eax, eax ; PORTION_ZERO ?
                                jz vp_fin_traitement
                                    adresse_from_numero r13, rax ; r13 = adresse de la destination
                                    movzx rdx, byte [r13+OS_TYPESTEV_1] ; type de cible
                                    and rdx, MASQUE_TYPE_TSx
                                    ; bloc de verifications propres
                                    cmp rdx, TYPE_SEGMENT_GERME
                                    je vp_fin_traitement
                                    cmp rdx, TYPE_NEURONE_GERME
                                    je vp_fin_traitement
                                        inc r10 ; -> cible nulle ou incohérente
                                        cmp rdx, 0
                                        je vp_fin_traitement
                                            inc r11 ; -> cible incohérente
                                            ; FAIRE ICI LA CORRECTION EVENTUELLE
                                            jmp vp_fin_traitement
                        */
                    break;
                }
                case TYPE_SEGMENT_COMPLET: {
                    uint32_t portSuivantU = U32_FROMP(portion, OSS_SVT_U_4);
                    if (portSuivantU = myPackId) {
                        uint32_t portSuivantN = U32_FROMP(portion, OSS_SVT_N_4);
                        if (portSuivantN = 0) {
                            erreurCibleNulle += 1;
                        }
                        else {
                            uint8_t typeSuivant = U8_FROMP(portSuivantN, OS_TYPESTEV_1);
                            if (typeSuivant != TYPE_SEGMENT_COMPLET && typeSuivant != TYPE_NEURONE_COMPLET) {
                                erreurCibleIncoherente += 1;
                            }
                        }
                    }
                    /* Assembly
                        vp_segment:
                            ; CIBLE SUIVANTE SEGMENT OU NEURONE
                            mov eax, [r9+OSS_SVT_U_4]
                            cmp eax, [myPackId]
                            jne vp_fin_traitement
                                mov eax, [r9+OSS_SVT_N_4] ; portion suivante / num portion
                                test eax, eax ; PORTION_ZERO ?
                                jz vp_fin_traitement
                                    adresse_from_numero r13, rax ; r13 = adresse de la suivante
                                    movzx rdx, byte [r13+OS_TYPESTEV_1] ; type de cible
                                    and rdx, MASQUE_TYPE_TSx
                                    ; bloc de verifications propres
                                    cmp rdx, TYPE_SEGMENT_GERME
                                    je vp_fin_traitement
                                    cmp rdx, TYPE_NEURONE_GERME
                                    je vp_fin_traitement
                                        inc r10 ; -> cible nulle ou incohérente
                                        cmp rdx, 0
                                        je vp_fin_traitement
                                            inc r11 ; -> cible incohérente
                                            ; FAIRE ICI LA CORRECTION EVENTUELLE
                                            jmp vp_fin_traitement
                        */
                    break;
                }
                case TYPE_EXTENSION_COMPLET: {
                    uint32_t portAntecedentU = U32_FROMP(portion, OSE_ANT_U_4);
                    if (portAntecedentU = myPackId) {
                        uint32_t portAntecedentN = U32_FROMP(portion, OSE_ANT_N_4);
                        if (portAntecedentN = 0) {
                            erreurCibleNulle += 1;
                        }
                        else {
                            uint8_t typeAntec = U8_FROMP(portAntecedentN, OS_TYPESTEV_1);
                            if (typeAntec != TYPE_NEURONE_COMPLET && typeAntec != TYPE_EXTENSION_COMPLET) {
                                erreurCibleIncoherente += 1;
                            }
                        }
                    }
                    /* Assembly
                        vp_extension:
                        ; CIBLE ANTECEDENTE NEURONE OU AXONE
                        mov eax, [r9+OSE_ANT_U_4]
                        cmp eax, [myPackId]
                        jne vp_extension_suite
                            mov eax, [r9+OSE_ANT_N_4] ; portion antécédente
                            test eax, eax ; PORTION_ZERO ?
                            jz vp_extension_suite
                                adresse_from_numero r13, rax ; r13 = adresse de l'antécédent
                                movzx rdx, byte [r13+OS_TYPESTEV_1] ; type de cible
                                and rdx, MASQUE_TYPE_TSx
                                ; bloc de verifications propres
                                cmp rdx, TYPE_NEURONE_GERME
                                je vp_extension_suite
                                cmp rdx, TYPE_EXTENSION_GERME
                                je vp_extension_suite
                                    inc r10 ; -> cible nulle ou incohérente
                                    cmp rdx, 0
                                    je vp_extension_suite
                                        inc r11 ; -> cible incohérente
                                        ; FAIRE ICI LA CORRECTION EVENTUELLE
                                        ; je vp_extension_suite
                        */
                    uint32_t portDestinationU = U32_FROMP(portion, OSE_DST_U_4);
                    if (portDestinationU = myPackId) {
                        uint32_t portDestinationN = U32_FROMP(portion, OSE_DST_N_4);
                        if (portDestinationN = 0) {
                            erreurCibleNulle += 1;
                        }
                        else {
                            uint8_t typeDestin = U8_FROMP(portDestinationN, OS_TYPESTEV_1);
                            if (typeDestin != TYPE_SEGMENT_COMPLET && typeDestin != TYPE_NEURONE_COMPLET) {
                                erreurCibleIncoherente += 1;
                            }
                        }
                    }
                    /* Assembly
                        vp_extension_suite:
                            ; CIBLE DESTINATION SEGMENT OU PORTION
                            mov eax, [r9+OSE_DST_U_4]
                            cmp eax, [myPackId]
                            jne vp_fin_traitement
                                mov eax, [r9+OSE_DST_N_4] ; portion destination / num portion
                                test eax, eax ; PORTION_ZERO ?
                                jz vp_fin_traitement
                                    adresse_from_numero r13, rax ; r13 = adresse de la destination
                                    movzx rdx, byte [r13+OS_TYPESTEV_1] ; type de cible
                                    and rdx, MASQUE_TYPE_TSx
                                    ; bloc de verifications propres
                                    cmp rdx, TYPE_SEGMENT_GERME
                                    je vp_fin_traitement
                                    cmp rdx, TYPE_NEURONE_GERME
                                    je vp_fin_traitement
                                        inc r10 ; -> cible nulle ou incohérente
                                        cmp rdx, 0
                                        je vp_fin_traitement
                                            inc r11 ; -> cible incohérente
                                            ; FAIRE ICI LA CORRECTION EVENTUELLE
                                            ; jmp vp_fin_traitement
                        */
                    break;
                }
            }
            /* Assembly
                    vp_fin_traitement:
                    add r9, TAILLE_DES_PORTIONS
                    inc r8
                    cmp r8, USERLA_NB_P
                jne vp_boucle
                sub r10, r11 ; -> cibles nulles seulement
                mov [erreurCibleNulle], r10
                mov [erreurCibleIncoherente], r11
                ;
                pop rdx
                pop rax
                pop r13
                pop r11
                pop r10
                pop r9
                pop r8
                ; add rsp, 8
                ret
                */
        }
    }
    // ; Outils graphiques
    void retracer_la_fenetre() {
        InvalidateRect(gWindowHandle, 0, 0);
        RedrawWindow(gWindowHandle, 0, 0, RDW_INTERNALPAINT);
        /* Assembly
            retracer_la_fenetre:
            sub rsp, 8
            ; recuperation de la zone d'affichage
            mov rcx, [Windowhandle]             ; hWnd
            mov rdx, 0                          ; lpRect
            mov r8, 0                           ; bErase
            add rsp, SHADOW_SPACE_SIZE
            call InvalidateRect
            sub rsp, SHADOW_SPACE_SIZE
            ; demande de retracage de la fenetre
            mov rcx, qword [Windowhandle]       ; hWnd
            mov rdx, 0                          ; lprcUpdate
            mov r8, 0                           ; hrgnUpdate
            mov r9, RDW_INTERNALPAINT           ; flags
            sub rsp, SHADOW_SPACE_SIZE
            call RedrawWindow
            add rsp, SHADOW_SPACE_SIZE
            add rsp, 8
            ret
            */
    }
    // ; Outils console
    // convertir
        /* Assembly
            convertir:
                ; ---------- en entree :
                ; rcx = nombre a convertir
                ; rdx = adresse ou placer le texte
                ; r8 = longueur maximale du texte
                ; Prologue
                push rcx
                push rdx
                push r8
                push r9
                ; raz de la chaine qui est a l'adresse rdx
                init_chaine:
                    mov byte [rdx], '.'
                    inc rdx
                    dec r8
                    test r8, r8
                    jnz init_chaine
                ; Convert number to string
                mov rax, rcx            ; nombre a convertir
                lea r9, [rdx - 1]      ; position de la fin de la chaine de sortie
                mov byte [r9], 0x0A    ; on finit par le caractere de retour a la ligne
                convert:
                                        ; le nombre est dans eax (dividende)
                    dec r9             ; on remonte pour le caractere d'avant
                    xor rdx, rdx        ; on met rdx a 0
                    mov rcx, 10         ; on fixe la base de conversion (diviseur)
                    div rcx             ; on fait la division de eax par rcx => quotient=rax / reste=rdx
                    add dl, '0'         ; on ajoute le numero ascii de "0" au dernier octet des rdx
                    mov byte [r9], dl  ; on ecrit le dernier octet
                    test rax, rax       ; on fait AND eax, eax pour savoir si eax et nul ou pas
                    jnz convert         ; si ce n'est pas nul on continue
                xor rax, rax        ; on remet rax a 0 (on devrait renvoyer le nombre de caracteres)
                ; epilogue
                pop r9
                pop r8
                pop rdx
                pop rcx
                ret
            */
    void ecrire_ligne_4(HANDLE consoleHandle, const char *texte, int64_t nombre, SHORT positionX, SHORT positionY) {
        // Positionnement eventuel du curseur
        if ((positionX != -1) && (positionY != -1)) {
            COORD coord;
            coord.X = positionX;
            if (positionY != -1) {
                coord.Y = positionY;
            }
            else {
                CONSOLE_SCREEN_BUFFER_INFO csbi;
                if (!GetConsoleScreenBufferInfo(consoleHandle, &csbi)) {
                    printf("Erreur (%d) lors de la recuperation des informations de la console (%d).\n", GetLastError(), consoleHandle);
                    return;
                }
                coord.Y = csbi.dwCursorPosition.Y;
            }
            SetConsoleCursorPosition(consoleHandle, coord);
        }
        // Initialisation de la chaine
        memset(ligneAEcrire, ' ', LONGUEUR_LIGNES);
        ligneAEcrire[LONGUEUR_LIGNES] = '\0';
        // Prefixe
        ligneAEcrire[0] = '#';
        ligneAEcrire[1] = ' ';
        // Texte
        if (texte) {
            size_t maxTexteLength = LONGUEUR_LIGNES - NOMBRE_CARS_AVANT - NOMBRE_CARS_APRES;
            size_t i = 0;
            while (i < maxTexteLength && texte[i] != '\0') {
                ligneAEcrire[i+NOMBRE_CARS_AVANT] = texte[i];
                i++;
            }
        }
        // Suffixe
        ligneAEcrire[LONGUEUR_LIGNES-2] = ' ';
        ligneAEcrire[LONGUEUR_LIGNES-1] = '#';
        // Ajout eventuel du nombre
        if (nombre != -1) {
            char nombreStr[21]; // Buffer for 64-bit number
            snprintf(nombreStr, sizeof(nombreStr), "%lld", nombre);
            size_t nombreLen = strlen(nombreStr);
            if (nombreLen < LONGUEUR_LIGNES - NOMBRE_CARS_AVANT - NOMBRE_CARS_APRES) {
                size_t i = 0;
                while (i < nombreLen) {
                    ligneAEcrire[LONGUEUR_LIGNES - NOMBRE_CARS_APRES - nombreLen + i] = nombreStr[i];
                    i++;
                }
            } 
            else {
                ligneAEcrire[LONGUEUR_LIGNES-3] = '?';
            }
        }
        // Write the line to the console
        DWORD written;
        WriteConsoleA(consoleHandle, ligneAEcrire, (DWORD)strlen(ligneAEcrire), &written, NULL);
        /* Assembly
            ecrire_ligne_4:
            ; ---------- en entree :
            ; Arguments : HandleConsole / AdresseTexte (texte fini par 0)(ou vide) / Nombre (ou -1 sur 64 bits) / position (-1 ou x ou y+x)
            ; la ligne sera basculée sur la suivante à la fin, à x=0
            ; ---------- alignement pile + sauvegarde registres
                push rax
                push rbx
                push rcx
                push rdx
                sub rsp, 8
            ; ---------- positionnement du curseur
                mov rcx, [rsp+8*(5+4)] ; handle console
                mov rbx, [rsp+8*(5+1)] ; position voulue / < 0x1000 si x seulement / -1 si aucune
                cmp rbx, -1
                je el4_apres_positionnement
                    cmp rbx, 0x10000
                    jae el4_faire_position
                        lea rax, [csbi]
                        call_winapi64_style GetConsoleScreenBufferInfo, rcx, rax
                        movzx rdx, word [csbi + 6] ; position Y actuelle du curseur = dwCursorPosition / Y
                        shl rdx, 16
                        add rbx, rdx
                    el4_faire_position: ; (rbx)
                    mov rcx, [rsp+8*(5+4)] ; handle console
                    call_winapi64_style SetConsoleCursorPosition, rcx, rbx
                el4_apres_positionnement:
            ; ---------- définir le caractère de remplissage
                mov cl, byte ' ' ; caractère de remplissage par défaute = espace
                mov rax, [rsp+8*(5+2)] ; nombre à écrire
                cmp rax, -1
                je el4_pas_nombre
                    mov cl, byte '.' ; s'il y a un nombre, alors ce sera un point
                el4_pas_nombre:
            ; ---------- précharger le fond dans texte_a_ecrire
                lea rbx, [texte_a_ecrire] ; adresse de début de texte
                mov rax, LONGUEUR_LIGNES ; nombre de caractères dans la ligne
                ; premiers caractères
                mov [rbx], word '# '
                add rbx, NOMBRE_CARS_AVANT ; on commence apres les premiers caracteres
                sub rax, NOMBRE_CARS_AVANT ; et il en faudra aussi en moins pour le début
                ; fond
                sub rax, NOMBRE_CARS_APRES ; caractères à ne pas faire pour la fin de la ligne
                el4_raz_chaine:
                    mov [rbx], cl ; écriture du caractère
                    inc rbx ; emplacement du caractère suivant
                    dec rax ; décompte
                    test rax, rax
                jnz el4_raz_chaine
                mov [rbx], word ' #'
            ; ---------- s'il y a un texte, on le surajoute
                mov rdx, [rsp+8*(5+3)] ; adresse de début de texte à ajouter
                lea rbx, [texte_a_ecrire] ; adresse de début du texte à écrire
                add rbx, NOMBRE_CARS_AVANT ; on commence après les caractères de début
                mov rcx, LONGUEUR_LIGNES - NOMBRE_CARS_AVANT - NOMBRE_CARS_APRES ; nombre de caracteres maxi
                el4_surajouter:
                    mov al, [rdx] ; caractère à écrire
                    test al, al
                    je el4_surajouter_fin
                    mov [rbx], al
                    inc rbx ; emplacement du caractère suivant à écrire
                    inc rdx ; emplacement suivant où écrire
                    dec rcx ; compteur
                    test rcx, rcx
                jnz el4_surajouter
                el4_surajouter_fin:
            ; ---------- s'il y a un nombre, le surajouter
                lea rbx, [texte_a_ecrire] ; adresse de début de texte
                add rbx, LONGUEUR_LIGNES - NOMBRE_CARS_APRES ; après l'adresse finale où commencer l'écriture
                mov rax, [rsp+8*(5+2)] ; nombre à écrire
                cmp rax, -1
                je el4_pas_nombre_2
                    el4_convertir:
                        dec rbx ; remonter dans la ligne de texte à écrire
                        ; extraction du dernier chiffre décimal
                        xor rdx, rdx        ; on met rdx a 0 (partie haute du dividende)
                                            ; le nombre est dans eax (partie basse du dividende)
                        mov rcx, 10         ; on fixe la base de conversion (diviseur)
                        div rcx             ; on fait la division de rdx:rax par rcx => quotient=rax / reste=rdx
                        add dl, '0'         ; on ajoute le numero ascii de "0" au dernier octet des rdx
                        mov byte [rbx], dl  ; on ecrit le dernier octet
                        test rax, rax       ; on regarde si rax est nul
                    jnz el4_convertir       ; si ce n'est pas nul on continue
                el4_pas_nombre_2:
            ; ---------- écrire le texte à la position courante
                mov rcx, [rsp+8*(5+4)] ; handle console
                call_winapi64_style WriteConsoleA, rcx, texte_a_ecrire, LONGUEUR_LIGNES+1, written
            ; ---------- récupération registres + désalignement pile
            add rsp, 8
            pop rdx
            pop rcx
            pop rbx
            pop rax
            ret
            */
    }
    /*
    save_registers: ; not used
        mov [registersMemory + 0*8], rax
        mov [registersMemory + 1*8], rbx
        mov [registersMemory + 2*8], rcx
        mov [registersMemory + 3*8], rdx
        ;
        mov [registersMemory + 8*8], r8
        mov [registersMemory + 9*8], r9
        mov [registersMemory + 10*8], r10
        mov [registersMemory + 11*8], r11
        mov [registersMemory + 12*8], r12
        mov [registersMemory + 13*8], r13
        mov [registersMemory + 14*8], r14
        mov [registersMemory + 15*8], r15
        ret
    recall_registers: ; not used
        mov rax, [registersMemory + 0*8]
        mov rbx, [registersMemory + 1*8]
        mov rcx, [registersMemory + 2*8]
        mov rdx, [registersMemory + 3*8]
        ;
        mov r8, [registersMemory + 8*8]
        mov r9, [registersMemory + 9*8]
        mov r10, [registersMemory + 10*8]
        mov r11, [registersMemory + 11*8]
        mov r12, [registersMemory + 12*8]
        mov r13, [registersMemory + 13*8]
        mov r14, [registersMemory + 14*8]
        mov r15, [registersMemory + 15*8]
        ret
        */
    void ecrire_messages_console() {
        // Console
        CONSOLE_SCREEN_BUFFER_INFO csbi;
        HANDLE hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
        // Recuperation des informations de la console
        if (!GetConsoleScreenBufferInfo(hConsole, &csbi)) {
            printf("Erreur (%d) lors de la recuperation des informations de la console (%d).\n", GetLastError(), hConsole);
            return;
        }
        // Position de retour
        SHORT positionDeRetourX = csbi.dwCursorPosition.X;
        SHORT positionDeRetourY = (csbi.dwCursorPosition.Y > HAUTEUR_TABLE - 1) ? csbi.dwCursorPosition.Y : HAUTEUR_TABLE - 1 ;
        // Positionnement du curseur
        SHORT positionX = POSITION_TABLE_X;
        SHORT hautDeLaTable = positionDeRetourY - (HAUTEUR_TABLE - 1);
        SHORT positionY = hautDeLaTable;
        COORD newCursorPosition = { positionX, positionY };
        SetConsoleCursorPosition(hConsole, newCursorPosition);
        // Ecriture des premieres lignes
        ecrire_ligne_4(hConsole, ligne_de_cadre, -1, positionX, positionY);
        positionY += 1;
        ecrire_ligne_4(hConsole, ligne_vide, -1, positionX, positionY);
        /* Assembly
            ecrire_messages_console:
            sub rsp, 8
            push rax ; utilise pour travail
            push rbx ; utilise pour adresse de portion
            push rcx ; utilise pour numero de portion / compteur
            push rdx ; utilise pour ?
            ; -------------------- Récupération des informations
            ; handle de sortie standard
            call_winapi64_style GetStdHandle, STD_OUTPUT_HANDLE
            mov r15, rax
            ; positionnement actuel
            lea rax, [csbi]
            call_winapi64_style GetConsoleScreenBufferInfo, r15, rax
            ; position x initale du curseur
            mov cx, [csbi + 4] ; position x du curseur = dwCursorPosition / X
            mov [positionDeRetour_xCurseur], cx
            ; récupérer la position y initale du curseur
            mov dx, [csbi + 6] ; position y du curseur = dwCursorPosition / Y
            ; recalage éventuel position y
            mov bx, HAUTEUR_TABLE - 2
            cmp dx, bx
            cmovb dx, bx
            mov [positionDeRetour_yCurseur], dx
            ; -------------------- Positionnement curseur
            xor rdx, rdx
            mov dx, [positionDeRetour_yCurseur]
            sub dx, HAUTEUR_TABLE - 2
            shl edx, 16
            mov rcx, POSITION_TABLE_X
            add rdx, rcx
            ; -------------------- Ecriture premières lignes
            call_datapush_style ecrire_ligne_4, r15, ligne_de_cadre, -1, rdx
            add rdx, 0x10000
            call_datapush_style ecrire_ligne_4, r15, ligne_vide, -1, rdx
            */
        switch (affichageConsole) {
            /* Assembly
                ; -------------------- Aiguillage selon affichage demandé
                mov al, [affichageConsole]
                comparer_et_jump_si_egal al, 1, aff_aide1
                comparer_et_jump_si_egal al, 2, aff_aide2
                comparer_et_jump_si_egal al, 3, aff_evolut
                comparer_et_jump_si_egal al, 4, aff_detail
                jmp emc_fin
                */
            case 1:
                positionY += 1;
                ecrire_ligne_4(hConsole, help_00, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_01, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_10, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_11, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_12, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_13, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_14, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_15, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_20, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_21, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_22, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_23, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_30, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_31, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_32, -1, positionX, positionY);
                break;
                /* Assembly
                    ; -------------------- Affichage aide 1
                    aff_aide1:
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_00, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_01, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_10, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_11, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_12, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_13, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_14, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_15, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_20, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_21, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_22, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_23, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_30, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_31, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_32, -1, rdx
                        jmp emc_fin
                        */
            case 2:
                positionY += 1;
                ecrire_ligne_4(hConsole, help_50, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_51, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_52, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_53, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_54, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_55, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_56, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_60, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_61, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_62, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_63, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, help_64, -1, positionX, positionY);
                break;
                /* Assembly
                    ; -------------------- Affichage aide 2
                    aff_aide2:
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_50, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_51, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_52, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_53, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_54, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_55, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_56, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_60, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_61, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_62, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_63, -1, rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, help_64, -1, rdx
                        jmp emc_fin
                        */
            case 3:
                positionY += 1;
                ecrire_ligne_4(hConsole, texte_numero_de_boucle, numero_de_boucle_lente, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, texte_duree_de_boucle, duree_cycle, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, texte_duree_de_posttrait, dureeDePostTraitement, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, texte_nombre_d_utilisateurs, nombreDUtilisateurs, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, texte_nombre_de_messages_a_emettre, notesATraiter, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, texte_nombre_de_messages_recus, -1, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, texte_nombre_de_cibles_nulles, erreurCibleNulle, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, texte_nombre_de_cibles_incoherentes, erreurCibleIncoherente, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, texte_valeur_de_test, valeurDeTest, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, texte_erreur_a_voir, -1, positionX, positionY);
                break;
                /* Assembly
                    ; -------------------- Affichage évolutions et chronos
                    aff_evolut:
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, texte_numero_de_boucle, qword [numero_de_boucle_lente], rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, texte_duree_de_boucle, qword [dureeDeTraitement], rdx ; durée de boucle
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, texte_duree_de_posttrait, qword [dureeDePostTraitement], rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, texte_nombre_d_utilisateurs, qword [nombreDUtilisateurs], rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, texte_nombre_de_messages_a_emettre, qword [nombreDeMessageAEmettre], rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, texte_nombre_de_messages_recus, qword [nombreDeMessagesRecus], rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, texte_nombre_de_cibles_nulles, qword [erreurCibleNulle], rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, texte_nombre_de_cibles_incoherentes, qword [erreurCibleIncoherente], rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, texte_valeur_de_test, qword [valeurDeTest], rdx
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, texte_erreur_a_voir, qword [erreurAVoir], rdx
                        jmp emc_fin
                        */
            case 4:
                {
                // portion en analyse
                uint32_t numPort = xyz_to_u32((id_prt){visualize_x, visualize_y, visualize_z});
                positionY += 1;
                ecrire_ligne_4(hConsole, portion_numr_text, numPort, positionX, positionY);
                // position en analyse
                positionY += 1;
                ecrire_ligne_4(hConsole, texte_position_x, visualize_x, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, texte_position_y, visualize_y, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, texte_position_z, visualize_z, positionX, positionY);
                positionY += 1;
                ecrire_ligne_4(hConsole, ligne_vide, -1, positionX, positionY);
                // elements pour la suite
                /* Assembly
                    ; -------------------- Affichage détailes du point
                    aff_detail:
                        ; ----- calcul numéro de portion
                        mov rax, [visualize_z]
                        shl rax, BITS_POUR_Y
                        add rax, [visualize_y]
                        shl rax, BITS_POUR_X
                        add rax, [visualize_x]
                        mov rcx, rax
                        ; ----- adresse portion        
                        adresse_from_numero rbx, rax ; adresse de la portion
                        ; ----- écritures communes
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, portion_numr_text, rcx, rdx ; numero de la portion
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, texte_position_x, qword [visualize_x], rdx ; position x
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, texte_position_y, qword [visualize_y], rdx ; position y
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, texte_position_z, qword [visualize_z], rdx ; position z
                        add rdx, 0x10000
                        call_datapush_style ecrire_ligne_4, r15, interligne_text, -1, rdx ; interligne
                        add rdx, 0x10000
                        movzx rcx, byte [rbx+OS_TYPESTEV_1] ; type de portion
                        call_datapush_style ecrire_ligne_4, r15, portion_type_text, rcx, rdx
                    */
                uint8_t typestev = U8_FROMP(numPort, OS_TYPESTEV_1);
                switch (typestev) {
                    /* Assembly
                        ; ----- distribution
                        ; r15 = sortie standard
                        ; rbx = adresse de la portion
                        movzx rcx, byte [rbx+OS_TYPESTEV_1]
                        and cl, MASQUE_TYPE_TSx
                        comparer_et_jump_si_egal cl, TYPE_SEGMENT_GERME,                    dp_psd
                        comparer_et_jump_si_egal cl, TYPE_NEURONE_GERME,                    dp_pn
                        comparer_et_jump_si_egal cl, TYPE_EXTENSION_GERME,                  dp_pea
                        comparer_et_jump_si_egal cl, TYPE_SOURCE,                          dp_sdm
                        comparer_et_jump_si_egal cl, TYPE_LECTEUR,                         dp_plc
                        comparer_et_jump_si_egal cl, TYPE_PULSEUR,                          dp_ppl
                        */
                    /* Assembly // default before other cases
                        ; ----- traitement par defaut
                            inc rbx ; on n'écrit pas le premier octet (type)
                            mov rcx, 1
                            dp_boucle:
                                xor rax, rax
                                mov al, [rbx]
                                add rdx, 0x10000
                                call_datapush_style ecrire_ligne_4, r15, portion_octt_text, rax, rdx
                                ; call_winapi64_style convertir, rax, nombre_text, LONGUEUR_NOMBRES
                                ; call_winapi64_style WriteConsoleA, r15, nombre_text, LONGUEUR_NOMBRES, reponse_long_ret
                                inc rbx
                                inc rcx
                                cmp rcx, TAILLE_DES_PORTIONS
                            jbe dp_boucle
                            jmp dp_fin                                
                        */
                    case TYPE_SEGMENT_GERME:
                    case TYPE_SEGMENT_PARTIEL:
                    case TYPE_SEGMENT_COMPLET:
                        // type
                        positionY += 1;
                        ecrire_ligne_4(hConsole, portion_typeS_text, typestev, positionX, positionY);
                        // temporisation d'evolution de segment
                        positionY += 1;
                        uint8_t tempoEvo_1U_seg = U8_FROMP(numPort, OSS_TEVOS_1);
                        ecrire_ligne_4(hConsole, portion_devo_text, tempoEvo_1U_seg, positionX, positionY);
                        // potentiel synaptique restant
                        positionY += 1;
                        uint16_t tempoEvo_2U_seg = U16_FROMP(numPort, OSS_PSYNA_2);
                        ecrire_ligne_4(hConsole, portion_psyn_text, tempoEvo_2U_seg, positionX, positionY);
                        // Niveau activite temporel
                        positionY += 1;
                        uint8_t niveauActTemp_1U_seg = U8_FROMP(numPort, OS_NIVACTMP_1);
                        ecrire_ligne_4(hConsole, portion_ntac_text, niveauActTemp_1U_seg, positionX, positionY);
                        // retro activite
                        positionY += 1;
                        uint8_t activite_1U_seg = U8_FROMP(numPort, OS_ACTIVITE_1);
                        ecrire_ligne_4(hConsole, portion_actv_text, activite_1U_seg, positionX, positionY);
                        // nombre de charges internes utilisées
                        positionY += 1;
                        uint8_t nombreCharg_1U_seg = U8_FROMP(numPort, OSS_NBRCH_1);
                        ecrire_ligne_4(hConsole, portion_ind1_text, nombreCharg_1U_seg, positionX, positionY);
                        // charge 0
                        positionY += 1;
                        int16_t charge0_2S_seg = S16_FROMP(numPort, OS_CHARG0_2);
                        ecrire_ligne_4(hConsole, portion_chrg_text, charge0_2S_seg, positionX, positionY);
                        // charge 1
                        positionY += 1;
                        int16_t charge1_2S_seg = S16_FROMP(numPort, OSS_CHARG1_2);
                        ecrire_ligne_4(hConsole, portion_chrg_text, charge1_2S_seg, positionX, positionY);
                        // charge 2
                        positionY += 1;
                        int16_t charge2_2S_seg = S16_FROMP(numPort, OSS_CHARG2_2);
                        ecrire_ligne_4(hConsole, portion_chrg_text, charge2_2S_seg, positionX, positionY);
                        // charge 3
                        positionY += 1;
                        int16_t charge3_2S_seg = S16_FROMP(numPort, OSS_CHARG3_2);
                        ecrire_ligne_4(hConsole, portion_chrg_text, charge3_2S_seg, positionX, positionY);
                        // numero de segment/neurone suivant / num portion
                        positionY += 1;
                        uint32_t destinationN_4U_seg = U32_FROMP(numPort, OSS_SVT_N_4);
                        ecrire_ligne_4(hConsole, portion_dstn_text, destinationN_4U_seg, positionX, positionY);
                        // numero de segment/neurone suivant / utilisateur
                        positionY += 1;
                        uint32_t destinationU_4U_seg = U32_FROMP(numPort, OSS_SVT_U_4);
                        ecrire_ligne_4(hConsole, portion_dstu_text, destinationU_4U_seg, positionX, positionY);
                        // potentiel de segments restants
                        positionY += 1;
                        uint8_t potSegRest_1U_seg = U8_FROMP(numPort, OSS_PRSEG_1);
                        ecrire_ligne_4(hConsole, portion_ind1_text, potSegRest_1U_seg, positionX, positionY);
                        // N/U 7o
                        positionY += 1;
                        ecrire_ligne_4(hConsole, portion_nnux_text, 7, positionX, positionY);
                        break;
                        /* Assembly
                            dp_psd: ; portion segment dendritique
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OS_ACTIVITE_1] ; activité
                                call_datapush_style ecrire_ligne_4, r15, portion_actv_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, word [rbx+OS_CHARG0_2] ; charge 0 en cours
                                call_datapush_style ecrire_ligne_4, r15, portion_chrg_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, word [rbx+OSS_CHARG1_2] ; charge 1 ou invalide
                                call_datapush_style ecrire_ligne_4, r15, portion_chrg_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, word [rbx+OSS_CHARG2_2] ; charge 2 ou invalide
                                call_datapush_style ecrire_ligne_4, r15, portion_chrg_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OS_NIVACTMP_1] ; niveau temporel d'activité
                                call_datapush_style ecrire_ligne_4, r15, portion_ntac_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSS_TEVOS_1] ; decompte évolution
                                call_datapush_style ecrire_ligne_4, r15, portion_devo_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, word [rbx+OSS_PSYNA_2] ; potentiel synaptique dispo
                                call_datapush_style ecrire_ligne_4, r15, portion_psyn_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSS_PRSEG_1] ; potentiel de segments restants
                                call_datapush_style ecrire_ligne_4, r15, portion_pseg_text, rcx, rdx
                                add rdx, 0x10000
                                mov ecx, [rbx+OSS_SVT_U_4] ; numero segment/neurone suivant / utilisateur
                                call_datapush_style ecrire_ligne_4, r15, portion_segs_text, rcx, rdx
                                add rdx, 0x10000
                                mov ecx, [rbx+OSS_SVT_N_4] ; numero segment/neurone suivant / num portion
                                call_datapush_style ecrire_ligne_4, r15, portion_segs_text, rcx, rdx
                                jmp dp_fin
                            */
                    case TYPE_NEURONE_GERME:
                    case TYPE_NEURONE_PARTIEL:
                    case TYPE_NEURONE_COMPLET:
                        // type
                        positionY += 1;
                        ecrire_ligne_4(hConsole, portion_typeN_text, typestev, positionX, positionY);
                        // N/U 3
                        positionY += 1;
                        ecrire_ligne_4(hConsole, portion_nnux_text, 3, positionX, positionY);
                        // Niveau activite temporel
                        positionY += 1;
                        uint8_t niveauActTemp_1U_neu = U8_FROMP(numPort, OS_NIVACTMP_1);
                        ecrire_ligne_4(hConsole, portion_ntac_text, niveauActTemp_1U_neu, positionX, positionY);
                        // activité
                        positionY += 1;
                        uint8_t activite_1U_neu = U8_FROMP(numPort, OS_ACTIVITE_1);
                        ecrire_ligne_4(hConsole, portion_actv_text, activite_1U_neu, positionX, positionY);
                        // compteur refractaire
                        positionY += 1;
                        uint8_t comptRef_1U = U8_FROMP(numPort, OSN_CREFR_1);
                        ecrire_ligne_4(hConsole, portion_cref_text, comptRef_1U, positionX, positionY);
                        // base decompte refractaire
                        positionY += 1;
                        uint8_t baseRefr_1U = U8_FROMP(numPort, OSN_BREFR_1);
                        ecrire_ligne_4(hConsole, portion_bref_text, baseRefr_1U, positionX, positionY);
                        // charge
                        positionY += 1;
                        int16_t charge_2S_neu = S16_FROMP(numPort, OS_CHARG0_2);
                        ecrire_ligne_4(hConsole, portion_chrg_text, charge_2S_neu, positionX, positionY);
                        // seuil
                        positionY += 1;
                        uint8_t puisSeuil_1U = U8_FROMP(numPort, OSN_SEUIL_1);
                        ecrire_ligne_4(hConsole, portion_chrs_text, puisSeuil_1U, positionX, positionY);
                        // N/U 1
                        positionY += 1;
                        ecrire_ligne_4(hConsole, portion_nnux_text, 1, positionX, positionY);
                        // puissance potentiel rayonnant
                        positionY += 1;
                        uint8_t potRayo_1U = U8_FROMP(numPort, OSN_PRAYO_1);
                        ecrire_ligne_4(hConsole, portion_pray_text, potRayo_1U, positionX, positionY);
                        // puissance potentiel planaire
                        positionY += 1;
                        uint8_t potPlan_1U = U8_FROMP(numPort, OSN_PPLAN_1);
                        ecrire_ligne_4(hConsole, portion_ppla_text, potPlan_1U, positionX, positionY);
                        // puissance potentiel apical
                        positionY += 1;
                        uint8_t potApic_1U = U8_FROMP(numPort, OSN_PAPIC_1);
                        ecrire_ligne_4(hConsole, portion_papi_text, potApic_1U, positionX, positionY);
                        // puissance potentiel panier
                        positionY += 1;
                        uint8_t potPani_1U = U8_FROMP(numPort, OSN_PPANI_1);
                        ecrire_ligne_4(hConsole, portion_ppan_text, potPani_1U, positionX, positionY);
                        // N/U 6
                        positionY += 1;
                        ecrire_ligne_4(hConsole, portion_nnux_text, 6, positionX, positionY);
                        // puissance potentiel axonal
                        positionY += 1;
                        uint8_t potAxon_U1 = U8_FROMP(numPort, OSN_PAXON_1);
                        ecrire_ligne_4(hConsole, portion_paxo_text, potAxon_U1, positionX, positionY);
                        // orientation des développements
                        positionY += 1;
                        uint8_t orient_1U = U8_FROMP(numPort, OSN_ORIENT_1);
                        ecrire_ligne_4(hConsole, portion_ornt_text, orient_1U, positionX, positionY);
                        // N/U 8
                        positionY += 1;
                        ecrire_ligne_4(hConsole, portion_nnux_text, 8, positionX, positionY);
                        break;
                        /* Assembly
                            dp_pn: ; portion neurone
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OS_ACTIVITE_1] ; activité
                                call_datapush_style ecrire_ligne_4, r15, portion_actv_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, word [rbx+OS_CHARG0_2] ; charge
                                call_datapush_style ecrire_ligne_4, r15, portion_chrg_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSN_SEUIL_1] ; seuil charge
                                call_datapush_style ecrire_ligne_4, r15, portion_chrs_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSN_CREFR_1] ; decompte refractaire
                                call_datapush_style ecrire_ligne_4, r15, portion_cref_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSN_BREFR_1] ; base decompte refractaire
                                call_datapush_style ecrire_ligne_4, r15, portion_bref_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSN_ORIENT_1] ; orientation des développements
                                call_datapush_style ecrire_ligne_4, r15, portion_ornt_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSN_PRAYO_1] ; puissance potentiel rayonnant
                                call_datapush_style ecrire_ligne_4, r15, portion_pray_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSN_PPLAN_1] ; puissance potentiel planaire
                                call_datapush_style ecrire_ligne_4, r15, portion_ppla_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSN_PAPIC_1] ; puissance potentiel apical
                                call_datapush_style ecrire_ligne_4, r15, portion_papi_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSN_PPANI_1] ; puissance potentiel ppanier
                                call_datapush_style ecrire_ligne_4, r15, portion_ppan_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSN_PAXON_1] ; puissance potentiel axonal
                                call_datapush_style ecrire_ligne_4, r15, portion_paxo_text, rcx, rdx
                                jmp dp_fin
                            */
                    case TYPE_EXTENSION_GERME:
                    case TYPE_EXTENSION_PARTIEL:
                    case TYPE_EXTENSION_COMPLET:
                        // type
                        positionY += 1;
                        ecrire_ligne_4(hConsole, portion_typeE_text, typestev, positionX, positionY);
                        // decompte evolution
                        positionY += 1;
                        uint8_t decptEvo_1U = U8_FROMP(numPort, OSE_TEVOE_1);
                        ecrire_ligne_4(hConsole, portion_devo_text, decptEvo_1U, positionX, positionY);
                        // N/U 2
                        positionY += 1;
                        ecrire_ligne_4(hConsole, portion_nnux_text, 2, positionX, positionY);
                        // compteur activite temporelle
                        positionY += 1;
                        uint8_t comptTAct_1U = U8_FROMP(numPort, OSE_CACTT_1);
                        ecrire_ligne_4(hConsole, portion_ctac_text, comptTAct_1U, positionX, positionY);
                        // activite
                        positionY += 1;
                        uint8_t activite_1U_ext = U8_FROMP(numPort, OS_ACTIVITE_1);
                        ecrire_ligne_4(hConsole, portion_actv_text, activite_1U_ext, positionX, positionY);
                        // valeur du bloc de boutons
                        positionY += 1;
                        int8_t valBB_1S = S8_FROMP(numPort, OSE_BSYNV_1);
                        ecrire_ligne_4(hConsole, portion_blcv_text, valBB_1S, positionX, positionY);
                        // masse du bloc de boutons
                        positionY += 1;
                        uint8_t masseBB_1U = U8_FROMP(numPort, OSE_BSYNM_1);
                        ecrire_ligne_4(hConsole, portion_blcp_text, masseBB_1U, positionX, positionY);
                        // destination portion
                        positionY += 1;
                        uint32_t destinN_4U = U32_FROMP(numPort, OSE_DST_N_4);
                        ecrire_ligne_4(hConsole, portion_dstn_text, destinN_4U, positionX, positionY);
                        // destination utilisateur
                        positionY += 1;
                        uint32_t destinU_4U = U32_FROMP(numPort, OSE_DST_U_4);
                        ecrire_ligne_4(hConsole, portion_dstu_text, destinU_4U, positionX, positionY);
                        // antecedant portion
                        positionY += 1;
                        uint32_t antecN_4U = U32_FROMP(numPort, OSE_ANT_N_4);
                        ecrire_ligne_4(hConsole, portion_antn_text, antecN_4U, positionX, positionY);
                        // antecedant utilisateur
                        positionY += 1;
                        uint32_t antecU_4U = U32_FROMP(numPort, OSE_ANT_U_4);
                        ecrire_ligne_4(hConsole, portion_antu_text, antecU_4U, positionX, positionY);
                        // potentiel axonal restant
                        positionY += 1;
                        uint8_t potExtRest_1U = U8_FROMP(numPort, OSE_PREXT_1);
                        ecrire_ligne_4(hConsole, portion_pext_text, potExtRest_1U, positionX, positionY);
                        // N/U 7
                        positionY += 1;
                        ecrire_ligne_4(hConsole, portion_nnux_text, 7, positionX, positionY);
                        break;
                        /* Assembly
                            dp_pea: ; portion extension axonale
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OS_ACTIVITE_1] ; activité
                                call_datapush_style ecrire_ligne_4, r15, portion_actv_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSE_BSYNM_1] ; masse du bloc de boutons
                                call_datapush_style ecrire_ligne_4, r15, portion_blcp_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSE_BSYNV_1] ; valeur du bloc de boutons
                                call_datapush_style ecrire_ligne_4, r15, portion_blcv_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSE_CACTT_1] ; compteur temporel d'activité
                                call_datapush_style ecrire_ligne_4, r15, portion_ctac_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSE_TEVOE_1] ; decompte évolution
                                call_datapush_style ecrire_ligne_4, r15, portion_devo_text, rcx, rdx
                                add rdx, 0x10000
                                mov ecx, [rbx+OSE_DST_U_4] ; numero de destination / utilisateur
                                call_datapush_style ecrire_ligne_4, r15, portion_dstu_text, rcx, rdx
                                add rdx, 0x10000
                                mov ecx, [rbx+OSE_DST_N_4] ; numero de destination / num portion ; A_FAIRE : décomposer sur x/y/z/pack
                                call_datapush_style ecrire_ligne_4, r15, portion_dstn_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSE_PREXT_1] ; potentiel d'extensions restantes
                                call_datapush_style ecrire_ligne_4, r15, portion_pext_text, rcx, rdx
                                add rdx, 0x10000
                                mov ecx, [rbx+OSE_ANT_U_4] ; portion neurone ou axonale antecedente / utilisateur
                                call_datapush_style ecrire_ligne_4, r15, portion_eaxo_text, rcx, rdx
                                add rdx, 0x10000
                                mov ecx, [rbx+OSE_ANT_N_4] ; portion neurone ou axonale antecedente / num portion
                                call_datapush_style ecrire_ligne_4, r15, portion_eaxo_text, rcx, rdx
                                ; jmp dp_fin
                            */
                    case TYPE_SOURCE:
                        // type
                        positionY += 1;
                        ecrire_ligne_4(hConsole, portion_typeD_text, typestev, positionX, positionY);
                        // adresse
                        positionY += 1;
                        uint64_t adresse_U8 = U64_FROMP(numPort, OSD_ADDRS_8);
                        ecrire_ligne_4(hConsole, portion_adrs_text, adresse_U8, positionX, positionY);
                        // nombre de segments
                        positionY += 1;
                        uint16_t nbrSeg_U2 = U16_FROMP(numPort, OSD_NBSEG_2);
                        ecrire_ligne_4(hConsole, portion_nseg_text, nbrSeg_U2, positionX, positionY);
                        // taille des segments
                        positionY += 1;
                        uint8_t longSeg_U1 = U8_FROMP(numPort, OSD_LGSEG_1);
                        ecrire_ligne_4(hConsole, portion_tseg_text, longSeg_U1, positionX, positionY);
                        break;
                        /* Assembly
                            dp_sdm: ; portion source de données en mémoire
                                add rdx, 0x10000
                                mov rcx, [rbx+OSD_ADDRS_8]
                                call_datapush_style ecrire_ligne_4, r15, portion_adrs_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSD_LGSEG_1]
                                call_datapush_style ecrire_ligne_4, r15, portion_tseg_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, word [rbx+OSD_NBSEG_2]
                                call_datapush_style ecrire_ligne_4, r15, portion_nseg_text, rcx, rdx
                                jmp dp_fin
                            */
                    case TYPE_LECTEUR:
                        // type
                        positionY += 1;
                        ecrire_ligne_4(hConsole, portion_typeL_text, typestev, positionX, positionY);
                        // portion acces memoire
                        positionY += 1;
                        uint32_t portionAccMem_U4 = U32_FROMP(numPort, OSL_PAM_N_4);
                        ecrire_ligne_4(hConsole, portion_pamm_text, portionAccMem_U4, positionX, positionY);
                        // index de lecture
                        positionY += 1;
                        uint16_t indexLect_U2 = U16_FROMP(numPort, OSL_INDEX_2);
                        ecrire_ligne_4(hConsole, portion_idxl_text, indexLect_U2, positionX, positionY);
                        // numero de bit a lire
                        positionY += 1;
                        uint8_t numBit_U1 = U8_FROMP(numPort, OSL_NMBIT_1);
                        ecrire_ligne_4(hConsole, portion_bitl_text, numBit_U1, positionX, positionY);
                        // bloc de boutons
                        positionY += 1;
                        int8_t blocVal_S1 = S8_FROMP(numPort, OSL_BSYNV_1);
                        ecrire_ligne_4(hConsole, portion_blcv_text, blocVal_S1, positionX, positionY);
                        // persistance actuelle
                        positionY += 1;
                        uint8_t persistAct_U1 = U8_FROMP(numPort, OSL_PERSA_1);
                        ecrire_ligne_4(hConsole, portion_prsa_text, persistAct_U1, positionX, positionY);
                        // persistance de base
                        positionY += 1;
                        uint8_t persistBase_U1 = U8_FROMP(numPort, OSL_PERSB_1);
                        ecrire_ligne_4(hConsole, portion_prsb_text, persistBase_U1, positionX, positionY);
                        // destination numero portion
                        positionY += 1;
                        uint32_t destinN_U4 = U32_FROMP(numPort, OSL_DST_N_4);
                        ecrire_ligne_4(hConsole, portion_dstn_text, destinN_U4, positionX, positionY);
                        // destination utilisateur
                        positionY += 1;
                        uint32_t destinU_U4 = U32_FROMP(numPort, OSL_DST_U_4);
                        ecrire_ligne_4(hConsole, portion_dstu_text, destinU_U4, positionX, positionY);
                        break;
                        /* Assembly
                            dp_plc: ; portion lecteur
                                add rdx, 0x10000
                                movzx rcx, word [rbx+OSL_INDEX_2] ; index de lecture
                                call_datapush_style ecrire_ligne_4, r15, portion_idxl_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSL_NMBIT_1] ; numero de bit à lire
                                call_datapush_style ecrire_ligne_4, r15, portion_bitl_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSL_BSYNV_1] ; valeur du bloc de boutons
                                call_datapush_style ecrire_ligne_4, r15, portion_blcv_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSL_PERSA_1] ; persistance actuelle
                                call_datapush_style ecrire_ligne_4, r15, portion_prsa_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSL_PERSB_1] ; persistance de base
                                call_datapush_style ecrire_ligne_4, r15, portion_prsb_text, rcx, rdx
                                add rdx, 0x10000
                                mov ecx, [rbx+OSL_DST_U_4] ; portion de destination / utilisateur
                                call_datapush_style ecrire_ligne_4, r15, portion_dest_text, rcx, rdx
                                add rdx, 0x10000
                                mov ecx, [rbx+OSL_DST_N_4] ; portion de destination / num portion
                                call_datapush_style ecrire_ligne_4, r15, portion_dest_text, rcx, rdx
                                add rdx, 0x10000
                                mov ecx, [rbx+OSL_PAM_U_4] ; portion d'accès mémoire / utilisateur
                                call_datapush_style ecrire_ligne_4, r15, portion_pamm_text, rcx, rdx
                                add rdx, 0x10000
                                mov ecx, [rbx+OSL_PAM_N_4] ; portion d'accès mémoire / num portion
                                call_datapush_style ecrire_ligne_4, r15, portion_pamm_text, rcx, rdx
                                jmp dp_fin
                            */
                    case TYPE_PULSEUR:
                        // type
                        positionY += 1;
                        ecrire_ligne_4(hConsole, portion_typeP_text, typestev, positionX, positionY);
                        // charge actuelle
                        positionY += 1;
                        uint16_t chrono_U2 = U16_FROMP(numPort, OSP_CHARGI_2);
                        ecrire_ligne_4(hConsole, portion_chro_text, chrono_U2, positionX, positionY);
                        // seuil de charge
                        positionY += 1;
                        uint8_t pSeuil_U1 = U8_FROMP(numPort, OSP_SEUIL_1);
                        ecrire_ligne_4(hConsole, portion_chrl_text, pSeuil_U1, positionX, positionY);
                        // valeur du bloc de boutons
                        positionY += 1;
                        int8_t surcharge_S1 = S8_FROMP(numPort, OSP_BSYNV_1);
                        ecrire_ligne_4(hConsole, portion_blcv_text, surcharge_S1, positionX, positionY);
                        // fin
                        break;
                        /* Assembly
                            dp_ppl: ; portion pulseur
                                add rdx, 0x10000
                                movzx rcx, word [rbx+OSP_CHARGI_2] ; chrono
                                call_datapush_style ecrire_ligne_4, r15, portion_chro_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSP_SEUIL_1] ; seuil charge
                                call_datapush_style ecrire_ligne_4, r15, portion_chrl_text, rcx, rdx
                                add rdx, 0x10000
                                movzx rcx, byte [rbx+OSP_BSYNV_1] ; valeur du bloc de boutons
                                call_datapush_style ecrire_ligne_4, r15, portion_blcv_text, rcx, rdx
                                add rdx, 0x10000
                                mov ecx, [rbx+OSP_DST_U_4] ; numero de destination / utilisateur
                                call_datapush_style ecrire_ligne_4, r15, portion_dest_text, rcx, rdx
                                add rdx, 0x10000
                                mov ecx, [rbx+OSP_DST_N_4] ; numero de destination / num portion
                                call_datapush_style ecrire_ligne_4, r15, portion_dest_text, rcx, rdx
                                jmp dp_fin
                            */
                    default:
                        {
                        uint8_t valeur;
                        for (int n = 0; n < 10; n++) {
                            positionY += 1;
                            valeur = U8_FROMP(numPort, n);
                            ecrire_ligne_4(hConsole, portion_octt_text, valeur, positionX, positionY);
                        }
                        break;
                        }
                }
                break;
                }
            default:
                // printf("Aucune action pour affichageConsole = %d\n", affichageConsole);
                break;
                /*
            */
        }
        // Remplissage des lignes vides restantes
        while (positionY < hautDeLaTable + HAUTEUR_TABLE - 4) {
            positionY += 1;
            ecrire_ligne_4(hConsole, ligne_vide, -1, positionX, positionY);
        }
        // Ecriture des dernieres lignes
        positionY += 1;
        ecrire_ligne_4(hConsole, ligne_vide, -1, positionX, positionY);
        positionY += 1;
        ecrire_ligne_4(hConsole, ligne_de_cadre, -1, positionX, positionY);
        // Replacement du curseur a sa position initiale
        COORD originalCursorPosition = { positionDeRetourX, positionDeRetourY };
        SetConsoleCursorPosition(hConsole, originalCursorPosition);
        /* Assembly
            dp_fin:
                ; ----- sortie
                ; jmp emc_fin
            emc_fin:
            ; finalisation écritures
            mov rax, rdx
            shr rax, 16 ; position y actuelle
            boucle_fin_texte:
                add rdx, 0x10000
                call_datapush_style ecrire_ligne_4, r15, ligne_vide, -1, rdx
                inc ax
                cmp ax, word [positionDeRetour_yCurseur]
                jb boucle_fin_texte
            ; cadre final
            add rdx, 0x10000
            call_datapush_style ecrire_ligne_4, r15, ligne_de_cadre, -1, rdx
            ; repositionner le curseur à sa position initiale
            xor rcx, rcx
            mov cx, [positionDeRetour_xCurseur]
            xor rdx, rdx
            mov dx, [positionDeRetour_yCurseur]
            shl edx, 16
            add rdx, rcx
            call_winapi64_style SetConsoleCursorPosition, r15, rdx
            ; récupération des registres et recalage pile
            pop rdx
            pop rcx
            pop rbx
            pop rax
            add rsp, 8
            ret
            */
    }
    /*
    ; Outils pour créations (appels : push param1, param2, param3, ... + call + add rsp, nbParams*8)
    sub_adresse_de_numero_1: ; calcul de l'adresse d'une portion (Num->Adr)
        ; Arguments : NumeroDePortion->AdresseDePortion
        ; ----- sauvegarde des registres utilises
        push rdx
        push rax
        sub rsp, 8 ; alignement de la pile
        ; ----- recuperation des parametres
        mov rax, [rsp+8*(3+1)]
        ; ----- conversion numero en adresse
        adresse_from_numero rdx, rax
        ; ----- sauvegarde de la valeur de retour
        mov [rsp+8*(3+1)], rdx
        ; ----- recuperation des registres
        add rsp, 8
        pop rax
        pop rdx
        ; ----- sortie
        ret
    sub_virginiser_portion: ; remettre tous paramètres de portion à zéro
        ; Arguments : AdresseDePortion
        ; A_FAIRE : faire ça par boucle sur chaque octet
        ; ----- sauvegarde des registres utilises
        push rdx
        ; ----- recuperation des parametres
        mov rdx, [rsp+8*(1+1)]
        and qword [rdx], 0b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
        and qword [rdx+8], 0b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
        and qword [rdx+16], 0b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
        and qword [rdx+24], 0b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
        ; ----- recuperation des registres
        pop rdx
        ; ----- sortie
        ret
    sub_numero_de_xyz: ; calcule le numero de portion à partir de x, y et z (x,y,z->Num)
        ; Arguments : x,y,z->NumeroDePortion
        ; ----- sauvegarde des registres utilises
        push rax
        push rbx
        sub rsp, 8 ; alignement de la pile
        ; ----- parametres et calculs
        mov rbx, [rsp+8*(3+1)] ; z
        shl rbx, DECALAGE_Y
        mov rax, [rsp+8*(3+2)] ; y
        add rbx, rax
        shl rbx, DECALAGE_X
        mov rax, [rsp+8*(3+3)] ; x
        add rbx, rax
        ; ----- sauvegarde de la valeur de retour
        mov [rsp+8*(3+1)], rbx
        ; ----- recuperation des registres
        add rsp, 8
        pop rbx
        pop rax
        ; ----- sortie
        ret
        */
    // ; Creations unitaires
    void sub_creer_portion_datasource_4(id_prt port, uint64_t addr, uint16_t nmbr, uint8_t larg) {
        uint32_t portion = xyz_to_u32(port);
        FOR_U8_TOP(portion, OS_TYPESTEV_1) = TYPE_SOURCE; // type de portion
        FOR_U64_TOP(portion, OSD_ADDRS_8) = addr; // adresse memoire locale
        FOR_U16_TOP(portion, OSD_NBSEG_2) = nmbr; // nombre de segments de donnees
        FOR_U8_TOP(portion, OSD_LGSEG_1) = larg; // largeur du segment de donnees
        /* Assembly
            sub_creer_portion_datasource_4: ; NumP4,AdrMem8,NbrSeg2,LargSeg1
                ; ----- sauvegarde des registres utilises + alignement de la pile
                push rdx
                push rax
                sub rsp, 8 ; alignement de la pile
                ; ----- adresse de base de la paire de portions
                mov rdx, [rsp+8*(3+4)] ; numero de portion a creer
                ; ----- calcul adresse
                sub rsp, 8 ; pré-alignement de la pile
                push rdx ; envoi numéro
                call sub_adresse_de_numero_1
                pop rdx ; retour adresse
                ; add rsp, 8 ; ré-alignement de la pile
                ; ----- raz portion
                ; sub rsp, 8 ; pré-alignement de la pile
                push rdx
                call sub_virginiser_portion
                add rsp, 8
                add rsp, 8 ; ré-alignement de la pile
                ; ----- enregistrement des donnees
                ;
                mov byte [rdx+OS_TYPESTEV_1], TYPE_SOURCE
                ;
                mov rax, [rsp+8*(3+3)]
                mov [rdx+OSD_ADDRS_8], rax ; adresse mémoire des données
                ;
                mov rax, [rsp+8*(3+1)]
                mov [rdx+OSD_LGSEG_1], al ; largeur de segment
                ;
                mov rax, [rsp+8*(3+2)]
                mov [rdx+OSD_NBSEG_2], ax ; nombre de segments
                ;
                ; ----- recuperation des registres
                add rsp, 8 ; re-alignement de la pile
                pop rax
                pop rdx
                ; ----- sortie
                ret
            */
    }
    void sub_creer_portion_lecteur_7(id_prt port, id_prt srce, uint16_t idxlect, uint8_t bitlect, uint8_t persisb, int8_t valblocb, id_prt dest) {
        uint32_t portion = xyz_to_u32(port);
        FOR_U8_TOP(portion, OS_TYPESTEV_1) = TYPE_LECTEUR; // type de portion
        FOR_U32_TOP(portion, OSL_PAM_N_4) = xyz_to_u32(srce); // source
        FOR_U32_TOP(portion, OSL_PAM_U_4) = myPackId; // meme utilisateur source
        FOR_U16_TOP(portion, OSL_INDEX_2) = idxlect; // index de lecture
        FOR_U8_TOP(portion, OSL_NMBIT_1) = bitlect; // bit de lecture
        FOR_U8_TOP(portion, OSL_PERSB_1) = persisb; // persistance de base
        FOR_S8_TOP(portion, OSL_BSYNV_1) = valblocb; // valeur du bloc de boutons
        FOR_U32_TOP(portion, OSL_DST_N_4) = xyz_to_u32(dest); // destination (degment ou neurone)
        FOR_U32_TOP(portion, OSL_DST_U_4) = myPackId; // destinataire
        /* Assembly
            sub_creer_portion_lecteur_7: ; NumP4,NumPSrc4,IdxLect2,NumBit1,PersBase1,ValBloc1,PDest4
                ; ----- sauvegarde des registres utilises + alignement de la pile
                push rdx
                push rax
                sub rsp, 8 ; alignement de la pile
                ; ----- adresse de base de la portion
                mov rdx, [rsp+8*(3+7)] ; numero de portion a creer
                ; ----- calcul adresse
                sub rsp, 8 ; pré-alignement de la pile
                push rdx ; envoi numéro
                call sub_adresse_de_numero_1
                pop rdx ; retour adresse
                ; add rsp, 8 ; ré-alignement de la pile
                ; ----- raz portion
                ; sub rsp, 8 ; pré-alignement de la pile
                push rdx
                call sub_virginiser_portion
                add rsp, 8
                add rsp, 8 ; ré-alignement de la pile
                ; ----- enregistrement des donnees
                ;
                mov byte [rdx+OS_TYPESTEV_1], TYPE_LECTEUR ; type de portion
                ;
                mov rax, [rsp+8*(3+5)]
                mov [rdx+OSL_INDEX_2], ax ; index de lecture
                ;
                mov rax, [rsp+8*(3+4)]
                mov [rdx+OSL_NMBIT_1], al ; numero de bit à lire
                ;
                mov rax, [rsp+8*(3+2)]
                mov [rdx+OSL_BSYNV_1], al ; valeur du bloc de boutons
                ;
                mov rax, [rsp+8*(3+3)]
                mov [rdx+OSL_PERSB_1], al ; persistance de base
                ;
                mov rax, [rsp+8*(3+6)]
                mov [rdx+OSL_PAM_UN_8], rax ; numero complet portion des parametres d'accès mémoire
                ;
                mov rax, [rsp+8*(3+1)]
                mov [rdx+OSL_DST_UN_8], rax ; numero complet portion de destination
                ;
                ; ----- recuperation des registres
                add rsp, 8
                pop rax
                pop rdx
                ; ----- sortie
                ret
            */
    }
    void sub_creer_portion_pulseur_4(id_prt port, uint8_t seuil, uint8_t valblocb, uint32_t dest_u, id_prt dest) {
        uint32_t portion = xyz_to_u32(port);
        FOR_U8_TOP(portion, OS_TYPESTEV_1) = TYPE_PULSEUR; // type de portion
        FOR_U8_TOP(portion, OSP_SEUIL_1) = seuil; // seuil
        FOR_S8_TOP(portion, OSP_BSYNV_1) = valblocb; // valeur du bloc de boutons
        FOR_U32_TOP(portion, OSP_DST_N_4) = xyz_to_u32(dest); // destination (segment ou neurone)
        FOR_U32_TOP(portion, OSP_DST_U_4) = dest_u; // meme utilisateur destinatire
        /* Assembly
            sub_creer_portion_pulseur_4: ; NumP4,PuisSeuil1,ValBloc1,PDest4
                ; ----- sauvegarde des registres utilises + alignement de la pile
                    push rdx
                    push rax
                    sub rsp, 8 ; alignement de la pile
                    ; ----- adresse de base de la paire de portions
                    mov rdx, [rsp+8*(3+4)] ; numero de portion a creer
                    ; ----- calcul adresse
                    sub rsp, 8 ; pré-alignement de la pile
                    push rdx ; envoi numéro
                    call sub_adresse_de_numero_1
                    pop rdx ; retour adresse
                    ; add rsp, 8 ; ré-alignement de la pile
                    ; ----- raz portion
                    ; sub rsp, 8 ; pré-alignement de la pile
                    push rdx
                    call sub_virginiser_portion
                    add rsp, 16 ; ré-alignement de la pile
                    ; ----- enregistrement des donnees
                    ;
                    mov byte [rdx+OS_TYPESTEV_1], TYPE_PULSEUR
                    ;
                    mov rax, [rsp+8*(3+3)]
                    mov [rdx+OSP_SEUIL_1], al ; puissance seuil chrono
                    ;
                    mov rax, [rsp+8*(3+2)]
                    mov [rdx+OSP_BSYNV_1], al ; valeur du bloc de boutons
                    ;
                    mov rax, [rsp+8*(3+1)]
                    mov [rdx+OSP_DST_UN_8], rax ; numero complet portion de destination
                    ;
                    ; ----- recuperation des registres
                    add rsp, 8 ; re-alignement de la pile
                    pop rax
                    pop rdx
                    ; ----- sortie
                    ret
            */
    }
    void sub_creer_segment_dendritique_6(id_prt port, uint8_t typeSegm, uint8_t nivTempA, uint8_t potSegR, uint16_t synDispo, id_prt dest) {
        uint32_t portion = xyz_to_u32(port);
        FOR_U8_TOP(portion, OS_TYPESTEV_1) = typeSegm; // type de segment
        FOR_U8_TOP(portion, OS_NIVACTMP_1) = nivTempA; // niveau temporel d'activité
        FOR_U8_TOP(portion, OSS_PRSEG_1) = potSegR; // potentiel de segments restants
        FOR_U16_TOP(portion, OSS_PSYNA_2) = synDispo; // nombre de synapses disponibles
        FOR_U32_TOP(portion, OSS_SVT_N_4) = xyz_to_u32(dest); // destination (segment ou neurone)
        FOR_U32_TOP(portion, OSS_SVT_U_4) = myPackId; // meme utilisateur destinataire
        FOR_S16_TOP(portion, OSS_CHARG1_2) = CHARGE_INVALIDE;
        FOR_S16_TOP(portion, OSS_CHARG2_2) = CHARGE_INVALIDE;
        FOR_S16_TOP(portion, OSS_CHARG3_2) = CHARGE_INVALIDE;
        /* Assembly
            sub_creer_segment_dendritique_6: ; NumP4,Type1,NivTemp1,PotSegRestant1,SynDispo2,PSuiv4
                ; Pour création intiale :
                ; NumeroDePortion / TYPE_SEGMENT_GERME / NivTemporel / PotRest / TempoEvo / SynapsesRest / 0
                ; Pour création forcée :
                ; NumeroDePortion / TYPE_SEGMENT_COMPLET / NivTemporel / 0 / 0 / 0 / SegmentSuivant
                ; ----- sauvegarde des registres utilises + alignement de la pile
                push rdx
                push rax
                sub rsp, 8 ; alignement pile
                ; ----- adresse de travail
                mov rdx, [rsp+8*(3+6)] ; numero de portion a creer
                sub rsp, 8 ; pré-alignement de la pile
                push rdx
                call sub_adresse_de_numero_1
                pop rdx
                ; add rsp, 8 ; ré-alignement de la pile
                ; ----- raz portion
                ; sub rsp, 8 ; pré-alignement de la pile
                push rdx
                call sub_virginiser_portion
                add rsp, 16 ; ré-alignement de la pile
                ; ----- enregistrement des données
                ;
                mov rax, [rsp+8*(3+5)]
                mov [rdx+OS_TYPESTEV_1], al ; type
                ;
                mov rax, [rsp+8*(3+4)]
                mov [rdx+OS_NIVACTMP_1], al ; niveau temporel d'activité
                ;
                mov rax, [rsp+8*(3+3)]
                mov [rdx+OSS_PRSEG_1], al ; potentiel de segments restants
                ;
                mov rax, [rsp+8*(3+2)]
                mov [rdx+OSS_PSYNA_2], ax ; nombre de synapses disponibles
                ;
                mov rax, [rsp+8*(3+1)]
                mov [rdx+OSS_SVT_UN_8], rax ; numero complet portion segment ou neurone suivant
                ;
                mov word [rdx+OSS_CHARG1_2], CHARGE_INVALIDE
                ;
                mov word [rdx+OSS_CHARG2_2], CHARGE_INVALIDE
                ;
                ; ----- recuperation des registres
                add rsp, 8
                pop rax
                pop rdx
                ; ----- sortie
                ret
            */
    }
    void sub_creer_portion_neurone_10(id_prt port, uint8_t typeNeur, uint8_t orient, uint8_t pRay, uint8_t pPlan, uint8_t pApi, uint8_t pPan, uint8_t pAxo, uint8_t puisSeuil, uint8_t baseRef) {
        uint32_t portion = xyz_to_u32(port);
        FOR_U8_TOP(portion, OS_TYPESTEV_1) = typeNeur; // type de portion
        FOR_U8_TOP(portion, OSN_ORIENT_1) = orient; // orientation dendritique
        FOR_U8_TOP(portion, OSN_PRAYO_1) = pRay; // potentiel dendrites rayonnantes
        FOR_U8_TOP(portion, OSN_PPLAN_1) = pPlan; // potentiel dendrites planes
        FOR_U8_TOP(portion, OSN_PAPIC_1) = pApi; // potentiel dendrites apicales
        FOR_U8_TOP(portion, OSN_PPANI_1) = pPan; // potentiel dendrites panier
        FOR_U8_TOP(portion, OSN_PAXON_1) = pAxo; // potentiel extension axonale
        FOR_U8_TOP(portion, OSN_SEUIL_1) = puisSeuil; // puissance seuil charge
        FOR_U8_TOP(portion, OSN_BREFR_1) = baseRef; // base refractaire
        /* Assembly
            sub_creer_portion_neurone_10: ; NumP4,Type1,Orient1,PRay1,PPlan1,PApi1,PPan1,PAxo1,PuisSeuil1,BaseRef1
                ; Pour création initiale : 
                ; NumeroPortion / TYPE_NEURONE_GERME / Orient / PRay / PPlan / PApi / PPan / PAxo / PuissSeuil / BaseRefract
                ; Pour création forcée :
                ; NumeroPortion / TYPE_NEURONE_COMPLET / Orient / 0 / 0 / 0 / 0 / 0 / PuissSeuil / BaseRefract
                ; ----- sauvegarde des registres utilises + alignement de la pile
                push rdx
                push rax
                sub rsp, 8 ; alignement pile
                ; ----- adresse de travail
                mov rdx, [rsp+8*(3+10)] ; numero de portion a creer
                sub rsp, 8 ; pré-alignement de la pile
                push rdx
                call sub_adresse_de_numero_1
                pop rdx
                ; add rsp, 8 ; ré-alignement de la pile
                ; ----- raz portion
                ; sub rsp, 8 ; pré-alignement de la pile
                push rdx
                call sub_virginiser_portion
                add rsp, 16 ; ré-alignement de la pile
                ; ----- enregistrement des données
                ;
                mov rax, [rsp+8*(3+9)]
                mov [rdx+OS_TYPESTEV_1], al ; type
                ;
                mov rax, [rsp+8*(3+8)]
                mov [rdx+OSN_ORIENT_1], al ; orientation dendritique
                ;
                mov rax, [rsp+8*(3+7)]
                mov [rdx+OSN_PRAYO_1], al ; potentiel dendrites rayonnantes
                ;
                mov rax, [rsp+8*(3+6)]
                mov [rdx+OSN_PPLAN_1], al ; potentiel dendrites planes
                ;
                mov rax, [rsp+8*(3+5)]
                mov [rdx+OSN_PAPIC_1], al ; potentiel dendrites apicales
                ;
                mov rax, [rsp+8*(3+4)]
                mov [rdx+OSN_PPANI_1], al ; potentiel dendrites panier
                ;
                mov rax, [rsp+8*(3+3)]
                mov [rdx+OSN_PAXON_1], al ; potentiel extension axonale
                ;
                mov rax, [rsp+8*(3+2)]
                mov [rdx+OSN_SEUIL_1], al ; puissance seuil charge
                ;
                mov rax, [rsp+8*(3+1)]
                mov [rdx+OSN_BREFR_1], al ; base refractaire
                ;
                ; ----- recuperation des registres
                add rsp, 8
                pop rax
                pop rdx
                ; ----- sortie
                ret
            */
    }
    void sub_creer_extension_axonale_8(id_prt port, uint8_t typeExtA, id_prt ante, uint8_t cptTempA, uint8_t potAxoR, uint8_t masBlocB, int8_t valBlocB, uint32_t dest_u, id_prt dest) {
        uint32_t portion = xyz_to_u32(port);
        FOR_U8_TOP(portion, OS_TYPESTEV_1) = typeExtA; // valeurs
        FOR_U8_TOP(portion, OSE_TEVOE_1) = 0; // N/U 2
        FOR_U8_TOP(portion, OSE_CACTT_1) = cptTempA; // activite 1
        FOR_S8_TOP(portion, OSE_BSYNV_1) = valBlocB; // valeur du bloc de boutons
        FOR_U8_TOP(portion, OSE_BSYNM_1) = masBlocB; // masse du bloc de boutons
        FOR_U32_TOP(portion, OSE_DST_N_4) = xyz_to_u32(dest); // destination (segment ou neurone)
        FOR_U32_TOP(portion, OSE_DST_U_4) = dest_u; // utilisateur destinataire
        FOR_U32_TOP(portion, OSE_ANT_N_4) = xyz_to_u32(ante); // antecedent (neurone)
        FOR_U32_TOP(portion, OSE_ANT_U_4) = myPackId; // meme utilisateur antecedent
        FOR_U8_TOP(portion, OSE_PREXT_1) = potAxoR; // potentiel d'extension axonale
        /* Assembly
            sub_creer_extension_axonale_8: ; NumP4,Type1,NivTemp1,PotAxoRestant1,MasBB1,ValBB1,PDest4,PSuiv4
                ; A_FAIRE : Ajouter décompte évolution
                ; Pour création intiale :
                ; NumeroDePortion, TYPE_EXTENSION_GERME,   0,           PotAxoRest, 0,       0,     0,    0
                ; Pour création forcée :
                ; NumeroDePortion, TYPE_EXTENSION_COMPLET, NivTemporel, 0,          MasseBB, ValBB, Dest, SegmentAntecédent
                ; ----- sauvegarde des registres utilises + alignement de la pile
                push rdx
                push rax
                sub rsp, 8 ; alignement pile
                ; ----- adresse de travail
                mov rdx, [rsp+8*(3+8)] ; numero de portion a creer
                sub rsp, 8 ; pré-alignement de la pile
                push rdx
                call sub_adresse_de_numero_1
                pop rdx
                ; add rsp, 8 ; ré-alignement de la pile
                ; ----- raz portion
                ; sub rsp, 8 ; pré-alignement de la pile
                push rdx
                call sub_virginiser_portion
                add rsp, 16 ; ré-alignement de la pile
                ; ----- enregistrement des données
                ;
                mov rax, [rsp+8*(3+7)]
                mov [rdx+OS_TYPESTEV_1], al ; type
                ;
                mov rax, [rsp+8*(3+6)]
                mov [rdx+OSE_CACTT_1], al ; compteur temporel d'activité
                ;
                mov rax, [rsp+8*(3+5)]
                mov [rdx+OSE_PREXT_1], al ; potentiel d'extensions restantes
                ;
                mov rax, [rsp+8*(3+4)]
                mov [rdx+OSE_BSYNM_1], al ; masse du bloc de boutons
                ;
                mov rax, [rsp+8*(3+3)]
                mov [rdx+OSE_BSYNV_1], al ; valeur du bloc de boutons
                ;
                mov rax, [rsp+8*(3+2)]
                mov [rdx+OSE_DST_UN_8], rax ; numero complet portion destination
                ;
                mov rax, [rsp+8*(3+1)]
                mov [rdx+OSE_ANT_UN_8], rax ; numero complet portion neurone ou extension axonale antécédente
                ;
                ; ----- recuperation des registres
                add rsp, 8
                pop rax
                pop rdx
                ; ----- sortie
                ret
            */
    }
    // ; Creation de reseaux
    // sub_pile_6_sur_z:
        //
        //    ret
    void sub_creer_reseau(id_prt port, uint8_t typP, id_prt ante, uint32_t succ_u, id_prt succ, uint16_t nb_x, uint16_t nb_y, uint16_t nb_z, char pas) {
        id_prt portion = null_port;
        id_prt antecedente = null_port;
        uint32_t successeur = succ_u;
        id_prt successeure = null_port;
        uint16_t compteur = 0;
        for (int z = 0; z < nb_z; z++) {
            portion.z = port.z + z*pas;
            antecedente.z = ante.z + z*pas;
            successeure.z = succ.z + z*pas;
            for (int y = 0; y < nb_y; y++) {
                portion.y = port.y + y*pas;
                antecedente.y = ante.y + y*pas;
                successeure.y = succ.y + y*pas;
                for (int x = 0; x < nb_x; x++) {
                    portion.x = port.x + x*pas;
                    antecedente.x = ante.x + x*pas;
                    successeure.x = succ.x + x*pas;
                    compteur += 1;
                    switch (typP) {
                        case TYPE_LECTEUR:
                            sub_creer_portion_lecteur_7(portion, ante, compteur, 0, 1, 127, successeure);
                            break;
                        case TYPE_PULSEUR:
                            sub_creer_portion_pulseur_4(portion, 11, 127, successeur, successeure);
                            break;
                        case TYPE_SEGMENT_COMPLET:
                            sub_creer_segment_dendritique_6(portion, typP, 1, 0, 0, successeure);
                            break;
                        case TYPE_NEURONE_COMPLET:
                            sub_creer_portion_neurone_10(portion, typP, 0, 0, 0, 0, 0, 0, VS_PSEUIL, VS_BREFRC);
                            break;
                        case TYPE_EXTENSION_COMPLET:
                            sub_creer_extension_axonale_8(portion, typP, antecedente, 1, 0, VS_MASBB, VS_VALBB, successeur, successeure);
                            break;
                    }
                }
            }
        }
        /* Assembly
            sub_creer_reseau: ; creation d'un reseau : ...,...,...,Type,NumP,NbrHX,NbrVY,NbrPZ,Pas
                ; Arguments selon besoin : / / / /
                ; Arguments pour segment : 7=NumeroDeSuivant
                ; Arguments pour neurone : 7=NumeroDeSuivant
                ; Arguments pour extension : 8=NuméroDeDestination / 7=NumeroDeSuivant
                ; Arguments pour boucles : / 6=TypeDePortion / 5=NumeroDePortion / 4=NombreHsurX / 3=NombreVsurY / 2=NombrePsurZ / 1=Pas
                ; ----- sauvegarde des registres utilises (alignement pile inclus)
                sub rsp, 8
                push r8
                push r9
                push r10
                push r11
                push r12
                push r13
                push rax
                push rbx
                push rcx
                push rdx
                ; ----- variables
                mov rbx, [rsp+8*(11+5)] ; premiere portion a creer            
                mov rcx, [rsp+8*(11+7)] ; numSuiv à incrémenter
                mov rdx, [rsp+8*(11+8)] ; numDest à incrementer
                mov r8, [rsp+8*(11+1)] ; pas sur x
                mov r9, r8
                shl r9, DECALAGE_Y ; pas sur y
                mov r10, r8
                shl r10, DECALAGE_Z ; pas sur z
                mov r14, 0 ; initialisation d'un compteur
                ; ----- boucles z y x 
                mov r13, [rsp+8*(11+2)] ; initialisation z
                rdnc_boucle_z:
                    push rdx ; sauvegarde numSuiv avant travail sur y
                    push rcx ; sauvegarde numDest avant travail sur y
                    push rbx ; sauvegarde portion avant travail sur y
                    mov r12, [rsp+8*(11+3+3)] ; initialisation y
                    rdnc_boucle_y:
                        push rdx ; sauvegarde numSuiv avant travail sur x
                        push rcx ; sauvegarde numDest avant travail sur x
                        push rbx ; sauvegarde portion avant travail sur x
                        mov r11, [rsp+8*(11+6+4)] ; initialisation x
                        rdnc_boucle_x:
                            inc r14
                            mov rax, [rsp+8*(11+6+6)]
                            comparer_et_jump_si_egal al, TYPE_LECTEUR,              scr_l
                            comparer_et_jump_si_egal al, TYPE_PULSEUR,              scr_p
                            comparer_et_jump_si_egal al, TYPE_SEGMENT_COMPLET,      scr_sc
                            comparer_et_jump_si_egal al, TYPE_NEURONE_COMPLET,      scr_nc
                            comparer_et_jump_si_egal al, TYPE_EXTENSION_COMPLET,    scr_ec
                            ; ... autres
                            jmp scr_suite
                            ; traitement différencié
                            ; rbx = 5 = neurone
                            ;     = 6 = type
                            ; rcx = 7 = adresse suivant (ou destination)
                            ; rdx = 8 = adresse destination complète
                            ; suivants ...
                                scr_l:
                                    call_datapush_style sub_creer_portion_lecteur_7, \
                                    rbx,      1+0*FY+0*FZ,   r14,       0,      1,       127,     rcx
                                    ; Numero, PortionSource, IndexLect, NumBit, Persist, ValBloc, Destination
                                    jmp scr_suite
                                scr_p:
                                    call_datapush_style sub_creer_portion_pulseur_4, \
                                    rbx,     11,         127,      rcx
                                    ; NumP4, PuisSeuil1, ValBloc1, PDest4
                                    jmp scr_suite
                                scr_sc:
                                    call_datapush_style sub_creer_segment_dendritique_6, \
                                    rbx, TYPE_SEGMENT_COMPLET, 1,       0,             0,          rcx
                                    ; NumPor, Type,            NivTemp, PotSegRestant, SynapDispo, SegSuivant
                                    jmp scr_suite
                                scr_nc:
                                    call_datapush_style sub_creer_portion_neurone_10, \
                                    rbx, TYPE_NEURONE_COMPLET, 0,      0,       0,        0,       0,       0,      VS_PSEUIL,  VS_BREFRC
                                    ; NumPor, Type,            Orient, PotDRay, PotDPlan, PotDApi, PotDPan, PotAxo, PuissSeuil, BaseRefract
                                    jmp scr_suite
                                scr_ec:
                                    call_datapush_style sub_creer_extension_axonale_8, \
                                    rbx, TYPE_EXTENSION_COMPLET, 1,           0,          VS_MASBB,   VS_VALBB,    rdx,  rcx
                                    ; NumPor, Type,              NivTemporel, PotAxoRest, MasseBlocB, ValeurBlocB, Dest, Antécédent
                                    jmp scr_suite
                            ; fin traitement différencié
                            scr_suite:
                            add rbx, r8 ; portion suivante sur x
                            add rcx, r8 ; numDest suivant sur x
                            add rdx, r8 ; numSuiv suivant sur x
                            sub r11, 1
                        jnz rdnc_boucle_x
                        pop rbx ; récupération portion après travail sur x
                        pop rcx ; récupération numDest après travail sur x
                        pop rdx ; récupération numSuiv après travail sur x
                        add rbx, r9 ; portion suivante sur y
                        add rcx, r9 ; numDest suivant sur y
                        add rdx, r9 ; numSuiv suivant sur y
                        sub r12, 1
                    jnz rdnc_boucle_y
                    pop rbx ; récupération portion après travail sur y
                    pop rcx ; récupération numDest après travail sur y
                    pop rdx ; récupération numSuiv après travail sur y
                    add rbx, r10 ; portion suivante sur z
                    add rcx, r10 ; numDest suivant sur z
                    add rdx, r10 ; numSuiv suivant sur z
                    sub r13, 1
                jnz rdnc_boucle_z
                ; ----- recuperation des registres originaux
                pop rdx
                pop rcx
                pop rbx
                pop rax
                pop r13
                pop r12
                pop r11
                pop r10
                pop r9
                pop r8
                add rsp, 8
                ret
            */
    }
// #---------------------------------------------------------------------------------------- THREADS RESEAU
    DWORD WINAPI sendThread(LPVOID param) {
        uint64_t position;
        uint32_t adresseip;
        uint16_t portip;
        uint64_t longueur;
        while (1) {
            // printf("Handle de l'evenement : %p\n", (void*)sendThreadEventHandle);
            WaitForSingleObject(sendThreadEventHandle, INFINITE);
            /* Assembly
                ; Thread d'envoi
                sendThread:
                    ; attendre declenchement de sendingThreadEvent
                        mov rcx, [sendThreadEventHandle]    ; Handle de l'événement
                        mov rdx, -1                         ; Timeout INFINITE
                        call WaitForSingleObject            ; Attend que l'événement soit signalé
                    envoi_messages:
                */
            // Envoi des messages
            // printf("..");
            pointeurDUnite = 0;
            for (int paquet = 0; paquet < nombreDePaquetsAEmettre; paquet++) {
                toDestSocket.sin_family = AF_INET;
                toDestSocket.sin_addr.s_addr = U32_FROMM(pointeurDUnite, OMP_IPAD_4); // Adresse cible (ou inet_addr("192.168.1.1"))
                toDestSocket.sin_port = U16_FROMM(pointeurDUnite, OMP_PORT_2); // Port cible (ou htons(8080))
                longueur = U64_FROMM(pointeurDUnite, OMP_TAIP_8); // Longueur du paquet en unites
                pointeurDUnite++;
                sendto(sendSocketHandle, msgToSnd[pointeurDUnite], longueur * TAILLE_DES_MESSAGES, 0, (struct sockaddr*)&toDestSocket, SOCKET_ADDRESS_LEN);
                pointeurDUnite += longueur;
                // printf("%d.", longueur);
            }
            // printf("/");
            /* Assembly
                ; envoi des messages
                    lea r12, [msgToSnd]
                    boucle_sur_messages_a_envoyer:
                        ; adresse IP pour envoi UDP
                        mov eax, [r12 + OMP_IPAD_4] ; adresse ip destinataire
                        ; test pour sortie
                        cmp eax, 0
                        je fin_envoi_messages
                            ; autres éléments pour envoi UDP
                            mov word [toDestSocket], AF_INET_IPV4 ; --> type d'adresse
                            mov [toDestSocket + 4], eax ; ------------> adresse ip destinataire
                            mov ax, [r12 + OMP_PORT_2]
                            mov [toDestSocket + 2], ax ; -------------> port ip destinataire
                            ; paramètres de l'appel à sendto
                            mov rcx, [sendSocketFileDescriptor] ; ----> id du socket
                            mov rdx, r12
                            add rdx, TAILLE_DES_MESSAGES ; -----------> pointeur sur le paquet de messages
                            mov r8, [r12 + OMP_TAIP_8] ; -------------> longueur du paquet de messages
                            xor r9, r9 ; -----------------------------> flags
                            push SOCKET_ADDRESS_LEN ; ----------------> longueur de toDestSocket
                            lea rax, [toDestSocket]
                            push rax ; -------------------------------> pointeur sur toDestSocket
                            ; envoi
                            sub rsp, SHADOW_SPACE_SIZE
                            call sendto
                            add rsp, SHADOW_SPACE_SIZE
                            add rsp, 2*8
                            ; cmp rax, SOCKET_ERROR
                            ; je erreur_sendTo
                            ; decalage sur plage de messages suivante
                            mov rax, [r12 + OMP_TAIP_8]
                            add r12, TAILLE_DES_MESSAGES
                            add r12, rax
                            ; add dword [valeurDeTest], 10000000
                    jmp boucle_sur_messages_a_envoyer
                    fin_envoi_messages:
                */
            if (!ResetEvent(sendThreadEventHandle)) {
                printf("Probleme de reset de l'evenement d'envoi des messages");
            }
            sendThreadEnded = TRUE;
        }
        return 0;
        /* Assembly
            fin_du_thread:
            ; validation envoi fini au thread principal
                mov rcx, [sendThreadEventHandle]
                sub rsp, SHADOW_SPACE_SIZE
                call ResetEvent
                add rsp, SHADOW_SPACE_SIZE
            ; renvoi ok au thread principal
                mov byte [sendThreadEnded], 1
            ; retour au debut pour attente envoi suivant
            jmp sendThread
            */
    }
    /*
    DWORD WINAPI recvThread(LPVOID param) {
        struct sockaddr_in senderAddr;
        int senderAddrSize = sizeof(senderAddr);
        while (1) {
            int bytesReceived = recvfrom(recvSocketHandle, bufferIn, BUFFERINLEN, 0,
                                        (struct sockaddr*)&senderAddr, &senderAddrSize);
            if (bytesReceived == SOCKET_ERROR) {
                printf("Erreur recvfrom: %d\n", WSAGetLastError());
            } else {
                printf("Message reçu: %s\n", bufferIn);
            }
        }
        return 0;
    */
    /* Assembly
        ; Thread de réception
        recvThread:
            ; pour réception
            mov word [localInSocket + 0], AF_INET_IPV4 ; family address -> AF_INET (2)
            mov word [localInSocket + 2], 0x901F       ; port -> 8080 (0x1F90)
            mov eax, 0 ; 0x7C01A8C0                         ; adresse ip 192.168.1.124
            mov dword [localInSocket + 4], eax         ; ip address (0.0.0.0 = INADDR_ANY)
            ; recuperation eventuel message
            mov rcx, [recvSocketHandle] ; Socket
            lea rdx, [bufferIn]                 ; Pointeur vers le tampon de réception
            mov r8, BUFFERINLEN                 ; Taille du tampon
            mov r9, [localInSocket]            ; Pointeur vers sockaddr_in (rempli par recvfrom)
            sub rsp, SHADOW_SPACE_SIZE
            call recvfrom
            add rsp, SHADOW_SPACE_SIZE
                        mov [valeurDeTest], rax
                        cmp eax, -1
                        jne suite_recv
                                sub rsp, SHADOW_SPACE_SIZE
                                call WSAGetLastError
                                add rsp, SHADOW_SPACE_SIZE
                                mov [erreurAVoir], rax
                        suite_recv:
            inc qword [nombreDeMessagesRecus]
            jmp recvThread
        */
// #---------------------------------------------------------------------------------------- PROCEDURE DE FENETRE
    LRESULT CALLBACK WindowProc(HWND windowHandle, UINT uMsg, WPARAM wParam, LPARAM lParam) {
        /* Assembly
            global WindowProc
            WindowProc:
            */
        switch (uMsg) {
            /* Assembly
                ; Aiguillage
                    ; recuperation des elements
                        push rbp            ; Saves the base pointer (pile naturellement recalee)
                        mov rbp, rsp        ; Saves the stack pointer for later use
                        mov [rbp+16], rcx                                   ; hWnd
                        mov [rbp+24], rdx                                   ; Msg
                        mov [rbp+32], r8                                    ; wParam
                        mov [rbp+40], r9                                    ; lParam
                    ; envoi vers les actions selon le message
                        mov rdx, qword [rbp+24]                             ; Msg
                        comparer_et_jump_si_egal rdx, WM_KEYDOWN, cas_WM_KEYDOWN
                        comparer_et_jump_si_egal rdx, WM_SIZE, cas_WM_SIZE
                        comparer_et_jump_si_egal rdx, WM_PAINT, cas_WM_PAINT
                        comparer_et_jump_si_egal rdx, WM_DESTROY, cas_WM_DESTROY
                        jmp traitement_standard
                */
            case WM_KEYDOWN: {
                /* Assembly
                    cas_WM_KEYDOWN: ; ------------------------------------------------
                        ; test de touche
                            mov rax, qword [rbp+32]
                            comparer_et_jump_si_egal al, 0x41, cas_WM_KEYDOWN_A ; dessin types
                            comparer_et_jump_si_egal al, 0x5A, cas_WM_KEYDOWN_Z ; dessin seuils
                            comparer_et_jump_si_egal al, 0x45, cas_WM_KEYDOWN_E ; dessin charges (default)
                            comparer_et_jump_si_egal al, 0x52, cas_WM_KEYDOWN_R ; dessin retro-actions
                            ;
                            comparer_et_jump_si_egal al, 0x58, cas_WM_KEYDOWN_X ; x-
                            comparer_et_jump_si_egal al, 0x56, cas_WM_KEYDOWN_V ; x+
                            comparer_et_jump_si_egal al, 0x43, cas_WM_KEYDOWN_C ; y-
                            comparer_et_jump_si_egal al, 0x44, cas_WM_KEYDOWN_D ; y+
                            comparer_et_jump_si_egal al, 0x57, cas_WM_KEYDOWN_W ; z-
                            comparer_et_jump_si_egal al, 0x51, cas_WM_KEYDOWN_Q ; z+
                            ;
                            comparer_et_jump_si_egal al, 0x47, cas_WM_KEYDOWN_G ; traçage fenetre
                            comparer_et_jump_si_egal al, 0x48, cas_WM_KEYDOWN_H ; écritures console
                            ;
                            comparer_et_jump_si_egal al, 0x42, cas_WM_KEYDOWN_B ; mode pas à pas (toggle)
                            comparer_et_jump_si_egal al, 0x4E, cas_WM_KEYDOWN_N ; avancer d'un pas
                            comparer_et_jump_si_egal al, 0x49, cas_WM_KEYDOWN_I ; restaurer (seulement en pas à pas)
                            comparer_et_jump_si_egal al, 0x4F, cas_WM_KEYDOWN_O ; sauvegarder (seulement en pas à pas)
                            ;
                            comparer_et_jump_si_egal al, 0x50, cas_WM_KEYDOWN_P ; sortie
                            comparer_et_jump_si_egal al, 0x1B, cas_WM_KEYDOWN_Escape ; ne fonctionne pas ???
                            jmp cas_WM_KEYDOWN_Sortie
                    */
                // affichage fenetre
                /* Assembly
                    cas_WM_KEYDOWN_A: ; touche a
                        ; texte de fenetre
                        mov rcx, [rbp+16]               ; HWND
                        lea rdx, [NOM_FENETRE_A0]       ; LPCSTR
                        sub rsp, SHADOW_SPACE_SIZE
                        call SetWindowTextA
                        add rsp, SHADOW_SPACE_SIZE
                        ; changement du mode de tracage
                        mov byte [modeTracage], 0
                        jmp cas_WM_KEYDOWN_Sortie
                    cas_WM_KEYDOWN_Z: ; touche z
                        ; texte de fenetre
                        mov rcx, [rbp+16]               ; HWND
                        lea rdx, [NOM_FENETRE_Z1]       ; LPCSTR
                        sub rsp, SHADOW_SPACE_SIZE
                        call SetWindowTextA
                        add rsp, SHADOW_SPACE_SIZE
                        ; changement du mode de tracage
                        mov byte [modeTracage], 1
                        jmp cas_WM_KEYDOWN_Sortie
                    cas_WM_KEYDOWN_E: ; touche e
                        ; texte de fenetre
                        mov rcx, [rbp+16]               ; HWND
                        lea rdx, [NOM_FENETRE_E2]       ; LPCSTR
                        sub rsp, SHADOW_SPACE_SIZE
                        call SetWindowTextA
                        add rsp, SHADOW_SPACE_SIZE
                        ; changement du mode de tracage
                        mov byte [modeTracage], 2
                        jmp cas_WM_KEYDOWN_Sortie
                    cas_WM_KEYDOWN_R: ; touche r
                        ; texte de fenetre
                        mov rcx, [rbp+16]               ; HWND
                        lea rdx, [NOM_FENETRE_R3]       ; LPCSTR
                        sub rsp, SHADOW_SPACE_SIZE
                        call SetWindowTextA
                        add rsp, SHADOW_SPACE_SIZE
                        ; changement du mode de tracage
                        mov byte [modeTracage], 3
                        jmp cas_WM_KEYDOWN_Sortie
                    */
                // position visualisation
                if (wParam == 'X' || wParam == 'x') { // x-1
                    visualize_x = (visualize_x > 0) ? visualize_x - 1 : DIMENSION_X - 1;
                    return 0;
                }
                if (wParam == 'V' || wParam == 'v') { // x+1
                    visualize_x = (visualize_x < (DIMENSION_X - 1)) ? visualize_x + 1 : 0 ;
                    return 0;
                }
                if (wParam == 'C' || wParam == 'c') { // y-1
                    visualize_y = (visualize_y < (DIMENSION_Y - 1)) ? visualize_y + 1 : 0 ;
                    return 0;
                }
                if (wParam == 'D' || wParam == 'd' || wParam == 'f' || wParam == 'F') { // y+1
                    visualize_y = (visualize_y > 0) ? visualize_y -1 : DIMENSION_Y - 1;
                    return 0;
                }
                if (wParam == 'W' || wParam == 'w') { // z-1
                    visualize_z = (visualize_z > 0 ) ? visualize_z - 1 : DIMENSION_Z - 1;
                    return 0;
                }
                if (wParam == 'Q' || wParam == 'q' || wParam == 's' || wParam == 'S') { // z+1
                    visualize_z = (visualize_z < (DIMENSION_Z - 1)) ? visualize_z + 1 : 0 ;
                    return 0;
                }
                /* Assembly
                    cas_WM_KEYDOWN_X: ; touche x
                        mov rax, [visualize_x]
                        cmp rax, 0
                        jbe cas_WM_KEYDOWN_Sortie
                        dec rax
                        mov [visualize_x], rax
                        jmp cas_WM_KEYDOWN_Sortie
                    cas_WM_KEYDOWN_V: ; touche v
                        mov rax, [visualize_x]
                        cmp rax, DIMENSION_X-1
                        jae cas_WM_KEYDOWN_Sortie
                        inc rax
                        mov [visualize_x], rax
                        jmp cas_WM_KEYDOWN_Sortie
                    cas_WM_KEYDOWN_C: ; touche c
                        mov rax, [visualize_y]
                        cmp rax, DIMENSION_Y-1
                        jae cas_WM_KEYDOWN_Sortie
                        inc rax
                        mov [visualize_y], rax
                        jmp cas_WM_KEYDOWN_Sortie
                    cas_WM_KEYDOWN_D: ; touche d
                        mov rax, [visualize_y]
                        cmp rax, 0
                        jbe cas_WM_KEYDOWN_Sortie
                        dec rax
                        mov [visualize_y], rax
                        jmp cas_WM_KEYDOWN_Sortie
                    cas_WM_KEYDOWN_W: ; touche w
                        mov rax, [visualize_z]
                        cmp rax, 0
                        jbe cas_WM_KEYDOWN_Sortie
                        dec rax
                        mov [visualize_z], rax
                        jmp cas_WM_KEYDOWN_Sortie
                    cas_WM_KEYDOWN_Q: ; touche q
                        mov rax, [visualize_z]
                        cmp rax, DIMENSION_Z-1
                        jae cas_WM_KEYDOWN_Sortie
                        inc rax
                        mov [visualize_z], rax
                        jmp cas_WM_KEYDOWN_Sortie
                    */
                // Affichage graphique
                /* Assembly
                    cas_WM_KEYDOWN_G: ; touche g
                        mov al, [affichageGraphique]
                        cmp al, 0
                        sete al
                        mov [affichageGraphique], al
                        jmp cas_WM_KEYDOWN_Sortie
                    cas_WM_KEYDOWN_H: ; touche h
                        xor bl, bl
                        mov al, [affichageConsole]
                        add al, 1
                        cmp al, 5
                        cmove ax, bx
                        mov [affichageConsole], al
                        jmp cas_WM_KEYDOWN_Sortie
                    */
                // Mode pas a pas (toggle)
                if (wParam == 'B' || wParam == 'b') {
                    mode_pas_a_pas = (mode_pas_a_pas == 0 ? 1 : 0);
                    return 0;
                }
                // Avancer d'un pas (en mode pas a pas)
                if (wParam == 'N' || wParam == 'n') {
                    if (mode_pas_a_pas == 1) {
                        mode_pas_a_pas = 2;
                    }
                    return 0;
                }
                /* Assembly
                    cas_WM_KEYDOWN_B: ; touche b
                        mov al, [mode_pas_a_pas]
                        cmp al, 0
                        je mettre_pasapas_a_1
                        mov [mode_pas_a_pas], byte 0
                        jmp fin_modifier_pasapas
                        mettre_pasapas_a_1:
                        mov [mode_pas_a_pas], byte 1
                        fin_modifier_pasapas:
                        jmp cas_WM_KEYDOWN_Sortie
                    cas_WM_KEYDOWN_N: ; touche n
                        mov al, [mode_pas_a_pas]
                        cmp al, 1
                        jne cas_WM_KEYDOWN_X_suite
                        mov [mode_pas_a_pas], byte 2
                        cas_WM_KEYDOWN_X_suite:
                        jmp cas_WM_KEYDOWN_Sortie
                    */
                // Affichage console
                if (wParam == 'H' || wParam == 'h') {
                    affichageConsole = (affichageConsole + 1) % 5;
                    return 0;
                }
                // Load / save (en mode pas a pas seulement)
                if (wParam == 'I' || wParam == 'i') {
                    if (mode_pas_a_pas == 1) {
                        LoadCervelet();
                    }
                    return 0;
                    /* Assembly
                        cas_WM_KEYDOWN_I: ; touche i
                            mov al, [mode_pas_a_pas]
                            cmp al, 1
                            jne cas_WM_KEYDOWN_Sortie
                                ; texte
                                call_winapi64_style GetStdHandle, STD_OUTPUT_HANDLE
                                mov r15, rax
                                call_datapush_style ecrire_ligne_4, r15, texte_chargement, -1, -1 ; interparagraphes
                                ; ouverture du fichier
                                lea rcx, [nom_de_fichier_sauvegarde] ;          LPCSTR          lpFileName
                                call_winapi64_style CreateFileA, rcx, GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL
                                mov [Filehandle], rax
                                ; GetFileSizeEx
                                mov rcx, [Filehandle] ;                         HANDLE          hFile,
                                lea rdx, [longueurFichier] ;                    PLARGE_INTEGER  lpFileSize
                                call_winapi64_style GetFileSizeEx, rcx, rdx
                                ; verification coherence longueur
                                mov rbx, [longueurFichier]
                                cmp rbx, USERLA_NB_P * TAILLE_DES_PORTIONS
                                je cas_WM_KEYDOWN_I_lg_ok
                                    call_datapush_style ecrire_ligne_4, r15, texte_erreur_longueur, -1, -1
                                    jmp cas_WM_KEYDOWN_Sortie
                                cas_WM_KEYDOWN_I_lg_ok:
                                ; lecture du contenu
                                mov rcx, [Filehandle] ;                         HANDLE      hFile,
                                lea rdx, [portions] ;                           LPVOID      lpBuffer,
                                mov rbx, [longueurFichier] ;                    DWORD       nNumberOfBytesToRead
                                call_winapi64_style ReadFile, rcx, rdx, rbx, 0, 0
                                ; fermeture du fichier
                                mov rcx, [Filehandle] ;                         HANDLE      hObject
                                call_winapi64_style CloseHandle, rcx
                                ; rechercher toutes les portions sources de données et ecrire les paramètres
                                lea rdx, [portions]
                                mov rcx, USERLA_NB_P
                                boucle_de_scan:
                                    mov al, [rdx+OS_TYPESTEV_1]
                                    cmp al, TYPE_SOURCE
                                    jne scan_suivant
                                        lea rax, [rawData_Texte]
                                        mov [rdx+OSD_ADDRS_8], rax
                                    scan_suivant:
                                    add rdx, TAILLE_DES_PORTIONS
                                    sub rcx, 1
                                jnz boucle_de_scan
                                ; ecriture fin de lecture
                                call_datapush_style ecrire_ligne_4, r15, texte_chargement_ok, -1, -1
                            jmp cas_WM_KEYDOWN_Sortie
                        */
                }
                if (wParam == 'O' || wParam == 'o') {
                    if (mode_pas_a_pas == 1) {
                        SaveCervelet();
                    }
                    return 0;
                    /* Assembly
                        cas_WM_KEYDOWN_O: ; touche o
                            mov al, [mode_pas_a_pas]
                            cmp al, 1
                            jne cas_WM_KEYDOWN_Sortie
                                ; texte
                                call_winapi64_style GetStdHandle, STD_OUTPUT_HANDLE
                                mov r15, rax
                                call_datapush_style ecrire_ligne_4, r15, texte_sauvegarde, -1, -1 ; interparagraphes
                                ; ouverture du fichier
                                lea rcx, [nom_de_fichier_sauvegarde] ;             LPCSTR      lpFileName
                                call_winapi64_style CreateFileA, rcx, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL
                                mov [Filehandle], rax
                                ; ecriture du contenu
                                mov rcx, [Filehandle] ;                 HANDLE      hFile,
                                lea rdx, [portions] ;                   LPVOID      lpBuffer,
                                call_winapi64_style WriteFile, rcx, rdx, USERLA_NB_P*TAILLE_DES_PORTIONS, 0, 0
                                ; fermeture du fichier
                                mov rcx, [Filehandle] ;                 HANDLE      hObject
                                call_winapi64_style CloseHandle, rcx
                                ; ecriture fin d ecriture
                                call_datapush_style ecrire_ligne_4, r15, texte_sauvegarde_ok, -1, -1
                            jmp cas_WM_KEYDOWN_Sortie
                        */
                }
                if (wParam == 'P' || wParam == 'p') {
                    printf("Key P pressed, exiting...");
                    PostQuitMessage(0);
                    return 0;
                }
                /* Assembly
                    cas_WM_KEYDOWN_P: ; touche p
                        cas_WM_KEYDOWN_Escape: ; touche escape / ne fonctionne pas
                        xor rcx, rcx
                        sub rsp, SHADOW_SPACE_SIZE
                        call PostQuitMessage
                        add rsp, SHADOW_SPACE_SIZE
                        jmp cas_WM_KEYDOWN_Sortie
                    cas_WM_KEYDOWN_Sortie: ; rax et sortie
                        xor rax, rax
                        jmp fin_WindowProc
                    */
                return 0;
            }
            /* Assembly
                cas_WM_SIZE: ; ---------------------------------------------------
                    ; rax et sortie
                        xor rax, rax
                        jmp fin_WindowProc
                    */
            case WM_PAINT: {
                /* Assembly
                    cas_WM_PAINT:  ; -------------------------------------------------
                    */
                PAINTSTRUCT paintStructure;
                HDC DrawingCtxHandle = BeginPaint(windowHandle, &paintStructure);
                //
                HDC DrawingCtxHandle2 = CreateCompatibleDC(DrawingCtxHandle);
                //
                BITMAPINFO bitMapInfos;
                ZeroMemory(&bitMapInfos, sizeof(BITMAPINFO)); // biSizeImage, biXPelsPerMeter, biYPelsPerMeter, biClrUsed, biClrImportant
                bitMapInfos.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
                bitMapInfos.bmiHeader.biWidth = LARGEUR_FENETRE;
                bitMapInfos.bmiHeader.biHeight = -HAUTEUR_FENETRE;
                bitMapInfos.bmiHeader.biPlanes = 1;
                bitMapInfos.bmiHeader.biBitCount = NOMBRE_OCTET_PAR_POINT*8;
                bitMapInfos.bmiHeader.biCompression = BI_RGB;
                void* pBitDataAdress = NULL;
                HBITMAP pBitDataHandle = CreateDIBSection(DrawingCtxHandle2, &bitMapInfos, DIB_RGB_COLORS, &pBitDataAdress, NULL, 0);
                if ((pBitDataHandle == 0) || (pBitDataAdress == 0)) {
                    goto cWP_no_dibs;
                }
                HGDIOBJ OldBitmapHandle2 = SelectObject(DrawingCtxHandle2, pBitDataHandle);
                GdiFlush();
                /* Assembly
                    ; initialisation du contexte de tracage => hdc
                        mov rcx, qword [rbp+16]             ; hWnd
                        lea rdx, [PaintStruct]              ; lpPaint
                        sub rsp, SHADOW_SPACE_SIZE
                        call BeginPaint
                        add rsp, SHADOW_SPACE_SIZE
                        mov [DrawingCtxHandle], rax                     ; hdc
                    ; creation d'un contexte de tracage compatible 2 => hdc2
                        mov rcx, qword [DrawingCtxHandle]
                        sub rsp, SHADOW_SPACE_SIZE
                        call CreateCompatibleDC
                        add rsp, SHADOW_SPACE_SIZE
                        mov [DrawingCtxHandle2], rax
                    ; creation d'une section DIB dans hdc2 => pBitDataHandle
                        mov rcx, qword [DrawingCtxHandle2]      ; hdc
                        lea rdx, [bitMapInfos]                  ; pbmi
                        mov r8, DIB_RGB_COLORS                  ; usage
                        lea r9, [pBitDataAdress]                ; ppvBits
                        push 0                                  ; offset
                        push 0                                  ; hSection
                        sub rsp, SHADOW_SPACE_SIZE
                        call CreateDIBSection
                        add rsp, SHADOW_SPACE_SIZE
                        add rsp, 16
                        mov [pBitDataHandle], rax                   ; handle de la DIB
                    ; verification DIBSection bien creee (handle et adresse != 0)
                        mov rax, [pBitDataHandle]
                        test rax, rax
                        jz cWP_no_dibs
                        mov rax, [pBitDataAdress]
                        test rax, rax
                        jz cWP_no_dibs
                    ; adaptation de hdc2 a la section DIB
                        mov rcx, qword [DrawingCtxHandle2]
                        mov rdx, qword [pBitDataHandle]
                        sub rsp, SHADOW_SPACE_SIZE
                        call SelectObject
                        add rsp, SHADOW_SPACE_SIZE
                        mov [OldBitmapHandle2], rax             ; handle de la precedente bitmap
                    ; purge actions GDI (necessaire avant de tracer sur la section DIB)
                        sub rsp, SHADOW_SPACE_SIZE
                        call GdiFlush
                        add rsp, SHADOW_SPACE_SIZE
                    */
                unsigned char* pixelData = (unsigned char*)pBitDataAdress;
                /* Assembly
                    ; tracage sur DIBSection
                        ; pointeur d'adresse de portion (init sur z)
                        mov rax, [visualize_z]
                        shl rax, DECALAGE_Z
                        push rax
                        call sub_adresse_de_numero_1
                        pop r10
                        ; pointeur d'adresse de point à tracer
                        mov r11, [pBitDataAdress]
                        mov r15, r11
                    */
                for (uint16_t lig = 0; lig < HAUTEUR_FENETRE; lig++) {
                    /* Assembly
                        ; traçage du contenu
                        mov r9, 0 ; compteur de ligne
                        cWP_boucle_lignes:
                        */
                    for (uint16_t col = 0; col < LARGEUR_FENETRE; col++) {
                        /* Assembly
                            mov r8, 0 ; compteur de colonne
                            cWP_boucle_colonnes:
                            */
                        switch (modeTracage) {
                            /* Assembly
                                xor rdx, rdx
                                mov cl, byte [modeTracage]
                                comparer_et_jump_si_egal cl, 0, cWP_tracage_mode_A0
                                comparer_et_jump_si_egal cl, 1, cWP_tracage_mode_Z1
                                comparer_et_jump_si_egal cl, 2, cWP_tracage_mode_E2
                                comparer_et_jump_si_egal cl, 3, cWP_tracage_mode_R3
                                jmp cWP_boucle_tracage_fin
                                */
                            case 0:
                                /* Assembly
                                    cWP_tracage_mode_A0: ; mode A0 / type de portion ------------------------------
                                        ; type
                                        mov dl, [r10+OS_TYPESTEV_1]
                                        and dl, MASQUE_TYPE_TSx
                                        comparer_et_jump_si_egal dl, TYPE_SEGMENT_GERME, cWP_bta0_vert
                                        comparer_et_jump_si_egal dl, TYPE_NEURONE_GERME, cWP_bta0_bleu
                                        comparer_et_jump_si_egal dl, TYPE_EXTENSION_GERME, cWP_bta0_rouge
                                        jmp cWP_boucle_tracage_fin
                                        ; ---------------- couleurs
                                        cWP_bta0_vert:
                                            mov [r11+VERT], byte 255
                                            jmp cWP_boucle_tracage_fin
                                        cWP_bta0_bleu:
                                            mov [r11+ROUG], byte 63
                                            mov [r11+VERT], byte 63
                                            mov [r11+BLEU], byte 255
                                            jmp cWP_boucle_tracage_fin
                                        cWP_bta0_violet:
                                            mov [r11+ROUG], byte 255
                                            mov [r11+BLEU], byte 255
                                            jmp cWP_boucle_tracage_fin
                                        cWP_bta0_rouge:
                                            mov [r11+ROUG], byte 255
                                            jmp cWP_boucle_tracage_fin
                                        */
                                break;
                            case 1:
                                /* Assembly
                                    cWP_tracage_mode_Z1: ; mode Z1 / bloc de boutons synaptiques ------------------------------
                                        ; affichage valeur bloc de boutons => vert
                                        mov dl, [r10+OSE_BSYNV_1]
                                        mov [r11+VERT], dl
                                        jmp cWP_boucle_tracage_fin
                                    */
                                break;
                            case 2: {
                                /* Assembly
                                    cWP_tracage_mode_E2: ; mode E2 / charge actuelle ------------------------------
                                    ; type
                                    */
                                uint16_t charge = 0;
                                switch (crvDatas[visualize_z][lig][col][OS_TYPESTEV_1]) {
                                    /* Assembly
                                        mov dl, [r10+OS_TYPESTEV_1]
                                        and dl, MASQUE_TYPE_TSx
                                        comparer_et_jump_si_egal dl, TYPE_PULSEUR, cWP_bte2_pulseur
                                        comparer_et_jump_si_egal dl, TYPE_SEGMENT_GERME, cWP_bte2_seg_neu
                                        comparer_et_jump_si_egal dl, TYPE_NEURONE_GERME, cWP_bte2_seg_neu
                                        jmp cWP_boucle_tracage_fin
                                        */
                                    case TYPE_PULSEUR: {
                                        uint8_t* baseAddress = &crvDatas[visualize_z][lig][col][OSP_CHARGI_2];
                                        charge = *(uint16_t*)baseAddress;
                                        /* Assembly
                                            ; ---------------- affichage charge/chrono => vert ou rouge
                                            cWP_bte2_pulseur:
                                            mov dx, [r10+OSP_CHARGI_2]
                                            jmp cWP_bte2_traiter
                                            */
                                        break;
                                    }
                                    case TYPE_SEGMENT_COMPLET:
                                    case TYPE_NEURONE_COMPLET: {
                                        uint8_t* baseAddress = &crvDatas[visualize_z][lig][col][OSP_CHARGI_2];
                                        charge = *(uint16_t*)baseAddress;
                                        /* Assembly
                                            cWP_bte2_seg_neu:
                                                mov dx, [r10+OS_CHARG0_2]
                                            */
                                        break;
                                    }
                                }
                                if (charge > 255) { // grand => vert
                                    uint8_t vert = (uint8_t)(32+charge/293);
                                    pixelData[NOMBRE_OCTET_PAR_POINT*(lig*(LARGEUR_FENETRE+COMPLEMENT_LIGNE_DWORD)+col)+VERT] = vert;
                                }
                                else { // petit => rouge
                                    uint8_t rouge = (uint8_t)charge;
                                    pixelData[NOMBRE_OCTET_PAR_POINT*(lig*(LARGEUR_FENETRE+COMPLEMENT_LIGNE_DWORD)+col)+ROUG] = rouge;
                                }
                                /* Assembly
                                    cWP_bte2_traiter:
                                        cmp dx, 256
                                        jae cWP_bte2_grand
                                        jmp cWP_bte2_petit
                                        cWP_bte2_grand:
                                            mov [r11+VERT], dl
                                            jmp cWP_boucle_tracage_fin
                                        cWP_bte2_petit:
                                            shr dx, 8
                                            mov [r11+ROUG], dl
                                            jmp cWP_boucle_tracage_fin
                                    */
                            }
                            case 3:
                                /* Assembly
                                    cWP_tracage_mode_R3: ; mode R3 / activité + retroactivité ------------------------------
                                        ; type
                                        mov dl, [r10+OS_TYPESTEV_1]
                                        and dl, MASQUE_TYPE_TSx
                                        comparer_et_jump_si_egal dl, TYPE_SEGMENT_GERME, cWP_btr3_traiter
                                        comparer_et_jump_si_egal dl, TYPE_NEURONE_GERME, cWP_btr3_traiter
                                        comparer_et_jump_si_egal dl, TYPE_EXTENSION_GERME, cWP_btr3_traiter
                                        jmp cWP_boucle_tracage_fin
                                        ; affichage cible retroactive (forcement simple ou liaison) => bleu
                                        cWP_btr3_traiter:
                                        mov dl, [r10+OS_ACTIVITE_1]
                                        comparer_et_jump_si_egal dl, 0, cWP_boucle_tracage_fin
                                        mov [r11+BLEU], byte 255
                                        ; jmp cWP_boucle_tracage_fin
                                        ; fin des options ----------------------
                                    */
                                break;
                        }
                        /* Assembly
                            cWP_boucle_tracage_fin:
                            ; recuperation adresse pour viseur
                            cmp r8, [visualize_x]
                            jne cWP_boucle_tracage_fin2
                            cmp r9, [visualize_y]
                            jne cWP_boucle_tracage_fin2
                            mov r15, r11
                            cWP_boucle_tracage_fin2:
                            ; bouclages
                            add r10, TAILLE_DES_PORTIONS ; portion suivante
                            add r11, NOMBRE_OCTET_PAR_POINT ; adresse point suivant
                            inc r8 ; colonne suivante
                            cmp r8, LARGEUR_FENETRE
                            */
                    }
                    // Le complement de ligne DWORD n'est pas la car il est integre au calcul d'adresse
                    /* Assembly
                        jne cWP_boucle_colonnes
                        ; fin de boucle de colonnes
                        add r11, COMPLEMENT_LIGNE_DWORD ; adresse point ligne suivante
                        inc r9 ; ligne suivante
                        cmp r9, HAUTEUR_FENETRE
                        cmovne r14, r11 ; pour calcul decalage d'adresse par ligne (pour viseur)
                        */
                }
                /* Assembly
                    jne cWP_boucle_lignes
                    */
                // Faire ici le tracage d'un point simple
                pixelData[NOMBRE_OCTET_PAR_POINT*(visualize_y*(LARGEUR_FENETRE+COMPLEMENT_LIGNE_DWORD)+visualize_x)+ROUG] = 255;
                pixelData[NOMBRE_OCTET_PAR_POINT*(visualize_y*(LARGEUR_FENETRE+COMPLEMENT_LIGNE_DWORD)+visualize_x)+VERT] = 255;
                pixelData[NOMBRE_OCTET_PAR_POINT*(visualize_y*(LARGEUR_FENETRE+COMPLEMENT_LIGNE_DWORD)+visualize_x)+BLEU] = 255;
                /* Assembly
                    ; tracage point simple
                    mov [r15+ROUG], byte 255
                    mov [r15+VERT], byte 255
                    mov [r15+BLEU], byte 255
                    jmp cWP_suite_apres_tracage
                        ; décalages
                        mov r8, NOMBRE_OCTET_PAR_POINT ; décalage d'adresse par colonne
                        mov r9, r11
                        sub r9, r14 ; décalage d'adresse par ligne
                        ; traçage viseur
                        shl r8, 1
                        shl r9, 1
                        ; A_FAIRE : mieux dessiner
                        ; A_FAIRE : empecher le traçage des points hors de la zone
                        ; point 1
                            mov rax, r15
                            add rax, r8
                            add rax, r9
                            mov [rax+ROUG], byte 255
                            mov [rax+VERT], byte 255
                            mov [rax+BLEU], byte 255
                        ; point 2
                            mov rax, r15
                            add rax, r8
                            sub rax, r9
                            mov [rax+ROUG], byte 255
                            mov [rax+VERT], byte 255
                            mov [rax+BLEU], byte 255
                        ; point 3
                            mov rax, r15
                            sub rax, r8
                            sub rax, r9
                            mov [rax+ROUG], byte 255
                            mov [rax+VERT], byte 255
                            mov [rax+BLEU], byte 255
                        ; point 4
                            mov rax, r15
                            sub rax, r8
                            add rax, r9
                            mov [rax+ROUG], byte 255
                            mov [rax+VERT], byte 255
                            mov [rax+BLEU], byte 255
                    cWP_suite_apres_tracage:
                    */
                // copie de ce qui a ete trace vers le contexte de la fenetre
                BitBlt(DrawingCtxHandle, 0, 0, LARGEUR_FENETRE, HAUTEUR_FENETRE, DrawingCtxHandle2, 0, 0, MERGECOPY); // devrait etre SRCCOPY
                /* Assembly
                    ; copie d'un morceau de section DIB vers hDC
                        mov rcx, qword [DrawingCtxHandle]           ; Destination device context
                        mov edx, 0                                  ; Destination X
                        mov r8d, 0                                  ; Destination Y
                        mov r9d, LARGEUR_FENETRE                    ; Width
                        sub rsp, 8                              ; pre-alignement de la pile
                        push ROP                                    ; Operation
                        push 0                                      ; Source Y
                        push 0                                      ; Source X
                        mov rax, qword [DrawingCtxHandle2]
                        push rax                                    ; Source device context
                        push HAUTEUR_FENETRE                        ; Height
                        sub rsp, SHADOW_SPACE_SIZE
                        call BitBlt                                 ; Blit a rectangle
                        add rsp, SHADOW_SPACE_SIZE
                        add rsp, 48
                    */
                // recuperation de l'ancien objet bitmap 2
                SelectObject(DrawingCtxHandle2, OldBitmapHandle2);
                /* Assembly
                    ; recuperation de l'ancien objet bitmap 2
                        mov rcx, qword [DrawingCtxHandle2]
                        mov rdx, qword [OldBitmapHandle2]
                        sub rsp, SHADOW_SPACE_SIZE
                        call SelectObject
                        add rsp, SHADOW_SPACE_SIZE
                    */
                // destruction de l'objet bitmap 2
                DeleteObject(pBitDataHandle);
                /* Assembly
                    ; destruction de l'objet bitmap 2
                        mov rcx, qword [pBitDataHandle]
                        sub rsp, SHADOW_SPACE_SIZE
                        call DeleteObject
                        add rsp, SHADOW_SPACE_SIZE
                    */
                // destruction du DIB ?
                DeleteDC(DrawingCtxHandle2);
                /* Assembly
                    ; destruction du DIB
                        mov rcx, qword [DrawingCtxHandle2]
                        sub rsp, SHADOW_SPACE_SIZE
                        call DeleteDC
                        add rsp, SHADOW_SPACE_SIZE
                        */
                // cloture du tracage
                cWP_no_dibs:
                EndPaint(windowHandle, &paintStructure);
                return 0;
                /* Assembly
                    cWP_no_dibs: ; cloture du tracage
                        mov rcx, qword [rbp+16]                 ; hWnd
                        lea rdx, [PaintStruct]                    ; lpPaint
                        sub rsp, SHADOW_SPACE_SIZE
                        call EndPaint
                        add rsp, SHADOW_SPACE_SIZE
                    ; rax et sortie
                        xor rax, rax
                        jmp fin_WindowProc
                    */
            }
            case WM_DESTROY:
                PostQuitMessage(0);
                return 0;
                /* Assembly
                    cas_WM_DESTROY: ; ------------------------------------------------
                        ; envoi du message de fin
                            xor rcx, rcx
                            sub rsp, SHADOW_SPACE_SIZE
                            call PostQuitMessage
                            add rsp, SHADOW_SPACE_SIZE
                        ; rax et sortie
                            xor rax, rax
                            jmp fin_WindowProc
                    */
            default:
                return DefWindowProcA(windowHandle, uMsg, wParam, lParam);
                /* Assembly
                    traitement_standard: ; -------------------------------------------
                        ; recuperation des valeurs
                            mov rcx, qword [rbp+16]                             ; hWnd
                            mov rdx, qword [rbp+24]                             ; Msg
                            mov r8, qword [rbp+32]                              ; wParam
                            mov r9, qword [rbp+40]                              ; lParam
                        ; appel
                            sub rsp, SHADOW_SPACE_SIZE
                            call DefWindowProcA
                            add rsp, SHADOW_SPACE_SIZE
                            ; rax de sortie de DefWindowProcA est renvoye par WindowProc
                    */
        }
        /* Assembly
                fin_WindowProc: ; ------------------------------------------------
                    mov rsp, rbp        ; Restores the stack pointer
                    pop rbp             ; Restores the base pointer
                    ret
            */
    }
