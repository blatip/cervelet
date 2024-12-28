; #------------------------------------------------------------------------------------ GPL LICENCE
    ; Copyright (C) 2024 Philippe BLATIERE
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
; #------------------------------------------------------------------------------------ A_FAIRE
    ; résoudre tous les points A_FAIRE
    ; trouver une solution pour une mise en réseau
    ; coder les développements dendrites / axones

; #------------------------------------------------------------------------------------ REFERENCES
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
        extern FillConsoleOutputCharacterA
        extern SetConsoleCursorPosition         ; Positionnement du curseur

        extern WriteConsoleA                    ; Sortie en mode console (ANSI)
        extern ReadConsoleA                     ; Entree en mode console (ANSI)
        extern GetNumberOfConsoleInputEvents    ; Nombre d'entrees console
        extern ReadConsoleInputA                ; Lecture du tampon de console
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
    ; pour communication reseau
        extern WSAStartup
        extern WSACleanup
        extern socket
        ; extern connect
        ; extern send
        extern sendto
        extern closesocket
        ;
        extern bind
        extern listen
        extern accept
        extern recv
    ; pour gestion des threads
        extern CreateThread
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
; #----------- entrees : FVTM / sorties : S / imperatifs : (ABCD 89012345 ou L/N) ----- MACROS
    ; outils divers
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
            ; %1 REG mod = sortie adresse portion
            ; %2 REG mod = entree numero portion
            ; ----- recuperation adresse de base
            lea %1, [users]
            ; ----- ajout de TAILLE_DES_UTILISATEURS fois le numero de portion
            shl %2, 3 ; %2 x 8
            add %1, %2 ; +8xTDU
            shl %2, 2 ; %2 x 32
            add %1, %2 ; +40xTDU
            ; ----- fin
            %endmacro

        %macro longueur_from_quantity 2 ; calcul de la longueur d'un paquet d'après le nombre de messages dans msgToSnd (%1 <-- %2)
            ; a garder en coherence avec TAILLE_DES_MESSAGES
            ; utilisation pour traitement
            ; %1 REG mod = sortie longueur paquet
            ; %2 REG mod = entree nombre de messages
            ; ----- calcul
            shl %2, 4 ; %2 x 16
            mov %1, %2
            ; ----- fin
            %endmacro

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
    ; pour évolutions
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
    ; macros hors boucle
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
        %macro ligne_de_valeur 6
            ; ----- parametres
            ; %1 IN = handle sortie standard
            ; %2 IN = adresse
            ; %3 TXT = offset
            ; %4 IN = registre a la bonne longueur
            ; %5 IN = registre 64 bits associe
            ; %6 IN = texte de presentation
            ; exemple : ligne_de_valeur r15, rdx, OSN_CREFR_1, cx, rcx, portion_chrs_text
            ; -----
            xor %5, %5
            mov %4, [%2+%3]
            ; call_winapi64_style WriteConsoleA, %1, %6, %7, reponse_long_ret
            ; call_winapi64_style convertir, %5, nombre_text, LONGUEUR_NOMBRES
            ; call_winapi64_style WriteConsoleA, %1, nombre_text, LONGUEUR_NOMBRES, reponse_long_ret
            call_datapush_style ecrire_ligne_4, %1, %6, %5, -1
            %endmacro
; #------------------------------------------------------------------------------------ CONSTANTES EQU
    ; dimensionnement utilisateur (portions)
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
        FULLGA_BITS: equ 48 ; 6 bytes ; same for all ; maxi = 64 - bits pour taille des portions ; 24bytes=>5bits=>64-5=59max
        FULLGA_NB_P: equ (1<<FULLGA_BITS)
        FULLGA_MASK: equ FULLGA_NB_P-1
        ;
        PACKID_BITS: equ FULLGA_BITS-LOCMAX_BITS
        PACKID_NB_P: equ (1<<PACKID_BITS)
        PACKID_MASK: equ FULLGA_MASK-LOCMAX_MASK
        ;
        LOCMAX_BITS: equ 28 ; locally max / is shifted for packId / same for all / 
        LOCMAX_NB_P: equ (1<<LOCMAX_BITS)
        LOCMAX_MASK: equ LOCMAX_NB_P-1
        ;
        USERLA_BITS: equ BITS_POUR_X+BITS_POUR_Y+BITS_POUR_Z ; must be <= LOCMAX_BITS
        USERLA_NB_P: equ (1<<USERLA_BITS) ; = DIMENSION_X*DIMENSION_Y*DIMENSION_Z
        USERLA_MASK: equ USERLA_NB_P-1
        ;
        ; A_FAIRE : faire un test pour vérifier que USERLA_BITS <= LOCMAX_BITS
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
        DEST_IP_PORT: equ 0x5000 ; port IP 80 (en big-endian)
        ; DEST_IP_ADDRESS: equ 0x2201A8C0 ; adresse en hexadécimal (192.168.1.34) (bswap eax) - portable Va
        DEST_IP_ADDRESS: equ 0x0101A8C0 ; adresse en hexadécimal (192.168.1.34) (bswap eax) - pour tests
    ; codes de demandes (1 octet ?)
        MC_SURCHARGE_QW: equ 0x4843525300000000 ; '____SRCH'
    ; reglages portions
        ; communs
        PORTION_ZERO: equ 0 ; portion zéro invalide = pas de portion
        DUREE_REFRACTAIRE: equ 10 ; maxi 255 / selon longueur temporelle maxi des dendrites apicales
        ; pulseurs / pour traitement
        INCREMENT_PULSEURS: equ 100 ; maxi 65535
        CHRONO_INITIAL: equ 0 ; maxi = CHRONO_MAXI
        CHRONO_MAXI: equ 65535
        ; neurones
        CHARGE_INVALIDE: equ -32768 ; charge invalide (word)
        CHARGE_MINI: equ -32767 ; limite de charge basse (word) (exclusion de la charge invalide)
        CHARGE_MAXI: equ +32767 ; limite de charge haute (word)
        ; increment par surcharge
        VITESSE_DECHARGE: equ 0
    ; valeurs pour réseaux préconfigurés
        VS_VALBB: equ 127 ; maxi 127
        VS_MASBB: equ 255 ; maxi 255
        VS_PSEUIL: equ 8 ; maxi 15
        VS_BREFRC: equ 15 ; mini = durée maxi de remontée réfractaire (cumul chaine apicale de segments le plus long)
    ; réglages bouclages et écritures
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
        ; dans bouclege lent : ecritures console
        ; BOUCLAGE LENT : systematique sauf sur fermeture fenetre
    ; types de portions
        TYPE_SOURCE: equ                            0b101_00_01_0 ; 162 type source de données en mémoire
        TYPE_LECTEUR: equ                           0b101_00_00_0 ; 160 type lecteur
        TYPE_PULSEUR: equ                           0b100_00_00_0 ; 128 type pulseur
        TYPE_SEGMENT_GERME: equ                     0b001_00_00_0 ; 32 type dendrite figé sans dendrite amont
        TYPE_SEGMENT_PARTIEL: equ                   0b001_01_00_0 ; 40 type dendrite auquel il manque des dendrites
        TYPE_SEGMENT_COMPLET: equ                   0b001_10_00_0 ; 48 type dendrite avec toutes dendrite amont
        TYPE_NEURONE_GERME: equ                     0b011_00_00_0 ; 96 type neurone figé sans dendrite ni axone
        TYPE_NEURONE_PARTIEL: equ                   0b011_01_00_0 ; 104 type neurone auquel il manque des dendrites et/ou l'axone
        TYPE_NEURONE_COMPLET: equ                   0b011_10_00_0 ; 112 type neurone avec toutes dendrites et neurone
        TYPE_EXTENSION_GERME: equ                   0b010_00_00_0 ; 64 type axone figé sans destination ni extension
        TYPE_EXTENSION_PARTIEL: equ                 0b010_01_00_0 ; 72 type axone auquel il manque la destination ou l'extension
        TYPE_EXTENSION_COMPLET: equ                 0b010_10_00_0 ; 80 type axone avec destination et extension
    ; masques or, and, xor T3 E2 S2 A1
        MASQUE_TYPE_Txxx: equ       0b111_00_00_0
        MASQUE_TYPE_xExx: equ       0b000_11_00_0
        MASQUE_TYPE_xxSx: equ       0b000_00_11_0
        MASQUE_TYPE_xxxA: equ       0b000_00_00_1
        MASQUE_TYPE_Txxx_X: equ     0b000_11_11_1
        MASQUE_TYPE_xExx_X: equ     0b111_00_11_1
        MASQUE_TYPE_xxSx_X: equ     0b111_11_00_1
        MASQUE_TYPE_xxxA_X: equ     0b111_11_11_0
    ; taille et offsets dans portions : OS + */D/L/P/S/N/E
        TAILLE_DES_PORTIONS: equ 32
        ; ----- offsets communs impératifs
        OS_TYPEVSA_1:   equ 0 ; offset du type/evolution/sous-type/activation
        OS_NACTT_1:    equ 4 ; offset niveau activité temporelle
        OS_CHARG0_2:    equ 8 ; offset charge 0 en cours
        ; ----- offsets accès mémoire
        ; commun type-evo-activité : 1<0
        OSD_LGSEG_1:    equ 4 ; offset de la largeur des segments (en octets / 8 maxi)
        OSD_NBSEG_2:    equ 8 ; offset du nombre de segments
        OSD_ADDRS_8:    equ 16 ; offset de l'adresse de base
        ; ----- offsets lecteurs
        ; commun type-evo-activité : 1<0
        OSL_PERSA_1:    equ 2 ; offset de la persistance actuelle (2)
        OSL_PERSB_1:    equ 3 ; offset de la base de persistance (3)
        OSL_INDEX_2:    equ 4 ; offset de l'index du segment à lire (4)
        OSL_NMBIT_1:    equ 6 ; offset de numéro de bit à lire (6)
        OSL_BSYNV_1:    equ 7 ; offset valeur du bloc de boutons (7)
        OSL_PAMEM_4:    equ 8 ; offset du numéro de portion des paramètres d'accès mémoire
        OSL_DESTN_4:    equ 16 ; offset du numero de portion de destination
        ; ----- offsets pulseurs
        ; commun type-evo-activité : 1<0
        OSP_CHARGI_2:   equ 8 ; offset charge interne
        OSP_SEUIL_1:    equ 10 ; offset puissance de seuil
        OSP_BSYNV_1:    equ 11 ; offset valeur du bloc de boutons
        OSP_DESTN_4:    equ 16 ; offset du numero de portion de destination
        ; ----- offsets segments dendritiques
        ; commun type-evo-activité : 1<0
        OSS_TEVOS_1:    equ 1 ; offset de temporisation d'evolution de segment
        OSS_PSYNA_2:    equ 2 ; offset potentiel synaptique restant
        ; commun charge 0 : 2<8
        OSS_CHARG1_2:   equ 10 ; offset charge 1 stockée
        OSS_CHARG2_2:   equ 12 ; offset charge 2 stockée
        OSS_SEGNS_4:    equ 16 ; offset du numero de segment/neurone suivant
        OSS_PRSEG_1:    equ 22 ; offset potentiel de segments restants
        ; ----- offsets neurones
        ; commun type-evo-activité : 1<0
        OSN_CREFR_1:    equ 6 ; offset du (dé-)compteur réfractaire
        OSN_BREFR_1:    equ 7 ; offset de la base réfractaire
        ; commun : charge 0 : 2<8
        OSN_SEUIL_1:    equ 10 ; offset de la puissance du seuil de charge
        OSN_PRAYO_1:    equ 12 ; offset puissance de segments dendritiques rayonnants
        OSN_PPLAN_1:    equ 13 ; offset puissance de segments dendritiques planaires
        OSN_PAPIC_1:    equ 14 ; offset puissance de segments dendritiques apicaux
        OSN_PPANI_1:    equ 15 ; offset puissance de segments dendritiques paniers
        OSN_PAXON_1:    equ 22 ; offset puissance de potentiel d'extensions axonales
        OSN_ORIENT_1:   equ 23 ; offset orientation des développements
        ; ----- offsets extensions axonales
        ; commun type-evo-activité : 1<0
        OSE_TEVOE_1:    equ 1 ; offset de temporisation d'evolution d'extension
        OSE_CACTT_1:    equ 4 ; offset compteur d'activité temporelle extension
        OSE_BSYNV_1:    equ 6 ; offset de la valeur du bloc de boutons synaptiques
        OSE_BSYNM_1:    equ 7 ; offset de la masse du bloc de boutons synaptiques
        OSE_DESTN_6:    equ 8 ; offset du numero de portion de destination
        OSE_ANTEC_4:    equ 16 ; offset du numero de portion antécédente
        OSE_PREXT_1:    equ 22 ; offset potentiel d'extensions restantes
    ; taille et offsets dans liste des utilisateurs users : OU_
        TAILLE_DES_UTILISATEURS: equ 40
        OU_IPAD_4: equ 0 ; adresse IP de l'utilisateur
        OU_PORT_2: equ 4 ; port ouvert
        OU_NM_4_4: equ 8 ; 2x nombre de messages pour ce destinataire
        OU_ADRM_8: equ 16 ; adresse dans msgToSnd pour ce destinataire (début)
        OU_ADRT_8: equ 24 ; adresse dans msgToSnd pour ce destinataire (en cours)
        OU_LNGM_8: equ 32 ; nombre d'octet de la plage
    ; taille et offsets dans liste de notes msgStack : ON_
        TAILLE_DES_NOTES: equ 32
        ON_DEST_8: equ 8 ; numero de destinataire complet
        ON_DEMD_8: equ 16 ; demande complete (code + param)
        ON_ADST_8: equ 24 ; adresse memoire du destinataire
    ; taille et offsets dans liste de messages dans msgToSnd : OM_
        TAILLE_DES_MESSAGES: equ 16
        OM_DEST_8: equ 0 ; numero de destinataire complet
        OM_DEMS_8: equ 8 ; demande complète (code + param)
        ; OM_ADST_8: equ 24 ; adresse memoire du destinataire
    ; paramètres graphiques
        ; dimensions fenêtre
        LARGEUR_FENETRE: equ DIMENSION_X
        HAUTEUR_FENETRE: equ DIMENSION_Y
        ; codage mémoire / graphique
        NOMBRE_OCTET_PAR_POINT: equ 3
        COMPLEMENT_LIGNE_DWORD: equ ((LARGEUR_FENETRE*NOMBRE_OCTET_PAR_POINT*8+7)/8) % 4
        ; offsets couleurs
        ROUG:   equ 2
        VERT:   equ 1
        BLEU:   equ 0
        ; dimensions viseur
        VISEUR_INT: equ 3
        VISEUR_EXT: equ 10
    ; pour console
        POSITION_TABLE_X: equ 50
        POSITION_TABLE_Y: equ 5
        LONGUEUR_LIGNES: equ 45 ; NOMBRE_CARS_AVANT+NOMBRE_CARS_APRES+20 mini pour accepter les nombres jusqu'à 64 bits / 254 maxi au total
        HAUTEUR_TABLE: equ 19
        NOMBRE_CARS_AVANT: equ 2
        NOMBRE_CARS_APRES: equ 2
        LONGUEUR_NOMBRES: equ 20 + 1 ; 1 pour marge
        REPONSE_LONG_MAX: equ 12
    ; pour lecture de fichier
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
    
section .rodata ; --------------------------------------------------------------------- TEXTES ET TABLES
        ; lecture du fichier
            nom_de_fichier_texte: db '.\textealire.txt', 0
        ; sauvegardes
            nom_de_fichier_sauvegarde: db '.\structure.crv', 0
        ; winapi handles/instances/adresses
            app_instance: db 'Handle process winapi : ', 0
            drawing_handle: db 'Handle de drawing : ', 0
            DIB_handle: db 'Handle de DIB : ', 0
            DIB_address: db 'Adresse de DIB : ', 0
        ; winapi noms divers
            windowClassName: db 'Classe de fenetre', 0
            windowName: db 'Fenetre de tracage', 0
            NOM_FENETRE_A0: db 'Types de portions', 0
            NOM_FENETRE_Z1: db 'Valeurs du bloc de boutons', 0
            NOM_FENETRE_E2: db 'Charges et chronos', 0
            NOM_FENETRE_R3: db 'Activation et retro-activation', 0
        ; pseudo-textes
            ligne_de_cadre: db LONGUEUR_LIGNES dup ('#'), 0
            ligne_vide: db '', 0
            interparagraphes_text: db LONGUEUR_LIGNES dup ('#'), 0
            interligne_text: db '----------', 0
        ; textes communs
            texte_numero_de_boucle: db 'Numero de boucle longue : ', 0
            texte_duree_de_boucle: db 'Duree de boucle sur portions : ', 0
            texte_duree_de_posttrait: db 'Duree de post traitement : ', 0
            texte_nombre_d_utilisateurs: db 'Nombre d utilisateurs : ', 0
            texte_nombre_de_messages_a_emettre: db 'Nombre de message to send : ', 0
            texte_valeur_de_test: db '------ Valeur de test : ', 0
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
            portion_nonu_text: db 'Non utilise : ', 0
            ; textes commun de détails
            portion_numr_text: db 'Numero de la portion : ', 0
            texte_position_x: db 'Abscisse x : ', 0
            texte_position_y: db 'Ordonnee y : ', 0
            texte_position_z: db 'Profondeur z : ', 0
            ; détail portions
            portion_type_text: db 'Type de portion : ', 0
            portion_chrg_text: db 'Charge actuelle : ', 0
            portion_chrs_text: db 'Seuil charge : ', 0
            portion_cref_text: db 'Compteur refractaire : ', 0
            portion_bref_text: db 'Base refractaire : ', 0
            portion_blcv_text: db 'Valeur du bloc de boutons : ', 0
            portion_blcp_text: db 'Masse du bloc de boutons : ', 0
            portion_dest_text: db 'Portion de destination : ', 0
            portion_lias_text: db 'Portion de liaison : ', 0
            portion_ctac_text: db 'Compteur temporel d activite : ', 0
            portion_ntac_text: db 'Niveau temporel d activite : ', 0
            portion_devo_text: db 'Decompte d evolution : ', 0
            ; specifiques accès mémoire
            portion_adrs_text: db 'Adresse source : ', 0
            portion_tseg_text: db 'Taille des segments : ', 0
            portion_nseg_text: db 'Nombre de segments : ', 0
            ; specifiques lecteurs
            portion_idxl_text: db 'Index de lecture : ', 0
            portion_bitl_text: db 'Numero de bit a lire : ', 0
            portion_prsa_text: db 'Persistance actuelle : ', 0
            portion_prsb_text: db 'Persistance de base : ', 0
            portion_pamm_text: db 'Portion d acces memoire : ', 0
            ; specifiques pulseurs
            portion_chro_text: db 'Chrono actuel : ', 0
            portion_chrl_text: db 'Limite chrono : ', 0
            ; specifiques segments / extensions
            portion_segs_text: db 'Segment suivant : ', 0
            portion_psyn_text: db 'Potentiel synaptique restant : ', 0
            portion_pseg_text: db 'Potentiel segments restants : ', 0
            portion_pext_text: db 'Potentiel extensions restantes : ', 0
            ; spécifiques neurones
            portion_ornt_text: db 'Orientation developpements : ', 0
            portion_pray_text: db 'Potentiel rayonnant : ', 0
            portion_ppla_text: db 'Potentiel planaire : ', 0
            portion_papi_text: db 'Potentiel apical : ', 0
            portion_ppan_text: db 'Potentiel panier : ', 0
            portion_paxo_text: db 'Potentiel axonal : ', 0
            ; spécifiques extensions
            portion_eaxo_text: db 'Extension axonale : ', 0
        ; textes menu d'aides général
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

section .data ; ----------------------------------------------------------------------- VARIABLES INITALISEES
    ; systeme
        ErrorsBreak: db 1
    ; utilisateur
        userPackNumber: dq 0 * LOCMAX_NB_P ; remplacer 0 par le numéro d'utilisateur
    ; interface graphique
        modeTracage: db 2
        visualize_x: dq 20
        visualize_y: dq 10
        visualize_z: dq 0
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
        messageDeTest: db 'CECI EST UN TEST'
        MESSAGE_DE_TEST_LEN: equ $ - messageDeTest
    ; section .data pour reseau
                    sockaddr_in: ; struc
                        family: dw 2           ; AF_INET (2)
                        port: dw 0x1F90        ; Port 8080 (0x1F90)
                        addr: dd 0x00000000    ; 0.0.0.0 (INADDR_ANY)
                        zero: db 8 dup(0)
                    ; endstruc

section .bss ; ------------------------------------------------------------------------ RESERVATIONS MEMOIRE
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
    ; utilisateur
        myPackId: resd 1
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
        nombredutilisateurs: resq 1
        valeurdetest: resq 1
    ; pour deroulement et duree des boucles
        numero_de_boucle_lente: resq 1
        procCycleCntr_avant_bouclage_moyen: resq 1
        procCycleCntr_avant_bouclage_rapide: resq 1
        procCycleCntr_avant_bouclage_portions: resq 1
        procCycleCntr_apres_bouclage_portions: resq 1
        ; vitesse
        dureeDeTraitement: resq 1
        dureeDePostTraitement: resq 1
        ; communication
        nombreDeMessageAEmettre: resq 1
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
    ; pour communication reseau
        wsaDataStruct: resb 400
        mainSocketFileDescriptor: resq 1
        socketFileDescriptor: resq 1
        toDestSocket: resb SOCKET_ADDRESS_LEN

    ; section .bss pour communication reseau
                ; émission
                    sock_fd: resq 1
                    sockaddr: resb 16
                    buffer: resb 512             ; Buffer de réception
                    buffer_len: equ 512        ; Taille du buffer


section .text ; ------------------------------------------------------------------------------- DEBUT DU CODE
    global main
    main:
        ; alignement de la pile
        sub rsp, 8
        ; call_winapi64_style GetStdHandle, STD_OUTPUT_HANDLE
        ; mov [outputHandle], rax
        ; utilisateur
        mov dword [myPackId], 0
; #-------------------------------------------------------------------------------------------- CREATION FENETRE ET ASSOCIES
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
        ; affichage de la fenetre
            mov   rcx, qword [Windowhandle]                     ; hWnd
            mov   rdx, SW_SHOW                                  ; nCmdShow
            sub   rsp, SHADOW_SPACE_SIZE
            call  ShowWindow
            add   rsp, SHADOW_SPACE_SIZE
        ; mise a jour de la fenetre
            mov   rcx, qword [Windowhandle]                     ; hWnd
            sub   rsp, SHADOW_SPACE_SIZE
            call  UpdateWindow
            add   rsp, SHADOW_SPACE_SIZE
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
; #-------------------------------------------------------------------------------------------- LECTURE DU FICHIER D'ENTREES
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
; #-------------------------------------------------------------------------------------------- INITIALISATION DU CERVEAU
    ; =============================================== RAZ PORTIONS
        lea rdx, [portions]
        mov rax, USERLA_NB_P*TAILLE_DES_PORTIONS
        boucle_init_portions:
            mov [rdx], byte 0
            sub rax, 1
        jnz boucle_init_portions
    ; =============================================== DIVERS
        ; --------------- source de données texte
            lea rdx, [rawData_Texte]
            xor rax, rax
            mov ax, [longueur_Texte]
            call_datapush_style sub_creer_portion_datasource_4, \
            1+0*FY+0*FZ, rdx,     rax,             1
            ; Numero,    Adresse, NombreDElements, LongueurElement
        ; --------------- réseau de pulseurs à droite
            call_datapush_style sub_creer_reseau, \
            250+1*FY+1*FZ, TYPE_PULSEUR, 250+1*FY+0*FZ, 240, 110, 1, 1 
            ; Dest       Type          NumP         n/X  n/Y n/Z Pas
    ; =============================================== CREATIONS PAR RESEAUX (/32 couches)
        ; 0 --------------- réseau de lecteurs
            ; creer macro : params_lecteurs_pnsd myPackId, 2+2*FY+0*FZ, 1+0*FY+0*FZ, 2+2*FY+1*FZ
            mov ebx, dword [myPackId]
            mov ecx, dword [myPackId]
            mov edx, dword [myPackId]
            add rbx, 1+0*FY+0*FZ ; 1ère source
            add rcx, 2+2*FY+1*FZ ; 1ère destination
            add rdx, 2+2*FY+0*FZ ; 1ère portion
            call_datapush_style sub_creer_reseau, \
            rbx, rcx, TYPE_LECTEUR, rdx, 240, 110, 1, 1
            ; Source,    Dest         Type          NumP         n/X  n/Y n/Z Pas
        ; 1 --------------- réseau de neurones
                                    call_datapush_style sub_creer_reseau, \
                                    TYPE_NEURONE_COMPLET, 2+2*FY+1*FZ, 490, 110, 1, 1
                                    ; Type                NumP         n/X  n/Y n/Z Pas
        ; 2 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            2+2*FY+3*FZ, 2+2*FY+1*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+2*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 3 --------------- réseau de segments dendritiques
                        call_datapush_style sub_creer_reseau, \
                        2+2*FY+4*FZ, TYPE_SEGMENT_COMPLET, 2+2*FY+3*FZ, 490, 210, 1, 1
                        ; Suivant(N) Type                  NumP         n/X  n/Y n/Z Pas
        ; 4 --------------- réseau de neurones
                                    call_datapush_style sub_creer_reseau, \
                                    TYPE_NEURONE_COMPLET, 2+2*FY+4*FZ, 490, 110, 1, 1
                                    ; Type                NumP         n/X  n/Y n/Z Pas
        ; 5 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            2+2*FY+9*FZ, 2+2*FY+4*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+5*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 6 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            3+2*FY+9*FZ, 2+2*FY+4*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+6*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 7 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            2+3*FY+9*FZ, 2+2*FY+4*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+7*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 8 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            3+3*FY+9*FZ, 2+2*FY+4*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+8*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 9 --------------- réseau de neurones
                                    call_datapush_style sub_creer_reseau, \
                                    TYPE_NEURONE_COMPLET, 2+2*FY+9*FZ, 490, 110, 1, 1
                                    ; Type                NumP         n/X  n/Y n/Z Pas
        ; 10 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            2+2*FY+19*FZ, 2+2*FY+9*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+10*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 11 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            3+2*FY+19*FZ, 2+2*FY+9*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+11*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 12 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            4+2*FY+19*FZ, 2+2*FY+9*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+12*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 13 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            2+3*FY+19*FZ, 2+2*FY+9*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+13*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 14 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            3+3*FY+19*FZ, 2+2*FY+9*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+14*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 15 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            4+3*FY+19*FZ, 2+2*FY+9*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+15*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 16 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            2+4*FY+19*FZ, 2+2*FY+9*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+16*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 17 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            3+4*FY+19*FZ, 2+2*FY+9*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+17*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 18 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            4+4*FY+19*FZ, 2+2*FY+9*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+18*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 19 --------------- réseau de neurones
                                    call_datapush_style sub_creer_reseau, \
                                    TYPE_NEURONE_COMPLET, 2+2*FY+19*FZ, 490, 110, 1, 1
                                    ; Type                NumP         n/X  n/Y n/Z Pas
        ; 20 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            2+2*FY+21*FZ, 2+2*FY+19*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+20*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 21 --------------- réseau de segments dendritiques
                        call_datapush_style sub_creer_reseau, \
                        2+2*FY+22*FZ, TYPE_SEGMENT_COMPLET, 2+2*FY+21*FZ, 490, 210, 1, 1
                        ; Suivant(N) Type                  NumP         n/X  n/Y n/Z Pas
        ; 22 --------------- réseau de segments dendritiques
                        call_datapush_style sub_creer_reseau, \
                        2+2*FY+23*FZ, TYPE_SEGMENT_COMPLET, 2+2*FY+22*FZ, 490, 210, 1, 1
                        ; Suivant(N) Type                  NumP         n/X  n/Y n/Z Pas
        ; 23 --------------- réseau de segments dendritiques
                        call_datapush_style sub_creer_reseau, \
                        2+2*FY+24*FZ, TYPE_SEGMENT_COMPLET, 2+2*FY+23*FZ, 490, 210, 1, 1
                        ; Suivant(N) Type                  NumP         n/X  n/Y n/Z Pas
        ; 24 --------------- réseau de neurones
                                    call_datapush_style sub_creer_reseau, \
                                    TYPE_NEURONE_COMPLET, 2+2*FY+24*FZ, 490, 110, 1, 1
                                    ; Type                NumP         n/X  n/Y n/Z Pas
        ; 25 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            2+2*FY+30*FZ, 2+2*FY+4*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+25*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 26 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            2+2*FY+30*FZ, 2+2*FY+9*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+26*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 27 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            2+2*FY+30*FZ, 2+2*FY+19*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+27*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 28 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            2+2*FY+30*FZ, 2+2*FY+19*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+28*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 29 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            2+2*FY+31*FZ, 2+2*FY+1*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+29*FZ, 490, 110, 1, 1
            ; Dest       Antécédent   Type                    NumP         n/X  n/Y n/Z Pas
        ; 30 --------------- réseau de neurones
                                    call_datapush_style sub_creer_reseau, \
                                    TYPE_NEURONE_COMPLET, 2+2*FY+30*FZ, 490, 110, 1, 1
                                    ; Type                NumP         n/X  n/Y n/Z Pas
        ; 31 --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            2+2*FY+0*FZ+1*LOCMAX_NB_P, 2+2*FY+1*FZ, TYPE_EXTENSION_COMPLET, 2+2*FY+31*FZ, 10, 1, 1, 1
            ; Dest                      Antécédent   Type                    NumP          n/X  n/Y n/Z Pas

    ; =============================================== MODELES DE CREATIONS A L'UNITE
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

    jmp fin_modeles
    ; =============================================== MODELES DE CREATIONS DE RESEAUX
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

    jmp fin_modeles
    ; =============================================== MODELES DE RESEAUX
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
; #-------------------------------------------------------------------------------------------- ANNONCES
        call_winapi64_style GetStdHandle, STD_OUTPUT_HANDLE
        mov r15, rax
        call_datapush_style ecrire_ligne_4, r15, interparagraphes_text, -1, -1 ;            interligne
        call_datapush_style ecrire_ligne_4, r15, rawData_Texte, -1, -1 ;                    texte de lecture
        call_datapush_style ecrire_ligne_4, r15, interligne_text, -1, -1 ;                  interligne
        call_datapush_style ecrire_ligne_4, r15, app_instance, qword [Instance], -1 ;       instance de l'application
        call_datapush_style ecrire_ligne_4, r15, windowClassName, qword [ClassAtom], -1 ;   atome de classe de fenetre
        call_datapush_style ecrire_ligne_4, r15, windowName, qword [Windowhandle], -1 ;     handle de la fenetre 
        call_datapush_style ecrire_ligne_4, r15, interligne_text, -1, -1 ;                  interligne
; #-------------------------------------------------------------------------------------------- PREPARATIFS RESEAU
    ; forçage de quelques utilisateurs
        ; ----------------------- utilisateur 0 (maitre)
        lea rax, [users]
        user0: equ  192 + \
                    168 *256+ \
                    173 *256*256+ \
                    124 *256*256*256
        port0: equ  80
        mov [rax+OU_IPAD_4], dword user0
        mov [rax+OU_PORT_2], word port0
        ; ----------------------- utilisateur 1
        add rax, TAILLE_DES_UTILISATEURS
        user1: equ  192 + \
                    168 *256+ \
                    1 *256*256+ \
                    1 *256*256*256
        port1: equ  80
        mov [rax+OU_IPAD_4], dword user1
        mov [rax+OU_PORT_2], word port1
        ; ----------------------- utilisateur dernier
        lea rax, [users]
        add rax, TAILLE_DES_UTILISATEURS*(65000) ; pour tests réalistes
        ; add rax, TAILLE_DES_UTILISATEURS*(PACKID_NB_P-1) ; pour tests au maximum de charge
        userX: equ  192 + \
                    168 *256+ \
                    1 *256*256+ \
                    3 *256*256*256
        portX: equ  80
        mov [rax+OU_IPAD_4], dword userX
        mov [rax+OU_PORT_2], word portX
    ; initialisation communications reseau
        ; ----- initialisation WinSock
        mov rcx, WSA_VERSION
        lea rdx, [wsaDataStruct]
        sub rsp, SHADOW_SPACE_SIZE
        call WSAStartup
        add rsp, SHADOW_SPACE_SIZE
        ; cmp rax, WSA_STARTUP_OK
        ; je no_erreur_wsaStartup
        ;     call erreur_wsaStartup
        ; no_erreur_wsaStartup
        ; ----- socket de controle
        mov rcx, AF_INET_IPV4 ; famille d'adresses (IPV4)
        mov rdx, SOCK_DGRAM ; type de socket (datagramme)
        mov r8, DEFAULT_PROTOCOL ; protocole (par défaut => UDP car datagramme)
        sub rsp, SHADOW_SPACE_SIZE
        call socket
        add rsp, SHADOW_SPACE_SIZE
        ; cmp rax, INVALID_SOCKET
        ; je erreur_creationSocket
        mov [mainSocketFileDescriptor], rax

    ; identification du dernier utilisateur enregistré r11
        lea rdx, [users] ; adresse en analyse
        xor r10, r10 ; dernier utilisateur
        mov rcx, 0 ; compteur
        boucle_dernier_utilisateur:
            mov rax, [rdx+OU_IPAD_4]
            test rax, rax
            cmovnz r10, rcx
            add rdx, TAILLE_DES_UTILISATEURS
            inc rcx
            cmp rcx, PACKID_NB_P
        jne boucle_dernier_utilisateur
        mov [nombredutilisateurs], r10
    ; erreurs non bloquantes
        mov byte [ErrorsBreak], 0
; #------------------------------------------------------------------------------------ BOUCLAGES DIVERS
    ; bouclage lent
    mov qword [numero_de_boucle_lente], 0
    bouclage_lent: ; boucle lente pour ecrire des infos et choisir de sortir
        ; initialisation boucle rapide
        xor rax, rax
        rdtsc ; -> edx:eax
        shl rdx, 32
        or rdx, rax ; procCycleCntr condensé
        mov [procCycleCntr_avant_bouclage_moyen], rdx ; stockage procCycleCntr pour durée boucles moyennes
        bouclage_moyen: ; boucle moyenne pour redessiner regulierement
            ; initialisation serie de bouclages rapides
            xor rax, rax
            rdtsc ; -> edx:eax
            shl rdx, 32
            or rdx, rax ; procCycleCntr condensé
            mov [procCycleCntr_avant_bouclage_rapide], rdx ; stockage procCycleCntr pour durée boucle rapide
            bouclage_rapide:
                ; initialisation calcul de la duree du bouclage sur portions
                xor rax, rax
                rdtsc ; -> edx:eax
                shl rdx, 32
                or rdx, rax ; procCycleCntr 64 bits
                mov [procCycleCntr_avant_bouclage_portions], rdx
; #---------------------------------------------------------------------------------------- BOUCLAGE CYCLE
                xor rbx, rbx ; valeur 0
                ; initialisation pile messages (premier message à 0)
                lea rdx, [msgStack]
                mov [rdx+ON_DEST_8], rbx
                ; initialisation messages à envoyer (premier message à 0)
                lea rdx, [msgToSnd]
                mov [rdx+OM_DEST_8], rbx
                ; initialisation nombre d'utilisateurs
                mov r10, [nombredutilisateurs]
                ; initialisation boucle portions
                mov r8, 0 ; premiere portion
                adresse_from_numero r9, r8 ; calcul adresse de la première portion
                mov r8, 0 ; initialisation numéro de la première portion
                lea r11, [msgStack]
                bouclage_portions:
                    ; registre constant sur toutes les boucles
                    ;   r10 = dernier utilisateur/packId enregistré
                    ; registres constant dans la boucle sur portions :
                    ;   r8 = numero de la portion
                    ;   r9 = pointeur/adresse de la portion
                    ;   r11 = pointeur de la pile de notes
                    ; registres internes à chaque type de portion traitée ou etape traitée
                    ;   rax, rbx, rcx, rdx = variables de travail
                    ;   r13 = adresse origine
                    ;   r14 = adresse source
                    ;   r15 = adresse destination
; #-------------------------------------------------------------------------------------------- TRAITEMENT PORTION (r8/r9)
                    ; aiguillage vers traitements concernés
                        ; A_FAIRE : réorganiser les tests selon la fréquence d'apparition du cas
                        ; Sauts pour Segments-Neurones-Extensions => sur base MASQUE_TYPE_TExx
                        mov al, [r9+OS_TYPEVSA_1] ; récuperation du type de portion
                        and al, MASQUE_TYPE_Txxx + MASQUE_TYPE_xExx
                        comparer_et_jump_si_egal al, TYPE_SEGMENT_COMPLET,                  portion_segment_complet
                        comparer_et_jump_si_egal al, TYPE_NEURONE_COMPLET,                  portion_neurone_complet
                        comparer_et_jump_si_egal al, TYPE_EXTENSION_COMPLET,                portion_extension_complet
                        comparer_et_jump_si_egal al, TYPE_SEGMENT_PARTIEL,                  portion_segment_partiel
                        comparer_et_jump_si_egal al, TYPE_NEURONE_PARTIEL,                  portion_neurone_partiel
                        comparer_et_jump_si_egal al, TYPE_EXTENSION_PARTIEL,                portion_extension_partiel
                        comparer_et_jump_si_egal al, TYPE_SEGMENT_GERME,                    portion_segment_germe
                        comparer_et_jump_si_egal al, TYPE_NEURONE_GERME,                    portion_neurone_germe
                        comparer_et_jump_si_egal al, TYPE_EXTENSION_GERME,                  portion_extension_germe
                        ; Sauts pour autres => sur base MASQUE_TYPE_TESA
                        mov al, [r9+OS_TYPEVSA_1] ; récuperation du type de portion
                        comparer_et_jump_si_egal al, TYPE_LECTEUR,                          portion_lecteur
                        comparer_et_jump_si_egal al, TYPE_PULSEUR,                          portion_pulseur
                        ; ne pas traiter les portions de type : 0 et TYPE_SOURCE et autres
                        jmp fin_traitement
                    ; portions speciales ---------------------------------------------------
                    portion_lecteur:
                        xor rax, rax
                        mov eax, [r9+OSL_PAMEM_4] ; portion des paramètres de l'accès mémoire
                        test eax, eax ; PORTION_ZERO ?
                        jz fin_traitement
                            adresse_from_numero r13, rax ; r13 = adresse des paramètres des données à lire
                            xor rax, rax
                            mov eax, dword [r9+OSL_DESTN_4] ; portion destination
                            test eax, eax ; PORTION_ZERO ?
                            jz fin_traitement
                                adresse_from_numero r15, rax ; r15 = adresse de la destination
                                mov r14, [r13+OSD_ADDRS_8] ; r14 = adresse de base des données
                                movzx rdx, byte [r13+OSD_LGSEG_1] ; largeur des segments en octets (maxi 8)
                                mov bx, word [r9+OSL_INDEX_2] ; bx = index de lecture
                                movzx eax, bx
                                ; calcul de l'adresse de base à lire
                                ; A_FAIRE ? ajouter une variable de position de tête relatif et boucler sur maxi nombre à lire
                                mul edx ; => edx:eax (?)
                                and rax, 0x00ffffff ; effacement de la partie haute de rax (car maxi possible = 65535*8)
                                add r14, rax ; -> adresse de base à lire
                                ; lecture du bit de donnee
                                mov rax, qword [r14] ; contenu du segment (plus le suite jusqu'a 8 octets)
                                mov cl, byte [r9+OSL_NMBIT_1] ; numero de bit a lire (0 à 63)
                                shr rax, cl ; décalage du nombre de bits nécessaire
                                shr rax, 1 ; sortie sur le carry flag
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
                                ; persistance
                                mov cl, [r9+OSL_PERSA_1] ; persistance restante actuelle
                                test cl, cl
                                jnz pl_persistance_encore_active
                                    mov cl, [r9+OSL_PERSB_1] ; persistance de base (remplace actuelle)
                                    inc bx ; augmenter l'index de lecture
                                    cmp bx, word [r13+OSD_NBSEG_2] ; comparer au nombre de segments à lire
                                    jne pl_fin_pas_atteinte
                                        xor bx, bx ; reprendre la lecture au début
                                    pl_fin_pas_atteinte:
                                    mov [r9+OSL_INDEX_2], bx ; enregistrer l'index de lecture
                                pl_persistance_encore_active:
                                dec cl
                                mov [r9+OSL_PERSA_1], cl ; persistance restante
                                jmp fin_traitement
                    portion_pulseur:
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
                            xor rax, rax
                            mov eax, [r9+OSP_DESTN_4] ; neurone de destination
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
                    ; portions segments (dendrites) ---------------------------------------------------
                    portion_segment_germe:
                        ; A_FAIRE : coder le passage en mode partiel éventuel
                        jmp fin_traitement
                    portion_segment_partiel:
                        ; A_FAIRE : coder la création de la dendrite antecedente (même type)
                        ; A_FAIRE : conditionner tout ça à la vitesse de développement (?)
                        ; A_FAIRE : coder le passage en mode complet
                    portion_segment_complet:
                        and [r9+OS_TYPEVSA_1], byte MASQUE_TYPE_xxxA_X ; enlever la rétro-activité le cas échéant
                        xor rax, rax
                        mov eax, [r9+OSS_SEGNS_4] ; numero de segment/neurone suivant
                        test eax, eax ; PORTION_ZERO ?
                        jz psc_isole ; aller au cas d'un segment qui a perdu son segment/neurone aval
                            adresse_from_numero rdx, rax ; adresse du suivant
                            mov al, [rdx+OS_TYPEVSA_1] ; type complet du suivant
                            and al, MASQUE_TYPE_xxxA ; bit de retro-activité du suivant
                            or byte [r9+OS_TYPEVSA_1], al ; l'appliquer au type actuel
                            ; glissement des charges vers bx
                            mov bx, [r9+OS_CHARG0_2] ; charge en cours
                            mov [r9+OS_CHARG0_2], word 0 ; charge en cours = 0
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
                            psc_appliquer_surcharge:
                            ; application surcharge
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
                        psc_isole:
                            ; A_FAIRE : cas d'un segment qui n'a pas de neurone à alimenter : détruire ?
                            jmp fin_traitement
                    ; portions neurone ---------------------------------------------------
                    portion_neurone_germe:
                        ; A_FAIRE : coder le passage en mode partiel éventuel
                        jmp fin_traitement
                    portion_neurone_partiel:
                        ; A_FAIRE : coder la création des dendrites initiales
                        ; A_FAIRE : coder la création de l'axone initiale
                        ; A_FAIRE : conditionner tout ça à la vitesse de développement (?)
                        ; A_FAIRE : coder le passage en mode complet
                    portion_neurone_complet:
                        and [r9+OS_TYPEVSA_1], byte MASQUE_TYPE_xxxA_X ; enlever la rétro-activité le cas échéant
                        mov al, [r9+OSN_CREFR_1] ; decompte réfractaire en cours
                        test al, al ; tester si al<>0
                        jnz pnc_refractaire ; aller au traitement du cas réfractaire
                            mov ax, [r9+OS_CHARG0_2] ; charge actuelle (word signé -32767 à 32767)
                            mov cl, [r9+OSN_SEUIL_1] ; puissance de seuil avec décalage (maxi 15)
                            mov edx, 1
                            shl edx, cl
                            dec edx ; seuil calculé (0 à 32767)
                            cmp ax, dx ; tester si le seuil n'est pas atteint
                            jl pnc_seuil_non_atteint ; aller au traitement du cas du seuil non atteint
                                or [r9+OS_TYPEVSA_1], byte MASQUE_TYPE_xxxA ; s'auto-activer / réctro-activer
                                mov al, [r9+OSN_BREFR_1] ; récupérer la base réfractaire
                                mov [r9+OSN_CREFR_1], al ; lancer la refractarité
                                jmp fin_traitement
                        pnc_refractaire:
                            ; al est forcément positif en arrivant
                            dec al
                            mov [r9+OSN_CREFR_1], al
                            cmp al, 0
                            jne fin_traitement
                                mov [r9+OS_CHARG0_2], word 0
                                jmp fin_traitement
                        pnc_seuil_non_atteint:
                            ; ax est la charge actuelle = word signé de -32767 à 32767
                            sub ax, VITESSE_DECHARGE
                            mov bx, 0
                            cmp ax, 0
                            cmovl ax, bx
                            jmp fin_traitement
                    ; portions extensions (axonales) ---------------------------------------------------
                    portion_extension_germe:
                        ; A_FAIRE : coder le passage en mode partiel éventuel
                        jmp fin_traitement
                    portion_extension_partiel:
                        ; A_FAIRE : coder la recherche de destination + réalisation du lien
                        ; A_FAIRE : coder la création de l'extension suivante
                        ; A_FAIRE : conditionner tout ça à la vitesse de développement (?)
                        ; A_FAIRE : coder le passage en mode complet
                    portion_extension_complet:
                        xor rax, rax
                        mov eax, [r9+OSE_ANTEC_4] ; portion antecédente
                        test eax, eax ; PORTION_ZERO ?
                        jz pec_autodestruction
                            mov rdx, 0 ; initialiser l'adresse de la destination à 0 pour cas de retroactvité seulement
                            and byte [r9+OS_TYPEVSA_1], MASQUE_TYPE_xxxA_X ; s'auto-desactiver (pour éventuel suivant)
                            adresse_from_numero rbx, rax ; calculer l'adresse de l'antécédent
                            mov al, [rbx+OS_TYPEVSA_1]
                            and al, MASQUE_TYPE_xxxA
                            test al, al
                            jz pec_retrocompteur ; cas le plus fréquent => avant test d'existance de destination
                                or byte [r9+OS_TYPEVSA_1], MASQUE_TYPE_xxxA ; s'auto-activer (pour éventuel suivant)
                                mov rax, [r9+OSE_DESTN_6] ; portion de destination (sale)
                                mov r15, PACKID_MASK
                                and r15, rax ; packID (propre)
                                cmp r15, [userPackNumber]
                                jne demande_de_charge_autre_pack
                                test eax, USERLA_MASK ; NEURONE_ZERO ?
                                jz fin_traitement ; pas de destination donc rien à faire
                                    mov rbx, USERLA_MASK
                                    and rax, rbx
                                    adresse_from_numero rdx, rax ; adresse de la destination
                                    mov [r9+OSE_CACTT_1], byte 1 ; initier le compteur
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
                        pec_autodestruction:
                            ; A_FAIRE : auto-destruction
                            jmp fin_traitement
                        pec_retrocompteur: ; evolution ou pas du compteur d'activité
                            mov cl, [r9+OSE_CACTT_1]
                            test cl, cl
                            jz pec_retroaction
                                inc cl
                        pec_retroaction: ; retroaction aval sur bloc de boutons
                            test rdx, rdx ; la destination n'a pas été définie / pas de destination ?
                            jz fin_traitement
                            test byte [rdx+OS_TYPEVSA_1], MASQUE_TYPE_xxxA ; destination non rétro-active ?
                            jz fin_traitement
                            ; A_FAIRE : variation si test aléatoire au-dessus de la masse
                            ; jxx pec_desactiver_cpt
                                mov bl, [r9+OSE_BSYNV_1] ; valeur du bloc de boutons (byte signé)
                                mov cx, bx ; mise en réserve de la valeur du bloc de boutons
                                mov ax, [r9+OSE_CACTT_1] ; compteur d'activité actuel
                                test byte [rdx+OS_NACTT_1], al ; différence avec niveau d'activité de la destination
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
                            mov [r9+OSE_CACTT_1], byte 0 ; desactiver le compteur
                            jmp fin_traitement
; #-------------------------------------------------------------------------------------------- MESSAGE A EMETTRE
                    ; création des messages
                    demande_de_charge_autre_pack:
                        ; r9 = adresse portion demandeuse (inutile ici)
                        ; rax = portion destinataire complète (sale)
                        ; r15 = numero de pack destinataire complet (propre)
                        mov rdx, MC_SURCHARGE_QW
                        mov dl, [r9+OSE_BSYNV_1] ; valeur du bloc de boutons (byte signé)
                        jmp ajouter_note
                    demande_autre:
                        jmp ajouter_note
                    ; enregistrement de la note
                    ajouter_note:
                        ; r11 = adresse de la note à écrire
                        ; r9 = adresse portion demandeuse (A_FAIRE : à mettre dans la demande)
                        ; rax = portion destinataire complète (sale)
                        ; r15 = numero de pack destinataire complet (propre)
                        ; rdx = demande complète sur qword (propre)
                        mov rbx, FULLGA_MASK
                        and rax, rbx ; nettoyage destinataire
                        shr r15, LOCMAX_BITS ; numero de pack destinataire net -> r15d
                        adresse_from_user rcx, r15 ; adresse du destinataire
                        ; insertion note
                        mov [r11+ON_DEST_8], rax ; portion destinataire complète (pour utilisation chez destinataire)
                        mov [r11+ON_DEMD_8], rdx ; code de la demande (1) + paramètres (7)
                        mov [r11+ON_ADST_8], rcx ; adresse mémoire destinataire
                        ; incrementation compteur de l'utilisateur
                        add dword [rcx+OU_NM_4_4], 1 ; compte nombre de messages pour cet utilisateur (partie 1)
                        ; calage pour suivante
                        add r11, TAILLE_DES_NOTES
; #---------------------------------------------------------------------------------------- BOUCLAGE CYCLE + POST-TRAITEMENTS
                    fin_traitement:
                    add r9, TAILLE_DES_PORTIONS
                    inc r8
                    cmp r8, USERLA_NB_P
                jne bouclage_portions
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
; #---------------------------------------------------------------------------------------- PREPARATION / EXPEDITION MESSAGES
                ; calcul des zones de messages dans msgToSend
                    mov rbx, 0; nombre total de messages
                    lea r12, [msgToSnd] ; pointeur adresse paquet de messages à envoyer
                    lea r13, [users] ; pointeur utilisateur
                    mov r11, r10 ; dernier numero d'utilisateur
                    inc r11 ; nombre d'utilisateurs
                    boucle_sur_utilisateurs:
                        xor rcx, rcx
                        mov ecx, [r13+OU_NM_4_4] ; nombre de notes pour l'utilisateur
                        add rbx, rcx
                        mov [r13+OU_ADRM_8], r12 ; pointeur debut msgToSend avant déplacement sur suivant
                        mov [r13+OU_ADRT_8], r12 ; pointeur msgToSend avant déplacement sur suivant
                        mov rax, rcx
                        longueur_from_quantity rdx, rax
                        mov [r13+OU_LNGM_8], rdx ; enregistrement longueur en octets
                        add r12, rdx ; décalage sur emplacement message suivant
                        shl rcx, 32
                        mov [r13+OU_NM_4_4], rcx ; effacement du nombre et réécriture 4 octets plus loin
                        add r13, TAILLE_DES_UTILISATEURS ; adresse utilisateur suivante
                        sub r11, 1 ; compteur utilisateurs
                    jnz boucle_sur_utilisateurs
                    ; enregistrement du nombre de messages cumulé
                    mov [nombreDeMessageAEmettre], rbx
                ; transfert des notes vers msgToSend
                    cmp rbx, 0
                    je fin_expedition_messages
                    lea r11, [msgStack] ; adresse première note à copier
                    boucle_sur_messages:
                        ; récupération des données du message
                        mov rdx, [r11+ON_DEST_8] ; destinataire complet
                        mov rcx, [r11+ON_DEMD_8] ; demande complète
                        ; récupération de l'adresse d'enregistrement du message
                        mov r12, [r11+ON_ADST_8] ; adresse destinataire/user
                        mov r13, [r12+OU_ADRT_8] ; adresse message à envoyer en cours
                        ; enregistrement du message
                        mov [r13+OM_DEMS_8], rcx ; demande complète
                        mov [r13+OM_DEST_8], rdx ; destinataire complet
                        ; mov [r13+OM_ADST_8], r12 ; adresse destinataire/user
                        ; suivant dans users
                        add qword [r12+OU_ADRT_8], TAILLE_DES_MESSAGES
                        ; bouclage
                        add r11, TAILLE_DES_NOTES
                        sub rbx, 1
                    jnz boucle_sur_messages


                jmp fin_expedition_messages


                ; déclencher l'envoi socket de synchronisation vers tous les utilisateurs ?
                    ; cmp
                    

                ; USERS : attendre un message de declenchement de l'écoute
                ; MAITRE : envoyer un message de declenchement de l'écoute à tous les utilisateurs
                ; M+U : declencher son propre thread d'écoute
                ; USERS : attendre un message de declenchement des envois
                ; MAITRE : attendre tous les retours ?
                ; MAITRE : envoyer un message de declenchement des envois à tous les utilisateurs (arrêts automatiques)
                ; M+U : declencher son propre thread d'envoi (arrêt automatique)
                    ; sauvegarde des variables
                        mov [r10_backup], r10
                        mov byte [etatSendThread], 0



                ; envoyer les donnees
                    ; il faut peut-être tester que le socket est ok
                    ; socket destinataire
                    mov word [toDestSocket], AF_INET_IPV4
                    mov dword [toDestSocket + 4], DEST_IP_ADDRESS
                    mov word [toDestSocket + 2], DEST_IP_PORT
                    ; envoi
                    mov rcx, [mainSocketFileDescriptor]
                    lea rdx, [messageDeTest]
                    mov r8, MESSAGE_DE_TEST_LEN
                    mov r9, 0 ; flags
                    push SOCKET_ADDRESS_LEN
                    lea rax, [toDestSocket]
                    push rax
                    sub rsp, 32
                    call sendto                    ; Appeler sendto pour envoyer le paquet
                    add rsp, 32
                    ; cmp rax, SOCKET_ERROR
                    ; je erreur_sendTo


                    ; creation thread d'envoi
                        mov rcx, 0 ;                [in, optional] lpThreadAttributes
                        mov rdx, 0 ;                [in] dwStackSize
                        lea r8, [sending_thread] ;  [in] lpStartAddress
                        mov r9, 0 ;                 [in, optional] lpParameter
                        push 0 ;                    [out, optional] lpThreadId
                        push 0 ;                    [in] dwCreationFlags
                        sub rsp, 32
                        ; call CreateThread
                        add rsp, 32
                    ; attente pour continuer
                        boucle_attente:
                            mov al, [etatSendThread]
                            test al, al
                        ; jz boucle_attente


                    ; récupération des variables
                        mov r10, [r10_backup]


                    ; envoi des paquets de messages (adresse / longueur)
                    ; protocole d'identification / enregistrement du destinataire
                    ; expédition de chaque paquet


                    ; attendre le retour de fin d'envoi ? et de fin de réception ?
                    ; attendre le retour des fins de réception ?


                fin_expedition_messages:
; #---------------------------------------------------------------------------------------- RECEPTION / TRAITEMENT MESSAGES
                    ; réceptionner les messages
                    ; en faire un seul paquet

                    ; pour chaque message, le traiter et créer eventuellement le message de réponse
                    reponse_a_demande_de_charge:






                    mov qword [nombredutilisateurs], r10
; #------------------------------------------------------------------------------------ BOUCLAGES DIVERS
                ; forcage sortie si mode pas à pas
                    mov bl, [mode_pas_a_pas]
                    test bl, bl
                    jnz boucle_moyenne_suite
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
                ; conditionnement de sortie bouclage rapide
                mov rax, [procCycleCntr_avant_bouclage_rapide] ; récupération début de boucle
                mov rcx, [procCycleCntr_apres_bouclage_portions] ; recupération fin de cycle
                sub rcx, rax ; durée de la boucle rapide
                cmp rcx, NBCYCLES_BOUCLE_MOYENNE
            jb bouclage_rapide
            boucle_moyenne_suite:
            ; masque + demande de retraçage
                call retracer_la_fenetre
            ; ecritures si pas à pas
                mov bl, [mode_pas_a_pas]
                test bl, bl
                jz pas_d_ecriture_apres_boucle_rapide
                    call ecrire_messages_console
                pas_d_ecriture_apres_boucle_rapide:
            ; ----- liberation / traitement des messages fenetre en attente
            messagesLoop:
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
                    test rax, rax
                    jnz continue_MLoop_1 ; on continue la boucle s'il y a un message à traiter
                        test bl, bl
                        jz traitementMessagesFini ; on sort si pas mode pas-à-pas (et pas de message à traiter)
                    continue_MLoop_1:
                    mov bl, [mode_pas_a_pas]
                    cmp bl, 2
                    jne continue_MLoop_2 ; on continue si pas de demande d'avance d'un pas en pas-à-pas (mode_pas_a_pas <> 2)
                        mov bl, 1
                        mov [mode_pas_a_pas], bl
                        jmp traitementMessagesFini ; on sort si demande d'avance d'un pas (donc mode pas-à-pas)
                    continue_MLoop_2:
                ; demande de retraçage
                    test rax, rax
                    jz continue_MLoop_3 ; on ne fait rien si pas de message (donc mode pas-à-pas)
                        test bl, bl
                        jz continue_MLoop_3 ; on retrace si mode pas à pas + message
                            call retracer_la_fenetre
                            call ecrire_messages_console
                    continue_MLoop_3:
                ; recuperation du message
                    lea   rcx, [WindowMessage]              ; lpMsg
                    xor   edx, edx                          ; hWnd
                    xor   r8d, r8d                          ; wMsgFilterMin
                    xor   r9d, r9d                          ; wMsgFilterMax
                    sub   rsp, SHADOW_SPACE_SIZE
                    call  GetMessageA
                    add   rsp, SHADOW_SPACE_SIZE
                ; sortie si c'est une demande de fermeture WM_QUIT
                    cmp   rax, 0
                    je    sortieComplete                    ; on arrete tout = sortie toutes boucles
                ; analyse du message
                    mov   rcx, qword [Windowhandle]         ; hDlg
                    lea   rdx, [WindowMessage]                ; lpMsg
                    sub   rsp, SHADOW_SPACE_SIZE
                    call  IsDialogMessageA                  ; For keyboard strokes (?)
                    add   rsp, SHADOW_SPACE_SIZE
                ; bouclage si autre chose d'une touche clavier
                    cmp   rax, 0
                    jne   messagesLoop                 ; le message a ete traite par IsDialogMessageA => on reboucle
                ; traduction du message
                    lea   rcx, [WindowMessage]                ; lpMsg
                    sub   rsp, SHADOW_SPACE_SIZE
                    call  TranslateMessage
                    add   rsp, SHADOW_SPACE_SIZE
                ; diffusion du message
                    lea   rcx, [WindowMessage]            ; lpMsg
                    sub   rsp, SHADOW_SPACE_SIZE
                    call  DispatchMessageA
                    add   rsp, SHADOW_SPACE_SIZE
                ; c'est ici qu'il faudrait retracer la fenêtre ?
                
                ; bouclage messages
            jmp messagesLoop
            ; sortie de la boucle des messages
            traitementMessagesFini:
            xor rax, rax
            rdtsc ; -> edx:eax
            shl rdx, 32
            or rdx, rax ; procCycleCntr condensé
            ; conditionnement de sortie bouclage moyen
                mov rax, [procCycleCntr_avant_bouclage_moyen] ; récupération début de boucle
                sub rdx, rax ; durée de la boucle moyenne
                mov rax, NBCYCLES_BOUCLE_LENTE
                cmp rdx, rax
            ; bouclage moyen
        jb bouclage_moyen
        apres_boucle_moyenne:
        mov rax, qword [numero_de_boucle_lente]
        add rax, 1
        mov qword [numero_de_boucle_lente], rax
        ; écritures si pas mode pas-à-pas
            mov bl, [mode_pas_a_pas]
            test bl, bl
            jnz pas_d_ecriture_dans_boucle_lente
                call ecrire_messages_console
            pas_d_ecriture_dans_boucle_lente:
        ; bouclage lent
    jmp bouclage_lent
; #------------------------------------------------------------------------------------ SORTIE
    sortieComplete:
    ; cloture reseau
        ; ----- fermer la socket de contrôle
        mov rcx, [mainSocketFileDescriptor]
        sub rsp, SHADOW_SPACE_SIZE
        call closesocket
        add rsp, SHADOW_SPACE_SIZE
        ; cmp rax, CLOSE_SOCKET_OK
        ; jne erreur_closeSocket
        ; ----- nettoyer WinSock
        sub rsp, SHADOW_SPACE_SIZE
        call WSACleanup
        add rsp, SHADOW_SPACE_SIZE
    ; finalisation ----------------------------------------------
        call_winapi64_style GetStdHandle, STD_OUTPUT_HANDLE
        mov r15, rax
        call_datapush_style ecrire_ligne_4, r15, interligne_text, r15, -1
    ; fin du programme
        ; add rsp, 8
        xor rcx, rcx
        call ExitProcess
; #------------------------------------------------------------------------------------ TRAITEMENT DES ERREUR
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
; #---------------------------------------------------------------------------------------- OUTILS AFFICHAGES
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
        ; -------------------- Aiguillage selon affichage demandé
        mov al, [affichageConsole]
        comparer_et_jump_si_egal al, 1, aff_aide1
        comparer_et_jump_si_egal al, 2, aff_aide2
        comparer_et_jump_si_egal al, 3, aff_evolut
        comparer_et_jump_si_egal al, 4, aff_detail
        jmp emc_fin
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
        ; -------------------- Affichage évolutions et chronos
        aff_evolut:
            add rdx, 0x10000
            call_datapush_style ecrire_ligne_4, r15, texte_numero_de_boucle, qword [numero_de_boucle_lente], rdx ; numéro de boucle
            add rdx, 0x10000
            call_datapush_style ecrire_ligne_4, r15, texte_duree_de_boucle, qword [dureeDeTraitement], rdx ; durée de boucle
            add rdx, 0x10000
            call_datapush_style ecrire_ligne_4, r15, texte_duree_de_posttrait, qword [dureeDePostTraitement], rdx ; durée de post traitement
            add rdx, 0x10000
            call_datapush_style ecrire_ligne_4, r15, texte_nombre_d_utilisateurs, qword [nombredutilisateurs], rdx ; nombre d'utilisateurs
            add rdx, 0x10000
            call_datapush_style ecrire_ligne_4, r15, texte_nombre_de_messages_a_emettre, qword [nombreDeMessageAEmettre], rdx ; nombre de message
            add rdx, 0x10000
            call_datapush_style ecrire_ligne_4, r15, texte_valeur_de_test, qword [valeurdetest], rdx
            jmp emc_fin
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
            movzx rcx, byte [rbx+OS_TYPEVSA_1] ; type de portion
            call_datapush_style ecrire_ligne_4, r15, portion_type_text, rcx, rdx
            ; ----- distribution
            ; r15 = sortie standard
            ; rbx = adresse de la portion
            xor rcx, rcx
            mov cl, [rbx+OS_TYPEVSA_1]
            mov al, cl
            mov r8b, cl
            and cl, MASQUE_TYPE_Txxx + MASQUE_TYPE_xExx
            and al, MASQUE_TYPE_Txxx
            ; A_FAIRE : appliquer un masque et de faire les tests uniquement sur les 5 premiers bits
            comparer_et_jump_si_egal al, TYPE_SEGMENT_GERME,                    dp_psd
            comparer_et_jump_si_egal al, TYPE_NEURONE_GERME,                    dp_pn
            comparer_et_jump_si_egal al, TYPE_EXTENSION_GERME,                  dp_pea
            comparer_et_jump_si_egal r8b, TYPE_SOURCE,                          dp_sdm
            comparer_et_jump_si_egal r8b, TYPE_LECTEUR,                         dp_plc
            comparer_et_jump_si_egal cl, TYPE_PULSEUR,                          dp_ppl
            ; ----- traitement par defaut
            inc rbx ; on n'écrit pas le premier octet (type)
            mov rcx, 1
            dp_boucle:
                xor rax, rax
                mov al, [rbx]
                call_winapi64_style convertir, rax, nombre_text, LONGUEUR_NOMBRES
                call_winapi64_style WriteConsoleA, r15, nombre_text, LONGUEUR_NOMBRES, reponse_long_ret
                inc rbx
                inc rcx
                cmp rcx, TAILLE_DES_PORTIONS
            jbe dp_boucle
            jmp dp_fin
            ; -----------------------------------------------------------------------------------------------
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
                mov ecx, dword [rbx+OSL_DESTN_4] ; portion de destination
                call_datapush_style ecrire_ligne_4, r15, portion_dest_text, rcx, rdx
                add rdx, 0x10000
                mov ecx, dword [rbx+OSL_PAMEM_4] ; portion d'accès mémoire
                call_datapush_style ecrire_ligne_4, r15, portion_pamm_text, rcx, rdx
                jmp dp_fin
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
                mov ecx, dword [rbx+OSP_DESTN_4] ; numero de destination
                call_datapush_style ecrire_ligne_4, r15, portion_dest_text, rcx, rdx
                jmp dp_fin
            dp_psd: ; portion segment dendritique
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
                movzx rcx, byte [rbx+OS_NACTT_1] ; niveau temporel d'activité
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
                mov ecx, dword [rbx+OSS_SEGNS_4] ; numero segment/neurone suivant
                call_datapush_style ecrire_ligne_4, r15, portion_segs_text, rcx, rdx
                jmp dp_fin
            dp_pn: ; portion neurone
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
            dp_pea: ; portion extension axonale
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
                mov rcx, [rbx+OSE_DESTN_6] ; numero de destination ; A_FAIRE : décomposer sur x/y/z/pack
                mov rax, 0x0000FFFFFFFFFFFF
                and rcx, rax
                call_datapush_style ecrire_ligne_4, r15, portion_dest_text, rcx, rdx
                add rdx, 0x10000
                movzx rcx, byte [rbx+OSE_PREXT_1] ; potentiel d'extensions restantes
                call_datapush_style ecrire_ligne_4, r15, portion_pext_text, rcx, rdx
                add rdx, 0x10000
                mov ecx, dword [rbx+OSE_ANTEC_4] ; portion neurone ou axonale antecedente
                call_datapush_style ecrire_ligne_4, r15, portion_eaxo_text, rcx, rdx
                ; jmp dp_fin
            ; -----------------------------------------------------------------------------------------------
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
; #---------------------------------------------------------------------------------------- OUTILS POUR CREATIONS INITIALES
    ; Appels : push param1, param2, param3, ... + call + add rsp, nbParams*8
    ; Outils
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
    ; Creations unitaires
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
        mov [rdx+OS_TYPEVSA_1], byte TYPE_SOURCE
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
        mov [rdx+OS_TYPEVSA_1], byte TYPE_LECTEUR ; type de portion
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
        mov [rdx+OSL_PAMEM_4], eax ; numero de portion des parametres d'accès mémoire
        ;
        mov rax, [rsp+8*(3+1)]
        mov [rdx+OSL_DESTN_4], eax ; portion de destination
        ;
        ; ----- recuperation des registres
        add rsp, 8
        pop rax
        pop rdx
        ; ----- sortie
        ret
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
        mov [rdx+OS_TYPEVSA_1], byte TYPE_PULSEUR
        ;
        mov rax, [rsp+8*(3+3)]
        mov [rdx+OSP_SEUIL_1], al ; puissance seuil chrono
        ;
        mov rax, [rsp+8*(3+2)]
        mov [rdx+OSP_BSYNV_1], al ; valeur du bloc de boutons
        ;
        mov rax, [rsp+8*(3+1)]
        mov [rdx+OSP_DESTN_4], dword eax ; numero de portion de destination
        ;
        ; ----- recuperation des registres
        add rsp, 8 ; re-alignement de la pile
        pop rax
        pop rdx
        ; ----- sortie
        ret
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
        mov [rdx+OS_TYPEVSA_1], al ; type
        ;
        mov rax, [rsp+8*(3+4)]
        mov [rdx+OS_NACTT_1], al ; niveau temporel d'activité
        ;
        mov rax, [rsp+8*(3+3)]
        mov [rdx+OSS_PRSEG_1], al ; potentiel de segments restants
        ;
        mov rax, [rsp+8*(3+2)]
        mov [rdx+OSS_PSYNA_2], ax ; nombre de synapses disponibles
        ;
        mov rax, [rsp+8*(3+1)]
        mov [rdx+OSS_SEGNS_4], eax ; segment ou neurone suivant
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
        mov [rdx+OS_TYPEVSA_1], al ; type
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
        mov [rdx+OS_TYPEVSA_1], al ; type
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
        mov rax, FULLGA_MASK
        and [rsp+8*(3+2)], rax
        mov rax, [rsp+8*(3+2)]
        mov [rdx+OSE_DESTN_6], rax ; destination
        ;
        mov rax, [rsp+8*(3+1)]
        mov [rdx+OSE_ANTEC_4], eax ; neurone ou axonale antécédente
        ;
        ; ----- recuperation des registres
        add rsp, 8
        pop rax
        pop rdx
        ; ----- sortie
        ret

    ; Creation de reseaux
    sub_pile_6_sur_z:

        ret
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
                    ; rdx = 8 = adresse destination
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
; #---------------------------------------------------------------------------------------- THREADS RESEAU
    ; Thread d'envoi
    sending_thread:
        ; création d'une socket
            mov rcx, AF_INET_IPV4
            mov rdx, SOCK_DGRAM ; (TCP)
            xor r8, r8 ; Protocole par défaut (IP)
            sub rsp, SHADOW_SPACE_SIZE
            call socket
            add rsp, SHADOW_SPACE_SIZE
            ; cmp rax, INVALID_SOCKET
            ; je erreur_creationSocket
            mov [socketFileDescriptor], rax
        ; envoi des messages
            mov r11, [r10_backup]
            lea r13, [users]
            boucle_sur_utilisateurs_pour_envoi:
                mov r8, [r13+OU_LNGM_8] ; longueur du paquet en octets
                cmp r8, 0
                je continuer_boucle_sur_utilisateurs
                    ; sauvegarde variables
                    mov [r11_backup], r11
                    mov [r13_backup], r13
                    ; contenu toDestSocket
                    mov eax, [r13+OU_IPAD_4] ; adresse ip destinataire
                    mov [toDestSocket + 4], eax
                    mov ax, [r13+OU_PORT_2] ; port ip destinataire
                    mov [toDestSocket + 2], ax
                    ; envoyer les donnees
                    mov rcx, [socketFileDescriptor] ; id socket
                    mov rdx, [r13+OU_ADRM_8] ; pointeur de paquet de messages
                    ; r8 déjà calculé = longueur du paquet en octets
                    xor r9, r9 ; flags
                    push SOCKET_ADDRESS_LEN
                    lea rax, [toDestSocket]
                    push rax
                    sub rsp, SHADOW_SPACE_SIZE
                    call sendto
                    add rsp, SHADOW_SPACE_SIZE
                    add rsp, 2*8
                    ; cmp rax, SOCKET_ERROR
                    ; je erreur_sendTo
                    ; récupération variables
                    mov r13, [r13_backup]
                    mov r11, [r11_backup]
                continuer_boucle_sur_utilisateurs:
                add r13, TAILLE_DES_UTILISATEURS ; adresse utilisateur suivante
                sub r11, 1 ; compteur utilisateurs
            jnz boucle_sur_utilisateurs_pour_envoi
        ; fermer la socket
            mov rcx, [socketFileDescriptor]
            sub rsp, SHADOW_SPACE_SIZE
            call closesocket
            add rsp, SHADOW_SPACE_SIZE
            ; cmp rax, CLOSE_SOCKET_OK
            ; jne erreur_closeSocket
        ; validation envoi fini au thread principal
            mov byte [etatSendThread], 1
        ; sortie
            mov rcx, 1 ; [in] dwExitCode
            call ExitThread

    ; Thread de réception
    receiving_thread:

        ; sortie
            mov rcx, 2 ; [in] dwExitCode
            call ExitThread

    ; réception de données
                            ; 3. Créer un socket TCP
                                mov rcx, 2                   ; AF_INET (IPv4)
                                mov rdx, 1                   ; SOCK_STREAM (TCP)
                                xor r8, r8                   ; Protocole IP
                                call socket                  ; Appeler la fonction 'socket'
                                mov rdi, rax                 ; Stocker le descripteur du socket
                            ; 4. Associer le socket à une adresse (bind)
                                mov rcx, rdi                 ; Charger le socket dans RCX
                                lea rdx, [sockaddr_in]        ; Charger l'adresse de la structure sockaddr_in dans RDX
                                mov r8d, 16                  ; Taille de la structure sockaddr_in
                                call bind                    ; Appeler bind pour associer l'adresse au socket
                            ; 5. Écouter les connexions
                                mov rcx, rdi                 ; Charger le socket dans RCX
                                mov edx, 1                   ; Longueur de la file d'attente (1 connexion en attente)
                                call listen                  ; Appeler listen pour écouter les connexions
                            ; 6. Accepter une connexion
                                xor rdx, rdx                 ; Pas d'adresse de client à remplir
                                xor r8, r8                   ; Taille de l'adresse du client ignorée
                                mov rcx, rdi                 ; Charger le socket dans RCX
                                call accept                  ; Appeler accept pour accepter une connexion
                                mov rsi, rax                 ; Stocker le descripteur de la nouvelle connexion
                            ; 7. Recevoir des données
                                lea rdx, [buffer]            ; Charger l'adresse du buffer dans RDX
                                mov rcx, rsi                 ; Socket de connexion client
                                mov r8d, buffer_len          ; Taille du buffer
                                xor r9, r9                   ; Flags (aucun)
                                call recv                    ; Recevoir les données du client
    ; Pour mémoire, chargement dynamique
        ; extern LoadLibraryA

        ; ws2_32_dll: db 'Ws2_32.dll', 0 
        ; lea rcx, [ws2_32_dll]
        ; call LoadLibraryA
        ; mov r8, rax                  ; adresse de ws2_32_dll dans r8

        ; WSACleanup: db 'WSACleanup', 0
        ; lea rcx, [WSACleanup]        ; Nom de la fonction WSACleanup
        ; mov rdx, r8
        ; call GetProcAddress          ; Obtenir l'adresse de WSACleanup
        ; mov r9, rax                  ; Sauvegarder l'adresse de WSACleanup dans r9

        ; call r9                      ; Appel WSACleanup()

; #---------------------------------------------------------------------------------------- PROCEDURE DE FENETRE
    global WindowProc
    WindowProc:
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
            ; -----------------------------------------------------------------------------------------
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
                        mov al, [rdx+OS_TYPEVSA_1]
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
        cas_WM_SIZE: ; ---------------------------------------------------
            ; rax et sortie
                xor rax, rax
                jmp fin_WindowProc
        cas_WM_PAINT:  ; -------------------------------------------------
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
                ; traçage du contenu
                mov r9, 0 ; compteur de ligne
                cWP_boucle_lignes:
                    mov r8, 0 ; compteur de colonne
                    cWP_boucle_colonnes:
                        xor rdx, rdx
                        mov cl, byte [modeTracage]
                        comparer_et_jump_si_egal cl, 0, cWP_tracage_mode_A0
                        comparer_et_jump_si_egal cl, 1, cWP_tracage_mode_Z1
                        comparer_et_jump_si_egal cl, 2, cWP_tracage_mode_E2
                        comparer_et_jump_si_egal cl, 3, cWP_tracage_mode_R3
                        jmp cWP_boucle_tracage_fin
                        cWP_tracage_mode_A0: ; mode A0 / type de portion ------------------------------
                            ; type
                            mov dl, [r10+OS_TYPEVSA_1]
                            and dl, MASQUE_TYPE_Txxx
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
                        cWP_tracage_mode_Z1: ; mode Z1 / bloc de boutons synaptiques ------------------------------
                            ; affichage valeur bloc de boutons => vert
                            mov dl, byte [r10+OSE_BSYNV_1]
                            mov [r11+VERT], dl
                            jmp cWP_boucle_tracage_fin
                        cWP_tracage_mode_E2: ; mode E2 / charge actuelle ------------------------------
                            ; type
                            mov dl, [r10+OS_TYPEVSA_1]
                            and dl, MASQUE_TYPE_Txxx
                            comparer_et_jump_si_egal dl, TYPE_PULSEUR, cWP_bte2_pulseur
                            comparer_et_jump_si_egal dl, TYPE_SEGMENT_GERME, cWP_bte2_seg_neu
                            comparer_et_jump_si_egal dl, TYPE_NEURONE_GERME, cWP_bte2_seg_neu
                            jmp cWP_boucle_tracage_fin
                            ; ---------------- affichage charge/chrono => vert ou rouge
                            cWP_bte2_pulseur:
                                mov dx, word [r10+OSP_CHARGI_2]
                                jmp cWP_bte2_traiter
                            cWP_bte2_seg_neu:
                                mov dx, word [r10+OS_CHARG0_2]
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
                        cWP_tracage_mode_R3: ; mode R3 / activité + retroactivité ------------------------------
                            ; type
                            mov dl, [r10+OS_TYPEVSA_1]
                            and dl, MASQUE_TYPE_Txxx
                            comparer_et_jump_si_egal dl, TYPE_SEGMENT_GERME, cWP_btr3_traiter
                            comparer_et_jump_si_egal dl, TYPE_NEURONE_GERME, cWP_btr3_traiter
                            comparer_et_jump_si_egal dl, TYPE_EXTENSION_GERME, cWP_btr3_traiter
                            jmp cWP_boucle_tracage_fin
                            ; affichage cible retroactive (forcement simple ou liaison) => bleu
                            cWP_btr3_traiter:
                            mov dl, [r10+OS_TYPEVSA_1]
                            and dl, MASQUE_TYPE_xxxA
                            comparer_et_jump_si_egal dl, MASQUE_TYPE_xxxA, cWP_btr3_traiter
                            jmp cWP_boucle_tracage_fin
                            cWP_btr3_suite:
                            mov [r11+BLEU], byte 255
                            ; jmp cWP_boucle_tracage_fin
                        ; fin des options ----------------------
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
                    jne cWP_boucle_colonnes
                    ; fin de boucle de colonnes
                    add r11, COMPLEMENT_LIGNE_DWORD ; adresse point ligne suivante
                    inc r9 ; ligne suivante
                    cmp r9, HAUTEUR_FENETRE
                    cmovne r14, r11 ; pour calcul decalage d'adresse par ligne (pour viseur)
                jne cWP_boucle_lignes
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
            ; recuperation de l'ancien objet bitmap 2
                mov rcx, qword [DrawingCtxHandle2]
                mov rdx, qword [OldBitmapHandle2]
                sub rsp, SHADOW_SPACE_SIZE
                call SelectObject
                add rsp, SHADOW_SPACE_SIZE
            ; destruction de l'objet bitmap 2
                mov rcx, qword [pBitDataHandle]
                sub rsp, SHADOW_SPACE_SIZE
                call DeleteObject
                add rsp, SHADOW_SPACE_SIZE
            ; destruction du DIB ?
                mov rcx, qword [DrawingCtxHandle2]
                sub rsp, SHADOW_SPACE_SIZE
                call DeleteDC
                add rsp, SHADOW_SPACE_SIZE
            cWP_no_dibs: ; cloture du tracage
                mov rcx, qword [rbp+16]                 ; hWnd
                lea rdx, [PaintStruct]                    ; lpPaint
                sub rsp, SHADOW_SPACE_SIZE
                call EndPaint
                add rsp, SHADOW_SPACE_SIZE
            ; rax et sortie
                xor rax, rax
                jmp fin_WindowProc
        cas_WM_DESTROY: ; ------------------------------------------------
            ; envoi du message de fin
                xor rcx, rcx
                sub rsp, SHADOW_SPACE_SIZE
                call PostQuitMessage
                add rsp, SHADOW_SPACE_SIZE
            ; rax et sortie
                xor rax, rax
                jmp fin_WindowProc
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
        fin_WindowProc: ; ------------------------------------------------
            mov rsp, rbp        ; Restores the stack pointer
            pop rbp             ; Restores the base pointer
            ret
