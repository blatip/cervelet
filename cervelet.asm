; ------------------------------------------------------------------------------------------------- GPL LICENCE
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
; ------------------------------------------------------------------------------------------------- INITIALISATIONS
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
    ; extern traitement messages window
        extern PeekMessageA
        extern GetMessageA
        extern IsDialogMessageA
        extern TranslateMessage
        extern DispatchMessageA
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
        extern ReadFile
        extern CloseHandle
; ----------- entrees : FVTM / sorties : S / imperatifs : (ABCD 89012345 ou L/N) ------------------ MACROS
    ; outils divers
        %macro adresse_from_numero 2 ; calcul de l'adresse d'une portion (%1 <-- %2)
                ; a garder en coherence avec TAILLE_DES_PORTIONS
                ; utilisation pour traitement
                ; %1 REG mod = sortie adresse portion
                ; %2 REG mod = entree numero portion
            ; ----- recuperation adresse de base
                lea %1, [portions]
            ; ----- ajout de TAILLE_DES_PORTIONS fois le numero de portion
                shl %2, 4 ; %2 x 16
                add %1, %2 ; +16xTDP
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
            ; sauvegarde des registres eventuellement utilises
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
            ; 4 premiers parametres
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
            ; pre-calage eventuel de la pile selon le nombre d'arguments au dela de 4
                %if p % 2 == 1
                    sub rsp, 8
                %endif
            ; parametres en push en partant de la fin
                %if p > 0
                    %rep p
                        %rotate -1
                        push %1
                    %endrep
                %endif
            ; shadow space
                sub rsp, 32
            ; the call
                call s
            ; shadow space
                add rsp, 32
            ; reposition stack
                %assign i p*8
                add rsp, i
            ; recalage eventuel de la pile selon le nombre d'arguments au dela de 4
                %if p % 2 == 1
                    add rsp, 8
                %endif
            ; recuperation des 10 registres sauvegardes
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
                ; exemple : ligne_de_valeur r15, rdx, OS_CREFR_1, cx, rcx, portion_chrs_text
            ; -----
                xor %5, %5
                mov %4, [%2+%3]
                ; call_winapi64_style WriteConsoleA, %1, %6, %7, reponse_long_ret
                ; call_winapi64_style convertir, %5, nombre_text, NOMBRE_LONG
                ; call_winapi64_style WriteConsoleA, %1, nombre_text, NOMBRE_LONG, reponse_long_ret
                call_datapush_style ecrire_ligne_3, %1, %6, %5
            %endmacro
section .data ; ----------------------------------------------------------------------------------- PARAMETRES
    ; dimensionnement
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
        NOMBRE_DE_PORTIONS: equ DIMENSION_X*DIMENSION_Y*DIMENSION_Z
        TAILLE_DES_PORTIONS: equ 16
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
    ; constantes de types de portions
        TYPE_SOURCE: equ                            0b101_00_01_0 ; type source de données en mémoire
        TYPE_LECTEUR: equ                           0b101_00_00_0 ; type lecteur
        TYPE_PULSEUR: equ                           0b100_00_00_0 ; type pulseur
        TYPE_SEGMENT_GERME: equ                     0b001_00_00_0 ; type dendrite figé sans dendrite amont
        TYPE_SEGMENT_PARTIEL: equ                   0b001_01_00_0 ; type dendrite auquel il manque des dendrites
        TYPE_SEGMENT_COMPLET: equ                   0b001_10_00_0 ; type dendrite avec toutes dendrite amont
        TYPE_NEURONE_GERME: equ                     0b011_00_00_0 ; type neurone figé sans dendrite ni axone
        TYPE_NEURONE_PARTIEL: equ                   0b011_01_00_0 ; type neurone auquel il manque des dendrites et/ou l'axone
        TYPE_NEURONE_COMPLET: equ                   0b011_10_00_0 ; type neurone avec toutes dendrites et neurone
        TYPE_EXTENSION_GERME: equ                   0b010_00_00_0 ; type axone figé sans destination ni extension
        TYPE_EXTENSION_PARTIEL: equ                 0b010_01_00_0 ; type axone auquel il manque la destination ou l'extension
        TYPE_EXTENSION_COMPLET: equ                 0b010_10_00_0 ; type axone avec destination et extension
    ; masques or, and, xor T3 E2 S2 A1
        MASQUE_TYPE_Txxx: equ       0b111_00_00_0
        MASQUE_TYPE_TExx: equ       0b111_11_00_0
        MASQUE_TYPE_xxxA: equ       0b000_00_00_1
        MASQUE_TYPE_xxxA_X: equ     0b111_11_11_0
    ; positionnement de donnees / offsets
        ; offsets structurels
        OS_TYPEVSA_1:   equ 0 ; offset du type/evolution/sous-type/activation
        OS_SEGNS_4:     equ 12 ; offset du numero de segment ou neurone suivant
        OS_EXTAX_4:     equ 12 ; offset du numero de portion suivante
        OS_NCACT_1:     equ 5 ; offset niveau / compteur d'activité
        OS_DCEVO_1:     equ 6 ; offset de décompte d'evolution A_FAIRE : définition précise du rôle de ce champs
        ; offset développements neurone
        OS_ORIENT_1:    equ 6 ; offset orientation des développements
        OS_PRAYO_1:     equ 7 ; offset puissance de segments dendritiques rayonnants
        OS_PPLAN_1:     equ 8 ; offset puissance de segments dendritiques planaires
        OS_PAPIC_1:     equ 9 ; offset puissance de segments dendritiques apicaux
        OS_PPANI_1:     equ 10 ; offset puissance de segments dendritiques paniers
        OS_PAXON_1:     equ 11 ; offset puissance d'extensions axonales
        ; offset développements segments/extensions
        OS_PSYNA_2:     equ 9 ; offset potentiel synaptique restant
        OS_PRSEG_1:     equ 11 ; offset potentiel de segments restants
        OS_PREXT_1:     equ 11 ; offset potentiel d'extensions restantes
        ; offsets charges
        OS_BSYNM_1:     equ 3 ; offset de la masse du bloc de boutons synaptiques
        OS_BSYNV_1:     equ 4 ; offset de la valeur du bloc de boutons synaptiques
        OS_DESTN_4:     equ 7 ; offset du numero de portion de destination
        OS_CHARG0_2:    equ 1 ; offset charge 0 en cours
        OS_CHARG1_2:    equ 3 ; offset charge 1 stockée
        OS_CHARG2_2:    equ 7 ; offset charge 2 stockée
        OS_SEUIL_1:     equ 3 ; offset de la puissance du seuil de charge
        OS_CREFR_1:     equ 4 ; offset du (dé-)compteur réfractaire
        OS_BREFR_1:     equ 5 ; offset de la base réfractaire
        ; offsets lecteur
        OS_INDEX_2:     equ 1 ; offset de l'index du segment à lire
        OS_NMBIT_1:     equ 3 ; offset de numéro de bit à lire
        OS_PERSA_1:     equ 5 ; offset de la persistance actuelle
        OS_PERSB_1:     equ 6 ; offset de la base de persistance
        OS_PAMEM_4:     equ 12 ; offset du numéro de portion des paramètres d'accès mémoire
        ; offsets accès mémoire
        OS_ADDRS_8:     equ 1 ; offset de l'adresse de base
        OS_LGSEG_1:     equ 9 ; offset de la largeur des segments (en octets / 8 maxi)
        OS_NBSEG_2:     equ 10 ; offset du nombre de segments
    ; initialisations graphiques
        ; dimensions graphiques
        LARGEUR_FENETRE: equ DIMENSION_X
        HAUTEUR_FENETRE: equ DIMENSION_Y
        ; paramètres de codage mémoire / graphique
        NOMBRE_OCTET_PAR_POINT: equ 3
        COMPLEMENT_LIGNE_DWORD: equ ((LARGEUR_FENETRE*NOMBRE_OCTET_PAR_POINT*8+7)/8) % 4
        ; offsets couleurs
        ROUG:   equ 2
        VERT:   equ 1
        BLEU:   equ 0
        ; dimensions viseur
        VISEUR_INT: equ 3
        VISEUR_EXT: equ 10

    ; textes de messages console
        LONGUEUR_LIGNES: equ 40 ; 20 minimum pour accepter les nombres jusqu'à 64 bits / 254 maxi
        texte_a_ecrire: db LONGUEUR_LIGNES dup ('?'), 0x0A
        texte_de_test: db 'Ceci est un test : ', 0
        texte_vide: db '/', 0
        ; à propos de la fenêtre
        app_instance: db 'Handle process winapi : ', 0
        drawing_handle: db 'Handle de drawing : ', 0
        DIB_handle: db 'Handle de DIB : ', 0
        DIB_address: db 'Adresse de DIB : ', 0
        ; textes communs
        texte_numero_de_boucle: db 'Numero de boucle longue : ', 0
        texte_duree_de_boucle: db 'Duree de boucle sur potions : ', 0
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
        portion_ntac_text: db 'Niveau temporel d activite : ', 0
        portion_devo_text: db 'Décompte d evolution : ', 0
        ; specifiques accès mémoire
        portion_adrs_text: db 'Adresse source : ', 0
        portion_tseg_text: db 'Taille des segments : ', 0
        portion_nseg_text: db 'Nombre de segments : ', 0
        ; specifiques lecteurs
        portion_idxl_text: db 'Index de lecture : ', 0
        portion_bitl_text: db 'Numero de bit a lire : ', 0
        portion_prsa_text: db 'Persistance actuelle : ', 0
        portion_prsb_text: db 'Persistance de base : ', 0
        portion_pamm_text: db 'Portion d accès mémoire : ', 0
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
        ; speciaux
        interparagraphes_text: db LONGUEUR_LIGNES dup ('#'), 0
        interligne_text: db '-----', 0
        portion_nonu_text: db 'Non utilise : ', 0
        personalise_text: db '>>> On est passe ici <<<', 0
        nombre_text: db 24 dup ('.')
        NOMBRE_LONG: equ $ - nombre_text
        REPONSE_LONG_MAX: equ 12
        message_0_text: db 'Saisir un texte :', 10
        MESSAGE_0_LONG: equ $ - message_0_text
    ; fenetre windows
        windowClassName: db 'Classe de fenetre', 0
        windowName: db 'Fenetre de tracage', 0
        NOM_FENETRE_A0: db 'Types de portions', 0
        NOM_FENETRE_Z1: db 'Valeurs du bloc de boutons', 0
        NOM_FENETRE_E2: db 'Charges et chronos', 0
        NOM_FENETRE_R3: db 'Activation et retro-activation', 0
    ; lecture du fichier
        nom_de_fichier: db '.\textealire.txt', 0
        NOMBRE_A_LIRE: equ 52736 ; 65535 maxi
; ------------------------------------------------------------------------------------------------- CONSTANTES WINAPI
    ; general
        SHADOW_SPACE_SIZE: equ 32
        KEY_EVENT: equ 1
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
        OPEN_EXISTING: equ 3
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
        SRCERASE: equ 0x00440328
        DIB_RGB_COLORS: equ 0x00000000
        BI_RGB: equ 0x00000000
    ; constantes winapi dans procedure de fenetre : clavier
        VK_ESCAPE: equ 0x1B

section .bss ; ------------------------------------------------------------------------------------ RESERVATIONS MEMOIRE
    ; moteur
        ; pour cerveau
        align 64 ; est-ce vraiment utile ?
        portions: times NOMBRE_DE_PORTIONS resb TAILLE_DES_PORTIONS
        ; pour boucles exterieures
        bouclage_rapide_actuel: resq 1
        debut_bouclage_rapide: resq 1
        bouclage_rapide_en_cours: resq 1
        bouclage_moyen_actuel: resq 1
        debut_bouclage_moyen: resq 1
        bouclage_lent_actuel: resq 1
        mode_pas_a_pas: resb 1
        ; vitesse
        dureeDeTraitement: resq 1
        lenteur: resb 1
        ; pour lecture fichier
        Filehandle: resq 1
        contenu_du_fichier: resw NOMBRE_A_LIRE
        pointeur_entree: resq 1
    ; interface
        ; divers
        modeTracage: resb 1
        visualize_x: resq 1
        visualize_y: resq 1
        visualize_z: resq 1
        detailConsole: resb 1


            positionCurseur: resw 2
            csbi: resb 22
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
section .text ; ----------------------------------------------------------------------------------- DEBUT DU CODE
    global main
    main:
        sub rsp, 8
; ------------------------------------------------------------------------------------------------- CREATION FENETRE ET ASSOCIES
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
        ; mode de tracage et texte en console
            mov byte [modeTracage], 2
            mov qword [visualize_x], 50
            mov qword [visualize_y], 50
            mov qword [visualize_z], 0
            mov byte [mode_pas_a_pas], 0
            mov byte [detailConsole], 0
; ------------------------------------------------------------------------------------------------- LECTURE DU FICHIER D'ENTREES
        ; ouverture du fichier
            lea rcx, [nom_de_fichier] ;     LPCSTR                  lpFileName
            mov edx, GENERIC_READ ;         DWORD                   dwDesiredAccess,
            mov r8d, 0 ;                    DWORD                   dwShareMode,
            mov r9, 0 ;                     LPSECURITY_ATTRIBUTES   lpSecurityAttributes,
            ; parametres suivants en commencant par la fin
            push 0 ; pour alignement pile
            push 0 ;                        HANDLE                  hTemplateFile
            push FILE_ATTRIBUTE_NORMAL ;    DWORD                   dwFlagsAndAttributes,
            push OPEN_EXISTING ;            DWORD                   dwCreationDisposition,
            sub rsp, SHADOW_SPACE_SIZE
            call CreateFileA
            add rsp, SHADOW_SPACE_SIZE
            mov [Filehandle], rax
        ; lecture du contenu
            mov rcx, [Filehandle] ;                  HANDLE          hFile,
            lea rdx, [contenu_du_fichier] ; LPVOID          lpBuffer,
            mov r8d, NOMBRE_A_LIRE ;        DWORD           nNumberOfBytesToRead,
            mov r9d, 0 ;                    LPDWORD         lpNumberOfBytesRead,
            ; parametres suivants en commencant par la fin
            push 0 ; pour alignement pile
            push 0 ;                        LPOVERLAPPED    lpOverlapped
            sub rsp, SHADOW_SPACE_SIZE
            call ReadFile
            add rsp, SHADOW_SPACE_SIZE
        ; fermeture du fichier
            mov rcx, [Filehandle] ;         HANDLE          hObject
            sub rsp, SHADOW_SPACE_SIZE
            call CloseHandle
            add rsp, SHADOW_SPACE_SIZE

; ------------------------------------------------------------------------------------------------- INITIALISATION DU CERVEAU
    ; =============================================== CREATIONS PAR RESEAUX

        ; --------------- pulseur
            call_datapush_style sub_creer_portion_pulseur_4, \
            40+50*FY+0*FZ, VS_PSEUIL,  127,     45+50*FY+0*FZ
            ; Numero,      PuissSeuil, ValBloc, Dest
        ; --------------- réseau de segments dendritiques
            call_datapush_style sub_creer_reseau, \
            50+50*FY+0*FZ, TYPE_SEGMENT_COMPLET, 49+50*FY+0*FZ, 80,  40,  5,  5
            ; Suivant(N),  Type                  NumP           n/X  n/Y n/Z Pas
        ; --------------- réseau de neurones
            call_datapush_style sub_creer_reseau, \
            51+50*FY+0*FZ, TYPE_NEURONE_COMPLET, 50+50*FY+0*FZ, 80,  40,  5,  5
            ; suivant(E),  Type                  NumP           n/X  n/Y n/Z Pas
        ; --------------- réseau d'extensions axonales
            call_datapush_style sub_creer_reseau, \
            54+50*FY+0*FZ, 0,         TYPE_EXTENSION_COMPLET, 51+50*FY+0*FZ, 80,  40,  5,  5
            ; Dest,        Suivant(E) Type                    NumP           n/X  n/Y n/Z Pas

    ; =============================================== CREATIONS A L'UNITE
        ; --------------- source de données
            lea rax, [contenu_du_fichier]
            call_datapush_style sub_creer_portion_datasource_4, \
            1+1*FY+0*FZ, rax,     1,               NOMBRE_A_LIRE
            ; Numero,    Adresse, LongueurElement, NombreDElements
        ; --------------- lecteur
            call_datapush_style sub_creer_portion_lecteur_7, \
            10+10*FY, 0,         0,      127,     10,      1+1*FY+0*FZ,   30+10*FY
            ; Numero, IndexLect, NumBit, ValBloc, Persist, PortionSource, Destination
        ; --------------- pulseur
            call_datapush_style sub_creer_portion_pulseur_4, \
            10+20*FY, 11,         127,     20+20*FY
            ; Numero, PuissSeuil, ValBloc, Dest
        ; --------------- segment dendritique
            call_datapush_style sub_creer_segment_dendritique_7, \
            TYPE_SEGMENT_COMPLET, 20+20*FY, 0,       0,      0,           0,             30+20*FY
            ; Type,               Numero,   NivTemp, DecEvo, SynaptDispo, PotSegRestant, SegSuivant
        ; --------------- neurone
            call_datapush_style sub_creer_portion_neurone_10, \
            TYPE_NEURONE_COMPLET, 30+20*FY, 10,         10,      0,    0,     0,    0,    0,    40+20*FY
            ; Type,               Numero,   PuissSeuil, BaseRef, PRay, PPlan, PApi, PPan, PAxo, ExtAxo
        ; --------------- extension axonale
            call_datapush_style sub_creer_extension_axonale_8, \
            TYPE_EXTENSION_COMPLET, 40+20*FY, 0,     0,     0,       50+20*FY,    0,             0
            ; Type,                 Numero,   MasBB, ValBB, NivTemp, Destination, PotAxoRestant, ExtSuivante
    ; =============================================== FIN CREATIONS
; ------------------------------------------------------------------------------------------------- ANNONCES
        call_winapi64_style GetStdHandle, STD_OUTPUT_HANDLE
        mov r15, rax
        call_datapush_style ecrire_ligne_3, r15, interparagraphes_text, -1 ;                  interligne
        call_datapush_style ecrire_ligne_3, r15, contenu_du_fichier, -1 ;               texte de lecture
        call_datapush_style ecrire_ligne_3, r15, interligne_text, -1 ;                  interligne
        call_datapush_style ecrire_ligne_3, r15, app_instance, qword [Instance] ;     instance de l'application
        call_datapush_style ecrire_ligne_3, r15, windowClassName, qword [ClassAtom] ;   atome de classe de fenetre
        call_datapush_style ecrire_ligne_3, r15, windowName, qword [Windowhandle] ;     handle de la fenetre 
        call_datapush_style ecrire_ligne_3, r15, interligne_text, -1 ;                  interligne
; ------------------------------------------------------------------------------------------------- BOUCLAGES AVANT
    mov qword [bouclage_lent_actuel], 0
    bouclage_lent: ; boucle lente pour ecrire des infos et choisir de sortir
        ; initialisation boucle rapide
        xor rax, rax
        rdtsc ; -> edx:eax
        shl rdx, 32
        or rdx, rax ; timestamp condensé
        mov [debut_bouclage_moyen], rdx ; stockage timestamp pour durée boucles moyennes
        bouclage_moyen: ; boucle moyenne pour redessiner regulierement
            ; initialisation serie de bouclages rapides
            xor rax, rax
            rdtsc ; -> edx:eax
            shl rdx, 32
            or rdx, rax ; timestamp condensé
            mov [debut_bouclage_rapide], rdx ; stockage timestamp pour durée boucle rapide
            bouclage_rapide:
                ; initialisation calcul de la duree du bouclage sur portions
                xor rax, rax
                rdtsc ; -> edx:eax
                shl rdx, 32
                or rdx, rax ; timestamp condensé
                ;
                push rdx ; stockage timestamp initial et pre-alignement pile pour push r8
                mov r8, 0 ; initialisation bouclage sur portions
                bouclage_portions:
                    push r8
; -------------------------------------------------------------------------------------------- TRAITEMENT PORTION r8
                    ; adresse r9
                        adresse_from_numero r9, r8 ; calcul adresse portion a traiter
                        mov r8, [rsp] ; récuperation du numero de portion déjà sauvegardé
                        ; aiguillage
                        mov al, [r9+OS_TYPEVSA_1] ; récuperation du type de portion
                        and al, MASQUE_TYPE_TExx ; isolement du type + état d'évolution
                        ; saut vers traitement concerne
                        ; A_FAIRE : réorganiser les tests selon la fréquence d'apparition du cas
                        comparer_et_jump_si_egal al, TYPE_SEGMENT_GERME,                    portion_segment_germe
                        comparer_et_jump_si_egal al, TYPE_SEGMENT_PARTIEL,                  portion_segment_partiel
                        comparer_et_jump_si_egal al, TYPE_SEGMENT_COMPLET,                  portion_segment_complet
                        comparer_et_jump_si_egal al, TYPE_NEURONE_GERME,                    portion_neurone_germe
                        comparer_et_jump_si_egal al, TYPE_NEURONE_PARTIEL,                  portion_neurone_partiel
                        comparer_et_jump_si_egal al, TYPE_NEURONE_COMPLET,                  portion_neurone_complet
                        comparer_et_jump_si_egal al, TYPE_EXTENSION_GERME,                  portion_extension_germe
                        comparer_et_jump_si_egal al, TYPE_EXTENSION_PARTIEL,                portion_extension_partiel
                        comparer_et_jump_si_egal al, TYPE_EXTENSION_COMPLET,                portion_extension_complet
                        comparer_et_jump_si_egal al, TYPE_LECTEUR,                          portion_lecteur
                        comparer_et_jump_si_egal al, TYPE_PULSEUR,                          portion_pulseur
                        ; ne pas traiter les portions de type 0
                        jmp fin_traitement

                    ; portions segments (dendrites)
                    portion_segment_germe:
                        ; A_FAIRE : coder le passage en mode partiel éventuel
                        jmp fin_traitement
                    portion_segment_partiel:
                        ; A_FAIRE : coder la création de la dendrite antecedentes (même type)
                        ; A_FAIRE : conditionner tout ça à la vitesse de développement (?)
                        ; A_FAIRE : coder le passage en mode complet
                    portion_segment_complet:
                        and [r9+OS_TYPEVSA_1], byte MASQUE_TYPE_xxxA_X ; enlever la rétro-activité le cas échéant
                        xor rax, rax
                        mov eax, [r9+OS_SEGNS_4] ; numero de segment/neurone suivant
                        test eax, eax ; PORTION_ZERO ?
                        jz psc_isole ; aller au cas d'un segment qui a perdu son segment/neurone aval
                            adresse_from_numero rdx, rax ; adresse du suivant
                            mov al, [rdx+OS_TYPEVSA_1] ; type complet du suivant
                            and al, MASQUE_TYPE_xxxA ; bit de retro-activité du suivant
                            or byte [r9+OS_TYPEVSA_1], al ; l'appliquer au type actuel
                            ; glissement des charges vers bx
                            mov bx, [r9+OS_CHARG0_2] ; charge en cours
                            mov [r9+OS_CHARG0_2], word 0 ; charge en cours = 0
                            mov ax, [r9+OS_CHARG1_2] ; charge 1
                            cmp ax, CHARGE_INVALIDE
                            je psc_appliquer_surcharge ; bx = charge 0 ; charge en cours = 0 ; pas de glissement
                            mov [r9+OS_CHARG1_2], bx ; glissement charge 0 -> charge 1
                            mov bx, ax
                            mov ax, [r9+OS_CHARG2_2] ; charge 2
                            cmp ax, CHARGE_INVALIDE
                            je psc_appliquer_surcharge ; bx = charge 1 ; charge en cours = 0 ; glissement 0->1 fait
                            mov [r9+OS_CHARG2_2], bx ; glissement charge 1 -> charge 2
                            mov bx, ax
                            ; ... suite ; bx = charge 2 ; charge en cours = 0 ; glissement 1->2 fait
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
                    ; portions neurone
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
                        mov al, [r9+OS_CREFR_1] ; decompte réfractaire en cours
                        test al, al ; tester si al<>0
                        jnz pnc_refractaire ; aller au traitement du cas réfractaire
                            mov ax, [r9+OS_CHARG0_2] ; charge actuelle (word signé -32767 à 32767)
                            mov cl, [r9+OS_SEUIL_1] ; puissance de seuil avec décalage (maxi 15)
                            mov edx, 1
                            shl edx, cl
                            dec edx ; seuil calculé (0 à 32767)
                            cmp ax, dx ; tester si le seuil n'est pas atteint
                            jl pnc_seuil_non_atteint ; aller au traitement du cas du seuil non atteint
                                mov al, [r9+OS_BREFR_1] ; récupérer la base réfractaire
                                mov [r9+OS_CREFR_1], al ; lancer la refractarité
                                or [r9+OS_TYPEVSA_1], byte MASQUE_TYPE_xxxA ; mettre la rétro-activité
                                xor rax, rax
                                mov eax, [r9+OS_EXTAX_4] ; portion de l'extension axonale
                                test eax, eax ; PORTION_ZERO ?
                                jz fin_traitement ; aller à la fin du traitement
                                    adresse_from_numero rdx, rax ; calculer l'adresse de la destination
                                    or [rdx+OS_TYPEVSA_1], byte MASQUE_TYPE_xxxA ; activer la destination
                                    jmp fin_traitement ; fin
                        pnc_refractaire:
                            ; al est forcément positif en arrivant
                            dec al
                            mov [r9+OS_CREFR_1], al
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
                    ; portions extensions (axonales)
                    portion_extension_germe:
                        ; A_FAIRE : coder le passage en mode partiel éventuel
                        jmp fin_traitement
                    portion_extension_partiel:
                        ; A_FAIRE : coder la recherche de destination + réalisation du lien
                        ; A_FAIRE : coder la création de l'extension suivante
                        ; A_FAIRE : conditionner tout ça à la vitesse de développement (?)
                        ; A_FAIRE : coder le passage en mode complet
                    portion_extension_complet:
                        mov rdx, 0 ; initialiser l'adresse de la destination à 0
                        test byte [r9+OS_TYPEVSA_1], MASQUE_TYPE_xxxA ; tester l'activité actuelle
                        jz pec_incrementer_seulement
                            and byte [r9+OS_TYPEVSA_1], MASQUE_TYPE_xxxA_X ; se desactiver
                            xor rax, rax
                            mov eax, [r9+OS_EXTAX_4] ; portion de l'extension axonale suivante
                            test eax, eax ; NEURONE_ZERO ?
                            jz pec_pas_de_suivante
                                adresse_from_numero rcx, rax ; adresse de l'extension axonale suivante
                                or [rcx+OS_TYPEVSA_1], byte MASQUE_TYPE_xxxA ; activer l'extension axonale suivante
                            pec_pas_de_suivante:
                            xor rax, rax
                            mov eax, [r9+OS_DESTN_4] ; segment de destination
                            test eax, eax ; NEURONE_ZERO ?
                            jz pec_retroaction
                                adresse_from_numero rdx, rax ; adresse de la destination
                                mov byte [r9+OS_NCACT_1], 1 ; initier le compteur
                                ; surcharge segment destination
                                movsx ecx, word [rdx+OS_CHARG0_2] ; charge actuelle destination (word signé -> dword signé)
                                movsx eax, byte [r9+OS_BSYNV_1] ; valeur du bloc de boutons (byte signé -> dword signé)
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
                        pec_incrementer_seulement:
                            mov cl, [r9+OS_NCACT_1] ; compteur d'activité actuel
                            test cl, cl
                            jz pec_retroaction
                                inc cl
                        pec_retroaction:
                        test rdx, rdx ; l'adresse n'a pas été définie / pas de destination ?
                        jz fin_traitement
                            test byte [rdx+OS_TYPEVSA_1], MASQUE_TYPE_xxxA ; destination non rétro-active ?
                            jz fin_traitement
                                ; A_FAIRE : Conditionner la variation à un test aléatoire au-dessus de la masse
                                ; jxx pec_desactiver_cpt
                                    mov bl, [r9+OS_BSYNV_1] ; valeur du bloc de boutons (byte signé)
                                    mov cx, bx ; mise en réserve de la valeur du bloc de boutons
                                    mov ax, [r9+OS_NCACT_1] ; compteur d'activité actuel
                                    test byte [rdx+OS_NCACT_1], al ; différent du niveau d'activité de la destination ?
                                    jz pec_negativer
                                        add bl, 1 ; incrémenter la valeur du bloc de boutons
                                        jmp pec_affectation_et_masse
                                    pec_negativer:
                                        sub bl, 1 ; décrémenter la valeur du bloc de boutons
                                    pec_affectation_et_masse:
                                    cmovo bx, cx ; bornage à la valeur précédente si on dépasse la capacité
                                    mov [r9+OS_BSYNV_1], bl ; enregistrer résultat
                                    ; modification de la masse du bloc de boutons
                                    mov bl, [r9+OS_BSYNM_1] ; masse du bloc de boutons (byte non signé)
                                    mov cl, bl ; mise en réserve de la masse du bloc de boutons
                                    add bl, 1 ; incrémenter la valeur du bloc de boutons
                                    cmovc bx, cx ; plafonnement à la valeur précédente
                                    mov [r9+OS_BSYNM_1], bl
                                pec_desactiver_cpt:
                                mov byte [r9+OS_NCACT_1], 1 ; desactiver le compteur
                                jmp fin_traitement
                    ; portions speciales
                    portion_lecteur:
                        xor rax, rax
                        mov eax, [r9+OS_PAMEM_4] ; portion des paramètres de l'accès mémoire
                        test eax, eax ; PORTION_ZERO ?
                        jz fin_traitement
                            adresse_from_numero r11, rax ; r11 = adresse des paramètres des données à lire
                            xor rax, rax
                            mov eax, dword [r9+OS_DESTN_4] ; portion destination
                            test eax, eax ; PORTION_ZERO ?
                            jz fin_traitement
                                adresse_from_numero r15, rax ; r15 = adresse de la destination
                                mov r13, [r11+OS_ADDRS_8] ; r13 = adresse de base des données
                                movzx rdx, byte [r11+OS_LGSEG_1] ; largeur des segments en octets (maxi 8)
                                mov r10w, word [r9+OS_INDEX_2] ; r10w = index de lecture
                                movzx eax, r10w
                                ; calcul de l'adresse de base à lire
                                ; A_FAIRE ? ajouter une variable de position de tête relatif et boucler sur maxi nombre à lire
                                mul edx ; => edx:eax (?)
                                and rax, 0x00ffffff ; effacement de la partie haute de rax (car maxi possible = 65535*8)
                                add r13, rax ; -> adresse de base à lire
                                ; lecture du bit de donnee
                                mov rax, qword [r13] ; contenu du segment (plus le suite jusqu'a 8 octets)
                                mov cl, byte [r9+OS_NMBIT_1] ; numero de bit a lire (0 à 63)
                                shr rax, cl ; décalage du nombre de bits nécessaire
                                shr rax, 1 ; sortie sur le carry flag
                                ; ----- surcharge destination
                                jnc pl_pas_d_activation ; on n'active pas la destination si le bit lu est 0
                                    ; surcharge segment destination
                                    movsx ecx, word [r15+OS_CHARG0_2] ; charge actuelle destination (word signé -> dword signé)
                                    movsx eax, byte [r9+OS_BSYNV_1] ; valeur du bloc de boutons (byte signé -> dword signé)
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
                                mov bl, [r9+OS_PERSA_1] ; persistance restante actuelle
                                test bl, bl
                                jnz pl_persistance_encore_active
                                    mov bl, [r9+OS_PERSB_1] ; persistance de base (remplace actuelle)
                                    inc r10w ; augmenter l'index de lecture
                                    cmp r10w, word [r11+OS_NBSEG_2] ; comparer au nombre de segments à lire
                                    jne pl_fin_pas_atteinte
                                        xor r10w, r10w ; reprendre la lecture au début
                                    pl_fin_pas_atteinte:
                                    mov [r9+OS_INDEX_2], r10w ; enregistrer l'index de lecture
                                pl_persistance_encore_active:
                                dec bl
                                mov [r9+OS_PERSA_1], bl ; persistance restante
                                jmp fin_traitement
                    portion_pulseur:
                        ; augmentation chrono
                        movzx ebx, word [r9+OS_CHARG0_2] ; chrono actuel (word non signé)
                        add ebx, INCREMENT_PULSEURS ; incrementation chrono
                        mov eax, CHRONO_MAXI
                        cmp ebx, eax
                        cmova ebx, eax ; limiter ecx à un word non signé
                        ; seuil
                        mov cl, [r9+OS_SEUIL_1] ; puissance de seuil avec décalage (maxi 16)
                        mov eax, 1
                        shl eax, cl
                        dec eax ; seuil calculé (0 à 65535)
                        ; conséquences
                        mov cx, CHRONO_INITIAL
                        cmp bx, ax ; tester si le seuil n'est pas atteint
                        cmovae bx, cx ; réinitialiser le compteur si le seuil a été atteint
                        mov [r9+OS_CHARG0_2], bx ; enregistrer le nouveau chrono (0 ou valeur incrémentée)
                        jb fin_traitement ; ne rien faire
                            xor rax, rax
                            mov eax, [r9+OS_DESTN_4] ; neurone de destination
                            test rax, rax ; NEURONE_ZERO ?
                            jz fin_traitement ; si pas de destination, on ne fait rien
                                adresse_from_numero rdx, rax ; adresse neurone de destination
                                movsx ecx, word [rdx+OS_CHARG0_2] ; charge actuelle destination (word signé -> dword signé)
                                movsx eax, byte [r9+OS_BSYNV_1] ; valeur du bloc de boutons (byte signé -> dword signé)
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
                    ; fin
                    fin_traitement:

; -------------------------------------------------------------------------------------------- TRAITEMENT EXTERNE
    ;
    ; Ici on pourra peut-etre traiter les actionneurs
    ; Donc la modification de variables
    ; et le resultat sur les percepteurs
    ; Exemple : champs visuel + deplacement vers la gauche ou la droite
    ;
; ------------------------------------------------------------------------------------------------- BOUCLAGES APRES
                    pop r8
                    inc r8
                    cmp r8, NOMBRE_DE_PORTIONS
                jne bouclage_portions
                ; recuperation du timestamp
                xor rax, rax
                rdtsc ; -> edx:eax
                shl rdx, 32
                or rdx, rax ; timestamp condensé
                mov rcx, rdx ; mise de côté pour durée boucle rapide
                ; durée en nombre de cycles processeur
                pop rax ; recuperation du time stamp de debut et re-alignement de la pile suite pop r8
                sub rdx, rax ; durée boucle de traitement en cycles (environ 30_000_000 à vide)
                ; calcul de la duree en nanoSecondes
                shr rdx, 10 ; durée en kiloCycles (environ 30_000 à vide)
                mov rax, DUREE_DE_CYCLE_PS ; en ps/cycle (369 sur mon ordi)
                mul rdx ; durée en kilocycle*ps/cycle => ns/cycle (11_000_000 sur mon ordi à vide)
                shr rax, 10 ; durée en us/cycle (environ 11_000 sur mon ordi à vide)
                mov [dureeDeTraitement], rax ; stocker le resultat
                ; forcage sortie si mode pas à pas
                mov bl, [mode_pas_a_pas]
                test bl, bl
                jnz boucle_moyenne_suite
                ; conditionnement de sortie bouclage rapide
                mov rax, [debut_bouclage_rapide] ; récupération début de boucle
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
            or rdx, rax ; timestamp condensé
            ; conditionnement de sortie bouclage moyen
                mov rax, [debut_bouclage_moyen] ; récupération début de boucle
                sub rdx, rax ; durée de la boucle moyenne
                mov rax, NBCYCLES_BOUCLE_LENTE
                cmp rdx, rax
            ; bouclage moyen
        jb bouclage_moyen
        apres_boucle_moyenne:
        mov rax, qword [bouclage_lent_actuel]
        add rax, 1
        mov qword [bouclage_lent_actuel], rax
        ; écritures si pas mode pas-à-pas
            mov bl, [mode_pas_a_pas]
            test bl, bl
            jnz pas_d_ecriture_dans_boucle_lente
                call ecrire_messages_console
            pas_d_ecriture_dans_boucle_lente:
        ; bouclage lent
    jmp bouclage_lent
; ------------------------------------------------------------------------------------------------- SORTIE
    sortieComplete:
    ; finalisation ----------------------------------------------
        call_winapi64_style GetStdHandle, STD_OUTPUT_HANDLE
        mov r15, rax
        call_datapush_style ecrire_ligne_3, r15, interligne_text, r15
    ; fin du programme
        ; add rsp, 8
        xor rcx, rcx
        call ExitProcess
; ------------------------------------------------------------------------------------------------- OUTILS AFFICHAGES
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
    ecrire_ligne_3:
        ; ---------- en entree :
        ; Arguments : HandleConsole / AdresseTexte (texte fini par 0)(ou vide) / Nombre (ou -1 sur 64 bits)
        ; Le curseur doit déjà être à sa place
        ; ---------- alignement pile + sauvegarde registres
        push rax
        push rbx
        push rcx
        push rdx
        sub rsp, 8
        ; ---------- définir le caractère de remplissage
            mov cl, byte ' ' ; caractère de remplissage par défaute = espace
            mov rax, [rsp+8*(5+1)] ; nombre à écrire
            cmp rax, -1
            je el_pas_nombre
                mov cl, byte '.' ; s'il y a un nombre, alors ce sera un point
            el_pas_nombre:
        ; ---------- précharger le fond dans texte_a_ecrire
            lea rbx, [texte_a_ecrire] ; adresse de début de texte
            mov rax, LONGUEUR_LIGNES ; nombre de caractères dans la ligne
            raz_chaine:
                mov [rbx], cl ; écriture du caractère
                inc rbx ; emplacement du caractère suivant
                dec rax ; décompte
                test rax, rax
            jnz raz_chaine
        ; ---------- s'il y a un texte, on le surajoute
            mov rdx, [rsp+8*(5+2)] ; adresse de début de texte à ajouter
            lea rbx, [texte_a_ecrire] ; adresse de début de texte
            mov rcx, LONGUEUR_LIGNES ; nombre de caracteres maxi
            el_surajouter:
                mov al, [rdx]
                test al, al
                je el_surajouter_fin
                mov [rbx], al
                inc rbx
                inc rdx
                dec rcx
                test rcx, rcx
            jnz el_surajouter
            el_surajouter_fin:
        ; ---------- s'il y a un nombre, le surajouter
            lea rbx, [texte_a_ecrire] ; adresse de début de texte
            add rbx, LONGUEUR_LIGNES
            mov rax, [rsp+8*(5+1)] ; nombre à écrire
            cmp rax, -1
            je el_pas_nombre_2
                el_convertir:
                    dec rbx ; remonter dans la ligne de texte à écrire
                    xor rdx, rdx    ; on met rdx a 0 (partie haute du dividende)
                                    ; le nombre est dans eax (partie basse du dividende)
                    mov rcx, 10     ; on fixe la base de conversion (diviseur)
                    div rcx             ; on fait la division de rdx:rax par rcx => quotient=rax / reste=rdx
                    add dl, '0'         ; on ajoute le numero ascii de "0" au dernier octet des rdx
                    mov byte [rbx], dl  ; on ecrit le dernier octet
                    test rax, rax       ; on regarde si rax est nul
                jnz el_convertir        ; si ce n'est pas nul on continue
            el_pas_nombre_2:
        ; ---------- écrire le texte à la position courante
            mov rcx, [rsp+8*(5+3)] ; handle console
            call_winapi64_style WriteConsoleA, rcx, texte_a_ecrire, LONGUEUR_LIGNES+1, written
        ; ---------- récupération registres + désalignement pile
        add rsp, 8
        pop rdx
        pop rcx
        pop rbx
        pop rax
        ret
    detailler_portion:
        ; ----- recalage et sauvegarde des registres modifies
            sub rsp, 8
            push rax ; utilise pour travail
            push rbx ; utilise pour handle de sortie
            push rcx ; utilise numero de portion / compteur
            push rdx ; utilise adresse de portion
        ; ----- numero de portion -> rcx
            mov rax, [visualize_z]
            shl rax, BITS_POUR_Y
            add rax, [visualize_y]
            shl rax, BITS_POUR_X
            add rax, [visualize_x]
            mov rcx, rax
        ; ----- préparatifs        
            adresse_from_numero rdx, rax ; adresse de la portion -> rdx
            call_winapi64_style GetStdHandle, STD_OUTPUT_HANDLE
            mov rbx, rax ; Handle de sortie -> rbx
        ; ----- écritures communes
            call_datapush_style ecrire_ligne_3, rbx, interligne_text, -1 ;              interligne
            call_datapush_style ecrire_ligne_3, rbx, portion_numr_text, rcx ;           numero de la portion
            call_datapush_style ecrire_ligne_3, rbx, texte_position_x, qword [visualize_x] ;  position x
            call_datapush_style ecrire_ligne_3, rbx, texte_position_y, qword [visualize_y] ;  position y
            call_datapush_style ecrire_ligne_3, rbx, texte_position_z, qword [visualize_z] ;  position z
            call_datapush_style ecrire_ligne_3, rbx, interligne_text, -1 ;              interligne
            ligne_de_valeur rbx, rdx, OS_TYPEVSA_1, cl, rcx, portion_type_text ;        type de portion
        ; ----- distribution
            xor rcx, rcx
            mov cl, [rdx+OS_TYPEVSA_1]
                mov al, cl
                and cl, MASQUE_TYPE_TExx
                and al, MASQUE_TYPE_Txxx
            ; A_FAIRE : appliquer un masque et de faire les tests uniquement sur les 5 premiers bits
            comparer_et_jump_si_egal al, TYPE_SEGMENT_GERME,                    dp_psd
            comparer_et_jump_si_egal al, TYPE_NEURONE_GERME,                    dp_pn
            comparer_et_jump_si_egal al, TYPE_EXTENSION_GERME,                  dp_pea
            comparer_et_jump_si_egal cl, TYPE_SOURCE,                           dp_sdm
            comparer_et_jump_si_egal cl, TYPE_LECTEUR,                          dp_plc
            comparer_et_jump_si_egal cl, TYPE_PULSEUR,                          dp_ppl
        ; ----- traitement par defaut
            inc rdx ; on n'écrit pas le premier octet (type)
            mov rcx, 1
            dp_boucle:
                xor rax, rax
                mov al, [rdx]
                call_winapi64_style convertir, rax, nombre_text, NOMBRE_LONG
                call_winapi64_style WriteConsoleA, rbx, nombre_text, NOMBRE_LONG, reponse_long_ret
                inc rdx
                inc rcx
                cmp rcx, TAILLE_DES_PORTIONS
            jbe dp_boucle
            jmp dp_fin
        ; -----------------------------------------------------------------------------------------------
        dp_sdm: ; portion source de données en mémoire
            ligne_de_valeur rbx, rdx, OS_ADDRS_8, rcx, rcx, portion_adrs_text ; adresse source
            ligne_de_valeur rbx, rdx, OS_LGSEG_1, cl, rcx, portion_tseg_text ; taille de segment
            ligne_de_valeur rbx, rdx, OS_NBSEG_2, cx, rcx, portion_nseg_text ; nombre de segments
            jmp dp_fin
        dp_plc: ; portion lecteur
            ligne_de_valeur rbx, rdx, OS_INDEX_2, cx, rcx, portion_idxl_text ; index de lecture
            ligne_de_valeur rbx, rdx, OS_NMBIT_1, cl, rcx, portion_bitl_text ; numero de bit a lire
            ligne_de_valeur rbx, rdx, OS_BSYNV_1, cl, rcx, portion_blcv_text ; valeur du bloc de boutons
            ligne_de_valeur rbx, rdx, OS_PERSA_1, cl, rcx, portion_prsa_text ; persistance actuelle
            ligne_de_valeur rbx, rdx, OS_PERSB_1, cl, rcx, portion_prsb_text ; persistance de base
            ligne_de_valeur rbx, rdx, OS_DESTN_4, ecx, rcx, portion_dest_text ; portion de destination
            ligne_de_valeur rbx, rdx, OS_PAMEM_4, ecx, rcx, portion_pamm_text ; portion d'accès mémoire
            jmp dp_fin
        dp_ppl: ; portion pulseur
            ligne_de_valeur rbx, rdx, OS_CHARG0_2, cx, rcx, portion_chro_text ; chrono
            ligne_de_valeur rbx, rdx, OS_SEUIL_1, cl, rcx, portion_chrl_text ; seuil charge
            ligne_de_valeur rbx, rdx, OS_BSYNV_1, cl, rcx, portion_blcv_text ; valeur du bloc de boutons
            ligne_de_valeur rbx, rdx, OS_DESTN_4, ecx, rcx, portion_dest_text ; numero de destination
            jmp dp_fin
        dp_psd: ; portion segment dendritique
            ligne_de_valeur rbx, rdx, OS_CHARG0_2, cx, rcx, portion_chrg_text ; charge 0 en cours
            ligne_de_valeur rbx, rdx, OS_CHARG1_2, cx, rcx, portion_chrs_text ; charge 1 ou invalide
            ligne_de_valeur rbx, rdx, OS_NCACT_1, cl, rcx, portion_ntac_text ; niveau temporel d'activité
            ligne_de_valeur rbx, rdx, OS_DCEVO_1, cl, rcx, portion_devo_text ; decompte évolution
            ligne_de_valeur rbx, rdx, OS_CHARG2_2, cx, rcx, portion_chrg_text ; charge 2 ou invalide
            ligne_de_valeur rbx, rdx, OS_PSYNA_2, cx, rcx, portion_psyn_text ; potentiel synaptique dispo
            ligne_de_valeur rbx, rdx, OS_PRSEG_1, cl, rcx, portion_pseg_text ; potentiel de segments restants
            ligne_de_valeur rbx, rdx, OS_SEGNS_4, ecx, rcx, portion_segs_text ; numero segment/neurone suivant
            jmp dp_fin
        dp_pn: ; portion neurone
            ligne_de_valeur rbx, rdx, OS_CHARG0_2, cx, rcx, portion_chrg_text ; charge
            ligne_de_valeur rbx, rdx, OS_SEUIL_1, cl, rcx, portion_chrs_text ; seuil charge
            ligne_de_valeur rbx, rdx, OS_CREFR_1, cl, rcx, portion_cref_text ; decompte refractaire
            ligne_de_valeur rbx, rdx, OS_BREFR_1, cl, rcx, portion_bref_text ; base decompte refractaire
            ligne_de_valeur rbx, rdx, OS_ORIENT_1, cl, rcx, portion_ornt_text ; orientation des développements
            ligne_de_valeur rbx, rdx, OS_PRAYO_1, cl, rcx, portion_pray_text ; puissance potentiel rayonnant
            ligne_de_valeur rbx, rdx, OS_PPLAN_1, cl, rcx, portion_ppla_text ; puissance potentiel planaire
            ligne_de_valeur rbx, rdx, OS_PAPIC_1, cl, rcx, portion_papi_text ; puissance potentiel apical
            ligne_de_valeur rbx, rdx, OS_PPANI_1, cl, rcx, portion_ppan_text ; puissance potentiel ppanier
            ligne_de_valeur rbx, rdx, OS_PAXON_1, cl, rcx, portion_paxo_text ; puissance potentiel axonal
            ligne_de_valeur rbx, rdx, OS_EXTAX_4, ecx, rcx, portion_eaxo_text ; première extension axonale
            jmp dp_fin
        dp_pea: ; portion extension axonale
            ligne_de_valeur rbx, rdx, OS_BSYNM_1, cl, rcx, portion_blcp_text ; masse du bloc de boutons
            ligne_de_valeur rbx, rdx, OS_BSYNV_1, cl, rcx, portion_blcv_text ; valeur du bloc de boutons
            ligne_de_valeur rbx, rdx, OS_NCACT_1, cl, rcx, portion_ntac_text ; niveau temporel d'activité
            ligne_de_valeur rbx, rdx, OS_DCEVO_1, cl, rcx, portion_devo_text ; decompte évolution
            ligne_de_valeur rbx, rdx, OS_DESTN_4, ecx, rcx, portion_dest_text ; numero de destination
            ligne_de_valeur rbx, rdx, OS_PREXT_1, cl, rcx, portion_pext_text ; potentiel d'extensions restantes
            ligne_de_valeur rbx, rdx, OS_EXTAX_4, ecx, rcx, portion_eaxo_text ; extension axonale suivante
            jmp dp_fin
        ; -----------------------------------------------------------------------------------------------
        dp_fin:
        ; ---------- recuperation registres
            pop rdx
            pop rcx
            pop rbx
            pop rax
            add rsp, 8
        ; ----- sortie
        ret
    ecrire_messages_console:
        sub rsp, 8
        ; recuperer le handle de sortie standard
        call_winapi64_style GetStdHandle, STD_OUTPUT_HANDLE
        mov r15, rax

        ; repositionner le curseur
        ; call_winapi64_style SetConsoleCursorPosition, r15, 0h00090005
        ; obtenir les informations du tampon de la console
        ; lea rax, [csbi]
        ; call_winapi64_style GetConsoleScreenBufferInfo, r15, rax
        ; calculer la taille de la console (lignes * colonnes)
        ; movzx rax, word [csbi + 4]      ; Charger la largeur (dwSize.X)
        ; movzx rcx, word [csbi + 6]      ; Charger la hauteur (dwSize.Y)
        ; imul rax, rcx                   ; Multiplier pour obtenir la taille totale en caractères
        ; remplir la console avec des espaces
        ; lea rdx, [written]
        ; call_winapi64_style FillConsoleOutputCharacterA, r15, 44, 100, 0h00090005, rdx
        ; call FillConsoleOutputAttribute

        call_datapush_style ecrire_ligne_3, r15, interparagraphes_text, -1 ; interparagraphes
        call_datapush_style ecrire_ligne_3, r15, texte_numero_de_boucle, qword [bouclage_lent_actuel] ; numéro de boucle
        call_datapush_style ecrire_ligne_3, r15, texte_duree_de_boucle, qword [dureeDeTraitement] ; durée de boucle

        ; Detailler eventuellement dans la console
        xor rax, rax
        mov al, [detailConsole]
        test rax, rax
        jz ne_pas_detailler
            call detailler_portion
        ne_pas_detailler:
        call_datapush_style ecrire_ligne_3, r15, interparagraphes_text, -1 ; interligne
        add rsp, 8
        ret
; -------------------------------------------------------------------------------------------- OUTILS POUR CREATIONS INITIALES
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
    sub_creer_portion_datasource_4: ; creation d'une portion de source de données (Num/Adr/LargSeg/NbrSeg)
        ; Arguments : NumeroDePortion / AdresseDesDonnées / LargeurDeSegment / NombreDeSegments
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
            mov [rdx+OS_TYPEVSA_1], byte TYPE_SOURCE
            ;
            mov rax, [rsp+8*(3+3)]
            mov [rdx+OS_ADDRS_8], rax ; adresse mémoire des données
            ;
            mov rax, [rsp+8*(3+2)]
            mov [rdx+OS_LGSEG_1], al ; largeur de segment
            ;
            mov rax, [rsp+8*(3+1)]
            mov [rdx+OS_NBSEG_2], ax ; nombre de segments
            ;
        ; ----- recuperation des registres
        add rsp, 8 ; re-alignement de la pile
        pop rax
        pop rdx
        ; ----- sortie
        ret
    sub_creer_portion_lecteur_7: ; creation d'une portion de lecture (Num/IdxLect/NumBit/ValBB/PersBase/NumDataSrc/Dest)
            ; Arguments : NumeroDePortion / IndexLecture / NumeroDeBit / ValeurBlocBoutons / PersistanceDeBase / PortionData / Destination
        ; ----- sauvegarde des registres utilises + alignement de la pile
        push rdx
        push rax
        sub rsp, 8 ; alignement de la pile
        ; ----- adresse de base de la paire de portions
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
        add rsp, 16 ; ré-alignement de la pile
        ; ----- enregistrement des donnees
            ;
            mov [rdx+OS_TYPEVSA_1], byte TYPE_LECTEUR ; type de portion
            ;
            mov rax, [rsp+8*(3+6)]
            mov [rdx+OS_INDEX_2], ax ; index de lecture
            ;
            mov rax, [rsp+8*(3+5)]
            mov [rdx+OS_NMBIT_1], al ; numero de bit à lire
            ;
            mov rax, [rsp+8*(3+4)]
            mov [rdx+OS_BSYNV_1], al ; valeur du bloc de boutons
            ;
            mov rax, [rsp+8*(3+3)]
            mov [rdx+OS_PERSB_1], al ; persistance de base
            ;
            mov rax, [rsp+8*(3+2)]
            mov [rdx+OS_PAMEM_4], eax ; numero de portion des parametres d'accès mémoire
            ;
            mov rax, [rsp+8*(3+1)]
            mov [rdx+OS_DESTN_4], eax ; portion de destination
            ;
        ; ----- recuperation des registres
        add rsp, 8
        pop rax
        pop rdx
        ; ----- sortie
        ret
    sub_creer_portion_pulseur_4: ; creation d'une portion pulseur (Num/PuissSeuil/ValBloc/Dest)
        ; Arguments : NumeroDePortion / PuissanceSeuilChrono / ValeurBlocBoutons / PortionDeDestination
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
            mov [rdx+OS_SEUIL_1], al ; puissance seuil chrono
            ;
            mov rax, [rsp+8*(3+2)]
            mov [rdx+OS_BSYNV_1], al ; valeur du bloc de boutons
            ;
            mov rax, [rsp+8*(3+1)]
            mov [rdx+OS_DESTN_4], dword eax ; numero de portion de destination
            ;
        ; ----- recuperation des registres
        add rsp, 8 ; re-alignement de la pile
        pop rax
        pop rdx
        ; ----- sortie
        ret
    sub_creer_segment_dendritique_7: ; creation d'un segment dendritique (Type/Num/NivTemp/DecEvo/SynaptDispo/PotSegRestant/SegSuivant)
        ; Pour création intiale : TYPE_SEGMENT_GERME / NumeroDePortion / NivTemporel / DecompteEvo / SynapsesRest / PotRest / 0
        ; Pour création forcée : TYPE_SEGMENT_COMPLET / NumeroDePortion / NivTemporel / 0 / 0 / 0 / SegmentSuivant
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
            mov rax, [rsp+8*(3+7)]
            mov [rdx+OS_TYPEVSA_1], al ; type
            ;
            mov rax, [rsp+8*(3+5)]
            mov [rdx+OS_NCACT_1], al ; niveau temporel d'activité
            ;
            mov rax, [rsp+8*(3+4)]
            mov [rdx+OS_DCEVO_1], al ; décompte d'évolution
            ;
            mov rax, [rsp+8*(3+3)]
            mov [rdx+OS_PSYNA_2], ax ; nombre de synapses disponibles
            ;
            mov rax, [rsp+8*(3+2)]
            mov [rdx+OS_PRSEG_1], al ; potentiel de segments restants
            ;
            mov rax, [rsp+8*(3+1)]
            mov [rdx+OS_SEGNS_4], eax ; segment ou neurone suivant
            ;
        ; ----- recuperation des registres
        add rsp, 8
        pop rax
        pop rdx
        ; ----- sortie
        ret
    sub_creer_portion_neurone_10: ; creation d'une portion neurone (Type/Num/PuissSeuil/BaseRef/PRay/PPlan/PApi/PPan/PAxo/ExtAxo)
        ; Pour création initiale : TYPE_NEURONE_GERME / NumeroPortion / PuissSeuil / BaseRefract / PRay / PPlan / PApi / PPan / PAxo / 0
        ; Pour création forcée : TYPE_NEURONE_COMPLET / NumeroPortion / PuissSeuil / BaseRefract / 0 / 0 / 0 / 0 / 0 / ExtensionAxonale
        ; ----- sauvegarde des registres utilises + alignement de la pile
        push rdx
        push rax
        sub rsp, 8 ; alignement pile
        ; ----- adresse de travail
        mov rdx, [rsp+8*(3+9)] ; numero de portion a creer
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
        mov rax, [rsp+8*(3+10)]
        mov [rdx+OS_TYPEVSA_1], al ; type
        ;
        mov rax, [rsp+8*(3+8)]
        mov [rdx+OS_SEUIL_1], al ; puissance seuil charge
        ;
        mov rax, [rsp+8*(3+7)]
        mov [rdx+OS_BREFR_1], al ; base refractaire
        ;
        mov rax, [rsp+8*(3+6)]
        mov [rdx+OS_PRAYO_1], al ; potentiel dendrites rayonnantes
        ;
        mov rax, [rsp+8*(3+5)]
        mov [rdx+OS_PPLAN_1], al ; potentiel dendrites rayonnantes
        ;
        mov rax, [rsp+8*(3+4)]
        mov [rdx+OS_PAPIC_1], al ; potentiel dendrites rayonnantes
        ;
        mov rax, [rsp+8*(3+3)]
        mov [rdx+OS_PPANI_1], al ; potentiel dendrites rayonnantes
        ;
        mov rax, [rsp+8*(3+2)]
        mov [rdx+OS_PAXON_1], al ; potentiel extension axonale
        ;
        mov rax, [rsp+8*(3+1)]
        mov [rdx+OS_EXTAX_4], eax ; numero portion extension axonale        ;
        ; ----- recuperation des registres
        add rsp, 8
        pop rax
        pop rdx
        ; ----- sortie
        ret
    sub_creer_extension_axonale_8: ; creation d'une extension axonale (Type/Num/MasBB/ValBB/NivTemp/Dest/PotAxoRestant/ExtSuivante)
        ; Pour création intiale : TYPE_EXTENSION_GERME / NumeroDePortion / 0 / 0 / 0 / 0 / PotAxoRest / 0
        ; Pour création forcée : TYPE_EXTENSION_COMPLET / NumeroDePortion / MasseBB / ValBB / NivTemporel / Dest / 0 / SegmentSuivant
        ; ----- sauvegarde des registres utilises + alignement de la pile
        push rdx
        push rax
        sub rsp, 8 ; alignement pile
        ; ----- adresse de travail
        mov rdx, [rsp+8*(3+7)] ; numero de portion a creer
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
            mov rax, [rsp+8*(3+8)]
            mov [rdx+OS_TYPEVSA_1], al ; type
            ;
            mov rax, [rsp+8*(3+6)]
            mov [rdx+OS_BSYNM_1], al ; masse du bloc de boutons
            ;
            mov rax, [rsp+8*(3+5)]
            mov [rdx+OS_BSYNV_1], al ; valeur du bloc de boutons
            ;
            mov rax, [rsp+8*(3+4)]
            mov [rdx+OS_NCACT_1], al ; niveau temporel d'activité
            ;
            mov rax, [rsp+8*(3+3)]
            mov [rdx+OS_DESTN_4], eax ; destination
            ;
            mov rax, [rsp+8*(3+2)]
            mov [rdx+OS_PREXT_1], al ; potentiel d'extensions restantes
            ;
            mov rax, [rsp+8*(3+1)]
            mov [rdx+OS_EXTAX_4], eax ; segment ou neurone suivant
            ;
        ; ----- recuperation des registres
        add rsp, 8
        pop rax
        pop rdx
        ; ----- sortie
        ret

    ; Creation de reseaux
    sub_creer_reseau: ; creation d'un reseau (......../Type/NumP/NbrHX/NbrVY/NbrPZ/Pas)
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
            mov rcx, [rsp+8*(11+7)] ; numDest à incrementer
            mov rdx, [rsp+8*(11+8)] ; numSuiv à incrémenter
            mov r8, [rsp+8*(11+1)] ; pas sur x
            mov r9, r8
            shl r9, DECALAGE_Y ; pas sur y
            mov r10, r8
            shl r10, DECALAGE_Z ; pas sur z
        ; ----- boucle
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
                        mov rax, [rsp+8*(11+6+6)]
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
                            scr_sc:
                                call_datapush_style sub_creer_segment_dendritique_7, \
                                TYPE_SEGMENT_COMPLET, rbx,    1,       0,      0,          0,             rcx
                                ; Type,               NumPor, NivTemp, DecEvo, SynapDispo, PotSegRestant, SegSuivant
                                jmp scr_suite
                            scr_nc:
                                call_datapush_style sub_creer_portion_neurone_10, \
                                TYPE_NEURONE_COMPLET, rbx,    VS_PSEUIL,  VS_BREFRC,   0,       0,        0,       0,       0,      rcx
                                ; Type,               NumPor, PuissSeuil, BaseRefract, PotDRay, PotDPlan, PotDApi, PotDPan, PotAxo, ExtAxo
                                jmp scr_suite
                            scr_ec:
                                call_datapush_style sub_creer_extension_axonale_8, \
                                TYPE_EXTENSION_COMPLET, rbx,    VS_MASBB,   VS_VALBB,    1,           rdx,  0,          rcx
                                ; Type,                 NumPor, MasseBlocB, ValeurBlocB, NivTemporel, Dest, PotAxoRest, SegmentSuivant
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
; ------------------------------------------------------------------------------------------------- PROCEDURE DE FENETRE
    global WindowProc
    WindowProc:
        ; recuperation des elements
            push rbp            ; Saves the base pointer (pile naturellement recalee)
            mov rbp, rsp        ; Saves the stack pointer for later use
            mov [rbp+16], rcx                                   ; hWnd
            mov [rbp+24], rdx                                   ; Msg
            mov [rbp+32], r8                                    ; wParam
            mov [rbp+40], r9                                    ; lParam
        ; envoi vers les actions selon le message
            mov rdx, qword [rbp+24]                             ; Msg
            cmp rdx, WM_KEYDOWN
                je cas_WM_KEYDOWN
            cmp rdx, WM_SIZE
                je cas_WM_SIZE
            cmp rdx, WM_PAINT
                je cas_WM_PAINT
            cmp rdx, WM_DESTROY
                je cas_WM_DESTROY
            jmp traitement_standard
        ; -----------------------------------------------------------------------------------------
        cas_WM_KEYDOWN:
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
                comparer_et_jump_si_egal al, 0x42, cas_WM_KEYDOWN_B ; mode pas à pas (toggle)
                comparer_et_jump_si_egal al, 0x4E, cas_WM_KEYDOWN_N ; avancer d'un pas
                ;
                comparer_et_jump_si_egal al, 0x55, cas_WM_KEYDOWN_U ; détail portion (toggle)
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
                ; changement du mode de tracage                    mov byte [modeTracage], 3
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
            cas_WM_KEYDOWN_B: ; touche b
                mov al, [mode_pas_a_pas]
                cmp al, 0
                je mettre_pasapas_a_1
                mov [mode_pas_a_pas], byte 0
                jmp fin_modifier_pasapas
                ; mettre a 0
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
            cas_WM_KEYDOWN_U: ; touche u
                mov al, [detailConsole]
                cmp al, 0
                je mettre_detail_a_1
                mov byte [detailConsole], 0
                jmp fin_modifier_detail
                ; mettre a 0
                mettre_detail_a_1:
                mov byte [detailConsole], 1
                fin_modifier_detail:
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
        ; -----------------------------------------------------------------------------------------
        cas_WM_SIZE:
            ; rax et sortie
                xor rax, rax
                jmp fin_WindowProc
        ; -----------------------------------------------------------------------------------------
        cas_WM_PAINT:
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
                adresse_from_numero r10, rax
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
                            mov dl, byte [r10+OS_BSYNV_1]
                            mov [r11+VERT], dl
                            jmp cWP_boucle_tracage_fin
                        cWP_tracage_mode_E2: ; mode E2 / charge actuelle ------------------------------
                            ; type
                            mov dl, [r10+OS_TYPEVSA_1]
                            and dl, MASQUE_TYPE_Txxx
                            comparer_et_jump_si_egal dl, 0b100_00_00_0, cWP_bte2_traiter
                            comparer_et_jump_si_egal dl, TYPE_SEGMENT_GERME, cWP_bte2_traiter
                            comparer_et_jump_si_egal dl, TYPE_NEURONE_GERME, cWP_bte2_traiter
                            jmp cWP_boucle_tracage_fin
                            ; ---------------- affichage charge/chrono => vert ou rouge
                            cWP_bte2_traiter:
                                mov dx, word [r10+OS_CHARG0_2]
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
        ; -----------------------------------------------------------------------------------------
        cas_WM_DESTROY:
            ; envoi du message de fin
                xor rcx, rcx
                sub rsp, SHADOW_SPACE_SIZE
                call PostQuitMessage
                add rsp, SHADOW_SPACE_SIZE
            ; rax et sortie
                xor rax, rax
                jmp fin_WindowProc
        ; -----------------------------------------------------------------------------------------
        traitement_standard:
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
        ; -----------------------------------------------------------------------------------------
        fin_WindowProc:
            mov rsp, rbp        ; Restores the stack pointer
            pop rbp             ; Restores the base pointer
            ret
