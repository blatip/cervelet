; ------------------------------------------------------------------------------------------------- LICENCE
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
    ; general
        extern GetModuleHandleA                 ; Recuperer le handle du module en cours
        extern GetLastError                     ; Recuperer la derniere erreur
        extern ExitProcess                      ; Fin du processus / kernel32
    ; pour console
        extern GetStdHandle                     ; Gestionnaire de peripheriques
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
        %macro decaler_numero_de_portion 4 ; effectuer un decalage sur le numero de portion
                ; %1 REG/IMM fix = numero de portion
                ; %2 REG/IMM fix = decalage sur x
                ; %3 REG/IMM fix = decalage sur y
                ; %4 REG mod = sortie numero modifie
            ; ----- calculs
                mov %4, %3
                shl %4, BITS_POUR_X
                add %4, %2
                add %4, %1
            ; ----- fin
            %endmacro


 
        %macro comparer_et_jump_si_egal 3 ; comparaison et jump si egalite / A_FAIRE : Remplacer par des comparer_al_valeur_vers_dl_AVCD
                ; %1 REG fix = valeur de referenceregistre 8 bits a comparer
                ; %2 REG/IMM fix = valeur a comparer (meme taille)
                ; %3 LABEL = label vers lequel sauter
            ; ----- comparaison et jump
                cmp %1, %2
                je %3
            ; ----- fin
            %endmacro
        %macro comparer_al_valeur_vers_dl_AVCD 4
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
        %macro recuperer_numero_par_decalage_FVA 3 ; recuperation d'un numero a une adresse
                ; %1 IN = adresse
                ; %2 IN/FIX = decalage pour numero
                ; %3 MOD = (rax)
            ; ----- recuperation du numero de portion
                xor rax, rax
                mov eax, [%1+%2]
            ; ----- fin
            %endmacro
        %macro soustraction_limitee_basse_SFA 3 ; soustraction avec limite basse a 0 / A_FAIRE : Voir si possible de supprimer
                ; %1 IN/OUT = registre valeur principale
                ; %2 IN/FIX = registre ou valeur
                ; %3 = (rax)
            ; ----- nouvelle methode
                xor rax, rax
                sub %1, %2
                cmovc %1, rax
            ; ----- ancienne methode
                ; cmp %1, %2
                ; ja %%slba_ok_soustraction
                ;     xor %1, %1
                ; jmp %%slba_pas_soustraction
                ; %%slba_ok_soustraction:
                ;     sub %1, %2
                ; %%slba_pas_soustraction:
            ; ----- fin
            %endmacro
        %macro limitation_registre_a_word_SM 2 ; limitation de la valeur d'un registre a 0xFFFF / A_FAIRE : Voir si possible de supprimer
                ; %1 IN/MOD = registre
                ; %2 (rax)
            ; ----- limitation haute
                mov rax, 0xFFFF
                cmp %1, rax
                cmova %1, rax
            ; ----- ancienne methode
                ; cmp %1, 0xFFFF
                ; jbe %%lraf_pas_de_limitation_haute
                ;     mov %1, 0xFFFF
                ; %%lraf_pas_de_limitation_haute:
            ; ----- fin
                %endmacro
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
    ; outils specifiques
        %macro chercher_portion_libre_FASMSTT 7
                ; %1 REG/IMM fix = numero de portion
                ; %2 = (rax)
                ; %3 REG mod
                ; %4 REG mod = sortie numero de portion
                ; %5 REG mod = sortie adresse portion
                ; %6 Texte : label pour jump si un numero dispo a ete trouve
                ; %7 Texte : label pour jump si pas de numero dispo trouve
            ; construction
                mov %4, %1 ; numero de portion (evo par inc)
                mov %3, NOMBRE_DE_PORTIONS ; plafond du numero de portion
                mov rax, %1
                adresse_from_numero %5, rax ; adresse de la portion (evo par add)
                %%cpl_rechercher:
                    add %5, TAILLE_DES_PORTIONS
                    inc %4
                    cmp %4, %3
                    jae %7
                    mov al, byte [%5]
                    cmp al, TYPE_VIERGE
                    je %6
                jmp %%cpl_rechercher
            ; fin
            %endmacro
        %macro creer_liaison_vierge_FN 2
                ; %1 REG/IMM fix = adresse de la portion
                ; %2 REGnum/IMM fix = numero de portion destination
            ; creation
                ; creer la liaison r11/r12 vers r13/r14
                mov [%1], byte TYPE_LIAISON_NONACTIVEE
                mov [%1+OS_LIAIS_L_4], dword 0 ; numero de liaison suivante
                mov [%1+OS_VALBB_1], byte VALEUR_BOUTONS_STANDARD ; valeur bloc
                mov [%1+OS_MASBB_1], byte MASSE_BOUTONS_STANDARD ; masse bloc
                mov [%1+07], byte 0 ; N/U
                mov [%1+OS_DESTN_4], %2d ; numero de destination
            ; fin
            %endmacro
        %macro tester_retroactivite_adresse_FA 2 ; test portion adresse retroactivee (drapeau e = portion retroactivee)
                ; %1 REG/IMM fix = adresse portion destination
                ; %2 (rax) = type de la portion
            ; ----- sortie
                ; si adresse retroactivee alors drapeau e active
            ; ----- lire le type de la destination
                mov al, [%1]
            ; ----- sauts si type retroactive
                comparer_et_jump_si_egal al, TYPE_SIMPLE_NONACTIVEE_RETROACTIVE, %%tra_suite
                comparer_et_jump_si_egal al, TYPE_SIMPLE_POSTACTIVEE_RETROACTIVE, %%tra_suite
                comparer_et_jump_si_egal al, TYPE_MERE_RETROACTIVE, %%tra_suite
                ; ici le drapeau d'egalite est forcement faux
            ; ----- fin
                %%tra_suite:
                %endmacro
        %macro tester_activite_adresse 2 ; test portion adresse active ou post-active (drapeau e = portion retroactivee)
                ; %1 IN/FIX = adresse portion destination (r11)
                ; %2 MOD = (rax)
            ; ----- sortie
                ; si adresse active alors drapeau e active
            ; ----- lire le type de la destination
                mov al, [%1]
            ; ----- sauts si type active
                comparer_et_jump_si_egal al, TYPE_SIMPLE_ACTIVEE, %%ta_suite
                comparer_et_jump_si_egal al, TYPE_SIMPLE_ACTIVEE_RETROACTIVE, %%ta_suite
                comparer_et_jump_si_egal al, TYPE_SIMPLE_POSTACTIVEE, %%ta_suite
                comparer_et_jump_si_egal al, TYPE_SIMPLE_POSTACTIVEE_RETROACTIVE, %%ta_suite
                comparer_et_jump_si_egal al, TYPE_LIAISON_ACTIVEE, %%ta_suite
                comparer_et_jump_si_egal al, TYPE_LIAISON_POSTACTIVEE, %%ta_suite
                ; ici le drapeau d'egalite est forcement faux
            ; ----- fin
                %%ta_suite:
                %endmacro
        %macro tester_charge 3 ; test de charge par rapport au seuil (drapeau b = seuil non atteint )
                ; %1 IN = adresse
                ; %2 MOD = (rax) -> charge
                ; %3 MOD = (rcx) -> seuil
            ; ----- charge actuelle
                mov ax, [%1+OS_CHARG_2]
            ; ----- seuil charge
                mov cx, [%1+OS_SEUIL_2]
            ; ----- test declenchement
                cmp ax, cx
            ; ----- fin
                %endmacro
    ; lois de fonctionnement
        %macro surcharger_portion_de_destination_FFACD 5 ; surcharge d'une portion de destination
                ; %1 IN/FIX = adresse portion actuelle (simple activee ou liaison activee)
                ; %2 IN/FIX = adresse portion destination (simple ou mere)
                ; %3 MOD = (rax)
                ; %4 MOD = (rcx)
                ; %5 MOD = (rdx)
            ; ----- fonctionnement
                ; pour portions : pulseur, simple, liaison ; quand le seuil est atteint
                ; si la destination n'est pas refractaire ([%2+OS_CREFR_1]=0)
                ; on ajoute la surcharge [%1+OS_VALBB_1]byte a [%2+OS_CHARG_2]word (avec limitation)
            ; ----- 
                xor rdx, rdx
                mov dx, word [%2+OS_CHARG_2] ; charge actuelle de la destination
                xor rcx, rcx
                mov cl, byte [%1+OS_VALBB_1] ; surcharge a appliquer (reference incluse)
                add rdx, rcx ; nouvelle charge (reference incluse)
                mov cl, byte REFERENCE_BOUTONS
                soustraction_limitee_basse_SFA rdx, rcx, (rax) ; correction charge avec limitation basse
                limitation_registre_a_word_SM rdx, (rax) ; limitation haute
                mov [%2+OS_CHARG_2], dx ; enregistrement de la nouvelle charge
            ; ----- fin
                %endmacro
        %macro decompte_refractaire 3
                ; %1 IN/FIX = registre contenant l'adresse d'une portion (simple ou mere)
                ; %2 MOD = (rax)
                ; %3 MOD = (rbx)
            ; ------
                mov al, [%1+OS_CREFR_1]
                mov bl, 1
                test al, al
                cmovz ax, bx
                dec al
                mov [%1+OS_CREFR_1], al
            ; ----- fin
                %endmacro
        %macro fuite_de_charge_portion 2 ; ramener progressivement la charge vers CHARGE_INITIALE
                ; %1 IN/FIX = registre contenant l'adresse d'une portion (simple ou mere)
                ; %2 MOD = (rax)
            ; ----- fonctionnement
                ; si charge > CHARGE_INITIALE : reduire charge de VITESSE_DECHARGE
                ; si charge < CHARGE_INITIALE : augmenter charge de VITESSE_DECHARGE
            ; ------
                xor rax, rax
                mov ax, [%1+OS_CHARG_2]
                cmp ax, CHARGE_INITIALE
                jb %%fcp_augmenter
                ja %%fcp_reduire
                jmp %%fcp_fin
            ; cas ax < CHARGE_INITIALE : augmenter
                %%fcp_augmenter:
                add ax, VITESSE_DECHARGE
                cmp ax, CHARGE_INITIALE
                jbe %%fcp_ecriture
                    mov ax, CHARGE_INITIALE
                je %%fcp_ecriture
            ; cas ax > CHARGE_INITIALE : reduire
                %%fcp_reduire:
                sub ax, VITESSE_DECHARGE
                cmp ax, CHARGE_INITIALE
                jae %%fcp_ecriture
                    mov ax, CHARGE_INITIALE
                jmp %%fcp_ecriture
            ; ----- fin
                %%fcp_ecriture:
                mov [%1+OS_CHARG_2], ax
                %%fcp_fin:
            %endmacro
    ; lois d'evolution
        %macro evolution_seuil_d_activation 1 ; inactif et non utilise pour le moment
            ; Pas d'evolution du seuil d'activation
            %endmacro
        %macro positiver_bloc_de_boutons 3 ; augmentation bloc d'une portion post-active suite destination retroactive
                            ; %1 IN = adresse
                            ; %2 MOD = (rax)
                            ; %3 MOD = (rdx)
                        ; ----- octet aleatoire -> dl
                            rdtsc
                            ; bits 8 et 4 -> 1 et 5
                            rol al, 1
                            mov dl, al
                            and dl, 0b0001_0001
                            ; bits 7 et 3 -> 2 et 6
                            rol al, 2
                            mov ah, al
                            and ah, 0b0010_0010
                            or dl, ah
                            ; bits 6 et 2 -> 3 et 7
                            rol al, 2
                            mov ah, al
                            and ah, 0b0100_0100
                            or dl, ah
                            ; bits 5 et 1 -> 4 et 8
                            rol al, 2
                            mov ah, al
                            and ah, 0b1000_1000
                            or dl, ah
                        ; ----- masse du bloc de boutons
                            mov al, [%1+OS_MASBB_1]
                        ; ----- ne faire le travail que si al < dl
                            cmp al, dl
                            jae %%fin_positiver
                            ; ici al <> 255
                        ; ----- incrementer + enregister
                            inc al
                            mov [%1+OS_MASBB_1], al
                        ; ----- valeur decalee du bloc de boutons
                            mov al, [%1+OS_VALBB_1]
                            cmp al, 255
                            je %%fin_positiver
                                inc al
                                mov [%1+OS_VALBB_1], al
                            %%fin_positiver:
                        ; ----- fin
                            %endmacro
        %macro negativer_bloc_de_boutons 3 ; diminution bloc d'une portion non active suite destination retroactive
                            ; %1 IN = adresse
                            ; %2 MOD = (rax)
                            ; %3 MOD = (rdx)
                        ; ----- octet aleatoire -> dl
                            rdtsc
                            ; bits 8 et 4 -> 1 et 5
                            rol al, 1
                            mov dl, al
                            and dl, 0b0001_0001
                            ; bits 7 et 3 -> 2 et 6
                            rol al, 2
                            mov ah, al
                            and ah, 0b0010_0010
                            or dl, ah
                            ; bits 6 et 2 -> 3 et 7
                            rol al, 2
                            mov ah, al
                            and ah, 0b0100_0100
                            or dl, ah
                            ; bits 5 et 1 -> 4 et 8
                            rol al, 2
                            mov ah, al
                            and ah, 0b1000_1000
                            or dl, ah
                        ; ----- masse du bloc de boutons
                            mov al, [%1+OS_MASBB_1]
                        ; ----- ne faire le travail que si al < dl
                            cmp al, dl
                            jae %%fin_negativer
                            ; ici al <> 255
                        ; ----- incrementer + enregister
                            inc al
                            mov [%1+OS_MASBB_1], al
                        ; ----- valeur decalee du bloc de boutons
                            mov al, [%1+OS_VALBB_1]
                        ; ----- comparer a 0, et decrementer et enregistrer
                            cmp al, 0
                            je %%fin_negativer
                                dec al
                                mov [%1+OS_VALBB_1], al
                            %%fin_negativer:
                        ; ----- fin
                            %endmacro
        %macro pousse_de_liaison 1 ; A FAIRE ET UTILISER
            ; une liaison se cree quand ?
            %endmacro
    ; macros hors boucle
        %macro make_a_call 1-*
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
        %macro make_a_winapi64_style_call 1-*
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
        %macro ligne_de_valeur 7
            ; ----- parametres
                ; %1 IN = handle sortie standard
                ; %2 IN = adresse
                ; %3 TXT = offset
                ; %4 IN = registre a la bonne longueur
                ; %5 IN = registre 64 bits associe
                ; %6 IN = texte de presentation
                ; %7 IN = longueur du texte de presentation
                ; exemple : ligne_de_valeur r15, rdx, OS_CREFR_1, cx, rcx, portion_chrs_text, PORTION_CHRS_LONG
            ; -----
                xor %5, %5
                mov %4, [%2+%3]
                make_a_winapi64_style_call WriteConsoleA, %1, %6, %7, reponse_long_ret
                make_a_winapi64_style_call convertir, %5, nombre_text, NOMBRE_LONG
                make_a_winapi64_style_call WriteConsoleA, %1, nombre_text, NOMBRE_LONG, reponse_long_ret
            %endmacro
section .data ; ----------------------------------------------------------------------------------- PARAMETRES
    ; dimensionnement cerveau
        BITS_POUR_X: equ 10 ; => DIMENSION_X 1024
            DECALAGE_X: equ 0
            MASQUE_X: equ (1<<DECALAGE_X)*((1<<BITS_POUR_X)-1)
            DIMENSION_X: equ (1<<BITS_POUR_X)
        BITS_POUR_Y: equ 9 ; => DIMENSION_Y 512
            DECALAGE_Y: equ BITS_POUR_X
            MASQUE_Y: equ (1<<DECALAGE_Y)*((1<<BITS_POUR_Y)-1)
            DIMENSION_Y: equ (1<<BITS_POUR_Y)
        NOMBRE_DE_PORTIONS: equ DIMENSION_X*DIMENSION_Y
        TAILLE_DES_PORTIONS: equ 16 ; en octets
        SURALIGNEMENT_PORTIONS: equ 15 ; = 16-1 pour alignement sur charge (2) et liaison de liaisons (4)
        ; tests : 2^(10+9) *2 *12 => 12+2 Mo / 350 Hz
        ; prevoir : 2^(12+11) *2 *12 => 96+2 Mo / 45 Hz
    ; reglages
        ; communs
            REFERENCE_BOUTONS: equ 64 ; => Blocs RZD-64 a RZD+191
            DUREE_REFRACTAIRE: equ 10 ; maxi 255
        ; lecteurs
            ; Persistance initiale = 0
            PERSISTANCE_STANDARD: equ 255 ; maxi 255
            VALEUR_BOUTONS_LECTEUR: equ 255 ; maxi 255
            TAILLE_SEGMENT_STANDARD: equ 1 ; maxi 256/8=32
        ; pulseurs
            ; Chrono initial = 0
            INCREMENT_PULSEURS: equ 100
            SEUIL_CHRONO_STANDARD: equ 65_000 ; maxi 65535
            VALEUR_BOUTONS_PULSEUR: equ REFERENCE_BOUTONS + 191 ; maxi 255
        ; neurones
            CHARGE_INITIALE: equ 512 ; VITESSE_DECHARGE-1 < ... < 65535+1-VITESSE_DECHARGE
            ; increment par surcharge
            SEUIL_STANDARD: equ CHARGE_INITIALE + 450 ; maxi 65535
            VALEUR_BOUTONS_STANDARD: equ REFERENCE_BOUTONS + 80 ; maxi 255
            MASSE_BOUTONS_STANDARD: equ 32 ; maxi 255
            VITESSE_DECHARGE: equ 0
        ; liaisons
            VALEUR_BOUTONS_LIAISONS: equ REFERENCE_BOUTONS + 100 ; maxi 255
        ; bouclages
                ; estimation : rapide x moyen = 350 => 1 seconde
            ; dans boucle rapide : traitement des portions
            BOUCLAGE_RAPIDE_A_FAIRE: equ 35
            ; dans boucle moyenne : affichages graphiques
            BOUCLAGE_MOYEN_A_FAIRE: equ 10
            ; dans bouclege lent : ecritures et controles
            ; BOUCLAGE LENT : systematique sauf sur fermeture fenetre
            PORTION_A_DETAILLER: equ 16+128*LARGEUR_DE_COUCHE
            ; PORTION_A_DETAILLER: equ 12+10*LARGEUR_DE_COUCHE
            ; PORTION_A_DETAILLER: equ 30+10*LARGEUR_DE_COUCHE
    ; constantes de types de portions
        ; pour info - approximatif
            ; bit 8 : capacite a la retroactivite
            ; bit 7 : capacite a l'activite
            ; bit 6 : emetteur simple (pulseur)
            ; bit 5 : 
            ; bit 4 : interface de sortie
            ; bit 3 : retroactivite
            ; bit 2 : postactivite
            ; bit 1 : activite immediate
        ; entrees
            TYPE_ENTREE: equ                            0b0001_0000 ; fonction a definir
            TYPE_LECTEUR: equ                           0b0011_0000
            TYPE_PULSEUR: equ                           0b0010_0000
        ; simple
            TYPE_SIMPLE_NONACTIVEE: equ                 0b1100_0000
            TYPE_SIMPLE_NONACTIVEE_RETROACTIVE: equ     0b1100_0100
            TYPE_SIMPLE_ACTIVEE: equ                    0b1100_0011
            TYPE_SIMPLE_ACTIVEE_RETROACTIVE: equ        0b1100_0111
            TYPE_SIMPLE_POSTACTIVEE: equ                0b1100_0010
            TYPE_SIMPLE_POSTACTIVEE_RETROACTIVE: equ    0b1100_0110
        ; mere
            TYPE_MERE: equ                              0b1000_0000
            TYPE_MERE_RETROACTIVE: equ                  0b1000_0100
        ; liaison (imperativement apres les autres, et remplies dans l'ordre)
            TYPE_LIAISON_NONACTIVEE: equ                0b0100_0000
            TYPE_LIAISON_ACTIVEE: equ                   0b0100_0011
            TYPE_LIAISON_POSTACTIVEE: equ               0b0100_0010
        ; sorties
            TYPE_SORTIE: equ                            0b0000_1000 ; fonction a definir
        ; non exploitables
            TYPE_VIERGE: equ                            0b0000_0000 ; (ne jamais reutiliser ; pourquoi ?)
            TYPE_CONSECUTIF: equ                        0b1111_1111
    ; constates de positionnement de donnees
        OS_LIAIS_L_4:   equ 1 ; (4) valide pour liaison (liaison sur liaison)
        OS_CHARG_2:     equ 1 ; (2) valide pour pulseur/simple/mere (charge)
        OS_SEUIL_2:     equ 3 ; (2) valide pour simple/mere (seuil de charge)
        OS_CREFR_1:     equ 5 ; (1) valide pour simple/mere (decompteur refractaire)
        OS_BREFR_1:     equ 6 ; (1) valide pour simple/mere (base refractaire)
        ; OS synapses disponibles (1)
        OS_EXPDX_L_1:   equ 8 ; (1) valide pour simple/mere/liaison (exploration sur x)
        OS_EXPDY_L_1:   equ 9 ; (1) valide pour simple/mere/liaison (rxploration sur y)
        ; OS potentiel restant de synapses (2)
        OS_EXPDX_SM_1:  equ 10 ; (1) valide pour simple/mere/liaison (exploration sur x)
        OS_EXPDY_SM_1:  equ 11 ; (1) valide pour simple/mere/liaison (rxploration sur y)
        OS_VALBB_1:     equ 10 ; (1) valide pour annexedelecteur/pulseur/simple/liaison (valeur du bloc de boutons)
        OS_MASBB_1:     equ 11 ; (1) valide pour simple/liaison (masse du bloc de boutons)
        OS_DESTN_4:     equ 12 ; (4) valide pour annexedelecteur/pulseur/simple/liaison
        OS_LIAIS_M_4:   equ 12 ; (4) valide pour mere
    ; constantes de fonctionnement
        ; Effets
            EFFET_OCTET_PLUS_LIMITE: equ    1
            EFFET_OCTET_MOINS_LIMITE: equ   2
    ; initialisations
        LARGEUR_DE_COUCHE: equ DIMENSION_X
        HAUTEUR_TOTALE: equ DIMENSION_Y
    ; dimensions graphiques
        LARGEUR_FENETRE: equ DIMENSION_X
        HAUTEUR_FENETRE: equ DIMENSION_Y
        NOMBRE_OCTET_PAR_POINT: equ 3
        COMPLEMENT_LIGNE_DWORD: equ ((LARGEUR_FENETRE*NOMBRE_OCTET_PAR_POINT*8+7)/8) % 4
    ; messages et console
        message_0_text: db 'Saisir un texte :', 10
        MESSAGE_0_LONG: equ $ - message_0_text
        message_1_text: db 'Handle de base : '
        MESSAGE_1_LONG: equ $ - message_1_text
        message_2_text: db 'Handle de drawing : '
        MESSAGE_2_LONG: equ $ - message_2_text
        message_3_text: db 'Handle de DIB : '
        MESSAGE_3_LONG: equ $ - message_3_text
        message_4_text: db 'Adresse de DIB : '
        MESSAGE_4_LONG: equ $ - message_4_text
    ; messages de portions
        ; ----- type de portion
            portion_type_simple_text: db '#################### Type simple : '
                PORTION_TYPE_SIMPLE_LONG: equ $ - portion_type_simple_text
            portion_type_mere_text: db '#################### Type mere : '
                PORTION_TYPE_MERE_LONG: equ $ - portion_type_mere_text
        ; ----- communs
            portion_numr_text: db 'Numero de la portion : '
                PORTION_NUMR_LONG: equ $ - portion_numr_text
            portion_type_text: db 'Type de portion : '
                PORTION_TYPE_LONG: equ $ - portion_type_text
            portion_chrg_text: db 'Charge actuelle : '
                PORTION_CHRG_LONG: equ $ - portion_chrg_text
            portion_chrs_text: db 'Seuil charge : '
                PORTION_CHRS_LONG: equ $ - portion_chrs_text
            portion_cref_text: db 'Compteur refractaire : '
                PORTION_CREF_LONG: equ $ - portion_cref_text
            portion_bref_text: db 'Base refractaire : '
                PORTION_BREF_LONG: equ $ - portion_bref_text
            portion_blcv_text: db 'Valeur du bloc de boutons : '
                PORTION_BLCV_LONG: equ $ - portion_blcv_text
            portion_blcp_text: db 'Masse du bloc de boutons : '
                PORTION_BLCP_LONG: equ $ - portion_blcp_text
            portion_dest_text: db 'Portion de destination : '
                PORTION_DEST_LONG: equ $ - portion_dest_text
            portion_lias_text: db 'Portion de liaison : '
                PORTION_LIAS_LONG: equ $ - portion_lias_text
        ; ----- specifiques lecteurs
            portion_adrs_text: db 'Adresse source : '
                PORTION_ADRS_LONG: equ $ - portion_adrs_text
            portion_nseg_text: db 'Nombre de segments : '
                PORTION_NSEG_LONG: equ $ - portion_nseg_text
            portion_tseg_text: db 'Taille des segments : '
                PORTION_TSEG_LONG: equ $ - portion_tseg_text
            portion_idxl_text: db 'Index de lecture : '
                PORTION_IDXL_LONG: equ $ - portion_idxl_text
            portion_prsb_text: db 'Persistance de base : '
                PORTION_PRSB_LONG: equ $ - portion_prsb_text
            portion_bitl_text: db 'Numero de bit a lire : '
                PORTION_BITL_LONG: equ $ - portion_bitl_text
        ; ----- specifiques pulseurs
            portion_chro_text: db 'Chrono actuel : '
                PORTION_CHRO_LONG: equ $ - portion_chro_text
            portion_chrl_text: db 'Limite chrono : '
                PORTION_CHRL_LONG: equ $ - portion_chrl_text
        ; ----- speciaux
            portion_nonu_text: db 'Non utilise : '
                PORTION_NONU_LONG: equ $ - portion_nonu_text
    ; autres messages
        interligne_text: db '----------------------------', 10
        INTERLIGNE_LONG: equ $ - interligne_text
        personalise_text: db '>>> On est passe ici <<<', 10
        PERSONALISE_LONG: equ $ - personalise_text
        nombre_text: db 24 dup ('.')
        NOMBRE_LONG: equ $ - nombre_text
        REPONSE_LONG_MAX: equ 12
    ; pour fenetre
        windowClassName: db 'ClasseDeFenetre', 0
        WINDOWCLASSNAME_LONG: equ $ - windowClassName
        windowName: db 'FenetreDeTracage', 0
        WINDOWNAME_LONG: equ $ - windowName
        NOM_FENETRE_A0: db 'Types de portions', 0
        NOM_FENETRE_Z1: db 'Valeurs du bloc de boutons', 0
        NOM_FENETRE_E2: db 'Charges et chronos', 0
        NOM_FENETRE_R3: db 'Activation et retro-activation', 0
    ; pour lecture du fichier
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
    ; pour utilitaires et console
        reponse_text: resb REPONSE_LONG_MAX
        input_record: resw 64 ; 2/...
        reponse_long_ret: resd 1 ; nombre de caracteres ecrits/lus ou d'evenements console/clavier
    ; pour boucles exterieures
        bouclage_rapide_actuel: resq 1
        bouclage_rapide_en_cours: resq 1
        bouclage_moyen_actuel: resq 1
        bouclage_lent_actuel: resq 1
    ; pour cerveau
        align 16
        resb SURALIGNEMENT_PORTIONS ; calage optimal sur les acces le plus frequents superiaurs a un octet
        portions: times NOMBRE_DE_PORTIONS resb TAILLE_DES_PORTIONS
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
    ; divers
        modeTracage: resb 1
        detailConsole: resb 1
        dureeDeTraitement: resq 1
        lenteur: resb 1
    ; pour lecture fichier
        Filehandle: resq 1
        contenu_du_fichier: resw NOMBRE_A_LIRE
        pointeur_entree: resq 1
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
            mov byte [detailConsole], 0
            mov byte [lenteur], 0
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

    ; =============================================== VARIABLES D ENTREE

        ; make_a_call lgn_variable_in, adr=1, TYPE_OCTET
        ; A utiliser par le paquet de lecture de texte, au travers de la variable Idx
        ; Elle sera (plus tard) modifiee par le cerveau lui-meme (par 2 neurones - et +)

    ; =============================================== VARIABLES DE SORTIE

        ; TYPE_SORTIE
        ; make_a_call sub_creer_portion_sortie, adr=LARGEUR_DE_COUCHE-2, Charge(1/2), EffetDeSortie(3), Adresse(4/5/6/7/8/9/10/11)
        ; modifie par l'effet d'un bloc de boutons sur la charge
        ; retombe directement a la valeur de reference apres realisation de l'effet (selon la charge ?)

    ; =============================================== TOUTES NEURONES INACTIVES
        make_a_call res_base_de_meres,     100*LARGEUR_DE_COUCHE,             4, DIMENSION_X/2, DIMENSION_Y/2
        ; make_a_call res_base_de_simples,   DIMENSION_X/2, 8, DIMENSION_X/2, DIMENSION_Y/1
    ; =============================================== LECTURE DU TEXTE
        lea r10, [contenu_du_fichier] ; adresse des donnees
        ;                                         Num             Adr      NbSeg     Idx Ht           Dest
        make_a_call pqt_lecture_std_GD, 10+10*LARGEUR_DE_COUCHE, r10, NOMBRE_A_LIRE, 0, 50, 30+10*LARGEUR_DE_COUCHE
        ; Arguments :                  NumeroDePortion / NombreALHorizontale / NombreALaVerticale / PortionDeDestination
        make_a_call pqt_simples_std_GD, 30+10*LARGEUR_DE_COUCHE, 16, 50, 50+10*LARGEUR_DE_COUCHE
        ; make_a_call pqt_simples_std_GD, 50+10*LARGEUR_DE_COUCHE, 16, 100, 70+10*LARGEUR_DE_COUCHE
        ; Rajouter la prise en compte d'une variable dans idx ? comment ?
    ; =============================================== ESSAIS D'IA AVEC AUTO-APPRENTISSAGE
        ; make_a_call pqt_identification, 59*LARGEUR_DE_COUCHE+10+20, 8, 256, 60*LARGEUR_DE_COUCHE+10+30
        ; make_a_call sgt_vrt_simples_std_GD, 60*LARGEUR_DE_COUCHE+10+30, 256, 60*LARGEUR_DE_COUCHE+10+40
    ; =============================================== TESTS UNITAIRES
        make_a_call sub_creer_portion_pulseur, 10+128*LARGEUR_DE_COUCHE, SEUIL_CHRONO_STANDARD, VALEUR_BOUTONS_PULSEUR, 16+128*LARGEUR_DE_COUCHE
        ; make_a_call sub_creer_portion_pulseur, 10+132*LARGEUR_DE_COUCHE, SEUIL_CHRONO_STANDARD, VALEUR_BOUTONS_PULSEUR, 16+132*LARGEUR_DE_COUCHE
        ; make_a_call sub_creer_portion_pulseur, 10+136*LARGEUR_DE_COUCHE, SEUIL_CHRONO_STANDARD, VALEUR_BOUTONS_PULSEUR, 16+136*LARGEUR_DE_COUCHE
        ; make_a_call sub_creer_portion_pulseur, 10+140*LARGEUR_DE_COUCHE, SEUIL_CHRONO_STANDARD, VALEUR_BOUTONS_PULSEUR, 16+140*LARGEUR_DE_COUCHE
        ; make_a_call sub_creer_portion_mere, LARGEUR_DE_COUCHE*200+120, CHARGE_INITIALE, SEUIL_STANDARD, DUREE_REFRACTAIRE, LARGEUR_DE_COUCHE*200+130
        ; make_a_call sub_creer_portion_liaison, LARGEUR_DE_COUCHE*200+130, VALEUR_BOUTONS_STANDARD, MASSE_BOUTONS_STANDARD, LARGEUR_DE_COUCHE*200+140, 0
        ; make_a_call sub_creer_portion_simple, LARGEUR_DE_COUCHE*200+140, CHARGE_INITIALE, SEUIL_STANDARD, DUREE_REFRACTAIRE, VALEUR_BOUTONS_STANDARD, MASSE_BOUTONS_STANDARD, LARGEUR_DE_COUCHE*200+150
    ; =============================================== FIN CREATIONS
; ------------------------------------------------------------------------------------------------- ANNONCES
        make_a_winapi64_style_call GetStdHandle, STD_OUTPUT_HANDLE
        mov r15, rax
    ; Interligne
        make_a_winapi64_style_call WriteConsoleA, r15, interligne_text, INTERLIGNE_LONG, reponse_long_ret
    ; Texte de lecture
        make_a_winapi64_style_call WriteConsoleA, r15, contenu_du_fichier, 450, reponse_long_ret ; -> NOMBRE_A_LIRE au lieu de 450
    ; Interligne
        make_a_winapi64_style_call WriteConsoleA, r15, interligne_text, INTERLIGNE_LONG, reponse_long_ret
    ; Instance de l'application
        make_a_winapi64_style_call WriteConsoleA, r15, message_1_text, MESSAGE_1_LONG, reponse_long_ret
        make_a_winapi64_style_call convertir, qword [Instance], nombre_text, NOMBRE_LONG
        make_a_winapi64_style_call WriteConsoleA, r15, nombre_text, NOMBRE_LONG, reponse_long_ret
    ; Atome de classe de fenetre
        make_a_winapi64_style_call WriteConsoleA, r15, windowClassName, WINDOWCLASSNAME_LONG, reponse_long_ret
        make_a_winapi64_style_call convertir, qword [ClassAtom], nombre_text, NOMBRE_LONG
        make_a_winapi64_style_call WriteConsoleA, r15, nombre_text, NOMBRE_LONG, reponse_long_ret
    ; Handle de la fenetre 
        make_a_winapi64_style_call WriteConsoleA, r15, windowName, WINDOWNAME_LONG, reponse_long_ret
        make_a_winapi64_style_call convertir, qword [Windowhandle], nombre_text, NOMBRE_LONG
        make_a_winapi64_style_call WriteConsoleA, r15, nombre_text, NOMBRE_LONG, reponse_long_ret
    ; Interligne
        make_a_winapi64_style_call WriteConsoleA, r15, interligne_text, INTERLIGNE_LONG, reponse_long_ret

; ------------------------------------------------------------------------------------------------- BOUCLAGES AVANT
    mov qword [bouclage_lent_actuel], 0
    bouclage_lent: ; Boucle lente pour ecrire des infos et choisir de sortir
        mov qword [bouclage_moyen_actuel], BOUCLAGE_MOYEN_A_FAIRE
        bouclage_moyen: ; Boucle moyenne pour redessiner regulierement
            mov rax, BOUCLAGE_RAPIDE_A_FAIRE
            ; temporisation
            mov cl, [lenteur]
            cmp cl, 23
            jbe bouclage_moyen_inchange
                sub cl, 23
                shr rax, cl
                or rax, 1
            bouclage_moyen_inchange:
            mov qword [bouclage_rapide_actuel], rax
            mov qword [bouclage_rapide_en_cours], rax
            bouclage_rapide: ; Boucle rapide pour arriver a 1/10 secondes environ
                ; initialisation calcul de la duree du bouclage sur portions
                xor rax, rax
                rdtsc ; -> edx:eax
                shl rdx, 32
                or rax, rdx
                ; stockage time stamp initial
                ; et pre-alignement pile pour push r8
                push rax
                ; initialisation bouclage sur portions
                mov r8, 0
                bouclage_portions:
                    push r8
; -------------------------------------------------------------------------------------------- TRAITEMENT PORTION r8
                    ; adresse r9 et aiguillage
                        ; calcul adresse portion a traiter
                        adresse_from_numero r9, r8
                        ; recuperation du numero de portion
                        pop r8
                        ; recuperation du type de portion
                        mov al, byte [r9]
                        ; re-sauvegarde du numero de portion
                        push r8
                        ; saut vers traitement concerne
                        comparer_et_jump_si_egal al, TYPE_SIMPLE_NONACTIVEE,               portion_simple_nonactivee
                        comparer_et_jump_si_egal al, TYPE_SIMPLE_NONACTIVEE_RETROACTIVE,   portion_simple_nonactivee_retroactive
                        comparer_et_jump_si_egal al, TYPE_SIMPLE_POSTACTIVEE,              portion_simple_postactivee
                        comparer_et_jump_si_egal al, TYPE_SIMPLE_POSTACTIVEE_RETROACTIVE,  portion_simple_postactivee_retroactive
                        comparer_et_jump_si_egal al, TYPE_LIAISON_NONACTIVEE,              portion_liaison_nonactivee
                        comparer_et_jump_si_egal al, TYPE_LIAISON_POSTACTIVEE,             portion_liaison_postactivee
                        comparer_et_jump_si_egal al, TYPE_LIAISON_ACTIVEE,                 portion_liaison_activee
                        comparer_et_jump_si_egal al, TYPE_MERE,                            portion_mere
                        comparer_et_jump_si_egal al, TYPE_MERE_RETROACTIVE,                portion_mere_retroactive
                        comparer_et_jump_si_egal al, TYPE_PULSEUR,                         portion_pulseur
                        comparer_et_jump_si_egal al, TYPE_LECTEUR,                         portion_lecteur
                        ; ne pas traiter les portions TYPE_CONSECUTIF
                        ; ne pas traiter les portions de type 0
                        jmp fin_traitement

                    ; portions speciales
                    portion_lecteur:
                        ; ----- adresse memoire a lire
                        mov r11, qword [r9+1] ; adresse de base
                        xor r12, r12
                        mov r12b, byte [r9+11] ; taille des segments (maxi 8)
                        xor rax, rax
                        mov ax, word [r9+TAILLE_DES_PORTIONS+1] ; index de lecture
                        ; RAJOUTER ICI LA VARIABLE DE POSITION DE LA TETE (RELATIF)
                        ; ET BOUCLER SUR MAXI NOMBRE_A_LIRE
                        mul r12d ; => r12d:eax ?
                        and rax, 0x00ffffff ; effacement de la partie haute de rax
                        add r11, rax ; -> adresse de base a lire
                        ; ----- lecture du bit de donnee
                        mov rax, qword [r11] ; contenu du segment (plus le suite jusqu'a 8 octets)
                        xor rcx, rcx
                        mov cl, byte [r9+TAILLE_DES_PORTIONS+4] ; numero de bit a lire
                        inc cl
                        shr rax, cl ; calage sur premier bit a lire
                        ; ----- surcharge destination
                        jnc suite1_pl ; on n'active pas la destination si bit lu est 0
                            xor r12, r12
                            mov r12d, dword [r9+TAILLE_DES_PORTIONS+8] ; portion destination
                            cmp r12d, 0
                            je suite1_pl ; si pas de destination, on ne surcharge rien
                                adresse_from_numero r13, r12 ; adresse destination
                                mov al, [r13+OS_CREFR_1]
                                test al, al ; destination refractaire ?
                                jnz suite1_pl

                                    ; normalement :
                                    ; surcharger_portion_de_destination_FFACD r9+TAILLE_DES_PORTIONS, r13, (rax), (rcx), (rdx)
                                    ; ICI JE TRICHE CAR JE N'AI PAS FAIT CORRESPONDRE LES OFFSETS DU LECTEUR ET DES PORTIONS
                                    ; OFFSET DANS LE LECTEUR = 5 alors que OS_VALBB_1 = 10 => -5
                                    ; ATTENTION : OS_VALBB_1 n'est lui déjà valable que pour un neurone avec une destination (pas en recherche)
                                    surcharger_portion_de_destination_FFACD r9+TAILLE_DES_PORTIONS-5, r13, (rax), (rcx), (rdx)

                        suite1_pl:
                        ; ----- persistance
                        mov r11b, byte [r9+TAILLE_DES_PORTIONS+3] ; persistance restante
                        cmp r11b, 0
                        jne suite2_pl
                            mov r11b, byte [r9+TAILLE_DES_PORTIONS+6]
                            mov ax, word [r9+TAILLE_DES_PORTIONS+1] ; index de lecture
                            inc ax ; augmenter l'index de lecture (recup r13)
                            cmp ax, word [r9+9] ; comparer au nombre de segments a lire
                            jne suite3_pl
                                xor ax, ax
                            suite3_pl:
                            mov [r9+TAILLE_DES_PORTIONS+1], ax
                        suite2_pl:
                            dec r11b
                            mov [r9+TAILLE_DES_PORTIONS+3], r11b ; persistance restante
                            jmp fin_traitement

                    portion_pulseur:
                        ; ---------- augmentation chrono
                        xor rcx, rcx
                        mov cx, [r9+OS_CHARG_2] ; chrono actuel
                        add rcx, INCREMENT_PULSEURS ; increment chrono
                        limitation_registre_a_word_SM rcx, (rax) ; limitation haute
                        mov [r9+OS_CHARG_2], cx ; enregistrement nouveau chrono
                        tester_charge r9, (rax), (rcx) ; tester chrono comme si c'etait une charge
                        jb fin_traitement ; jump si rax < rcx / seuil non atteint
                            mov [r9+OS_CHARG_2], word 0 ; reinitialiser le chrono
                            recuperer_numero_par_decalage_FVA r9, OS_DESTN_4, (rax) ; numero du neurone destination
                            test rax, rax
                            jz fin_traitement ; si pas de destination, on ne fait rien
                                adresse_from_numero r11, rax ; adresse neurone de destination
                                mov al, [r11+OS_CREFR_1]
                                test al, al ; destination refractaire ?
                                jnz fin_traitement
                                    surcharger_portion_de_destination_FFACD r9, r11, (rax), (rcx), (rdx) ; surcharge de la destination
                                    jmp fin_traitement
                    ; portions simples
                    portion_simple_nonactivee_retroactive:
                        mov [r9], byte TYPE_SIMPLE_NONACTIVEE
                    portion_simple_nonactivee:
                        tester_charge r9, (rax), (rcx) ; comparaison charge avec seuil
                        jb psnx_fuite_et_retroaction ; jump si rax < rcx / seuil non atteint
                            ; ici la portion est comme activee
                            mov [r9], byte TYPE_SIMPLE_POSTACTIVEE_RETROACTIVE
                            mov [r9+OS_CHARG_2], word CHARGE_INITIALE ; reinitialiser la charge
                            mov al, [r9+OS_BREFR_1]
                            mov [r9+OS_CREFR_1], al ; recharge de la duree refractaire
                            recuperer_numero_par_decalage_FVA r9, OS_DESTN_4, (rax) ; numero de destination
                            test rax, rax
                            jz portion_simple_pousse_axonale ; si pas de destination, on fait pousser l'axone
                                adresse_from_numero r11, rax ; adresse de destination
                                tester_retroactivite_adresse_FA r11, (rax)
                                jne tp_psnx_surcharge_destination
                                    mov [r9], byte TYPE_SIMPLE_NONACTIVEE_RETROACTIVE
                                    negativer_bloc_de_boutons r9, (rax), (rdx)
                                tp_psnx_surcharge_destination:
                                mov al, [r11+OS_CREFR_1]
                                test al, al ; destination refractaire ?
                                jnz fin_traitement
                                    surcharger_portion_de_destination_FFACD r9, r11, (rax), (rcx), (rdx) ; surcharge de la destination
                                    jmp fin_traitement ; sortie normale apres traitement
                        psnx_fuite_et_retroaction:
                            decompte_refractaire r9, (rax), (rbx)
                            fuite_de_charge_portion r9, (rax)
                            recuperer_numero_par_decalage_FVA r9, OS_DESTN_4, (rax) ; numero de destination
                            adresse_from_numero r11, rax ; adresse de destination
                            tester_retroactivite_adresse_FA r11, (rax)
                            jne fin_traitement
                                negativer_bloc_de_boutons r9, (rax), (rdx)
                                jmp fin_traitement ; sortie normale apres traitement

                    portion_simple_postactivee_retroactive:
                        mov [r9], byte TYPE_SIMPLE_POSTACTIVEE
                    portion_simple_postactivee:
                        tester_charge r9, (rax), (rcx) ; comparaison charge avec seuil
                        jb tp_pspx_fuite_et_retroaction ; jump si rax < rcx / seuil non atteint
                            ; ici la portion est comme activee
                            mov [r9], byte TYPE_SIMPLE_POSTACTIVEE_RETROACTIVE
                            mov [r9+OS_CHARG_2], word CHARGE_INITIALE ; reinitialiser la charge
                            mov al, [r9+OS_BREFR_1]
                            mov [r9+OS_CREFR_1], al ; recharge de la duree refractaire
                            recuperer_numero_par_decalage_FVA r9, OS_DESTN_4, (rax) ; numero de destination
                            test rax, rax
                            jz portion_simple_pousse_axonale ; si pas de destination, on fait evoluer l'axone
                                adresse_from_numero r11, rax ; adresse de destination
                                tester_retroactivite_adresse_FA r11, (rax)
                                jne tp_pspx_surcharge_destination
                                    mov [r9], byte TYPE_SIMPLE_NONACTIVEE_RETROACTIVE
                                    positiver_bloc_de_boutons r9, (rax), (rdx)
                                tp_pspx_surcharge_destination:
                                mov al, [r11+OS_CREFR_1]
                                test al, al ; destination refractaire ?
                                jnz fin_traitement
                                    surcharger_portion_de_destination_FFACD r9, r11, (rax), (rcx), (rdx) ; surcharge de la destination
                                    jmp fin_traitement ; sortie normale apres traitement
                        tp_pspx_fuite_et_retroaction:
                            decompte_refractaire r9, (rax), (rbx)
                            fuite_de_charge_portion r9, (rax)
                            recuperer_numero_par_decalage_FVA r9, OS_DESTN_4, (rax) ; numero de destination
                            test rax, rax
                            jz fin_traitement ; si pas de destination, on ne fait rien
                                adresse_from_numero r11, rax ; adresse de destination
                                tester_retroactivite_adresse_FA r11, (rax)
                                jne fin_traitement
                                    mov [r9], byte TYPE_SIMPLE_NONACTIVEE
                                    positiver_bloc_de_boutons r9, (rax), (rdx)
                                    jmp fin_traitement ; sortie normale apres traitement
                    portion_simple_pousse_axonale:
                        ; pousse axonale
                            ; offset x et y
                            mov r11b, byte [r9+OS_EXPDX_SM_1] ; offset actuel sur x
                            mov r12b, byte [r9+OS_EXPDY_SM_1] ; offset actuel sur y
                            movsx r11, r11b ; necessaire pour utilisation de cmov.
                            movsx r12, r12b ; necessaire pour utilisation de cmov.
                            ; progression axonale
                            faire_evoluer_dx_dy_SSABCDMMM r11, r12, (rax), (rbx), (rcx), (rdx), r13, r14, r15 ; evolution dx/dy
                            extraire_x_y_de_numero_FSS r8, rcx, rdx ; extraction des coordonnees de la portion
                            add rcx, r11 ; ajout du nouveau dx a x
                            add rdx, r12 ; ajout du nouveau dy a y
                            limiter_coordonnees_x_y_SSMMM rcx, rdx, (r13), (r14), (r15) ; limitations des coordonnees
                            construire_numero_de_x_y_FS rcx, rdx ; reconstitution numero de portion cible -> rdx
                            cmp rdx, r8 ; tester si la cible est la portion elle-meme
                            je tp_pspa_prolonger_seulement
                            ; compatibilite de la cible en tant que destination
                            mov r13, rdx ; sauvegarde du numero de la cible
                            adresse_from_numero r14, rdx ; adresse de la cible
                            mov al, byte [r14+0] ; lire le type de la cible
                            xor rdx, rdx
                            comparer_al_valeur_vers_dl_AVCD (rax), TYPE_SIMPLE_NONACTIVEE, (rcx), (rdx)
                            comparer_al_valeur_vers_dl_AVCD (rax), TYPE_SIMPLE_NONACTIVEE_RETROACTIVE, (rcx), (rdx)
                            comparer_al_valeur_vers_dl_AVCD (rax), TYPE_SIMPLE_POSTACTIVEE, (rcx), (rdx)
                            comparer_al_valeur_vers_dl_AVCD (rax), TYPE_SIMPLE_POSTACTIVEE_RETROACTIVE, (rcx), (rdx)
                            comparer_al_valeur_vers_dl_AVCD (rax), TYPE_MERE, (rcx), (rdx)
                            comparer_al_valeur_vers_dl_AVCD (rax), TYPE_MERE_RETROACTIVE, (rcx), (rdx)
                            ; passage a la creation de liaison si destination compatible
                            cmp dl, 1
                            je tp_pspa_enregistrer_destination
                            ; prolongation seulement, si la cible n'est pas utilisable comme destination
                            tp_pspa_prolonger_seulement:
                            ; enregistrement de ofs_x et ofs_y (r11w et r12w sont deja entre -128 et 127)
                            mov [r9+OS_EXPDX_SM_1], r11b
                            mov [r9+OS_EXPDY_SM_1], r12b
                            jmp fin_traitement ; sortie normale apres traitement
                            ; enregistrer la destination
                        tp_pspa_enregistrer_destination:
                            ; sont fixes ici : r8/r9 = simple / r13/r14 = destination
                            mov [r9+OS_CHARG_2], word CHARGE_INITIALE
                            mov [r9+OS_SEUIL_2], word SEUIL_STANDARD
                            mov [r9+OS_VALBB_1], byte VALEUR_BOUTONS_STANDARD
                            mov [r9+OS_MASBB_1], byte MASSE_BOUTONS_STANDARD
                            mov [r9+OS_EXPDX_SM_1], byte 0 ; reinit N/U
                            mov [r9+OS_EXPDY_SM_1], byte 0 ; reinit N/U
                            mov [r9+OS_DESTN_4], r13d
                            jmp fin_traitement ; sortie normale apres traitement
                    ; portions meres
                    portion_mere_retroactive:
                        mov [r9], byte TYPE_MERE ; changement en type non-retroactive
                    portion_mere:
                        tester_charge r9, (rax), (rcx) ; comparaison charge avec seuil
                        jb tp_pmx_fuite_de_charge ; jump si rax < rcx / seuil non atteint
                            mov [r9], byte TYPE_MERE_RETROACTIVE ; changement de nouveau en type retroactive
                            mov [r9+OS_CHARG_2], word CHARGE_INITIALE ; reinitialiser la charge
                            mov al, [r9+OS_BREFR_1]
                            mov [r9+OS_CREFR_1], al ; recharge de la duree refractaire
                            recuperer_numero_par_decalage_FVA r9, OS_LIAIS_M_4, (rax) ; numero du neurone de liaison
                            test rax, rax
                            jz tp_pmx_pousse_axonale ; si pas de liaison, on ne propage pas le declenchement a la liaison suivante
                                adresse_from_numero r11, rax ; calcul de l'adresse neurone de liaison
                                mov [r11], byte TYPE_LIAISON_ACTIVEE ; activation de la liaison
                                jmp fin_traitement ; sortie normale apres traitement
                            tp_pmx_pousse_axonale:
                                ; offset x et y
                                mov r11b, byte [r9+OS_EXPDX_SM_1] ; offset actuel sur x
                                mov r12b, byte [r9+OS_EXPDY_SM_1] ; offset actuel sur y
                                movsx r11, r11b ; necessaire pour utilisation de cmov.
                                movsx r12, r12b ; necessaire pour utilisation de cmov.
                                ; progression axonale
                                faire_evoluer_dx_dy_SSABCDMMM r11, r12, (rax), (rbx), (rcx), (rdx), r13, r14, r15 ; evolution dx/dy
                                extraire_x_y_de_numero_FSS r8, rcx, rdx ; extraction des coordonnees de la portion
                                add rcx, r11 ; ajout du nouveau dx a x
                                add rdx, r12 ; ajout du nouveau dy a y
                                limiter_coordonnees_x_y_SSMMM rcx, rdx, (r13), (r14), (r15) ; limitations des coordonnees
                                construire_numero_de_x_y_FS rcx, rdx ; reconstitution numero de portion cible -> rdx
                                cmp rdx, r8 ; tester si la cible est la portion elle-meme
                                je tp_pmx_prolonger_seulement
                                ; compatibilite de la cible en tant que destination
                                mov r13, rdx ; sauvegarde du numero de la cible
                                adresse_from_numero r14, rdx ; adresse de la cible
                                mov al, byte [r14+0] ; lire le type de la cible
                                xor rdx, rdx
                                comparer_al_valeur_vers_dl_AVCD (rax), TYPE_SIMPLE_NONACTIVEE, (rcx), (rdx)
                                comparer_al_valeur_vers_dl_AVCD (rax), TYPE_SIMPLE_NONACTIVEE_RETROACTIVE, (rcx), (rdx)
                                comparer_al_valeur_vers_dl_AVCD (rax), TYPE_SIMPLE_POSTACTIVEE, (rcx), (rdx)
                                comparer_al_valeur_vers_dl_AVCD (rax), TYPE_SIMPLE_POSTACTIVEE_RETROACTIVE, (rcx), (rdx)
                                comparer_al_valeur_vers_dl_AVCD (rax), TYPE_MERE, (rcx), (rdx)
                                comparer_al_valeur_vers_dl_AVCD (rax), TYPE_MERE_RETROACTIVE, (rcx), (rdx)
                                ; passage a la creation de liaison si destination compatible
                                cmp dl, 1
                                je tp_pmx_faire_liaison
                                ; prolongation seulement, si la cible n'est pas utilisable comme destination
                                tp_pmx_prolonger_seulement:
                                ; enregistrement de ofs_x et ofs_y (r11w et r12w sont deja entre -128 et 127)
                                mov [r9+OS_EXPDX_SM_1], r11b
                                mov [r9+OS_EXPDY_SM_1], r12b
                                jmp fin_traitement ; sortie normale apres traitement
                                ; creer la liaison
                            tp_pmx_faire_liaison:
                                ; sont fixes ici : r8/r9 = mere / r13/r14 = destination
                                chercher_portion_libre_FASMSTT r8, (rax), rdx, r11, r12, tp_pmx_creer_liaison, fin_traitement
                                tp_pmx_creer_liaison:
                                ; attribuer la liaison r11/r12 a la mere
                                mov [r9+OS_EXPDX_SM_1], byte 0 ; reinit N/U
                                mov [r9+OS_EXPDY_SM_1], byte 0 ; reinit N/U
                                mov [r9+OS_LIAIS_M_4], r11d
                                creer_liaison_vierge_FN r12, r13
                                jmp fin_traitement ; sortie normale apres traitement
                        tp_pmx_fuite_de_charge:
                            decompte_refractaire r9, (rax), (rbx)
                            fuite_de_charge_portion r9, (rax)
                            jmp fin_traitement ; sortie normale apres traitement
                    ; portions liaison
                    portion_liaison_nonactivee:
                        recuperer_numero_par_decalage_FVA r9, OS_DESTN_4, (rax) ; numero de destination
                        test rax, rax
                        jz fin_traitement ; si pas de destination, on ne fait rien
                            adresse_from_numero r11, rax ; adresse de destination
                            tester_retroactivite_adresse_FA r11, (rax)
                            jne fin_traitement
                                negativer_bloc_de_boutons r9, (rax), (rdx)
                                jmp fin_traitement ; sortie normale apres traitement
                    portion_liaison_activee:
                        mov [r9], byte TYPE_LIAISON_POSTACTIVEE ; s'auto postactiver
                        pla_1:
                        recuperer_numero_par_decalage_FVA r9, OS_DESTN_4, (rax) ; numero de destination
                        test rax, rax ; destination non-valide ?
                        jz portion_liaison_pousse_destinataire
                        pla_2:
                        adresse_from_numero r11, rax
                        tester_retroactivite_adresse_FA r11, (rax) ; destination non-retroactive ?
                        jne pla_4
                        pla_3:
                        mov [r9], byte TYPE_LIAISON_NONACTIVEE ; passage en mode non activee
                        positiver_bloc_de_boutons r9, (rax), (rdx)
                        pla_4:
                        mov al, [r11+OS_CREFR_1]
                        test al, al ; destination refractaire ?
                        jnz portion_liaison_pousse_collaterale
                        pla_5:
                        surcharger_portion_de_destination_FFACD r9, r11, (rax), (rcx), (rdx) ; surcharge de la destination
                        pla_6:
                        recuperer_numero_par_decalage_FVA r9, OS_LIAIS_L_4, (rax) ; numero de liaison suivante
                        test rax, rax ; liaison non valide ?
                        jz fin_traitement ; si pas de liaison, on ne fait plus rien (car surcharge deja utilisee)
                        pla_7:
                        adresse_from_numero r11, rax ; adresse de liaison suivante
                        mov [r11], byte TYPE_LIAISON_ACTIVEE ; activation de la liaison suivante
                        jmp fin_traitement

                        portion_liaison_pousse_destinataire:
                        ; cette situation ne peut en principe pas arriver
                        ; puisque une liaison n'est creee que quand on a deja une destination cible
                        ; A_FAIRE quand meme : pousse destination
                        jmp pla_6

                        portion_liaison_pousse_collaterale:
                        recuperer_numero_par_decalage_FVA r9, OS_LIAIS_L_4, (rax) ; numero de liaison suivante
                        test rax, rax ; liaison valide ?
                        jnz pla_7

                        ; pousse collaterale
                            ; offset x et y
                            mov r11b, byte [r9+OS_EXPDX_L_1] ; offset actuel sur x
                            mov r12b, byte [r9+OS_EXPDY_L_1] ; offset actuel sur y
                            movsx r11, r11b ; necessaire pour utilisation de cmov.
                            movsx r12, r12b ; necessaire pour utilisation de cmov.
                            ; progression axonale
                            faire_evoluer_dx_dy_SSABCDMMM r11, r12, (rax), (rbx), (rcx), (rdx), r13, r14, r15 ; evolution dx/dy
                            extraire_x_y_de_numero_FSS r8, rcx, rdx ; extraction des coordonnees de la portion
                            add rcx, r11 ; ajout du nouveau dx a x
                            add rdx, r12 ; ajout du nouveau dy a y
                            limiter_coordonnees_x_y_SSMMM rcx, rdx, (r13), (r14), (r15) ; limitations des coordonnees
                            construire_numero_de_x_y_FS rcx, rdx ; reconstitution numero de portion cible -> rdx
                            cmp rdx, r8 ; tester si la cible est la portion elle-meme
                            je tp_pla_pc_prolonger_seulement
                            ; compatibilite de la cible en tant que destination
                            mov r13, rdx ; sauvegarde du numero de la cible
                            adresse_from_numero r14, rdx ; adresse de la cible
                            mov al, byte [r14+0] ; lire le type de la cible
                            xor rdx, rdx
                            comparer_al_valeur_vers_dl_AVCD (rax), TYPE_SIMPLE_NONACTIVEE, (rcx), (rdx)
                            comparer_al_valeur_vers_dl_AVCD (rax), TYPE_SIMPLE_NONACTIVEE_RETROACTIVE, (rcx), (rdx)
                            comparer_al_valeur_vers_dl_AVCD (rax), TYPE_SIMPLE_POSTACTIVEE, (rcx), (rdx)
                            comparer_al_valeur_vers_dl_AVCD (rax), TYPE_SIMPLE_POSTACTIVEE_RETROACTIVE, (rcx), (rdx)
                            comparer_al_valeur_vers_dl_AVCD (rax), TYPE_MERE, (rcx), (rdx)
                            comparer_al_valeur_vers_dl_AVCD (rax), TYPE_MERE_RETROACTIVE, (rcx), (rdx)
                            ; passage a la creation de liaison si destination compatible
                            cmp dl, 1
                            je tp_pla_pc_faire_liaison
                            ; prolongation seulement, si la cible n'est pas utilisable comme destination
                            tp_pla_pc_prolonger_seulement:
                            ; enregistrement de ofs_x et ofs_y (r11w et r12w sont deja entre -128 et 127)
                            mov [r9+OS_EXPDX_L_1], r11b
                            mov [r9+OS_EXPDY_L_1], r12b
                            jmp fin_traitement ; sortie normale apres traitement
                            ; enregistrer la destination
                            tp_pla_pc_faire_liaison:
                            ; sont fixes ici : r8/r9 = mere / r13/r14 = destination
                            chercher_portion_libre_FASMSTT r8, (rax), rdx, r11, r12, tp_pla_pc_creer_liaison, fin_traitement
                            tp_pla_pc_creer_liaison:
                            ; attribuer la liaison r11/r12 a la mere
                            mov [r9+OS_EXPDX_SM_1], byte 0 ; reinit N/U
                            mov [r9+OS_EXPDY_SM_1], byte 0 ; reinit N/U
                            mov [r9+OS_LIAIS_M_4], r11d
                            creer_liaison_vierge_FN r12, r13
                            jmp fin_traitement ; sortie normale apres traitement
                    portion_liaison_postactivee:
                        recuperer_numero_par_decalage_FVA r9, OS_DESTN_4, (rax) ; numero de destination
                        adresse_from_numero r11, rax ; adresse de destination
                        test rax, rax
                        jz fin_traitement ; pas de destination, on ne fait rien
                            tester_retroactivite_adresse_FA r11, (rax)
                            jne fin_traitement
                                mov [r9], byte TYPE_LIAISON_NONACTIVEE ; passage en mode non activee
                                positiver_bloc_de_boutons r9, (rax), (rdx)
                                jmp fin_traitement ; sortie normale apres traitement

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
                ; recuperation du time stamp
                xor rax, rax
                rdtsc ; -> edx:eax
                shl rdx, 32
                or rax, rdx
                ; recuperation du time stamp de debut
                ; et re-alignement de la pile suite pop r8
                pop rcx
                ; calcul de la difference
                sub rax, rcx
                ; stocker le resultat
                mov [dureeDeTraitement], rax
                ; ----- temporisation
                xor rcx, rcx
                mov cl, [lenteur]
                cmp cl, 0
                je finTempo
                dec cl
                mov rax, 1
                shl rax, cl
                boucleTempo:
                    dec rax
                    test rax, rax
                jnz boucleTempo
                finTempo:
                ; bouclage
                mov rax, qword [bouclage_rapide_actuel]
                dec rax
                mov qword [bouclage_rapide_actuel], rax
                test rax, rax
            jnz bouclage_rapide
            ; recuperation de la zone d'affichage
                mov rcx, [Windowhandle]         ; hWnd
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
            ; ----------------- liberation / traitement des messages fenetre en attente
            boucleDesMessages:
                ; si plus de message en attente, on sort pour reboucler moyen
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
                ; sortie si pas/plus de messages en attente
                    test rax, rax
                    jz traitementMessagesFini ; on passe a la suite si pas de message en attente
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
                    je    sortieComplete                    ; on arrete tout = sortie toutes boucle
                ; analyse du message
                    mov   rcx, qword [Windowhandle]         ; hDlg
                    lea   rdx, [WindowMessage]                ; lpMsg
                    sub   rsp, SHADOW_SPACE_SIZE
                    call  IsDialogMessageA                  ; For keyboard strokes (?)
                    add   rsp, SHADOW_SPACE_SIZE
                ; bouclage si ... ?
                    cmp   rax, 0
                    jne   boucleDesMessages                 ; le message a ete traite par IsDialogMessageA => on reboucle
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
                ; bouclage
            jmp boucleDesMessages
            ; fin
            traitementMessagesFini:
            mov rax, qword [bouclage_moyen_actuel]
            dec rax
            mov qword [bouclage_moyen_actuel], rax
            test rax, rax
        jnz bouclage_moyen
        mov rax, qword [bouclage_lent_actuel]
        add rax, 1
        mov qword [bouclage_lent_actuel], rax
        ; Ecrire l'interligne + le numero de boucle
        make_a_winapi64_style_call convertir, rax, nombre_text, NOMBRE_LONG
        make_a_winapi64_style_call GetStdHandle, STD_OUTPUT_HANDLE
        mov r15, rax
        make_a_winapi64_style_call WriteConsoleA, r15, interligne_text, INTERLIGNE_LONG, reponse_long_ret
        make_a_winapi64_style_call WriteConsoleA, r15, nombre_text, NOMBRE_LONG, reponse_long_ret
        ; temps de boucle de portion
        mov rax, [dureeDeTraitement]
        make_a_winapi64_style_call convertir, rax, nombre_text, NOMBRE_LONG
        make_a_winapi64_style_call WriteConsoleA, r15, nombre_text, NOMBRE_LONG, reponse_long_ret
        ; nombre de boucles rapides actuel
        mov rax, qword [bouclage_rapide_en_cours]
        make_a_winapi64_style_call convertir, rax, nombre_text, NOMBRE_LONG
        make_a_winapi64_style_call WriteConsoleA, r15, nombre_text, NOMBRE_LONG, reponse_long_ret
        ; Detailler eventuellement dans la console
        xor rax, rax
        mov al, [detailConsole]
        test rax, rax
        jz ne_pas_detailler
            mov rax, PORTION_A_DETAILLER
            call detailler_portion
        ne_pas_detailler:
        ; bouclage lent
    jmp bouclage_lent
; ------------------------------------------------------------------------------------------------- SORTIE
    sortieComplete:
    ; finalisation ----------------------------------------------
        make_a_winapi64_style_call WriteConsoleA, r15, interligne_text, INTERLIGNE_LONG, reponse_long_ret
    ; fin du programme
        ; add rsp, 8
        xor rcx, rcx
        call ExitProcess
; ------------------------------------------------------------------------------------------------- OUTILS
    convertir: ;=====================================================
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
            mov byte [rdx], '>'
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
    detailler_portion: ;=====================================================
        ; ----- en entree :
            ; rax = numero de portion
        ; ----- en sortie :
            ; rax = type de la portion
        ; ----- recalage et sauvegarde des registres modifies
            push r15 ; utilise pour le handle de console
            push r14 ; utilise pour le compteur de boucle
            push r13 ; disponible (pour calage pile)
            push rdx ; utilise pour l'adresse de portion
            push rcx ; utilise pour les valeurs a afficher
            ; le push pour recuperer rax a la fin est fait plus tard
        ; ----- adresse de base -> rdx
            mov rcx, rax ; sauvegarde du numero de portion pour l'afficher plus tard
            adresse_from_numero rdx, rax
        ; ----- Handle de sortie -> r15
            make_a_winapi64_style_call GetStdHandle, STD_OUTPUT_HANDLE
            mov r15, rax
        ; ----- Interligne
            make_a_winapi64_style_call WriteConsoleA, r15, interligne_text, INTERLIGNE_LONG, reponse_long_ret
        ; ----- Numero de la portion
            make_a_winapi64_style_call WriteConsoleA, r15, portion_numr_text, PORTION_NUMR_LONG, reponse_long_ret
            make_a_winapi64_style_call convertir, rcx, nombre_text, NOMBRE_LONG
            make_a_winapi64_style_call WriteConsoleA, r15, nombre_text, NOMBRE_LONG, reponse_long_ret
        ; ----- Position a l'ecran
            push rcx
            push rdx
        ;
            mov rdx, rcx
            shr rdx, BITS_POUR_X ; = y
            mov rax, rdx
            shl rax, BITS_POUR_X ; = base y
            sub rcx, rax ; = x
            ; ----- position x
            make_a_winapi64_style_call convertir, rcx, nombre_text, NOMBRE_LONG
            make_a_winapi64_style_call WriteConsoleA, r15, nombre_text, NOMBRE_LONG, reponse_long_ret
            ; ----- position y
            make_a_winapi64_style_call convertir, rdx, nombre_text, NOMBRE_LONG
            make_a_winapi64_style_call WriteConsoleA, r15, nombre_text, NOMBRE_LONG, reponse_long_ret
        ;
            pop rdx
            pop rcx
        ; ----- type de portion
            xor rcx, rcx
            mov cl, [rdx]
            push rcx ; => sera recupere a la fin pour rax
            ligne_de_valeur r15, rdx, 0, cl, rcx, portion_type_text, PORTION_TYPE_LONG ; type de portion
        ; ----- distribution
            comparer_et_jump_si_egal cl, TYPE_LECTEUR,                         dp_plc
            comparer_et_jump_si_egal cl, TYPE_PULSEUR,                         dp_ppl
            comparer_et_jump_si_egal cl, TYPE_SIMPLE_NONACTIVEE,               dp_ps
            comparer_et_jump_si_egal cl, TYPE_SIMPLE_NONACTIVEE_RETROACTIVE,   dp_ps
            comparer_et_jump_si_egal cl, TYPE_SIMPLE_POSTACTIVEE,              dp_ps
            comparer_et_jump_si_egal cl, TYPE_SIMPLE_POSTACTIVEE_RETROACTIVE,  dp_ps
            comparer_et_jump_si_egal cl, TYPE_MERE,                            dp_pm
            comparer_et_jump_si_egal cl, TYPE_MERE_RETROACTIVE,                dp_pm
            comparer_et_jump_si_egal cl, TYPE_LIAISON_NONACTIVEE,              dp_pl
            comparer_et_jump_si_egal cl, TYPE_LIAISON_POSTACTIVEE,             dp_pl
            comparer_et_jump_si_egal cl, TYPE_LIAISON_ACTIVEE,                 dp_pl
        ; ----- traitement par defaut
            mov r14, 1
            dp_boucle:
                push r14
                add r14, rdx
                xor rax, rax
                mov al, [r14]
                make_a_winapi64_style_call convertir, rax, nombre_text, NOMBRE_LONG
                make_a_winapi64_style_call WriteConsoleA, r15, nombre_text, NOMBRE_LONG, reponse_long_ret
                pop r14
                inc r14
                cmp r14, TAILLE_DES_PORTIONS
            jne dp_boucle
            jmp dp_fin
        dp_plc:
            ligne_de_valeur r15, rdx, 1, rcx, rcx, portion_adrs_text, PORTION_ADRS_LONG ; adresse source
            ligne_de_valeur r15, rdx, 9, cx, rcx, portion_nseg_text, PORTION_NSEG_LONG ; nombre de segments
            ligne_de_valeur r15, rdx, 11, cl, rcx, portion_tseg_text, PORTION_TSEG_LONG ; taille de segment
            ligne_de_valeur r15, rdx, TAILLE_DES_PORTIONS, cl, rcx, portion_type_text, PORTION_TYPE_LONG ; type de la portion suivante
            ligne_de_valeur r15, rdx, TAILLE_DES_PORTIONS+1, cx, rcx, portion_idxl_text, PORTION_IDXL_LONG ; index de lecture
            ligne_de_valeur r15, rdx, TAILLE_DES_PORTIONS+3, cl, rcx, portion_prsb_text, PORTION_PRSB_LONG ; persistance de base
            ligne_de_valeur r15, rdx, TAILLE_DES_PORTIONS+4, cl, rcx, portion_bitl_text, PORTION_BITL_LONG ; numero de bit a lire
            ligne_de_valeur r15, rdx, TAILLE_DES_PORTIONS+5, cl, rcx, portion_blcv_text, PORTION_BLCV_LONG ; valeur du bloc de boutons
            ligne_de_valeur r15, rdx, TAILLE_DES_PORTIONS+6, cl, rcx, portion_nonu_text, PORTION_NONU_LONG ; non utilise
            ligne_de_valeur r15, rdx, TAILLE_DES_PORTIONS+7, cl, rcx, portion_nonu_text, PORTION_NONU_LONG ; non utilise
            ligne_de_valeur r15, rdx, TAILLE_DES_PORTIONS+8, ecx, rcx, portion_dest_text, PORTION_DEST_LONG ; portion de destination
            jmp dp_fin
        dp_ppl:
            ligne_de_valeur r15, rdx, OS_CHARG_2, cx, rcx, portion_chro_text, PORTION_CHRO_LONG ; chrono
            ligne_de_valeur r15, rdx, OS_SEUIL_2, cx, rcx, portion_chrl_text, PORTION_CHRL_LONG ; seuil charge
            ligne_de_valeur r15, rdx, OS_VALBB_1, cl, rcx, portion_blcv_text, PORTION_BLCV_LONG ; valeur du bloc de boutons
            ligne_de_valeur r15, rdx, OS_DESTN_4, ecx, rcx, portion_dest_text, PORTION_DEST_LONG ; numero de destination
            jmp dp_fin
        dp_ps:
            ligne_de_valeur r15, rdx, 0, cl, rcx, portion_type_simple_text, PORTION_TYPE_SIMPLE_LONG ; type simple
            ligne_de_valeur r15, rdx, OS_CHARG_2, cx, rcx, portion_chrg_text, PORTION_CHRG_LONG ; charge
            ligne_de_valeur r15, rdx, OS_SEUIL_2, cx, rcx, portion_chrs_text, PORTION_CHRS_LONG ; seuil charge
            ligne_de_valeur r15, rdx, OS_CREFR_1, cl, rcx, portion_nonu_text, PORTION_NONU_LONG ; decompte refractaire
            ligne_de_valeur r15, rdx, OS_BREFR_1, cl, rcx, portion_nonu_text, PORTION_NONU_LONG ; base decompte refractaire
            ; futur synapse dispo (1)
            ; futur synapses maxi (2)
            ligne_de_valeur r15, rdx, OS_VALBB_1, cl, rcx, portion_blcv_text, PORTION_BLCV_LONG ; exploration dx / valeur du bloc de boutons
            ligne_de_valeur r15, rdx, OS_MASBB_1, cl, rcx, portion_blcp_text, PORTION_BLCP_LONG ; exploration dy / masse du bloc de boutons
            ligne_de_valeur r15, rdx, OS_DESTN_4, ecx, rcx, portion_dest_text, PORTION_DEST_LONG ; numero de destination
            jmp dp_fin
        dp_pm:
            ligne_de_valeur r15, rdx, 0, cl, rcx, portion_type_mere_text, PORTION_TYPE_MERE_LONG ; type mere
            ligne_de_valeur r15, rdx, OS_CHARG_2, cx, rcx, portion_chrg_text, PORTION_CHRG_LONG ; charge
            ligne_de_valeur r15, rdx, OS_SEUIL_2, cx, rcx, portion_chrs_text, PORTION_CHRS_LONG ; seuil charge
            ligne_de_valeur r15, rdx, OS_CREFR_1, cl, rcx, portion_cref_text, PORTION_CREF_LONG ; decompte refractaire
            ligne_de_valeur r15, rdx, OS_BREFR_1, cl, rcx, portion_bref_text, PORTION_BREF_LONG ; base decompte refractaire
            ; futur synapse dispo (1)
            ; futur synapses maxi (2)
            ligne_de_valeur r15, rdx, OS_EXPDX_SM_1, cl, rcx, portion_nonu_text, PORTION_NONU_LONG ; exploration dx
            ligne_de_valeur r15, rdx, OS_EXPDY_SM_1, cl, rcx, portion_nonu_text, PORTION_NONU_LONG ; exploration dy
            ligne_de_valeur r15, rdx, OS_LIAIS_M_4, ecx, rcx, portion_lias_text, PORTION_LIAS_LONG ; liaison suivante
            jmp dp_fin
        dp_pl:
            ligne_de_valeur r15, rdx, OS_LIAIS_L_4, ecx, rcx, portion_lias_text, PORTION_LIAS_LONG ; liaison suivante
            ; vide pour le moment (3)
            ; futur dx (1)
            ; futur dy (1)
            ligne_de_valeur r15, rdx, OS_VALBB_1, cl, rcx, portion_blcv_text, PORTION_BLCV_LONG ; valeur du bloc de boutons
            ligne_de_valeur r15, rdx, OS_MASBB_1, cl, rcx, portion_blcp_text, PORTION_BLCP_LONG ; masse du bloc de boutons
            ligne_de_valeur r15, rdx, OS_DESTN_4, ecx, rcx, portion_dest_text, PORTION_DEST_LONG ; numero de destination
            jmp dp_fin
        dp_fin:
        ; ---------- recuperation registres
            pop rax
            pop rcx
            pop rdx
            pop r13
            pop r14
            pop r15
        ; ----- sortie
        ret
; -------------------------------------------------------------------------------------------- OUTILS POUR CREATIONS INITIALES
    ; Appels : push param1, param2, param3, ... + call + add rsp, nbParams*8
    ; Outils
    sub_adresse_de_numero: ; calcul de l'adresse d'une portion (1 argument)
            ; Arguments : NumeroDePortion->AdresseDePortion
        ; ----- sauvegarde des registres utilises
            push r11
            push rax
            sub rsp, 8 ; alignement de la pile
        ; ----- recuperation des parametres
            mov rax, [rsp+8*(3+1)] ; p1 IN = numero portion / OUT = adresse portion
        ; ----- conversion numero en adresse
            adresse_from_numero r11, rax
        ; ----- sauvegarde des valeurs de retour
            mov [rsp+8*(3+1)], r11
        ; ----- recuperation des registres
            add rsp, 8
            pop rax
            pop r11
        ; ----- sortie
        ret
    ; Creations unitaires
    sub_creer_portion_lecteur: ; creation d'une portion de lecture (Num/Adr/LgSeg/NbSeg/Index/Bit/Persist/ValBloc/Dest)
            ; Arguments : NumeroDePortion / AdresseMemoire / LargeurDeSegment / NombreDeSegments
            ; / IndexActuel / BitALire / PersistanceDeBase / ValeurDuBloc / PortionDeDestination
        ; ----- sauvegarde des registres utilises + alignement de la pile
            push r10
            push rax
            sub rsp, 8
        ; ----- adresse de base de la paire de portions
            mov r10, [rsp+8*(3+9)] ; numero de portion a creer => adresse de portion
            push rax
            push r10
            call sub_adresse_de_numero
            pop r10
            pop rax
        ; ----- enregistrement des donnees
            ;
            mov [r10+0], byte TYPE_LECTEUR ; type de portion
            ;
            mov rax, [rsp+8*(3+8)]
            mov [r10+1], qword rax ; adresse de lecture
            ;
            mov rax, [rsp+8*(3+6)]
            mov [r10+9], word ax ; nombre de segments de l'enregistrement
            ;
            mov rax, [rsp+8*(3+7)]
            mov [r10+11], byte al ; largeur de segment en octets
            ;
            mov [r10+TAILLE_DES_PORTIONS+0], byte TYPE_CONSECUTIF ; type de portion consecutive
            ;
            mov rax, [rsp+8*(3+5)]
            mov [r10+TAILLE_DES_PORTIONS+1], word ax ; index initial de segment
            ;
            mov rax, [rsp+8*(3+2)]
            mov [r10+TAILLE_DES_PORTIONS+3], byte al ; persistance en cours
            ;
            mov rax, [rsp+8*(3+4)]
            mov [r10+TAILLE_DES_PORTIONS+4], byte al ; bit a lire dans le segment
            ;
            mov rax, [rsp+8*(3+2)]
            mov [r10+TAILLE_DES_PORTIONS+5], byte al ; valeur bloc de boutons
            ;
            mov rax, [rsp+8*(3+3)]
            mov [r10+TAILLE_DES_PORTIONS+6], byte al ; persistance de base
            ;
            ; +7 = N/U
            ;
            mov rax, [rsp+8*(3+1)] ; portion de destination
            mov [r10+TAILLE_DES_PORTIONS+8], dword eax
            ;
        ; ----- recuperation des registres
            add rsp, 8
            pop rax
            pop r10
        ; ----- sortie
        ret
    sub_creer_portion_pulseur: ; creation d'une portion pulseur (Num/Seuil/ValBloc/Dest)
            ; Arguments : NumeroDePortion / SeuilChrono / BlocBoutons / PortionDeDestination
        ; ----- sauvegarde des registres utilises + alignement de la pile
            push r10
            push rax
            sub rsp, 8
        ; ----- adresse de base de la paire de portions
            mov r10, [rsp+8*(3+4)] ; numero de portion a creer => adresse de portion
            sub rsp, 8
            push r10
            call sub_adresse_de_numero
            pop r10
            add rsp, 8
        ; ----- enregistrement des donnees
            ;
            mov [r10], byte TYPE_PULSEUR
            ;
            mov [r10+OS_CHARG_2], word 0 ; chrono actuel
            ;
            mov rax, [rsp+8*(3+3)]
            mov [r10+OS_SEUIL_2], word ax ; seuil chrono
            ;
            mov rax, [rsp+8*(3+2)]
            mov [r10+OS_VALBB_1], byte al ; valeur du bloc de boutons
            ;
            ; +6 = N/U
            ;
            ; +7 = N/U
            ;
            mov rax, [rsp+8*(3+1)]
            mov [r10+OS_DESTN_4], dword eax ; numero de portion de destination
            ;
        ; ----- recuperation des registres
            add rsp, 8
            pop rax
            pop r10
        ; ----- sortie
        ret
    sub_creer_portion_simple: ; creation d'une portion simple (Num/Charge/Seuil/ValBloc/MasseBloc/DRefract/Dest)
        ; Arguments : NumeroDePortion / ChargeInitiale / Seuil / ValeurDuBloc / MasseDuBloc / DureeRefractaire / PortionDeDestination
        ; ----- sauvegarde des registres utilises + alignement de la pile
            push r10
            push rax
            sub rsp, 8
        ; ----- adresse de travail
            mov r10, [rsp+8*(3+7)] ; numero de portion a creer
            sub rsp, 8
            push r10
            call sub_adresse_de_numero
            pop r10
            add rsp, 8
        ; ----- parametres
            ;
            mov [r10], byte TYPE_SIMPLE_NONACTIVEE
            ;
            mov rax, [rsp+8*(3+6)]
            mov [r10+OS_CHARG_2], word ax ; charge actuelle
            ;
            mov rax, [rsp+8*(3+5)]
            mov [r10+OS_SEUIL_2], word ax ; seuil charge
            ;
            mov rax, [rsp+8*(3+4)]
            mov [r10+OS_VALBB_1], byte al ; valeur du bloc de boutons
            ;
            mov rax, [rsp+8*(3+3)]
            mov [r10+OS_MASBB_1], byte al ; masse du bloc de boutons
            ;
            mov rax, [rsp+8*(3+2)]
            mov [r10+OS_CREFR_1], byte al ; duree refractaire
            ;
            mov rax, [rsp+8*(3+1)]
            mov [r10+OS_DESTN_4], dword eax ; numero de portion de destination
            ;
        ; ----- recuperation des registres
            add rsp, 8
            pop rax
            pop r10
        ; ----- sortie
        ret
    sub_creer_portion_mere: ; creation d'une portion mere (Num/Charge/Seuil/DRefract/LSuiv)
            ; NumeroDePortion / ChargeInitiale / SeuilStandard / DureeRefractaire / PortionSuivante
        ; ----- sauvegarde des registres utilises + alignement de la pile
            push r10
            push rax
            sub rsp, 8
        ; ----- adresse de travail
            mov r10, [rsp+8*(3+5)] ; numero de portion a creer -> adresse
            sub rsp, 8
            push r10
            call sub_adresse_de_numero
            pop r10
            add rsp, 8
        ; ----- parametres
            ;
            mov [r10], byte TYPE_MERE
            ;
            mov rax, [rsp+8*(3+4)]
            mov [r10+OS_CHARG_2], word ax ; charge actuelle
            ;
            mov rax, [rsp+8*(3+3)]
            mov [r10+OS_SEUIL_2], word ax ; seuil de declenchement
            ;
            mov rax, 0
            mov [r10+OS_EXPDX_SM_1], byte al ; exploration axone dx
            ;
            mov rax, 0
            mov [r10+OS_EXPDY_SM_1], byte al ; exploration axone dy
            ;
            mov rax, 0
            mov [r10+OS_CREFR_1], byte al ; duree refractaire
            ;
            mov rax, [rsp+8*(3+2)]
            mov [r10+OS_BREFR_1], byte al ; duree refractaire
            ;
            mov rax, [rsp+8*(3+1)]
            mov [r10+OS_LIAIS_M_4], dword eax ; numero de suivant
            ;
        ; ----- recuperation des registres
            add rsp, 8
            pop rax
            pop r10
        ; ----- sortie
        ret
    sub_creer_portion_liaison: ; creation d'une portion de liaison (Num/ValBloc/MasseBloc/Dest/LSuiv)
            ; Arguments : NumeroDePortion / ValeurBoutons / MasseBoutons / PortionDeDestination / PortionSuivante
        ; ----- sauvegarde des registres utilises + alignement de la pile
            push r10
            push rax
            sub rsp, 8
        ; ----- adresse de travail
            mov r10, [rsp+8*(3+5)] ; numero de portion a creer
            sub rsp, 8
            push r10
            call sub_adresse_de_numero
            pop r10
            add rsp, 8
        ; ----- Parametres
            ;
            mov [r10], byte TYPE_LIAISON_NONACTIVEE
            ;
            mov rax, [rsp+8*(3+1)]
            mov [r10+OS_LIAIS_L_4], dword eax ; numero de suivant
            ;
            mov rax, [rsp+8*(3+4)]
            mov [r10+OS_VALBB_1], byte al ; valeur de bloc de boutons
            ;
            mov rax, [rsp+8*(3+3)]
            mov [r10+OS_MASBB_1], byte al ; masse du bloc de boutons
            ;
            ; +7 = N/U
            ;
            mov rax, [rsp+8*(3+2)]
            mov [r10+OS_DESTN_4], dword eax ; numero de portion de destination
        ; ----- recuperation des registres
            add rsp, 8
            pop rax
            pop r10
        ; ----- sortie
        ret
    ; Creation de segments
    sgt_hrz_lecture_std_GD: ; creation d'un segment horizontal de lecture standard G->D (Num/Adr/NbSeg/Index/Dest)
            ; Arguments : NumeroDePortion / AdresseMemoire / NombreDeSegments / IndexActuel / PortionDeDestination
        ; ----- sauvegarde des registres utilises (alignement pile inclus)
            push r10
            push r11
            push rcx
            push rax
            sub rsp, 8
        ; variables
            mov rcx, 0 ; index de bit
            mov r10, [rsp+8*(5+5)] ; premiere portion a creer
            mov r11, [rsp+8*(5+1)] ; premiere portion de destination
        ; ----- boucle
            shlsGD_bit:
                ; +0
                sub rsp, 8 ; pre-alignement de la pile
                ; +1
                push r10 ; numero de portion a creer
                ; +2
                mov rax, [rsp+8*(5+2+4)]
                push rax ; adresse memoire
                ; +3
                push TAILLE_SEGMENT_STANDARD ; taille de segment
                ; +4
                mov rax, [rsp+8*(5+4+3)]
                push rax ; nombre de segments
                ; +5
                mov rax, [rsp+8*(5+5+2)]
                push rax ; index de lecture
                ; +6
                push rcx ; bit a lire
                ; +7
                push PERSISTANCE_STANDARD ; persistance
                ; +8
                push VALEUR_BOUTONS_LECTEUR ; valeur de bloc
                ; +9
                push r11 ; portion destination
                ; +10
                ; Num/Adr/LgSeg/NbSeg/Index/Bit/Persist/ValBloc/Dest
                call sub_creer_portion_lecteur
                add rsp, 10*8
                ;
                add r10, 2
                add r11, 1 ; 1 si on veut des sorties qui se suivent, sinon, au meme pas 2
                inc rcx
                cmp rcx, TAILLE_SEGMENT_STANDARD*8
            jne shlsGD_bit
            add rsp, 8
            pop rax
            pop rcx
            pop r11
            pop r10
        ret
    sgt_vrt_simples_std_GD: ; creation d'un segment vertical de simples standard G->D (Num/Nbr/Dest)
            ; Arguments : NumeroDePortion / Nombre / PortionDeDestination
        ; ----- sauvegarde des registres utilises (alignement pile inclus)
            push r10
            push r11
            push rcx
            push rax
            sub rsp, 8
        ; variables
            mov rcx, [rsp+8*(5+2)] ; compteur
            mov r10, [rsp+8*(5+3)] ; premiere portion a creer
            mov r11, [rsp+8*(5+1)] ; premiere portion de destination
        ; ----- boucle
            svssGD_compteur:
                ; +0
                sub rsp, 8 ; pre-alignement de la pile
                ; +1
                push r10 ; numero de portion a creer
                ; +2
                push CHARGE_INITIALE ; charge
                ; +3
                push SEUIL_STANDARD ; seuil
                ; +4
                push VALEUR_BOUTONS_STANDARD ; valeur du bloc
                ; +5
                push MASSE_BOUTONS_STANDARD ; masse du bloc
                ; +6
                push DUREE_REFRACTAIRE ; duree refractaire
                ; +7
                push r11 ; portion destination
                ; +8
                ; Num/Charge/Seuil/ValBloc/MasseBloc/DRefract/Dest
                call sub_creer_portion_simple
                add rsp, 8*8
                ;
                add r10, LARGEUR_DE_COUCHE
                cmp r11, 0
                je svssGD_noDest
                    add r11, LARGEUR_DE_COUCHE
                svssGD_noDest:
                dec rcx
                cmp rcx, 0
            jne svssGD_compteur
        ; recuperation des registres
            add rsp, 8
            pop rax
            pop rcx
            pop r11
            pop r10
        ; sortie
        ret
    sgt_vrt_m_plus_l_std_GD: ; creation d'un segment vertical de mere+liaisons standard G->D (Mere/Liaisons/Nbr/Dest)
            ; Arguments : NumeroDePortionMere / NumeroDePortionLiaison / Nombre / PortionDeDestination
        ; ----- sauvegarde des registres utilises (alignement pile inclus)
            push r10
            push r11
            push rcx
            push rax
            sub rsp, 8
        ; ----- mere
            ; +0
            sub rsp, 8 ; pre-alignement de la pile
            ; +1
            mov rax, [rsp+8*(5+1+4)] ; portion mere
            push rax ; numero de portion a creer
            ; +2
            push CHARGE_INITIALE ; charge
            ; +3
            push SEUIL_STANDARD ; seuil
            ; +4
            push DUREE_REFRACTAIRE ; duree refractaire
            ; +5
            mov rax, [rsp+8*(5+5+3)] ; premiere portion de liaison
            push rax
            ; +6
            ; Num/Charge/Seuil/DRefract/LSuiv
            call sub_creer_portion_mere
            add rsp, 6*8
        ; ----- variables
            mov rcx, [rsp+8*(5+2)] ; compteur
            mov r10, [rsp+8*(5+3)] ; premiere portion de liaison
            mov r11, [rsp+8*(5+1)] ; premiere portion de destination
        ; ----- boucle liaisons
            svmplsGD_compteur:
                ; +0
                sub rsp, 8 ; pre-alignement de la pile
                ; +1
                push r10 ; numero de portion a creer
                ; +2
                push VALEUR_BOUTONS_LIAISONS ; valeur du bloc
                ; +3
                push MASSE_BOUTONS_STANDARD ; masse du bloc
                ; +4
                push r11 ; portion destination
                add r11, LARGEUR_DE_COUCHE ; incrementation de la destination
                ; +5
                add r10, LARGEUR_DE_COUCHE ; incrementation de la liaison
                push r10 ; numero de portion a creer
                ; +6
                ; Num/ValBloc/MasseBloc/Dest/LSuiv
                call sub_creer_portion_liaison
                add rsp, 6*8
                ;
                dec rcx
                cmp rcx, 0
            jne svmplsGD_compteur
        ; ecrasement de la derniere liaison pour ne pas avoir de suivante
            sub r10, LARGEUR_DE_COUCHE
            sub r11, LARGEUR_DE_COUCHE
            make_a_call sub_creer_portion_liaison, r10, VALEUR_BOUTONS_LIAISONS, MASSE_BOUTONS_STANDARD, r11, 0
        ; recuperation des registres
            add rsp, 8
            pop rax
            pop rcx
            pop r11
            pop r10
        ; sortie
        ret
    ; Creation de diffuseurs
    dif_hrz_m_plus_l_std_GD: ; creation d'un cone de diffusion horizontal de mere+liaisons G->D (Mere/Liaisons/Nbr/Dest)
            ; Arguments : NumeroDePortionMere / NumeroDePortionLiaison / Nombre / PortionDeDestination
        ; ----- sauvegarde des registres utilises (alignement pile inclus)
            push r10
            push r11
            push rcx
            push rax
            sub rsp, 8
        ; ----- mere
            ; +0
            sub rsp, 8 ; pre-alignement de la pile
            ; +1
            mov rax, [rsp+8*(5+1+4)] ; portion mere
            push rax ; numero de portion a creer
            ; +2
            push CHARGE_INITIALE ; charge
            ; +3
            push SEUIL_STANDARD ; seuil
            ; +4
            push DUREE_REFRACTAIRE ; duree refractaire
            ; +5
            mov rax, [rsp+8*(5+5+3)] ; premiere portion de liaison
            push rax
            ; +6
            ; Num/Charge/Seuil/DRefract/LSuiv
            call sub_creer_portion_mere
            add rsp, 6*8
        ; ----- variables
            mov rcx, [rsp+8*(5+2)] ; compteur
            mov r10, [rsp+8*(5+3)] ; premiere portion de liaison
            mov r11, [rsp+8*(5+1)] ; premiere portion de destination
        ; ----- boucle liaisons
            dhmplsGD_compteur:
                ; +0
                sub rsp, 8 ; pre-alignement de la pile
                ; +1
                push r10 ; numero de portion a creer
                ; +2
                push VALEUR_BOUTONS_LIAISONS ; valeur du bloc
                ; +3
                push MASSE_BOUTONS_STANDARD ; masse du bloc
                ; +4
                push r11 ; portion destination
                add r11, LARGEUR_DE_COUCHE ; incrementation de la destination
                ; +5
                add r10, 1 ; incrementation de la liaison
                push r10 ; numero de portion a creer
                ; +6
                ; Num/ValBloc/MasseBloc/Dest/LSuiv
                call sub_creer_portion_liaison
                add rsp, 6*8
                ;
                dec rcx
                cmp rcx, 0
            jne dhmplsGD_compteur
        ; ecrasement de la derniere liaison pour ne pas avoir de suivante
            sub r10, 1
            sub r11, LARGEUR_DE_COUCHE
            make_a_call sub_creer_portion_liaison, r10, VALEUR_BOUTONS_LIAISONS, MASSE_BOUTONS_STANDARD, r11, 0
        ; recuperation des registres
            add rsp, 8
            pop rax
            pop rcx
            pop r11
            pop r10
        ; sortie
        ret
    ; Creation de groupes
    pqt_lecture_std_GD: ; creation d'un paquet de lecture vertical standard (Num/Adr/NbSeg/Index/Haut/Dest)
            ; Arguments : NumeroDePortion / AdresseMemoire / NombreDeSegments / IndexActuel / HauteurDuPaquet / PortionDeDestination
        ; ----- sauvegarde des registres utilises (alignement pile inclus)
            push r10
            push r11
            push r12
            push rcx
            push rax
        ; variables
            mov r10, [rsp+8*(5+6)] ; premiere portion a creer
            mov r11, [rsp+8*(5+1)] ; premiere portion de destination
            mov r12, [rsp+8*(5+3)] ; index de segment
            mov rcx, [rsp+8*(5+2)] ; nombre de lignes a creer
        ; ----- boucle
            plsGD_compteur:
                ; +0
                sub rsp, 8 ; pre-alignement de la pile
                ; +1
                push r10 ; numero de portion a creer
                ; +2
                mov rax, [rsp+8*(5+2+5)]
                push rax ; adresse memoire
                ; +3
                mov rax, [rsp+8*(5+3+4)]
                push rax ; nombre de segments
                ; +4
                push r12 ; index de segments
                ; +5
                push r11 ; portion destination
                ; +6
                ; Num/Adr/NbSeg/Index/Dest
                call sgt_hrz_lecture_std_GD
                add rsp, 6*8
                ;
                add r10, LARGEUR_DE_COUCHE
                add r11, LARGEUR_DE_COUCHE
                inc r12
                dec rcx
                cmp rcx, 0
            jne plsGD_compteur
            pop rax
            pop rcx
            pop r12
            pop r11
            pop r10
        ret

    pqt_simples_std_GD: ; creation d'un paquet de lecture vertical standard (Num/NbrH/NbrV/Dest)
            ; Arguments : NumeroDePortion / NombreALHorizontale / NombreALaVerticale / PortionDeDestination
        ; ----- sauvegarde des registres utilises (alignement pile inclus)
            push r10
            push r11
            push rcx
            push rax
            sub rsp, 8
        ; variables
            mov r10, [rsp+8*(5+4)] ; premiere portion a creer
            mov r11, [rsp+8*(5+1)] ; premiere portion de destination
            mov rcx, [rsp+8*(5+3)] ; nombre de colonnes a creer
        ; ----- boucle
            pssGD_compteur:
                ; +0
                sub rsp, 8 ; pre-alignement de la pile
                ; +1
                push r10 ; numero de portion a creer
                ; +2
                mov rax, [rsp+8*(5+2+2)]
                push rax ; nombre de lignes a creer
                ; +3
                push r11 ; portion destination
                ; +4
                ; Num/Nbr/Dest
                call sgt_vrt_simples_std_GD
                add rsp, 4*8
                ;
                add r10, 1
                cmp r11, 0
                je pssGD_noDest
                    add r11, 1
                pssGD_noDest:
                dec rcx
                cmp rcx, 0
            jne pssGD_compteur
            add rsp, 8
            pop rax
            pop rcx
            pop r11
            pop r10
        ret
    pqt_identification: ; creation d'un paquet d'identification de code (Num/NbrH/NbrV/Dest)
            ; Arguments : NumeroDePortionMere / nombre de mere horizontales / Nombre de liaisons verticales / PortionDeDestination(!decalage)
        ; ----- sauvegarde des registres utilises (alignement pile inclus)
            push r11
            push r12
            push r13
            push r14
            push r15
            push rcx
            push rdx
            push rax
            sub rsp, 8
        ; ----- variables
            mov r11, [rsp+8*(9+4)] ; premiere mere
            mov r14, [rsp+8*(9+1)] ; premiere destination
        ; ----- boucle ==================================================
            mov rcx, [rsp+8*(9+3)] ; nombre de colonnes
            pid_meres:
                ; variables
                mov r12, r11 ; mere/liaison courante
                mov r13, r12
                add r13, LARGEUR_DE_COUCHE ; liaison suivante
                ; creation mere               (Num/Charge/Seuil/DRefract/LSuiv)
                make_a_call sub_creer_portion_mere, r12, CHARGE_INITIALE, CHARGE_INITIALE+1, 0, r13
                ; evolution variables
                add r12, LARGEUR_DE_COUCHE
                add r13, LARGEUR_DE_COUCHE
                mov r15, r14
                ; boucle ------------------------------------------------------
                mov rdx, [rsp+8*(9+2)] ; nombre de lignes
                pid_liaisons:
                    ; creation liaison              (Num/ValBloc/MasseBloc/Dest/LSuiv)
                    make_a_call sub_creer_portion_liaison, r12, 70, 0, r15, r13
                    add r12, LARGEUR_DE_COUCHE
                    add r13, LARGEUR_DE_COUCHE
                    add r15, LARGEUR_DE_COUCHE
                    ; bouclage
                    dec rdx
                    cmp rdx, 0
                jne pid_liaisons ; --------------------------------------------
                sub r12, LARGEUR_DE_COUCHE
                mov r13, 0
                sub r15, LARGEUR_DE_COUCHE
                ; ecraser derniere liaison       (Num/ValBloc/MasseBloc/Dest/LSuiv)
                make_a_call sub_creer_portion_liaison, r12, 10, 255, r15, 0
                add r11, 1 ; mere suivante
                ; bouclage
                dec rcx
                cmp rcx, 0
            jne pid_meres ; ==================================================
        ; recuperation des registres
            add rsp, 8
            pop rax
            pop rdx
            pop rcx
            pop r15
            pop r14
            pop r13
            pop r12
            pop r11
        ; sortie
        ret    
    pqt_voyant_lumineux: ; creation d'un paquet de lecture vertical standard (Num) A FINALISER
            ; Arguments : NumeroDePortion
        ; ----- sauvegarde des registres utilises (alignement pile inclus)
            push r10
            push r11
            push rcx
            push rax
            sub rsp, 8
        ; ----- variables principales
            mov r10, [rsp+8*(99+1)] ; portion de base, puis incrementation par ligne
            mov rdx, r10 ; destination finale (fixe)
            add rdx, 1
        ; ----- variables
            mov r11, r10 ; portion de base de la ligne, puis incrementation par 2
            mov r13, r11
            add r13, 2 ; liaison suivante, puis incrementation par 2
        ; ----- mere
            make_a_call sub_creer_portion_mere, r11, 1, 2, 0, r13 ; Num/Charge/Seuil/DRefract/LSuiv
        ; ----- boucle de liaisons
            mov r14, 10 ; remplacer par TAILLE_DE_VOYANT
            pvl_compteur_y:
                mov r15, 10 ; remplacer par TAILLE_DE_VOYANT
                pvl_compteur_x:
                    ; ici r11 est valide et pret a etre utilise
                    mov r12, r11
                    add r12, 1 ; portion de destination pae ligne, puis incrementation par 2
                    mov r13, r11
                    add r13, 2 ; liaison suivante, puis incrementation par 2
                    make_a_call sub_creer_portion_liaison, r11, 1, 0, r12, r13 ; Num/ValBloc/MasseBloc/Dest/LSuiv
                    ; bouclage
                    dec r15
                    cmp r15, 0
                jne pvl_compteur_x
                add r10, LARGEUR_DE_COUCHE
                mov r12, 0
                make_a_call sub_creer_portion_liaison, r11, 1, 0, r12, r13 ; Num/ValBloc/MasseBloc/Dest/LSuiv
                mov r11, r10
                ; bouclage
                dec r14
                cmp r14, 0
            jne pvl_compteur_y
            make_a_call sub_creer_portion_liaison, r11, 1, 0, r12, r13 ; Num/ValBloc/MasseBloc/Dest/LSuiv
                ; +0
                sub rsp, 8 ; pre-alignement de la pile
                ; +1
                push r10 ; numero de portion a creer
                ; +2
                mov rax, [rsp+8*(5+2+2)]
                push rax ; colonne
                ; +3
                push r11 ; portion destination
                ; +4
                ; Num/Nbr/Dest
                call sgt_vrt_simples_std_GD
                add rsp, 4*8
                ;
                add r10, 2
                add r11, 2
                dec rcx
                cmp rcx, 0
            jne pssGD_compteur
            add rsp, 8
            pop rax
            pop rcx
            pop r11
            pop r10
        ret
    ; Creation de reseaux
    res_base_de_simples: ; creation d'un reseau de simples vierges (Num/Pas/NbrH/NbrV)
            ; Arguments : NumeroDePortion / Pas / NombreALHorizontale / NombreALaVerticale
        ; ----- sauvegarde des registres utilises (alignement pile inclus)
            sub rsp, 8
            push r8
            push r9
            push r10
            push r11
            push r12
            push r13
            push rdx
            push rax
        ; ----- variables
            ; premiere portion a creer
            mov r10, [rsp+8*(9+4)]
            ; variable de recuperation du numero de portion
            mov r9, r10
            ; pas du reseau
            mov r8, [rsp+8*(9+3)]
            ; nombre de neurones axe horizontal
            xor rdx, rdx
            mov rax, [rsp+8*(9+2)]
            div r8
            mov r12, rax
            ; variable de recuperation du nombre de neurones horizontal
            mov r11, r12
            ; nombre de neurones axe vertical
            xor rdx, rdx
            mov rax, [rsp+8*(9+1)]
            div r8
            mov r13, rax
        ; ----- boucle
            rbds_compteur_y:
                mov r12, r11 ; nombre de neurones axe horizontal
                rbds_compteur_x:
                    ; +0
                    sub rsp, 8 ; pre-alignement de la pile
                    ; +1
                    push r10 ; numero de portion a creer
                    ; +2
                    push CHARGE_INITIALE ; charge actuelle
                    ; +3
                    push SEUIL_STANDARD ; seuil de declenchement
                    ; +4
                    push VALEUR_BOUTONS_STANDARD ; ? code liaisons ?
                    ; +5
                    push MASSE_BOUTONS_STANDARD ; ? code liaisons ?
                    ; +6
                    push DUREE_REFRACTAIRE ; Duree refractaire
                    ; +7
                    push 0 ; Liaison 0 = vierge
                    ; +8
                    ; */Num/Charge/Seuil/ValBloc/MasseBloc/DRefract/Dest
                    call sub_creer_portion_simple
                    add rsp, 8*8
                    ; -----
                    add r10, r8
                    ; -----
                    dec r12
                    cmp r12, 0
                jne rbds_compteur_x
                ; -----
                mov rax, LARGEUR_DE_COUCHE
                mov rdx, r8
                mul rdx
                add r9, rax
                mov r10, r9
                ; -----
                dec r13
                cmp r13, 0
            jne rbds_compteur_y
        ; ----- recuperation des registres originaux
            pop rax
            pop rdx
            pop r13
            pop r12
            pop r11
            pop r10
            pop r9
            pop r8
            add rsp, 8
        ret
    res_base_de_meres: ; creation d'un reseau de meres vierges (Num/Pas/NbrH/NbrV)
            ; Arguments : NumeroDePortion / Pas / NombreALHorizontale / NombreALaVerticale
        ; ----- sauvegarde des registres utilises (alignement pile inclus)
            sub rsp, 8
            push r8
            push r9
            push r10
            push r11
            push r12
            push r13
            push rdx
            push rax
        ; ----- variables
            ; premiere portion a creer
            mov r10, [rsp+8*(9+4)]
            ; variable de recuperation du numero de portion
            mov r9, r10
            ; pas du reseau
            mov r8, [rsp+8*(9+3)]
            ; nombre de neurones axe horizontal
            xor rdx, rdx
            mov rax, [rsp+8*(9+2)]
            div r8
            mov r12, rax
            ; variable de recuperation du nombre de neurones horizontal
            mov r11, r12
            ; nombre de neurones axe vertical
            xor rdx, rdx
            mov rax, [rsp+8*(9+1)]
            div r8
            mov r13, rax
        ; ----- boucle
            rbdm_compteur_y:
                mov r12, r11 ; nombre de neurones axe horizontal
                rbdm_compteur_x:
                    ; +0
                    sub rsp, 8 ; pre-alignement de la pile
                    ; +1
                    push r10 ; numero de portion a creer
                    ; +2
                    push CHARGE_INITIALE ; charge actuelle
                    ; +3
                    push SEUIL_STANDARD ; seuil de declenchement
                    ; +4
                    push DUREE_REFRACTAIRE ; Duree refractaire
                    ; +5
                    push 0 ; Liaison 0 = vierge
                    ; +6
                    ; */Num/Charge/Seuil/DRefract/LSuiv
                    call sub_creer_portion_mere
                    add rsp, 6*8
                    ; -----
                    add r10, r8
                    ; -----
                    dec r12
                    cmp r12, 0
                jne rbdm_compteur_x
                ; -----
                mov rax, LARGEUR_DE_COUCHE
                mov rdx, r8
                mul rdx
                add r9, rax
                mov r10, r9
                ; -----
                dec r13
                cmp r13, 0
            jne rbdm_compteur_y
        ; ----- recuperation des registres originaux
            pop rax
            pop rdx
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
                comparer_et_jump_si_egal al, 0x41, cas_WM_KEYDOWN_A
                comparer_et_jump_si_egal al, 0x5A, cas_WM_KEYDOWN_Z
                comparer_et_jump_si_egal al, 0x45, cas_WM_KEYDOWN_E
                comparer_et_jump_si_egal al, 0x52, cas_WM_KEYDOWN_R
                ;
                comparer_et_jump_si_egal al, 0x54, cas_WM_KEYDOWN_T
                comparer_et_jump_si_egal al, 0x59, cas_WM_KEYDOWN_Y
                ;
                comparer_et_jump_si_egal al, 0x55, cas_WM_KEYDOWN_U
                ;
                comparer_et_jump_si_egal al, 0x50, cas_WM_KEYDOWN_P
                comparer_et_jump_si_egal al, 0x1B, cas_WM_KEYDOWN_Escape ; ne fonctionne pas ???
                jmp cas_WM_KEYDOWN_Sortie
            ; touche a
                cas_WM_KEYDOWN_A:
                    ; texte de fenetre
                    mov rcx, [rbp+16]               ; HWND
                    lea rdx, [NOM_FENETRE_A0]       ; LPCSTR
                    sub rsp, SHADOW_SPACE_SIZE
                    call SetWindowTextA
                    add rsp, SHADOW_SPACE_SIZE
                    ; changement du mode de tracage
                    mov byte [modeTracage], 0
                    jmp cas_WM_KEYDOWN_Sortie
            ; touche z
                cas_WM_KEYDOWN_Z:
                    ; texte de fenetre
                    mov rcx, [rbp+16]               ; HWND
                    lea rdx, [NOM_FENETRE_Z1]       ; LPCSTR
                    sub rsp, SHADOW_SPACE_SIZE
                    call SetWindowTextA
                    add rsp, SHADOW_SPACE_SIZE
                    ; changement du mode de tracage                    mov byte [modeTracage], 1
                    mov byte [modeTracage], 1
                    jmp cas_WM_KEYDOWN_Sortie
            ; touche e
                cas_WM_KEYDOWN_E:
                    ; texte de fenetre
                    mov rcx, [rbp+16]               ; HWND
                    lea rdx, [NOM_FENETRE_E2]       ; LPCSTR
                    sub rsp, SHADOW_SPACE_SIZE
                    call SetWindowTextA
                    add rsp, SHADOW_SPACE_SIZE
                    ; changement du mode de tracage                    mov byte [modeTracage], 2
                    mov byte [modeTracage], 2
                    jmp cas_WM_KEYDOWN_Sortie
            ; touche r
                cas_WM_KEYDOWN_R:
                    ; texte de fenetre
                    mov rcx, [rbp+16]               ; HWND
                    lea rdx, [NOM_FENETRE_R3]       ; LPCSTR
                    sub rsp, SHADOW_SPACE_SIZE
                    call SetWindowTextA
                    add rsp, SHADOW_SPACE_SIZE
                    ; changement du mode de tracage                    mov byte [modeTracage], 3
                    mov byte [modeTracage], 3
                    jmp cas_WM_KEYDOWN_Sortie
            ; touche t
                cas_WM_KEYDOWN_T:
                    mov al, [lenteur]
                    cmp al, 0
                    je cas_WM_KEYDOWN_Sortie
                    dec al
                    mov [lenteur], al
                    jmp cas_WM_KEYDOWN_Sortie
            ; touche y
                cas_WM_KEYDOWN_Y:
                    mov al, [lenteur]
                    cmp al, 64 ; maxi = 64
                    je cas_WM_KEYDOWN_Sortie
                    inc al
                    mov [lenteur], al
                    jmp cas_WM_KEYDOWN_Sortie
            ; touche u
                cas_WM_KEYDOWN_U:
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
            ; touche p
                cas_WM_KEYDOWN_P:
            ; touche escape
                cas_WM_KEYDOWN_Escape:
                xor rcx, rcx
                sub rsp, SHADOW_SPACE_SIZE
                call PostQuitMessage
                add rsp, SHADOW_SPACE_SIZE
                jmp cas_WM_KEYDOWN_Sortie
            ; rax et sortie
                cas_WM_KEYDOWN_Sortie:
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
                jz no_dibs
                mov rax, [pBitDataAdress]
                test rax, rax
                jz no_dibs
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
            ; tracage sur DIBSection ------------- FAIRE D'ABORD LE CHOIX DE L'AFFICHAGE ET LES BOUCLES APRES
                mov r11, [pBitDataAdress] ; compteur de point
                mov r9, HAUTEUR_FENETRE ; compteur de ligne
                lea r10, [portions] ; adresse de portion
                boucle_lignes:
                    mov r8, LARGEUR_FENETRE ; compteur de colonne
                    boucle_colonnes:
                        xor rdx, rdx
                        mov cl, byte [modeTracage]
                        cmp cl, 0
                            je boucle_tracage_A0
                        cmp cl, 1
                            je boucle_tracage_Z1
                        cmp cl, 2
                            je boucle_tracage_E2
                        cmp cl, 3
                            je boucle_tracage_R3
                        jmp boucle_tracage_fin
                        ; mode A0 ------------------------------
                            boucle_tracage_A0:
                            ; type
                                mov dl, [r10+0]
                            ; verts = entrees
                                cmp dl, TYPE_PULSEUR
                                    je bta0_vert
                                cmp dl, TYPE_LECTEUR
                                    je bta0_vert
                            ; bleu = recepteur
                                cmp dl, TYPE_MERE
                                    je bta0_bleu
                                cmp dl, TYPE_MERE_RETROACTIVE
                                    je bta0_bleu
                            ; violet = emetteur/recepteur
                                cmp dl, TYPE_SIMPLE_NONACTIVEE
                                    je bta0_violet
                                cmp dl, TYPE_SIMPLE_NONACTIVEE_RETROACTIVE
                                    je bta0_violet
                                cmp dl, TYPE_SIMPLE_ACTIVEE
                                    je bta0_violet
                                cmp dl, TYPE_SIMPLE_ACTIVEE_RETROACTIVE
                                    je bta0_violet
                                cmp dl, TYPE_SIMPLE_POSTACTIVEE
                                    je bta0_violet
                                cmp dl, TYPE_SIMPLE_POSTACTIVEE_RETROACTIVE
                                    je bta0_violet
                            ; rouge = emetteur 
                                cmp dl, TYPE_LIAISON_NONACTIVEE
                                    je bta0_rouge
                                cmp dl, TYPE_LIAISON_ACTIVEE
                                    je bta0_rouge
                                cmp dl, TYPE_LIAISON_POSTACTIVEE
                                    je bta0_rouge
                            ; fin couleurs
                            jmp boucle_tracage_fin
                            ; ---------------- couleurs
                            bta0_vert:
                                mov [r11+1], byte 255
                                jmp boucle_tracage_fin
                            bta0_bleu:
                                mov [r11+2], byte 63
                                mov [r11+1], byte 63
                                mov [r11+0], byte 255
                                jmp boucle_tracage_fin
                            bta0_violet:
                                mov [r11+2], byte 255
                                mov [r11+0], byte 255
                                jmp boucle_tracage_fin
                            bta0_rouge:
                                mov [r11+2], byte 255
                                jmp boucle_tracage_fin
                            jmp boucle_tracage_fin
                        ; mode Z1 ------------------------------
                            boucle_tracage_Z1:
                                ; affichage valeur bloc de boutons => vert
                                    mov dl, byte [r10+OS_VALBB_1]
                                    mov [r11+1], dl
                                jmp boucle_tracage_fin
                        ; mode E2 ------------------------------
                            boucle_tracage_E2:
                                ; affichage charge/chrono => vert ou rouge
                                    mov dx, word [r10+OS_CHARG_2]
                                    cmp dx, 256
                                    ja bte2_vert
                                    jmp bte2_rouge
                                jmp boucle_tracage_fin
                                bte2_vert:
                                    mov [r11+1], dl
                                    jmp boucle_tracage_fin
                                bte2_rouge:
                                    shr rdx, 8
                                    mov [r11+2], dl
                                    jmp boucle_tracage_fin
                                jmp boucle_tracage_fin
                        ; mode R3 ------------------------------
                            boucle_tracage_R3:
                                ; affichage cible retroactive (forcement simple ou liaison) => bleu
                                    tester_retroactivite_adresse_FA r10, (rax)
                                    jne btr3_suite
                                        mov dl, 127
                                        mov [r11+0], dl ; bleu
                                ; tracer aussi en rouge l'activite
                                    btr3_suite:
                                    tester_activite_adresse r10, (rax)
                                    jne boucle_tracage_fin
                                        mov dl, 127
                                        mov [r11+2], dl ; bleu
                                ; sortie
                                    jmp boucle_tracage_fin
                        ; fin des options ----------------------
                        boucle_tracage_fin:
                        ; portion suivante ---------------------
                            add r10, TAILLE_DES_PORTIONS
                        ; point suivant ------------------------
                            add r11, 3
                        ; colonne suivante
                            dec r8
                            cmp r8, 0
                    jne boucle_colonnes
                    ; fin de boucle de colonnes
                        add r11, COMPLEMENT_LIGNE_DWORD
                        dec r9
                        cmp r9, 0
                jne boucle_lignes
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
            no_dibs:
            ; cloture du tracage
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
