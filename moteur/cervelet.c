#define USE_NOTES true // also same for net works and GPU works
#pragma region // -------------------------------------------------------------------------- GPL LICENCE
    /*
    ; Copyright (C) 2024-2025 Philippe BLATIERE
    ;
    ; This program is free software: you can redistribute it and/or modiDIMENSION_X
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
#pragma endregion
#pragma region // -------------------------------------------------------------------------- A_FAIRE
    /*
    ; résoudre tous les points marqués A_FAIRE
    ; separer les numeros d'utilisateurs et les numeros de portions dans builder
    ; coder les développements dendrites / axones
    */
#pragma endregion
#pragma region // -------------------------------------------------------------------------- REFERENCES
    #include <stdio.h>
    #include <stdlib.h>

    #include <stdint.h>
    #include <string.h>
    #include <time.h>
    #include <stdalign.h>

    // #define _WIN32_WINNT 0x0A00 // Pour Windows 10
    #include <winsock2.h>
    #include <windows.h>
    #include <iphlpapi.h>

    #include <ws2tcpip.h> // Bibliothèque pour certaines fonctions réseau avancées

    #pragma comment(lib, "Ws2_32.lib") // Linker automatiquement la librairie réseau
    // note : -lgdi32 est automatiquement lié par visual studio
    #pragma comment(lib, "iphlpapi.lib")
#pragma endregion
#pragma region // ------------------------------------------------------------------------------- MACROS
    // extraction de donnees non signees
        #define U8_FROM(table, taille, numero, offset) (((uint8_t *)table)[numero * taille + offset])
        #define U16_FROM(table, taille, numero, offset) (*((uint16_t *)((uint8_t *)table + numero * taille + offset)))
        #define U32_FROM(table, taille, numero, offset) (*((uint32_t *)((uint8_t *)table + numero * taille + offset)))
        #define U64_FROM(table, taille, numero, offset) (*((uint64_t *)((uint8_t *)table + numero * taille + offset)))
    // extraction de donnees non signees
        #define S8_FROM(table, taille, numero, offset) (((int8_t *)table)[numero * taille + offset])
        #define S16_FROM(table, taille, numero, offset) (*((uint16_t *)((uint8_t *)table + numero * taille + offset)))
    // ecritures de donnees non signees
        #define FOR_U8_TO(table, taille, numero, offset) *((uint8_t *)((uint8_t *)table + numero * taille + offset))
        #define FOR_U16_TO(table, taille, numero, offset) *((uint16_t *)((uint8_t *)table + numero * taille + offset))
        #define FOR_U32_TO(table, taille, numero, offset) *((uint32_t *)((uint8_t *)table + numero * taille + offset))
        #define FOR_U64_TO(table, taille, numero, offset) *((uint64_t *)((uint8_t *)table + numero * taille + offset))
    // ecritures de donnees signees
        #define FOR_S8_TO(table, taille, numero, offset) *((int8_t *)((uint8_t *)table + numero * taille + offset))
        #define FOR_S16_TO(table, taille, numero, offset) *((int16_t *)((uint8_t *)table + numero * taille + offset))
    //---------- macro pour transformation
        #define DO_CONVERT(ipstring, sockaddr) do { \
            if (inet_pton(AF_INET, ipstring, &(sockaddr.sin_addr)) <= 0) { \
                printf("Adresse IP invalide : %s\n", ipstring); \
                return 1; \
            } \
        } while(0)

        #define DO_UNCONVERT(sockaddr, ipstring_buf) do { \
            if (inet_ntop(AF_INET, &(sockaddr.sin_addr), ipstring_buf, INET_ADDRSTRLEN) == NULL) { \
                perror("inet_ntop"); \
                return 1; \
            } \
        } while(0)

#pragma endregion
#pragma region // ------------------------------------------------------------------------------- CONSTANTES #define
    // Pour préprocesseur
    #define false 0
    #define true 1
    // dimensionnement utilisateur (portions)
        #define DIMENSION_X 512
        #define DIMENSION_Y 256
        #define DIMENSION_Z 32
        //
        #define MAX_NBR_USERS 65536 // max (1ULL << 16) = 2^16 = 65536
        #define NBR_CRVPS DIMENSION_X * DIMENSION_Y * DIMENSION_Z // max 2^32 = 4Gp
    // dimensions des piles
        // pile locale
        #define LCL_STACK_SIZE NBR_CRVPS
        #define LCL_PACKS_SIZE NBR_CRVPS + NBR_CRVPS // pour les en-têtes
        #define LCL_MAX_MSG_PER_DST 0xFFFFFFFF // acceptable dans OLD_NMSGS_4
        // pile toSend
        #define SND_STACK_SIZE NBR_CRVPS
        #define SND_PACKS_SIZE NBR_CRVPS + MAX_NBR_USERS // pour les en-têtes
        #define SND_MAX_MSG_PER_DST 0xFFFFFFFFFFFFFFFF // acceptable pour OSU_NMES_8
        // mettre ici la taille d'envoi et réception
        #define RCV_STACK_SIZE 4 * 1024 * 1024
        #define RCV_PACKS_SIZE 4 * 1024 * 1024 + NBR_CRVPS
    // messages
        // #define MSG_TEST_PRES 0x54534554 // 'TEST' (dword little endian)
        // #define MSG_PRES_OK 0x00000001
        //
        // #define MSG_DMD_LECTURE 0x33333333 // 51 en décimal ^^
        // #define MSG_VALEUR_LUE      0x00000020 //
        //
    // Messages (dword little endian)
        // Pulseurs
        #define MSG_PUL_1   0x48435253  // 'SRCH' Simple demande de surcharge
        // Segments
        #define MSG_SEG_1   0x41435253  // 'SRCA' Demande de surcharge avec retour d'activité
        #define MSG_SEG_1A   0x54434152  // 'RACT' Retour d'activité
        // Extensions
        #define MSG_EXT_1   0x44544341  // 'ACTD' Demande d'activité amont
        #define MSG_EXT_1A  0x50544341  // 'ACTP' Cas activité amont positive
        #define MSG_EXT_1A1  0x53435253  // 'SRCS' Demande de surcharge simple
        #define MSG_EXT_1B  0x4E544341  // 'ACTN' Cas activité amont nulle
        #define MSG_EXT_1B1  0x41545244  // 'DRTA' Demande de rétroactivité
        #define MSG_EXT_1B1A  0x41544552  // 'RETA' Reponse niveau d'activité
// Divers
        #define MSG_DACT    0x54434151  // 'QACT' Demande de retour d'activité
        #define MSG_IERR    0x49525245  // 'ERRI' Info d'erreur
    // reglages portions
        #define INCREMENT_PULSEURS 100
        #define VITESSE_DECHARGE 0
        #define VS_VALBB 127 // maxi 127
        #define VS_MASBB 255 // maxi 255
        #define VS_PSEUIL 8 // maxi 15
        #define VS_BREFRC 15 // mini = durée maxi de remontée réfractaire (cumul chaine apicale de segments le plus long)
    // limites
        #define CHRONO_INITIAL 0
        #define CHRONO_MAXI 65535
        #define CHARGE_INVALIDE -32768
        #define CHARGE_MINI -32767
        #define CHARGE_MAXI +32767
        #define MINI_VALBB -127
        #define MAXI_VALBB +127
        #define MAXI_MASSEBB +255
    // réglages bouclages et écritures
        #define DUREE_BOUCLE_MOYENNE_MS 100
        #define DUREE_BOUCLE_LENTE_MS 1000
    // types de portions (0 = non utilisé, 255 = interdit/inutilisable/réservé)
        #define TYPE_SOURCE 0b01001000 // 72 type source de données en mémoire
        #define TYPE_LECTEUR 0b10001000 // 136 type lecteur
        #define TYPE_PULSEUR 0b10010000 // 144 type pulseur
        //
        #define TYPE_SEGMENT_GERME 0b11001000 // 200 type dendrite figé sans dendrite amont
        #define TYPE_SEGMENT_PARTIEL 0b11001001 // 201 type dendrite auquel il manque des dendrites
        #define TYPE_SEGMENT_COMPLET 0b11001010 // 202 type dendrite avec toutes dendrite amont
        //
        #define TYPE_NEURONE_GERME 0b11011000 // 216 type neurone figé sans dendrite ni axone
        #define TYPE_NEURONE_PARTIEL 0b11011001 // 217 type neurone auquel il manque des dendrites et/ou l'axone
        #define TYPE_NEURONE_COMPLET 0b11011010 // 218 type neurone avec toutes dendrites et neurone
        //
        #define TYPE_EXTENSION_GERME 0b11010000 // 208 type axone figé sans destination ni extension
        #define TYPE_EXTENSION_PARTIEL 0b11010001 // 209 type axone auquel il manque la destination ou l'extension
        #define TYPE_EXTENSION_COMPLET 0b11010010 // 210 type axone avec destination et extension
    // paramètres graphiques
        #define LARGEUR_FENETRE DIMENSION_X
        #define HAUTEUR_FENETRE DIMENSION_Y
        #define NOMBRE_OCTET_PAR_POINT 3
        #define COMPLEMENT_LIGNE_DWORD (((LARGEUR_FENETRE * NOMBRE_OCTET_PAR_POINT * 8 + 7) / 8) % 4)
        // offsets couleurs
        #define ROUG 2
        #define VERT 1
        #define BLEU 0
        // pour console
        #define POSITION_TABLE_X 55
        #define HAUTEUR_TABLE 23
        #define LONGUEUR_LIGNES 45
        #define NOMBRE_CARS_AVANT 2
        #define NOMBRE_CARS_APRES 2
        // pour lecture de fichier
        #define NOMBRE_MAX_SEGMENTS_TEXTE 65535
        #define LARGEUR_DE_SEGMENT_DE_TEXTE 1 // Octets par segment, maxi 8
        // reseau
        #define WSA_VERSION MAKEWORD(2, 2)
        #define AF_INET_IPV4 AF_INET
        #define DEFAULT_PROTOCOL 0
        // #define INET_ADDRSTRLEN 16 déjà definie dans <Ws2tcpip.h>
        // expédition
        #define CHOSEN_MTU 1000 // Usually 1500, with some of margin
        #define MAX_UNITS_PER_UDP_PAQUET 3 // ((CHOSEN_MTU - 20 - 8) / TAI_SND_MSG) // 40
        // réception
        #define INPORT 5151 // 8080
        #define SOCKET_ADDRESS_LEN sizeof(sockaddr_in_t)

#pragma endregion
#pragma region // ------------------------------------------------------------------------------- OFFSETS #define
    // ------------------------- offsets dans les portions
        #define TAI_CRVPS 32
        // offsets communs impératifs
        #define OCP_TYPESTEV_1 0
        #define OCP_NIVACTMP_1 4
        #define OCP_ACTIVITE_1 5
        #define OCP_CHARG0_2 8
        // ----- offsets accès mémoire
        // ! Type STEV 1 0
        #define OCP_D_LGSEG_1 4
        #define OCP_D_NBSEG_2 8
        #define OCP_D_ADDRS_8 16
        // ----- offsets lecteurs
        // ! Type STEV 1 0
        #define OCP_L_PERSA_1 2
        #define OCP_L_PERSB_1 3
        #define OCP_L_INDEX_2 4
        #define OCP_L_NMBIT_1 6
        #define OCP_L_BSYNV_1 7
        #define OCP_L_PAM_N_4 8
        #define OCP_L_PAM_U_4 12
        #define OCP_L_DST_N_4 16
        #define OCP_L_DST_U_4 20
        // ----- offsets pulseurs
        // ! Type STEV 1 0
        #define OCP_P_CHARGI_2 8
        #define OCP_P_SEUIL_1 10
        #define OCP_P_BSYNV_1 11
        #define OCP_P_DST_N_4 16
        #define OCP_P_DST_U_4 20
        // ----- offsets segments dendritiques
        // ! Type STEV 1 0
        #define OCP_S_TEVOCP_1 1
        #define OCP_S_PSYNA_2 2
        // ! Niveau d'activité 1 4
        // ! Activite 1 5
        #define OCP_S_NBRCH_1 6
        // N/D_1 7
        // ! Charge0 2 8
        #define OCP_S_CHARG1_2 10
        #define OCP_S_CHARG2_2 12
        #define OCP_S_CHARG3_2 14
        #define OCP_S_SVT_N_4 16
        #define OCP_S_SVT_U_4 20
        #define OCP_S_PRSEG_1 24
        // ----- offsets neurones
        // ! Type STEV 1 0
        // ! Niveau d'activité 1 4
        // ! Activite 1 5
        #define OCP_N_CREFR_1 6
        #define OCP_N_BREFR_1 7
        #define OCP_N_SEUIL_1 10
        #define OCP_N_PRAYO_1 12
        #define OCP_N_PPLAN_1 13
        #define OCP_N_PAPIC_1 14
        #define OCP_N_PPANI_1 15
        #define OCP_N_PAXON_1 22
        #define OCP_N_ORIENT_1 23
        // ----- offsets extensions axonales
        // ! Type STEV 1 0
        #define OCP_E_TEVOE_1 1
        // N/D_2 2
        #define OCP_E_CACTT_1 3
        // ! Niveau d'activité 1 4
        // ! Activite 1 5
        #define OCP_E_BSYNV_1 6
        #define OCP_E_BSYNM_1 7
        #define OCP_E_DST_N_4 8
        #define OCP_E_DST_U_4 12
        #define OCP_E_ANT_N_4 16
        #define OCP_E_ANT_U_4 20
        #define OCP_E_PREXT_1 24
        // N/D_7 25
    // ------------------------- Tailles et offsets utilisateurs
        // taille et offsets dans liste des utilisateurs users : OSU_
        #define TAI_USR_NET 8
        #define OUN_IPAD_4 0
        #define OUN_PORT_2 4
        #define OUN_VIDE_2 6
    // ------------------------- Tailles et offsets pour messages locaux
        // taille et offsets dans portions destinataires dests : OLD_
        #define TAI_LOC_DST 8
        #define OLD_NMSGS_4 0
        #define OLD_POSIT_4 4
        // taille et offsets dans stack en local : OLN_
        #define TAI_LOC_NOT 16
        #define OLN_EMM_4 0
        #define OLN_DST_4 4
        #define OLN_DEMC_4 8
        #define OLN_DEMP_4 12
        // taille et offsets dans blocs en local : OG_
        #define TAI_LOC_MSG 16
        // dans les préambules
        #define OLM_P_DEST_4 0
        #define OLM_P_TAIP_4 4
        // dans les messages
        #define OLM_M_EMM_4 0
        #define OLM_M_DES_4 4
        #define OLM_M_DEMC_4 8
        #define OLM_M_DEMP_4 12
    // ------------------------- Tailles et offsets pour envois messages
        // taille et offsets dans liste des utilisateurs users : OSU_
        #define TAI_SND_USR 24
        #define OSU_VIDE_6 0
        #define OSU_DECM_2 6
        #define OSU_NMES_8 8
        #define OSU_ADRP_8 16
        // taille et offsets dans liste de notes toSendNotesStack : OSN_
        #define TAI_SND_NOT 24
        #define OSN_EMMP_4 0
        #define OSN_EMMU_4 4
        #define OSN_DESP_4 8
        #define OSN_DESU_4 12
        #define OSN_DEMC_4 16
        #define OSN_DEMP_4 20
        // taille et offsets dans liste de messages dans toSendMessagesPacks : OM_ + ./P/M
        #define TAI_SND_MSG 24
        // dans le préambules
        #define OSM_P_IPAD_4 0
        #define OSM_P_PORT_2 4
        #define OSM_P_VIDE_2 6
        #define OSM_P_TAIP_8 8
        // dans les messages
        #define OSM_M_EMMP_4 0
        #define OSM_M_EMMU_4 4
        #define OSM_M_DESP_4 8
        #define OSM_M_DESU_4 12 // inutile car destinataire déjà dans en-tête
        #define OSM_M_DEMC_4 16
        #define OSM_M_DEMP_4 20
    // ------------------------- Tailles et offsets pour reception messages
        // taille et offsets dans destinations en réception : ORD_
        #define TAI_RCV_DST 8
            #define ORD_NMSGS_4 0
            #define ORD_POSIT_4 4
        // taille et offsets dans stack en réception : ORN_
        #define TAI_RCV_NOT 24
            #define ORN_EMMP_4 OSM_M_EMMP_4
            #define ORN_EMMU_4 OSM_M_EMMU_4
            #define ORN_DSTP_4 OSM_M_DESP_4
            // #define ORN_DSTU_4 OSM_M_DESU_4 inutile car forcément bon destinataire
            #define ORN_DEMC_4 OSM_M_DEMC_4
            #define ORN_DEMP_4 OSM_M_DEMP_4
        // taille et offsets dans packs en réception receiveMessagesPacks : ORM_
        #define TAI_RCV_MSG 24
            #define ORM_M_DSTP_4 0
            // #define ORM_M_DSTU_4 4 inutile
            #define ORM_M_EMMP_4 8
            #define ORM_M_EMMU_4 12
            #define ORM_M_DEMC_4 16
            #define ORM_M_DEMP_4 20
#pragma endregion
#pragma region // ------------------------------------------------------------------------------- TEXTES ET LISTES
    // noms de fichiers
    #define INPUT_FILE_NAME "textealire.txt"
    #define CERVELET_FILE_NAME "structure.crv"
    // noms de fentres
    const char* windowClassName = "GraphicWindowClass";
    /*
        windowName: db 'Fenetre de tracage', 0
        NOM_FENETRE_A0: db 'Types de portions', 0
        NOM_FENETRE_Z1: db 'Valeurs du bloc de boutons', 0
        NOM_FENETRE_E2: db 'Charges et chronos', 02
        NOM_FENETRE_R3: db 'Activation et retro-activation', 0
        */
    // pseudo textes
        const char* ligne_de_cadre = "####################################################################################################";
        const char* ligne_vide = "";
    // textes menu d'aides général
        const char* help_00 = "All controls with focus on gfx window";
        const char* help_01 = "";
        const char* help_10 = "H : Console writing (rotate)";
        const char* help_11 = "  - Main help ";
        const char* help_12 = "  - Point help";
        const char* help_13 = "  - Net and cervelet";
        const char* help_14 = "  - Runtime infos";
        const char* help_15 = "  - Local flow";
        const char* help_16 = "  - Net flow";
        const char* help_17 = "  - Target infos";
        const char* help_18 = "  - Silent";
        const char* help_20 = "Operating mode";
        const char* help_21 = "  B : Step by step mode (toggle)";
        const char* help_22 = "  N : Next step (on step by step mode)";
        const char* help_23 = "  P : Quit";
        const char* help_30 = "Load and save";
        const char* help_31 = "  I : Load from structure.crv";
        const char* help_32 = "  O : Save to structure.crv";
        // RAJOUTER
        // J = No flow generating
        // K = Local process pause
        // L = Net process pause
    // textes menu d'aides détails
        const char* help_50 = "Target positionning";
        const char* help_51 = "  V : X plus";
        const char* help_52 = "  X : X minus";
        const char* help_53 = "  C : Y plus";
        const char* help_54 = "  D/F : Y minus";
        const char* help_55 = "  Q/S : Z plus";
        const char* help_56 = "  W : Z minus";
        const char* help_60 = "Graphical windows";
        const char* help_61 = "  A : Draw type of portions";
        const char* help_62 = "  Z : Draw value of synpases blocs";
        const char* help_63 = "  E : Draw load of portions";
        const char* help_64 = "  R : Draw retro / activity";
    // textes de gestion
        char texte_reseau_up[] = "Fonctionnalites reseau up (net_Okay) ..";
        char texte_mode_maitre[] = "Mode maitre > ...............";
        char texte_mode_dependant[] = "Mode dependant de ...............";
        char texte_mode_autonome[] = "Mode autonome";
        const char* texte_nombre_d_utilisateurs = "Nombre d utilisateurs : ";
        const char* texte_nombre_de_cibles_validees = "Nombre de cibles validees : ";
        const char* texte_nombre_de_cibles_externes = "Nombre de cibles externes : ";
        const char* texte_nombre_de_cibles_nulles = "Nombre de cibles nulles : ";
        const char* texte_nombre_de_cibles_incoherentes = "Nombre de cibles incoherentes : ";
    // suivi
        const char* texte_numero_de_boucle = "Numero de boucle longue : ";
        const char* texte_duree_de_boucle = "Duree de boucle sur portions : ";
        const char* texte_duree_de_posttrait = "Duree de post traitement : ";
        const char* texte_notes_refusees = "Cumul notes refusees * : ";
        const char* texte_erreur_a_voir = "Erreur a voir : ";
        const char* texte_cpu_usage = "Utilisation cpu % : ";
        const char* texte_memory_usage = "Utilisation memoire % : ";
    // Application directe
        const char* texte_remplissage_local = "Taux de remplissage local % : ";
        const char* texte_nombre_de_messages_locaux = "Nombre de messages locaux : ";
        const char* texte_valeur_de_test = "------ Valeur de test : ";
    // informations réseau
        const char* texte_envois_actifs = "Envois reseau actifs (do_send) ........";
        const char* texte_remplissage_tosend = "Taux de remplissage expedition % : ";
        const char* texte_nombre_de_messages_a_emettre = "Nombre de message a envoyer * : ";
        const char* texte_reception_active = "Reception reseau active (do_recv) .....";
        const char* texte_flux_iocp_entrant = "Flux entrant IOCP * : ";
        const char* texte_paquets_udp_refuses = "Paquets udp refuses : ";
        const char* texte_flux_entrant_sur_pile_0 = "Flux entrant sur pile 0 * : ";
        const char* texte_flux_entrant_sur_pile_1 = "Flux entrant sur pile 1 * : ";
        const char* texte_nombre_de_messages_acceptes = "Nombre de message acceptes * : ";
        const char* texte_nombre_de_messages_detruits = "Nombre de message detruits * : ";
        const char* texte_remplissage_received = "Taux de remplissage reception % : ";
        const char* texte_udp_packet_loss = "Perte de paquets udp % : ";
        const char* texte_socket_queue_usage = "Utilisation file socket % : ";
        const char* texte_network_usage = "Utilisation reseau % : ";
    // détail de portion
        const char* portions_nonu_text = "Non utilise : ";
        const char* portions_nnux_text = "Non utilise ........................ x ";
        const char* portions_ind1_text = "Indefini 1 octet : ";
        const char* portions_ind2_text = "Indefini 2 octets : ";
        const char* portions_ind4_text = "Indefini 4 octets : ";
        const char* portions_ind8_text = "Indefini 8 octets : ";
        const char* portions_numr_text = "Numero de la portion : ";
        const char* texte_position__x = "Abscisse x : ";
        const char* texte_position__y = "Ordonnee y : ";
        const char* texte_position__z = "Profondeur z : ";
        // textes types de portions
        const char* portions_type_text = "Type de portion : ";
        const char* portions_typeD_text = "Type de portion / Data : ";
        const char* portions_typeL_text = "Type de portion / Lecteur : ";
        const char* portions_typeP_text = "Type de portion / Pulseur : ";
        const char* portions_typeS_text = "Type de portion / Segment : ";
        const char* portions_typeN_text = "Type de portion / Neurone : ";
        const char* portions_typeE_text = "Type de portion / Extension : ";
        // textes details communs
        const char* portions_octt_text = "Octet simple : ";
        const char* portions_actv_text = "Activite propre : ";
        const char* portions_chrg_text = "Charge actuelle : ";
        const char* portions_chrs_text = "Seuil charge : ";
        const char* portions_cref_text = "Compteur refractaire : ";
        const char* portions_bref_text = "Base refractaire : ";
        const char* portions_blcv_text = "Valeur du bloc de boutons : ";
        const char* portions_blcp_text = "Masse du bloc de boutons : ";
        const char* portions_dstn_text = "Portion de destination : ";
        const char* portions_dstu_text = "Utilisateur destination : ";
        const char* portions_ctac_text = "Compteur temporel d activite : ";
        const char* portions_ntac_text = "Niveau temporel d activite : ";
        const char* portions_devo_text = "Decompte d evolution : ";
        // specifiques portions data
        const char* portions_adrs_text = "Adresse source : ";
        const char* portions_tseg_text = "Taille des segments : ";
        const char* portions_nseg_text = "Nombre de segments : ";
        // specifiques lecteurs
        const char* portions_idxl_text = "Index de lecture : ";
        const char* portions_bitl_text = "Numero de bit a lire : ";
        const char* portions_prsa_text = "Persistance actuelle : ";
        const char* portions_prsb_text = "Persistance de base : ";
        const char* portions_pamm_text = "Portion d acces memoire : ";
        // specifiques pulseurs
        const char* portions_chro_text = "Chrono actuel : ";
        const char* portions_chrl_text = "Limite chrono : ";
        const char* portions_psyn_text = "Potentiel synaptique restant : ";
        const char* portions_pseg_text = "Potentiel segments restants : ";
        const char* portions_pext_text = "Potentiel extensions restantes : ";
        // spécifiques neurones
        const char* portions_ornt_text = "Orientation developpements : ";
        const char* portions_pray_text = "Potentiel rayonnant : ";
        const char* portions_ppla_text = "Potentiel planaire : ";
        const char* portions_papi_text = "Potentiel apical : ";
        const char* portions_ppan_text = "Potentiel panier : ";
        const char* portions_paxo_text = "Potentiel axonal : ";
        // spécifiques extensions
        const char* portions_antn_text = "Antecedant numero portion : ";
        const char* portions_antu_text = "Antecedant utilisateur : ";
    /* Textes d erreurs
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
    /* table d'exploration
        explorT0_x: db   0
        explorT1_x: db   1,  1,  0, -1, -1, -1,  0,  1
        explorT2_x: db   2,  2,  2,  1,  0, -1, -2, -2, -2, -2, -2, -1,  0,  1,  2,  2

        explorT0_y: db   0
        explorT1_y: db   0,  1,  1,  1,  0, -1, -1, -1
        explorT2_y: db   0,  1,  2,  2,  2,  2,  2,  1,  0, -1, -2, -2, -2, -2, -2, -1
        */
#pragma endregion
#pragma region // ------------------------------------------------------------------------------- VARIABLES GLOBALES
    // structures
        // portions 3d
        typedef struct {
            uint16_t x;
            uint16_t y;
            uint16_t z;
        } id_prt;
        // sockaddr_in
        typedef struct sockaddr_in sockaddr_in_t;
        // IOCP_UDP_PACKET
        typedef struct {
            OVERLAPPED incomeOverlapped;
            WSABUF incomeWsaBuffer;
            char incomeBuffer[2048];
            sockaddr_in_t incomeClientAddress;
            int incomeAddressLen;
        } IOCP_UDP_PACKET;
    // triplets portions
        id_prt p_port;
        id_prt p_srce;
        id_prt p_dest;
        id_prt p_suiv;
        id_prt p_antc;
        id_prt null_port = { 0, 0, 0 };
    // gestion
        uint8_t mode_pas_a_pas = 1;
    // fenetre graphqiue
        HANDLE instanceHandle = NULL;
        HWND gWindowHandle = NULL;
        BITMAPINFO bitmapInfo;
        uint8_t* bitmapData = NULL;
        uint8_t modeTracage = 2;
        uint16_t visualize_x = 100;
        uint16_t visualize_y = 100;
        uint16_t visualize_z = 0;
        // affichageGraphique: db 1
    // lecture fichier texte
        uint8_t* rawData_Texte = NULL;
        uint16_t longueur_Texte = 0;
    // ecritures console
        uint8_t affichageConsole = 1;
        char ligneAEcrire[LONGUEUR_LIGNES + 1];
    // decompte du temps
        clock_t timer_avant_rapides;
        clock_t timer_en_fin_de_rapides;
        uint64_t duree_cumul_des_rapides;
    // double nombre_de_cycles;
        clock_t timer_avant_moyennes;
        clock_t timer_en_fin_de_moyennes;
        uint64_t duree_cumul_des_moyennes;
        clock_t debut_cycle;
        clock_t fin_cycle;
        uint64_t duree_cycle;
    // tables moteur
        // cervelet
            __declspec(align(8)) uint8_t crvDatas[DIMENSION_Z][DIMENSION_Y][DIMENSION_X][TAI_CRVPS];
        // reseau
            __declspec(align(8)) uint8_t netUsers[MAX_NBR_USERS][TAI_USR_NET] = { 0 };
        // notes pour application locale
            int localStackToFill = 0;
            uint32_t localStackIndex[2];
            __declspec(align(8)) uint8_t localDests[2][NBR_CRVPS][TAI_LOC_DST];
            __declspec(align(8)) uint8_t localNotesStack[2][LCL_STACK_SIZE][TAI_LOC_NOT];
            __declspec(align(8)) uint8_t localMessagesPacks[LCL_PACKS_SIZE][TAI_LOC_MSG];
        // notes à envoyer vers un autre cervelet
            int toSendStackToFill = 0;
            uint32_t toSendStackIndex[2] = { 0 };
            __declspec(align(8)) uint8_t toSendUsers[2][MAX_NBR_USERS][TAI_SND_USR];
            __declspec(align(8)) uint8_t toSendNotesStack[2][SND_STACK_SIZE][TAI_SND_NOT];
            __declspec(align(8)) uint8_t toSendMessagesPacks[SND_PACKS_SIZE][TAI_SND_MSG];
        // notes reçues pour application locale
            __declspec(align(8)) uint8_t receiveDests[NBR_CRVPS][TAI_LOC_DST];
            int receiveStackToFill = 0;
            uint32_t receiveStackIndex[2] = { 0 };
            __declspec(align(8)) uint8_t receiveNotesStack[2][RCV_STACK_SIZE][TAI_RCV_NOT]; // utiliser aussi le tas ?
            __declspec(align(8)) uint8_t receiveMessagesPacks[RCV_PACKS_SIZE][TAI_RCV_MSG];
    // Pour les threads ---------------------------------------------------------------------------
        // Boucles
            uint64_t numero_de_boucle_lente = 0;
            uint64_t dureeDePostTraitement;
            uint64_t ciblesValidees;
            uint64_t ciblesExternes;
            uint64_t erreurCibleNulle;
            uint64_t erreurCibleIncoherente;
        // Reseau
            WSADATA wsaDataStruct;
            BOOL net_Okay;
            char my_ip_string[INET_ADDRSTRLEN] = { 0 };
            uint32_t my_ip_address;
            BOOL net_Server;
            char* ip_master_string = NULL;
            uint32_t ip_master;
        // Utilisateurs
            uint16_t nombreDUtilisateurs;
            uint32_t localId;
            uint32_t myPackId;
            // uint32_t PackId_1;
            // uint32_t PackId_2;
        // Suivi
            uint64_t valeurDeTest = 0; // pour info
        // Process thread
            HANDLE processThreadHandle;
            HANDLE startProcessEventHandle;
            HANDLE processEndedEventHandle;
            uint64_t cumulNotesATraiter;
            uint64_t cumulNotesRefusees;
            uint64_t remplissagePacksLocaux = 0;
            uint64_t remplissagePacksToSend = 0;
        // pour Application directe
            BOOL do_direct;
            HANDLE directThreadHandle;
            HANDLE startDirectEventHandle;
            HANDLE directEndedEventHandle;
        // pour SendThread
            BOOL do_send;
            HANDLE sendThreadHandle;
            sockaddr_in_t toDestSockAddr = { 0 };
            SOCKET sendSocketHandle;
            HANDLE sendStartEventHandle;
            HANDLE sendEndedEventHandle;
        // pour incomeThread
            BOOL do_recv;
            HANDLE incomeIocpHandle;
            SOCKET incomeSocketHandle;
            CRITICAL_SECTION incomeCriticalSection;
            int refusedUdp = 0;
        // pour ReceiveThread
            HANDLE receiveThreadHandle;
            HANDLE receiveStartEventHandle;
            HANDLE receiveFinishedEventHandle;
            int udp_packet_loss_percent = 0;
            int cpu_usage_percent = 0;
            int socket_queue_usage_percent = 0;
            int memory_usage_percent = 0;
            int network_usage_percent = 0;
            uint64_t remplissagePacksReceived = 0; // taux de remplissage de la pile de réception
            uint64_t cumulFluxIOCPEntrant = 0; // pour info
            uint64_t cumulMessagesAcceptes = 0; // pour info
            uint64_t cumulMessagesDetruits = 0; // pour info
#pragma endregion
#pragma region // ------------------------------------------------------------------------------- FONCTIONS PROTOTYPES
    // fichiers et donnees
    void find_first_active_ipv4();
    void read_input_file();
    void initialize_data();
    uint32_t pgcd(uint32_t a, uint32_t b);
    uint32_t ppcm(uint32_t a, uint32_t b);
    // console
    void ecrire_ligne_4(HANDLE consoleHandle, const char* texte, int64_t nombre, SHORT positionX, SHORT positionY);
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
    void ajouter_note_locale(uint32_t dmdr_prt, uint32_t demande, uint32_t parametre, uint32_t dest_prt);
    void ajouter_note_toSend(uint32_t dmdr_usr, uint32_t dmdr_prt, uint32_t demande, uint32_t parametre, uint32_t dest_usr, uint32_t dest_prt);
    // fenetre graphique
    int RegisterWindowClass(HINSTANCE hInstance);
    void CreateGraphicWindow(HINSTANCE hInstance);
    LRESULT CALLBACK WindowProc(HWND windowHandle, UINT uMsg, WPARAM wParam, LPARAM lParam);
    void InitializeBitmap();
    void retracer_la_fenetre();
    // communication reseau
    DWORD WINAPI ProcessThread(LPVOID param);
    DWORD WINAPI DirectThread(LPVOID param);
    DWORD WINAPI SendThread(LPVOID param);
    void InitIncomeIocp();
    void PostIncomeRecv(IOCP_UDP_PACKET* packet);
    DWORD WINAPI FillingThread(LPVOID param);
    DWORD WINAPI ReceiveThread(LPVOID param);
    void doApplicate(uint32_t dem_u, uint32_t dem_p, uint32_t dmand, uint32_t param, uint32_t dst_p);
    void RefreshSystemStats(SOCKET sock);
    // conversion
    uint32_t xyz_to_u32(id_prt id);
    id_prt u32_to_xyz(uint32_t num);
#pragma endregion
#pragma region // ------------------------------------------------------------------------- MAIN + VALEURS DE BASE
int main(int argc, char* argv[]) {
    // Informations préliminaires
    printf("Cervelet size XXX : %zu Mo \n",
        sizeof(crvDatas) / 1024 / 1024);
    printf("Local work sizes : %zu+%zu+%zu+%zu+%zu Mo \n",
        sizeof(localDests[0]) / 1024 / 1024,
        sizeof(localDests[1]) / 1024 / 1024,
        sizeof(localNotesStack[0]) / 1024 / 1024,
        sizeof(localNotesStack[1]) / 1024 / 1024,
        sizeof(localMessagesPacks) / 1024 / 1024);
    printf("Send sizes : %zu+%zu+%zu+%zu+%zu+%zu Mo \n",
        sizeof(netUsers) / 1024 / 1024,
        sizeof(toSendUsers)[0] / 1024 / 1024,
        sizeof(toSendUsers)[1] / 1024 / 1024,
        sizeof(toSendNotesStack[0]) / 1024 / 1024,
        sizeof(toSendNotesStack[1]) / 1024 / 1024,
        sizeof(toSendMessagesPacks) / 1024 / 1024);
    printf("Receive sizes : %zu+%zu+%zu+%zu Mo \n",
        sizeof(receiveDests) / 1024 / 1024,
        sizeof(receiveNotesStack[0]) / 1024 / 1024,
        sizeof(receiveNotesStack[1]) / 1024 / 1024,
        sizeof(receiveMessagesPacks) / 1024 / 1024);
    // parametrage
    localId = 0; // pour application locale
    myPackId = 1; // utilisateur
    // PackId_1 = 3; // destinataire de paquets de 2 messages
    // LoopBack = 65000; // destinataire de paquets de 1 message
    // mise à 0 des données
    initialize_data(); // Initialiser la structure de portions
    // instance
    instanceHandle = GetModuleHandleA(NULL); // récupération du handle d'instance
    // #------------------------------------------------------------------------- CREATION FENETRE ET ASSOCIES
    // Creer et enregistrer la classe de fenetre
    if (!RegisterWindowClass(instanceHandle)) {
        return -1;
    }
    // Creer la fenetre graphique
    CreateGraphicWindow(instanceHandle);
    ShowWindow(gWindowHandle, SW_SHOWNORMAL);
    UpdateWindow(gWindowHandle);
    // Initialiser le bitmap pour le tracage
    InitializeBitmap();
    // #------------------------------------------------------------------------- LECTURE DU FICHIER D'ENTREES
    read_input_file();
#pragma endregion
#pragma region // ------------------------------------------------------------------------- INITIALISATION DU CERVEAU
    uint32_t standard_uId = localId;
    uint32_t external_uId = myPackId;
    // uint32_t special_uId = PackId_2;
    // ; =============================================== COUCHE 0 (Data, Lecteurs, Pulseurs)
        // ; --------------- source de données texte en 1,0,0 POSITION IMPERATIVE => pour chargement de structure.crv
        p_port = (id_prt){ 1, 0, 0 };
        uint64_t adresse = (uint64_t)(uintptr_t)rawData_Texte;
        sub_creer_portion_datasource_4(p_port, adresse, longueur_Texte, 1);
        // ; --------------- réseau de lecteurs à gauche ->1
        p_port = (id_prt){ 5, 5, 0 };
        p_srce = (id_prt){ 1, 0, 0 };
        p_dest = (id_prt){ 5, 5, 1 };
        sub_creer_reseau(p_port, TYPE_LECTEUR, p_srce, standard_uId, p_dest, 250, 200, 1, 1);
        // ; --------------- réseau de pulseurs à droite ->1
        p_port = (id_prt){ 260, 45, 0 };
        p_dest = (id_prt){ 260, 45, 1 };
        sub_creer_reseau(p_port, TYPE_PULSEUR, null_port, standard_uId, p_dest, 250, 210, 1, 1);

        // ; =============================================== CHAINE DE TESTS SUR COUCHE UNIQUE - VIA INTERNE / EXTERNE
        uint16_t test_pos = 5;
        uint32_t test_uid = standard_uId;
        for (int key = 0; key < 2; key++) {

            // ; --------------- lecteurs
            p_port = (id_prt){ 270, test_pos, 0 };
            p_srce = (id_prt){ 1, 0, 0 };
            p_dest = (id_prt){ 290, test_pos, 0 };
            sub_creer_reseau(p_port, TYPE_LECTEUR, p_srce, test_uid, p_dest, 15, 15, 1, 1);
            // ; --------------- segments dendritiques
            p_port = (id_prt){ 290, test_pos, 0 };
            p_dest = (id_prt){ 310, test_pos, 0 };
            sub_creer_reseau(p_port, TYPE_SEGMENT_COMPLET, null_port, test_uid, p_dest, 15, 15, 1, 1);
            // ; ------------- neurones de réception
            p_port = (id_prt){ 310, test_pos, 0 };
            sub_creer_reseau(p_port, TYPE_NEURONE_COMPLET, null_port, test_uid, null_port, 15, 15, 1, 1);
            // ; --------------- extensions axonales
            p_port = (id_prt){ 330, test_pos, 0 };
            p_srce = (id_prt){ 310, test_pos, 0 };
            p_dest = (id_prt){ 350, test_pos, 0 };
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, test_uid, p_dest, 15, 15, 1, 1);
            // ; --------------- neurones finales
            p_port = (id_prt){ 350, test_pos, 0 };
            sub_creer_reseau(p_port, TYPE_NEURONE_COMPLET, null_port, test_uid, null_port, 15, 15, 1, 1);
            // ; --------------- extensions axonales
            p_port = (id_prt){ 370, test_pos, 0 };
            p_srce = (id_prt){ 350, test_pos, 0 };
            p_dest = (id_prt){ 410, test_pos, 0 };
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, test_uid, p_dest, 15, 15, 1, 1);
            // ; --------------- extensions axonales
            p_port = (id_prt){ 390, test_pos, 0 };
            p_srce = (id_prt){ 370, test_pos, 0 };
            p_dest = (id_prt){ 410, test_pos, 0 };
            sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, test_uid, p_dest, 15, 15, 1, 1);
            // ; --------------- segments dendritiques
            p_port = (id_prt){ 410, test_pos, 0 };
            p_dest = (id_prt){ 430, test_pos, 0 };
            sub_creer_reseau(p_port, TYPE_SEGMENT_COMPLET, null_port, test_uid, p_dest, 15, 15, 1, 1);
            // ; --------------- segments dendritiques
            p_port = (id_prt){ 430, test_pos, 0 };
            p_dest = (id_prt){ 450, test_pos, 0 };
            sub_creer_reseau(p_port, TYPE_SEGMENT_COMPLET, null_port, test_uid, p_dest, 15, 15, 1, 1);
            // ; ------------- neurones de réception
            p_port = (id_prt){ 450, test_pos, 0 };
            sub_creer_reseau(p_port, TYPE_NEURONE_COMPLET, null_port, test_uid, null_port, 15, 15, 1, 1);

            // CHANGEMENT DES PARAMETRES
            test_pos = 25;
            test_uid = external_uId;
        }

        // ; =============================================== CREATIONS PAR RESEAUX (couches 1 à 31)
    // ; -------------------------------------------------------------------------------- Pile simple
        // ; 1 --------------- réseau de neurones
        p_port = (id_prt){ 5, 5, 1 };
        sub_creer_reseau(p_port, TYPE_NEURONE_COMPLET, null_port, standard_uId, null_port, 505, 250, 1, 1);
        // ; 2 --------------- réseau d'extensions axonales 1-> ->3
        p_port = (id_prt){ 5, 5, 2 };
        p_srce = (id_prt){ 5, 5, 1 };
        p_dest = (id_prt){ 5, 5, 3 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 505, 250, 1, 1);
        // ; 3 --------------- réseau de segments dendritiques ->4
        p_port = (id_prt){ 5, 5, 3 };
        p_dest = (id_prt){ 5, 5, 4 };
        sub_creer_reseau(p_port, TYPE_SEGMENT_COMPLET, null_port, standard_uId, p_dest, 505, 250, 1, 1);
        // ; 4 --------------- réseau de neurones
        p_port = (id_prt){ 5, 5, 4 };
        sub_creer_reseau(p_port, TYPE_NEURONE_COMPLET, null_port, standard_uId, null_port, 505, 250, 1, 1);
    // ; -------------------------------------------------------------------------------- Diffusion 2x2
        // ; 5 --------------- réseau d'extensions axonales 4-> ->9
        p_port = (id_prt){ 5, 5, 5 };
        p_srce = (id_prt){ 5, 5, 4 };
        p_dest = (id_prt){ 5, 5, 9 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 504, 249, 1, 1);
        // ; 6 --------------- réseau d'extensions axonales 4-> ->9
        p_port = (id_prt){ 5, 5, 6 };
        p_srce = (id_prt){ 5, 5, 4 };
        p_dest = (id_prt){ 5, 6, 9 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 504, 249, 1, 1);
        // ; 7 --------------- réseau d'extensions axonales 4-> ->9
        p_port = (id_prt){ 5, 5, 7 };
        p_srce = (id_prt){ 5, 5, 4 };
        p_dest = (id_prt){ 6, 5, 9 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 504, 249, 1, 1);
        // ; 8 --------------- réseau d'extensions axonales 4-> ->9
        p_port = (id_prt){ 5, 5, 8 };
        p_srce = (id_prt){ 5, 5, 4 };
        p_dest = (id_prt){ 6, 6, 9 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 504, 249, 1, 1);
    // ; -------------------------------------------------------------------------------- Neurones simples
        // ; 9 --------------- réseau de neurones
        p_port = (id_prt){ 5, 5, 9 };
        sub_creer_reseau(p_port, TYPE_NEURONE_COMPLET, null_port, standard_uId, null_port, 505, 250, 1, 1);
    // ; -------------------------------------------------------------------------------- Diffusion 3x3
        // ; 10 --------------- réseau d'extensions axonales 9-> ->19
        p_port = (id_prt){ 5, 5, 10 };
        p_srce = (id_prt){ 5, 5, 9 };
        p_dest = (id_prt){ 5, 5, 19 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 503, 248, 1, 1);
        // ; 11 --------------- réseau d'extensions axonales 9-> ->19
        p_port = (id_prt){ 5, 5, 11 };
        p_srce = (id_prt){ 5, 5, 9 };
        p_dest = (id_prt){ 5, 6, 19 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 503, 248, 1, 1);
        // ; 12 --------------- réseau d'extensions axonales 9-> ->19
        p_port = (id_prt){ 5, 5, 12 };
        p_srce = (id_prt){ 5, 5, 9 };
        p_dest = (id_prt){ 5, 7, 19 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 503, 248, 1, 1);
        // ; 13 --------------- réseau d'extensions axonales 9-> ->19
        p_port = (id_prt){ 5, 5, 13 };
        p_srce = (id_prt){ 5, 5, 9 };
        p_dest = (id_prt){ 6, 5, 19 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 503, 248, 1, 1);
        // ; 14 --------------- réseau d'extensions axonales 9-> ->19
        p_port = (id_prt){ 5, 5, 14 };
        p_srce = (id_prt){ 5, 5, 9 };
        p_dest = (id_prt){ 6, 6, 19 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 503, 248, 1, 1);
        // ; 15 --------------- réseau d'extensions axonales 9-> ->19
        p_port = (id_prt){ 5, 5, 15 };
        p_srce = (id_prt){ 5, 5, 9 };
        p_dest = (id_prt){ 6, 7, 19 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 503, 248, 1, 1);
        // ; 16 --------------- réseau d'extensions axonales 9-> ->19
        p_port = (id_prt){ 5, 5, 16 };
        p_srce = (id_prt){ 5, 5, 9 };
        p_dest = (id_prt){ 7, 5, 19 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 503, 248, 1, 1);
        // ; 17 --------------- réseau d'extensions axonales 9-> ->19
        p_port = (id_prt){ 5, 5, 17 };
        p_srce = (id_prt){ 5, 5, 9 };
        p_dest = (id_prt){ 7, 6, 19 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 503, 248, 1, 1);
        // ; 18 --------------- réseau d'extensions axonales 9-> ->19
        p_port = (id_prt){ 5, 5, 18 };
        p_srce = (id_prt){ 5, 5, 9 };
        p_dest = (id_prt){ 7, 7, 19 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 503, 248, 1, 1);
    // ; -------------------------------------------------------------------------------- Pile minimale
        // ; 19 --------------- réseau de neurones
        p_port = (id_prt){ 5, 5, 19 };
        sub_creer_reseau(p_port, TYPE_NEURONE_COMPLET, null_port, standard_uId, null_port, 505, 250, 1, 1);
        // ; 20 --------------- réseau d'extensions axonales 19-> ->21
        p_port = (id_prt){ 5, 5, 20 };
        p_srce = (id_prt){ 5, 5, 19 };
        p_dest = (id_prt){ 5, 5, 21 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 505, 250, 1, 1);
    // ; -------------------------------------------------------------------------------- Chaine dendritique
        // ; 21 --------------- réseau de segments dendritiques ->22
        p_port = (id_prt){ 5, 5, 21 };
        p_dest = (id_prt){ 5, 5, 22 };
        sub_creer_reseau(p_port, TYPE_SEGMENT_COMPLET, null_port, standard_uId, p_dest, 505, 250, 1, 1);
        // ; 22 --------------- réseau de segments dendritiques ->23
        p_port = (id_prt){ 5, 5, 22 };
        p_dest = (id_prt){ 5, 5, 23 };
        sub_creer_reseau(p_port, TYPE_SEGMENT_COMPLET, null_port, standard_uId, p_dest, 505, 250, 1, 1);
        // ; 23 --------------- réseau de segments dendritiques ->24
        p_port = (id_prt){ 5, 5, 23 };
        p_dest = (id_prt){ 5, 5, 24 };
        sub_creer_reseau(p_port, TYPE_SEGMENT_COMPLET, null_port, standard_uId, p_dest, 505, 250, 1, 1);
    // ; -------------------------------------------------------------------------------- Neurones simples
        // ; 24 --------------- réseau de neurones
        p_port = (id_prt){ 5, 5, 24 };
        sub_creer_reseau(p_port, TYPE_NEURONE_COMPLET, null_port, standard_uId, null_port, 505, 250, 1, 1);
    // ; -------------------------------------------------------------------------------- Concentration axonale
        // ; 25 --------------- réseau d'extensions axonales 4-> ->28
        p_port = (id_prt){ 5, 5, 25 };
        p_srce = (id_prt){ 5, 5, 4 };
        p_dest = (id_prt){ 5, 5, 28 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 505, 250, 1, 1);
        // ; 26 --------------- réseau d'extensions axonales 9-> ->29
        p_port = (id_prt){ 5, 5, 26 };
        p_srce = (id_prt){ 5, 5, 9 };
        p_dest = (id_prt){ 5, 5, 28 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 505, 250, 1, 1);
        // ; 27 --------------- réseau d'extensions axonales 19-> ->29
        p_port = (id_prt){ 5, 5, 27 };
        p_srce = (id_prt){ 5, 5, 19 };
        p_dest = (id_prt){ 5, 5, 28 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 505, 250, 1, 1);
    // ; -------------------------------------------------------------------------------- Neurones simples
        // ; 28 --------------- réseau de neurones
        p_port = (id_prt){ 5, 5, 28 };
        sub_creer_reseau(p_port, TYPE_NEURONE_COMPLET, null_port, standard_uId, null_port, 505, 250, 1, 1);
    // ; -------------------------------------------------------------------------------- Rétro-influx divers
        // ; 29 --------------- réseau d'extensions axonales 28-> ->1
        p_port = (id_prt){ 100, 50, 29 };
        p_srce = (id_prt){ 100, 50, 28 };
        p_dest = (id_prt){ 100, 50, 1 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 300, 40, 1, 1);
        // ; 30 --------------- réseau d'extensions axonales 28-> ->9
        p_port = (id_prt){ 100, 150, 30 };
        p_srce = (id_prt){ 100, 150, 28 };
        p_dest = (id_prt){ 100, 150, 9 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 300, 40, 1, 1);
        // ; 31 --------------- réseau d'extensions axonales 28-> ->19
        p_port = (id_prt){ 100, 150, 31 };
        p_srce = (id_prt){ 100, 150, 28 };
        p_dest = (id_prt){ 100, 150, 19 };
        sub_creer_reseau(p_port, TYPE_EXTENSION_COMPLET, p_srce, standard_uId, p_dest, 300, 40, 1, 1);
#pragma endregion
#pragma region // ------------------------------------------------------------------------- SOCKETS THREADS EVENEMENTS
    // -------------------- Initialisation de WinSock
        net_Okay = TRUE;
        if (WSAStartup(WSA_VERSION, &wsaDataStruct) != 0) {
            printf("Échec de l'initialisation de WinSock\n");
            net_Okay = FALSE;
        }
    // -------------------- Pour process portions
        // Creation de l'evenement de lancement du process
        startProcessEventHandle = CreateEventA(NULL, FALSE, FALSE, NULL);
        if (startProcessEventHandle == NULL) {
            printf("Échec de la creation de l'evenement startProcessEventHandle : %lu\n", GetLastError());
            return EXIT_FAILURE;
        }
        // Creation de l'evenement de fin de process
        processEndedEventHandle = CreateEventA(NULL, FALSE, FALSE, NULL);
        if (processEndedEventHandle == NULL) {
            printf("Échec de la creation de l'evenement processEndedEventHandle : %lu\n", GetLastError());
            return EXIT_FAILURE;
        }
        // creation du thread de process
        processThreadHandle = CreateThread(NULL, 0, ProcessThread, NULL, 0, NULL);
        if (processThreadHandle == NULL) {
            printf("Echec de la creation du thread processThreadHandle : %lu\n", GetLastError());
            return EXIT_FAILURE;
        }
    // -------------------- pour application directe
        // Creation de l'evenement de lancement de l'application
        startDirectEventHandle = CreateEventA(NULL, FALSE, FALSE, NULL);
        if (startDirectEventHandle == NULL) {
            printf("Échec de la creation de l'evenement startDirectEventHandle : %lu\n", GetLastError());
            return EXIT_FAILURE;
        }
        // Creation de l'evenement de fin de traitement des portions
        directEndedEventHandle = CreateEventA(NULL, FALSE, FALSE, NULL);
        if (directEndedEventHandle == NULL) {
            printf("Échec de la creation de l'evenement directEndedEventHandle : %lu\n", GetLastError());
            return EXIT_FAILURE;
        }
        // creation du thread local
        directThreadHandle = CreateThread(NULL, 0, DirectThread, NULL, 0, NULL);
        if (directThreadHandle == NULL) {
            printf("Echec de la creation du thread directThreadHandle : %lu\n", GetLastError());
            return EXIT_FAILURE;
        }
    // -------------------- Pour envoi messages
        // creation socket
        sendSocketHandle = socket(AF_INET_IPV4, SOCK_DGRAM, DEFAULT_PROTOCOL);
        if (sendSocketHandle == INVALID_SOCKET) {
            printf("Échec de la création du socket d envoi\n");
            net_Okay = FALSE;
        }
        // Creation de l'evenement de declenchement de l'envoi
        sendStartEventHandle = CreateEventA(NULL, FALSE, FALSE, NULL);
        if (sendStartEventHandle == NULL) {
            printf("Échec de la creation de sendStartEventHandle : %lu\n", GetLastError());
            net_Okay = FALSE;
        }
        // Creation de l'evenement de fin de l'envoi
        sendEndedEventHandle = CreateEventA(NULL, FALSE, FALSE, NULL);
        if (sendEndedEventHandle == NULL) {
            printf("Échec de la creation de sendEndedEventHandle : %lu\n", GetLastError());
            net_Okay = FALSE;
        }
        // creation du thread d'envoi
        sendThreadHandle = CreateThread(NULL, 0, SendThread, NULL, 0, NULL);
        if (sendThreadHandle == NULL) {
            printf("Echec de la creation du thread d envoi\n");
            closesocket(sendSocketHandle);
            net_Okay = FALSE;
        }
    // -------------------- Pour reception messages IOCP
        incomeSocketHandle = WSASocket(AF_INET, SOCK_DGRAM, 0, NULL, 0, WSA_FLAG_OVERLAPPED);
        int incomeBufferSize = 256 * 1024 * 1024; // 256 Mo de buffer
        setsockopt(incomeSocketHandle, SOL_SOCKET, SO_RCVBUF, (char*)&incomeBufferSize, sizeof(incomeBufferSize));
        sockaddr_in_t serverAddr = { 0 };
        serverAddr.sin_family = AF_INET;
        serverAddr.sin_port = htons(INPORT);
        serverAddr.sin_addr.s_addr = INADDR_ANY;
        int bindResult = bind(incomeSocketHandle, (struct sockaddr*)&serverAddr, sizeof(serverAddr));
        if (bindResult == SOCKET_ERROR) {
            printf("Erreur du bind iocp : %d\n", WSAGetLastError());
            net_Okay = FALSE;
        }
        InitIncomeIocp();
        InitializeCriticalSection(&incomeCriticalSection);
        // Lancer les threads de traitement IOCP
        for (int i = 0; i < 4; i++) {
            CreateThread(NULL, 0, FillingThread, NULL, 0, NULL);
        }
        // Poster des lectures UDP
        for (int i = 0; i < 10; i++) {
            IOCP_UDP_PACKET* packet = (IOCP_UDP_PACKET*)malloc(sizeof(IOCP_UDP_PACKET));
            memset(packet, 0, sizeof(IOCP_UDP_PACKET));
            PostIncomeRecv(packet);
        }
    // -------------------- Pour reception messages
        // Creation de l'evenement de declenchement de la reception
        receiveStartEventHandle = CreateEventA(NULL, TRUE, FALSE, NULL);
        if (receiveStartEventHandle == NULL) {
            printf("Échec de la creation de receiveStartEventHandle : %lu\n", GetLastError());
            net_Okay = FALSE;
        }
        // Création de l'evenement de fin d'application des recus
        receiveFinishedEventHandle = CreateEventA(NULL, TRUE, FALSE, NULL);
        if (receiveFinishedEventHandle == NULL) {
            printf("Échec de la creation de receiveFinishedEventHandle : %lu\n", GetLastError());
            net_Okay = FALSE;
        }
        // creation du thread
        receiveThreadHandle = CreateThread(NULL, 0, ReceiveThread, NULL, 0, NULL);
        if (receiveThreadHandle == NULL) {
            printf("Echec de la creation de receiveThreadHandle : %lu\n", GetLastError());
            net_Okay = FALSE;
        }
#pragma endregion
#pragma region // ------------------------------------------------------------------------- PREPARATIFS RESEAU
    sockaddr_in_t work_sock_addr;
    memset(&work_sock_addr, 0, sizeof(work_sock_addr));
    // ----------------------- adresse de cet ordinateur
    find_first_active_ipv4();
    if (my_ip_string[0] == '\0') {
        net_Okay = FALSE;
        net_Server = TRUE;
    }
    else {
        net_Okay = TRUE;
        if (argc > 1) {
            ip_master = inet_pton(AF_INET, argv[1], &(work_sock_addr.sin_addr));
            if (ip_master > 0) {
                net_Server = FALSE;
                ip_master_string = argv[1];
                memcpy(texte_mode_dependant + 18, ip_master_string, 15);
            }
            else {
                net_Server = TRUE;
                memcpy(texte_mode_maitre + 14, "mon_ip........", 15);
            }
        }
        else {
            net_Server = TRUE;
            memcpy(texte_mode_maitre + 14, my_ip_string, 15);
        }
    }
    do_direct = TRUE;
    do_send = net_Okay;
    do_recv = net_Okay;
    // ----------------------- utilisateur 0 - local self
    nombreDUtilisateurs = 0;
    // ----------------------- utilisateur 1 - maitre
    nombreDUtilisateurs = 1;
    DO_CONVERT("192.168.1.124", work_sock_addr); // Philippe
    FOR_U32_TO(netUsers, TAI_USR_NET, nombreDUtilisateurs, OUN_IPAD_4) = work_sock_addr.sin_addr.s_addr;
    FOR_U16_TO(netUsers, TAI_USR_NET, nombreDUtilisateurs, OUN_PORT_2) = htons(INPORT);
    // ----------------------- utilisateur 2
    nombreDUtilisateurs = 2;
    DO_CONVERT("192.168.1.21", work_sock_addr); // Dorian
    FOR_U32_TO(netUsers, TAI_USR_NET, nombreDUtilisateurs, OUN_IPAD_4) = work_sock_addr.sin_addr.s_addr;
    FOR_U16_TO(netUsers, TAI_USR_NET, nombreDUtilisateurs, OUN_PORT_2) = htons(INPORT);
    // ----------------------- utilisateur 3
    nombreDUtilisateurs = 3;
    DO_CONVERT("192.168.1.16", work_sock_addr); // Va wifi
    FOR_U32_TO(netUsers, TAI_USR_NET, nombreDUtilisateurs, OUN_IPAD_4) = work_sock_addr.sin_addr.s_addr;
    FOR_U16_TO(netUsers, TAI_USR_NET, nombreDUtilisateurs, OUN_PORT_2) = htons(INPORT);
    // ----------------------- utilisateur 4
    nombreDUtilisateurs = 4;
    DO_CONVERT("192.168.1.1", work_sock_addr); // Box
    FOR_U32_TO(netUsers, TAI_USR_NET, nombreDUtilisateurs, OUN_IPAD_4) = work_sock_addr.sin_addr.s_addr;
    FOR_U16_TO(netUsers, TAI_USR_NET, nombreDUtilisateurs, OUN_PORT_2) = htons(8080);
    // ----------------------- utilisateur 65000
    nombreDUtilisateurs = 15;
    DO_CONVERT("192.168.1.124", work_sock_addr); // Loopback
    FOR_U32_TO(netUsers, TAI_USR_NET, nombreDUtilisateurs, OUN_IPAD_4) = work_sock_addr.sin_addr.s_addr;
    FOR_U16_TO(netUsers, TAI_USR_NET, nombreDUtilisateurs, OUN_PORT_2) = htons(INPORT);
    // ----------------------- nombre total d'utilisateurs
    nombreDUtilisateurs++;
#pragma endregion
#pragma region // ------------------------------------------------------------------------- BOUCLE PRINCIPALE
    verifier_portions();
    numero_de_boucle_lente = 0;
    // boucle grande
    while (1) {
        timer_avant_moyennes = clock();
        // boucle moyenne
        do {
            timer_avant_rapides = clock();
            // boucle rapide
            do {
                // -------------------------------------------------------------------------------------------------
                // Valeur de début pour calcul de durées
                debut_cycle = clock();
                // Lancement du traitement des portions
                if (!SetEvent(startProcessEventHandle)) {
                    printf("Erreur lors de l'activation de startProcessEventHandle. Code : %lu\n", GetLastError());
                    return EXIT_FAILURE;
                }
                // Attente de fin du traitement des portions -> creation des notes locales et toSend
                WaitForSingleObject(processEndedEventHandle, INFINITE);
                if (!ResetEvent(processEndedEventHandle)) {
                    printf("Probleme de reset de l'evenement processEndedEventHandle. Code : %lu\n", GetLastError());
                    return EXIT_FAILURE;
                }
                // Calcul de la durée de cycle
                fin_cycle = clock();
                duree_cycle = (uint64_t)(1000 * (fin_cycle - debut_cycle) / CLOCKS_PER_SEC);
                // -------------------------------------------------------------------------------------------------
                // Si envoi activé
                if (do_send) {
                    // Déclenchement du thread d'expedition => traitement de la pile notesStack
                    if (!SetEvent(sendStartEventHandle)) {
                        printf("Erreur lors de l'activation de sendStartEventHandle. Code : %lu\n", GetLastError());
                        return EXIT_FAILURE;
                    }
                }
                // Sinon
                else {
                    // changement de pile
                    toSendStackToFill = 1 - toSendStackToFill;
                    // initialisation de la nouvelle pile à remplir
                    toSendStackIndex[toSendStackToFill] = 0;
                    // Reinitialiser la pile précédente comme si on l'avait utilisée
                    for (uint16_t destUtil = 0; destUtil < nombreDUtilisateurs; destUtil++) {
                        FOR_U64_TO(toSendUsers[1 - toSendStackToFill], TAI_SND_USR, destUtil, OSU_NMES_8) = 0;
                    }
                }
                // -------------------------------------------------------------------------------------------------
                // Si application directe activée
                if (do_direct) {
                    for (uint8_t iterate = 0; iterate < 4; iterate++) {
                        // Déclenchement du thread d'application directe => traitement de la pile localsStack
                        if (!SetEvent(startDirectEventHandle)) {
                            printf("Erreur lors de l'activation de startDirectEventHandle. Code : %lu\n", GetLastError());
                            return EXIT_FAILURE;
                        }
                        // Attendre la fin de la partie application du thread direct
                        WaitForSingleObject(directEndedEventHandle, INFINITE);
                        if (!ResetEvent(directEndedEventHandle)) {
                            printf("Probleme de reset de l'evenement directEndedEventHandle. Code : %lu\n", GetLastError());
                            return EXIT_FAILURE;
                        }
                    }
                }
                // Sinon
                else {
                    // changement de pile
                    localStackToFill = 1 - localStackToFill;
                    // initialisation de la nouvelle pile à remplir
                    localStackIndex[localStackToFill] = 0;
                    // Reinitialiser la pile précédente comme si on l'avait utilisée
                    for (uint32_t destPort = 0; destPort < NBR_CRVPS; destPort++) {
                        FOR_U32_TO(localDests[1 - localStackToFill], TAI_LOC_DST, destPort, OLD_NMSGS_4) = 0;
                    }
                }
                // Trouver un moyen d'estimer s'il faut encore continuer à recevoir des messages
                //
                //
                //
                // Si envoi activé, on attend la fin de l'envoi
                if (do_send) {
                    // Attendre la fin du thread d'envoi
                    WaitForSingleObject(sendEndedEventHandle, INFINITE);
                    if (!ResetEvent(sendEndedEventHandle)) {
                        printf("Probleme de reset de l'evenement sendEndedEventHandle. Code : %lu\n", GetLastError());
                        return EXIT_FAILURE;
                    }
                }
                // -------------------------------------------------------------------------------------------------
                // Si réception activée
                if (do_recv) {
                    // Lancer l'application des messages reçus jusque-là
                    if (!SetEvent(receiveStartEventHandle)) {
                        printf("Erreur lors de l'activation de receiveStartEventHandle. Code : %lu\n", GetLastError());
                        return EXIT_FAILURE;
                    }
                    // Attendre la fin de l'application des messages recus
                    WaitForSingleObject(receiveFinishedEventHandle, INFINITE);
                    if (!ResetEvent(receiveFinishedEventHandle)) {
                        printf("Probleme de reset de l'evenement receiveFinishedEventHandle. Code : %lu\n", GetLastError());
                        return EXIT_FAILURE;
                    }
                }
                // Sinon
                else {
                    // Changer de buffer et le reinitialiser
                    EnterCriticalSection(&incomeCriticalSection);
                    receiveStackToFill = 1 - receiveStackToFill;
                    receiveStackIndex[receiveStackToFill] = 0;
                    LeaveCriticalSection(&incomeCriticalSection);
                    // Détruire les messages reçus
                    receiveStackIndex[1 - receiveStackToFill] = 0;
                }
                // -------------------------------------------------------------------------------------------------
#pragma endregion
#pragma region // ------------------------------------------------------------------------- BOUCLAGES
                // Pré-calcul indicateurs
                remplissagePacksLocaux = 100 * remplissagePacksLocaux * sizeof(localMessagesPacks[0]) / sizeof(localMessagesPacks);
                remplissagePacksToSend = 100 * remplissagePacksToSend * sizeof(toSendMessagesPacks[0]) / sizeof(toSendMessagesPacks);
                remplissagePacksReceived = 100 * remplissagePacksReceived * sizeof(receiveMessagesPacks[0]) / sizeof(receiveMessagesPacks);
                timer_en_fin_de_rapides = clock();
                dureeDePostTraitement = (uint64_t)(1000 * (timer_en_fin_de_rapides - fin_cycle) / CLOCKS_PER_SEC);
                //
                if (mode_pas_a_pas != 0) {
                    break;
                }
                duree_cumul_des_rapides = (uint64_t)(1000 * (timer_en_fin_de_rapides - timer_avant_rapides) / CLOCKS_PER_SEC);
            } while (duree_cumul_des_rapides < DUREE_BOUCLE_MOYENNE_MS);
            // sortie du bouclage rapide
            retracer_la_fenetre();
            // ecritures si pas a pas
            if (mode_pas_a_pas != 0) {
                verifier_portions();
                RefreshSystemStats(incomeSocketHandle);
                ecrire_messages_console();
            }
            BOOL there_is_message;
            MSG msg = { 0 };
            // boucle sur messages fenetre
            while (1) {
                there_is_message = PeekMessageA(&msg, NULL, 0, 0, PM_NOREMOVE);
                if (!there_is_message) {
                    if (mode_pas_a_pas == 0) {
                        break; // sortie si plus de message + pas mode pas-a-pas
                    }
                }
                if (mode_pas_a_pas == 2) {
                    mode_pas_a_pas = 1;
                    break; // sortie si N appuye en mode pas-a-pas
                }
                if (there_is_message) {
                    if (mode_pas_a_pas != 0) {
                        retracer_la_fenetre();
                        verifier_portions();
                        RefreshSystemStats(incomeSocketHandle);
                        ecrire_messages_console();
                    }
                }
                there_is_message = GetMessageA(&msg, NULL, 0, 0);
                if (there_is_message == 0) {
                    goto sortieComplete;
                }
                if (!IsDialogMessageA(gWindowHandle, &msg)) {
                    TranslateMessage(&msg);
                    DispatchMessageA(&msg);
                }
            }
            // Sortie uniquement sur un break
            // - soit pas mode pas-a-pas + tous les messages ont ete traites
            // - mode pas-a-pas + soit appui sur N
            timer_en_fin_de_moyennes = clock(); // End time measurement
            duree_cumul_des_moyennes = (uint64_t)(1000 * (timer_en_fin_de_moyennes - timer_avant_moyennes) / CLOCKS_PER_SEC);
        } while (duree_cumul_des_moyennes < DUREE_BOUCLE_LENTE_MS);
        // sortie de la boucle moyenne
        if (mode_pas_a_pas == 0) {
            verifier_portions();
            RefreshSystemStats(incomeSocketHandle);
            ecrire_messages_console();
        }
        // indicateurs de charge
        remplissagePacksLocaux = 0;
        remplissagePacksToSend = 0;
        remplissagePacksReceived = 0;
        // Indicateurs cumulatifs
            // cumulNotesATraiter = 0;
            // cumulNotesRefusees = 0;
            // cumulFluxIOCPEntrant = 0;
            // cumulMessagesDetruits = 0;
            // cumulMessagesAcceptes = 0;
        // variables fonctionnelles
        numero_de_boucle_lente++;
    }
#pragma endregion
#pragma region // ------------------------------------------------------------------------- SORTIE
    sortieComplete:
    // Nettoyage
    closesocket(sendSocketHandle);
    closesocket(incomeSocketHandle);
    CloseHandle(incomeIocpHandle);
    WSACleanup();
    // Liberation de critical section
    DeleteCriticalSection(&incomeCriticalSection);
    // Liberation de la memoire
    free(bitmapData);
    free(rawData_Texte);
    // Cleanup console
    CleanupConsole();
    return 0;
}
#pragma endregion
#pragma region // ------------------------------------------------------------------------------ OUTILS CREATIONS ET VERIFS
    void verifier_portions() {
        ciblesValidees = 0;
        ciblesExternes = 0;
        erreurCibleNulle = 0;
        erreurCibleIncoherente = 0;
        for (uint32_t portion = 0; portion < DIMENSION_Z * DIMENSION_Y * DIMENSION_X; portion++) {
            uint8_t typestev = U8_FROM(crvDatas, TAI_CRVPS, portion, OCP_TYPESTEV_1);
            switch (typestev) {
            case TYPE_LECTEUR: {
                uint32_t portAccesMemU = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_L_PAM_U_4);
                if (portAccesMemU == 0) {
                    uint32_t portAccesMemN = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_L_PAM_N_4);
                    if (portAccesMemN == 0) {
                        erreurCibleNulle++;
                    }
                    else {
                        uint8_t typeSource = U8_FROM(crvDatas, TAI_CRVPS, portAccesMemN, OCP_TYPESTEV_1);
                        if (typeSource == TYPE_SOURCE) {
                            ciblesValidees++;
                        }
                        else {
                            erreurCibleIncoherente++;
                        }

                    }
                }
                else {
                    ciblesExternes++;
                }
                uint32_t portDestinationU = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_L_DST_U_4);
                if (portDestinationU == 0) {
                    uint32_t portDestinationN = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_L_DST_N_4);
                    if (portDestinationN == 0) {
                        erreurCibleNulle++;
                    }
                    else {
                        uint8_t typeDestin = U8_FROM(crvDatas, TAI_CRVPS, portDestinationN, OCP_TYPESTEV_1);
                        if (typeDestin == TYPE_SEGMENT_COMPLET || typeDestin == TYPE_NEURONE_COMPLET) {
                            ciblesValidees++;
                        }
                        else {
                            erreurCibleIncoherente++;
                        }
                    }
                }
                else {
                    ciblesExternes++;
                }
                break;
            }
            case TYPE_PULSEUR: {
                uint32_t portDestinationU = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_P_DST_U_4);
                if (portDestinationU == 0) {
                    uint32_t portDestinationN = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_P_DST_N_4);
                    if (portDestinationN == 0) {
                        erreurCibleNulle++;
                    }
                    else {
                        uint8_t typeDestin = U8_FROM(crvDatas, TAI_CRVPS, portDestinationN, OCP_TYPESTEV_1);
                        if (typeDestin == TYPE_SEGMENT_COMPLET || typeDestin == TYPE_NEURONE_COMPLET) {
                            ciblesValidees++;
                        }
                        else {
                            erreurCibleIncoherente++;
                        }
                    }
                }
                else {
                    ciblesExternes++;
                }
                break;
            }
            case TYPE_SEGMENT_COMPLET: {
                uint32_t portSuivantU = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_S_SVT_U_4);
                if (portSuivantU == 0) {
                    uint32_t portSuivantN = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_S_SVT_N_4);
                    if (portSuivantN == 0) {
                        erreurCibleNulle++;
                    }
                    else {
                        uint8_t typeSuivant = U8_FROM(crvDatas, TAI_CRVPS, portSuivantN, OCP_TYPESTEV_1);
                        if (typeSuivant == TYPE_SEGMENT_COMPLET || typeSuivant == TYPE_NEURONE_COMPLET) {
                            ciblesValidees++;
                        }
                        else {
                            erreurCibleIncoherente++;
                        }
                    }
                }
                else {
                    ciblesExternes++;
                }
                break;
            }
            case TYPE_EXTENSION_COMPLET: {
                uint32_t portAntecedentU = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_ANT_U_4);
                if (portAntecedentU == 0) {
                    uint32_t portAntecedentN = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_ANT_N_4);
                    if (portAntecedentN == 0) {
                        erreurCibleNulle++;
                    }
                    else {
                        uint8_t typeAntec = U8_FROM(crvDatas, TAI_CRVPS, portAntecedentN, OCP_TYPESTEV_1);
                        if (typeAntec == TYPE_NEURONE_COMPLET || typeAntec == TYPE_EXTENSION_COMPLET) {
                            ciblesValidees++;
                        }
                        else {
                            erreurCibleIncoherente++;
                        }
                    }
                }
                else {
                    ciblesExternes++;
                }
                uint32_t portDestinationU = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_DST_U_4);
                if (portDestinationU == 0) {
                    uint32_t portDestinationN = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_DST_N_4);
                    if (portDestinationN == 0) {
                        erreurCibleNulle++;
                    }
                    else {
                        uint8_t typeDestin = U8_FROM(crvDatas, TAI_CRVPS, portDestinationN, OCP_TYPESTEV_1);
                        if (typeDestin == TYPE_SEGMENT_COMPLET || typeDestin == TYPE_NEURONE_COMPLET) {
                            ciblesValidees++;
                        }
                        else {
                            erreurCibleIncoherente++;
                        }
                    }
                }
                else {
                    ciblesExternes++;
                }
                break;
            }
            }
        }
    }
    void sub_creer_portion_datasource_4(id_prt port, uint64_t addr, uint16_t nmbr, uint8_t larg) {
        uint32_t portion = xyz_to_u32(port);
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_TYPESTEV_1) = TYPE_SOURCE; // type de portion
        FOR_U64_TO(crvDatas, TAI_CRVPS, portion, OCP_D_ADDRS_8) = addr; // adresse memoire locale
        FOR_U16_TO(crvDatas, TAI_CRVPS, portion, OCP_D_NBSEG_2) = nmbr; // nombre de segments de donnees
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_D_LGSEG_1) = larg; // largeur du segment de donnees
    }
    void sub_creer_portion_lecteur_7(id_prt port, id_prt srce, uint16_t idxlect, uint8_t bitlect, uint8_t persisb, int8_t valblocb, id_prt dest) {
        uint32_t portion = xyz_to_u32(port);
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_TYPESTEV_1) = TYPE_LECTEUR; // type de portion
        FOR_U32_TO(crvDatas, TAI_CRVPS, portion, OCP_L_PAM_N_4) = xyz_to_u32(srce); // source
        FOR_U32_TO(crvDatas, TAI_CRVPS, portion, OCP_L_PAM_U_4) = 0; // meme utilisateur source
        FOR_U16_TO(crvDatas, TAI_CRVPS, portion, OCP_L_INDEX_2) = idxlect; // index de lecture
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_L_NMBIT_1) = bitlect; // bit de lecture
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_L_PERSB_1) = persisb; // persistance de base
        FOR_S8_TO(crvDatas, TAI_CRVPS, portion, OCP_L_BSYNV_1) = valblocb; // valeur du bloc de boutons
        FOR_U32_TO(crvDatas, TAI_CRVPS, portion, OCP_L_DST_N_4) = xyz_to_u32(dest); // destination (degment ou neurone)
        FOR_U32_TO(crvDatas, TAI_CRVPS, portion, OCP_L_DST_U_4) = 0; // destinataire
    }
    void sub_creer_portion_pulseur_4(id_prt port, uint8_t seuil, uint8_t valblocb, uint32_t dest_u, id_prt dest) {
        uint32_t portion = xyz_to_u32(port);
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_TYPESTEV_1) = TYPE_PULSEUR; // type de portion
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_P_SEUIL_1) = seuil; // seuil
        FOR_S8_TO(crvDatas, TAI_CRVPS, portion, OCP_P_BSYNV_1) = valblocb; // valeur du bloc de boutons
        FOR_U32_TO(crvDatas, TAI_CRVPS, portion, OCP_P_DST_N_4) = xyz_to_u32(dest); // destination (segment ou neurone)
        FOR_U32_TO(crvDatas, TAI_CRVPS, portion, OCP_P_DST_U_4) = dest_u; // meme utilisateur destinatire
    }
    void sub_creer_segment_dendritique_6(id_prt port, uint8_t typeSegm, uint8_t nivTempA, uint8_t potSegR, uint16_t synDispo, id_prt dest) {
        uint32_t portion = xyz_to_u32(port);
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_TYPESTEV_1) = typeSegm; // type de segment
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_NIVACTMP_1) = nivTempA; // niveau temporel d'activité
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_S_PRSEG_1) = potSegR; // potentiel de segments restants
        FOR_U16_TO(crvDatas, TAI_CRVPS, portion, OCP_S_PSYNA_2) = synDispo; // nombre de synapses disponibles
        FOR_U32_TO(crvDatas, TAI_CRVPS, portion, OCP_S_SVT_N_4) = xyz_to_u32(dest); // destination (segment ou neurone)
        FOR_U32_TO(crvDatas, TAI_CRVPS, portion, OCP_S_SVT_U_4) = 0; // meme utilisateur destinataire
        FOR_S16_TO(crvDatas, TAI_CRVPS, portion, OCP_S_CHARG1_2) = CHARGE_INVALIDE;
        FOR_S16_TO(crvDatas, TAI_CRVPS, portion, OCP_S_CHARG2_2) = CHARGE_INVALIDE;
        FOR_S16_TO(crvDatas, TAI_CRVPS, portion, OCP_S_CHARG3_2) = CHARGE_INVALIDE;
    }
    void sub_creer_portion_neurone_10(id_prt port, uint8_t typeNeur, uint8_t orient, uint8_t pRay, uint8_t pPlan, uint8_t pApi, uint8_t pPan, uint8_t pAxo, uint8_t puisSeuil, uint8_t baseRef) {
        uint32_t portion = xyz_to_u32(port);
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_TYPESTEV_1) = typeNeur; // type de portion
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_N_ORIENT_1) = orient; // orientation dendritique
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_N_PRAYO_1) = pRay; // potentiel dendrites rayonnantes
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_N_PPLAN_1) = pPlan; // potentiel dendrites planes
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_N_PAPIC_1) = pApi; // potentiel dendrites apicales
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_N_PPANI_1) = pPan; // potentiel dendrites panier
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_N_PAXON_1) = pAxo; // potentiel extension axonale
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_N_SEUIL_1) = puisSeuil; // puissance seuil charge
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_N_BREFR_1) = baseRef; // base refractaire
    }
    void sub_creer_extension_axonale_8(id_prt port, uint8_t typeExtA, id_prt ante, uint8_t cptTempA, uint8_t potAxoR, uint8_t masBlocB, int8_t valBlocB, uint32_t dest_u, id_prt dest) {
        uint32_t portion = xyz_to_u32(port);
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_TYPESTEV_1) = typeExtA; // valeurs
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_E_TEVOE_1) = 0; // N/U 2
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_E_CACTT_1) = cptTempA; // activite 1
        FOR_S8_TO(crvDatas, TAI_CRVPS, portion, OCP_E_BSYNV_1) = valBlocB; // valeur du bloc de boutons
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_E_BSYNM_1) = masBlocB; // masse du bloc de boutons
        FOR_U32_TO(crvDatas, TAI_CRVPS, portion, OCP_E_DST_N_4) = xyz_to_u32(dest); // destination (segment ou neurone)
        FOR_U32_TO(crvDatas, TAI_CRVPS, portion, OCP_E_DST_U_4) = dest_u; // utilisateur destinataire
        FOR_U32_TO(crvDatas, TAI_CRVPS, portion, OCP_E_ANT_N_4) = xyz_to_u32(ante); // antecedent (neurone)
        FOR_U32_TO(crvDatas, TAI_CRVPS, portion, OCP_E_ANT_U_4) = 0; // meme utilisateur antecedent
        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_E_PREXT_1) = potAxoR; // potentiel d'extension axonale
    }
    void sub_creer_reseau(id_prt port, uint8_t typP, id_prt ante, uint32_t succ_u, id_prt succ, uint16_t nb_x, uint16_t nb_y, uint16_t nb_z, char pas) {
        id_prt portion = null_port;
        id_prt antecedente = null_port;
        uint32_t successeur = succ_u;
        id_prt successeure = null_port;
        uint16_t compteur = 0;
        for (int z = 0; z < nb_z; z++) {
            portion.z = port.z + z * pas;
            antecedente.z = ante.z + z * pas;
            successeure.z = succ.z + z * pas;
            for (int y = 0; y < nb_y; y++) {
                portion.y = port.y + y * pas;
                antecedente.y = ante.y + y * pas;
                successeure.y = succ.y + y * pas;
                for (int x = 0; x < nb_x; x++) {
                    portion.x = port.x + x * pas;
                    antecedente.x = ante.x + x * pas;
                    successeure.x = succ.x + x * pas;
                    compteur++;
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
    }
#pragma endregion
#pragma region // ------------------------------------------------------------------------------ OUTILS FONCTIONNELS
    uint32_t xyz_to_u32(id_prt id) {
        return id.x + id.y * DIMENSION_X + id.z * DIMENSION_X * DIMENSION_Y;
    }
    id_prt u32_to_xyz(uint32_t num) {
        uint16_t z = (uint16_t)(num / (DIMENSION_X * DIMENSION_Y));
        uint16_t y = (uint16_t)((num - z * DIMENSION_X * DIMENSION_Y) / DIMENSION_X);
        uint16_t x = (uint16_t)(num - z * DIMENSION_X * DIMENSION_Y - y * DIMENSION_X);
        return (id_prt) { x, y, z };
    }
    uint32_t pgcd(uint32_t a, uint32_t b) {
        while (b != 0) {
            uint32_t temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }
    uint32_t ppcm(uint32_t a, uint32_t b) {
        return (a / pgcd(a, b)) * b;  // Division avant multiplication pour éviter un débordement
    }
    void find_first_active_ipv4() {
        ULONG size = 0;
        GetAdaptersAddresses(AF_INET, GAA_FLAG_SKIP_ANYCAST | GAA_FLAG_SKIP_MULTICAST | GAA_FLAG_SKIP_DNS_SERVER,
            NULL, NULL, &size);

        IP_ADAPTER_ADDRESSES* adapters = (IP_ADAPTER_ADDRESSES*)malloc(size);
        if (!adapters) return;

        if (GetAdaptersAddresses(AF_INET, GAA_FLAG_SKIP_ANYCAST | GAA_FLAG_SKIP_MULTICAST | GAA_FLAG_SKIP_DNS_SERVER,
            NULL, adapters, &size) == NO_ERROR) {
            for (IP_ADAPTER_ADDRESSES* curr = adapters; curr; curr = curr->Next) {
                if (curr->OperStatus == IfOperStatusUp) {
                    for (IP_ADAPTER_UNICAST_ADDRESS* addr = curr->FirstUnicastAddress; addr; addr = addr->Next) {
                        if (addr->Address.lpSockaddr->sa_family == AF_INET) {
                            sockaddr_in_t* ipv4 = (sockaddr_in_t*)addr->Address.lpSockaddr;
                            if (ipv4->sin_addr.S_un.S_addr != htonl(INADDR_LOOPBACK)) {
                                inet_ntop(AF_INET, &ipv4->sin_addr, my_ip_string, sizeof(my_ip_string));
                                free(adapters);
                                return;
                            }
                        }
                    }
                }
            }
        }

        free(adapters);
        my_ip_string[0] = '\0';  // Aucun IP trouvé
    }
    void initialize_data() {
        for (int z = 0; z < DIMENSION_Z; z++) {
            for (int y = 0; y < DIMENSION_Y; y++) {
                for (int x = 0; x < DIMENSION_X; x++) {
                    memset(crvDatas[z][y][x], 0, TAI_CRVPS); // Zero initialize each element
                }
            }
        }
    }
    void ajouter_note_locale(uint32_t dmdr_prt, uint32_t demande, uint32_t parametre, uint32_t dest_prt) {
        uint32_t nbrMess = U32_FROM(localDests[localStackToFill], TAI_LOC_DST, dest_prt, OLD_NMSGS_4);
        if (localStackIndex[localStackToFill] < (LCL_STACK_SIZE - 1) && nbrMess < (LCL_MAX_MSG_PER_DST - 1)) {
            // ajouter la note
            FOR_U32_TO(localNotesStack[localStackToFill], TAI_LOC_NOT, localStackIndex[localStackToFill], OLN_EMM_4) = dmdr_prt;
            FOR_U32_TO(localNotesStack[localStackToFill], TAI_LOC_NOT, localStackIndex[localStackToFill], OLN_DST_4) = dest_prt;
            FOR_U32_TO(localNotesStack[localStackToFill], TAI_LOC_NOT, localStackIndex[localStackToFill], OLN_DEMC_4) = demande;
            FOR_U32_TO(localNotesStack[localStackToFill], TAI_LOC_NOT, localStackIndex[localStackToFill], OLN_DEMP_4) = parametre;
            // incrementation du nombre de messages pour cette portion
            FOR_U32_TO(localDests[localStackToFill], TAI_LOC_DST, dest_prt, OLD_NMSGS_4) = nbrMess + 1;
            localStackIndex[localStackToFill]++;
        }
        else {
            cumulNotesRefusees++;
        }
    }
    void ajouter_note_toSend(uint32_t dmdr_usr, uint32_t dmdr_prt, uint32_t demande, uint32_t parametre, uint32_t dest_usr, uint32_t dest_prt) {
        uint64_t nbrMess = U64_FROM(toSendUsers[toSendStackToFill], TAI_SND_USR, dest_usr, OSU_NMES_8);
        if (toSendStackIndex[toSendStackToFill] < ( SND_STACK_SIZE - 1 ) && nbrMess < ( SND_MAX_MSG_PER_DST - 1 )) {
            // ajouter la note
            FOR_U32_TO(toSendNotesStack[toSendStackToFill], TAI_SND_NOT, toSendStackIndex[toSendStackToFill], OSN_EMMP_4) = dmdr_prt;
            FOR_U32_TO(toSendNotesStack[toSendStackToFill], TAI_SND_NOT, toSendStackIndex[toSendStackToFill], OSN_EMMU_4) = dmdr_usr;
            FOR_U32_TO(toSendNotesStack[toSendStackToFill], TAI_SND_NOT, toSendStackIndex[toSendStackToFill], OSN_DESP_4) = dest_prt;
            FOR_U32_TO(toSendNotesStack[toSendStackToFill], TAI_SND_NOT, toSendStackIndex[toSendStackToFill], OSN_DESU_4) = dest_usr;
            FOR_U32_TO(toSendNotesStack[toSendStackToFill], TAI_SND_NOT, toSendStackIndex[toSendStackToFill], OSN_DEMC_4) = demande;
            FOR_U32_TO(toSendNotesStack[toSendStackToFill], TAI_SND_NOT, toSendStackIndex[toSendStackToFill], OSN_DEMP_4) = parametre;
            // incrementation du nombre de messages pour cet utilisateur
            FOR_U64_TO(toSendUsers[toSendStackToFill], TAI_SND_USR, dest_usr, OSU_NMES_8) = nbrMess + 1;
            toSendStackIndex[toSendStackToFill]++;
        }
        else {
            cumulNotesRefusees++;
        }
    }
    static inline __forceinline void doApplicate(uint32_t dem_u, uint32_t dem_p, uint32_t dmand, uint32_t param, uint32_t dst_p) {
        // application de la demande
        switch (dmand) {
            // Chaine Pulseur
            case MSG_PUL_1: {
                // charge actuelle
                int16_t chargeD16 = S16_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_CHARG0_2);
                // surcharge
                int32_t chargeD32 = (int32_t)(chargeD16) + param;
                chargeD32 = (chargeD32 > CHARGE_MINI) ? chargeD32 : CHARGE_MINI;
                chargeD32 = (chargeD32 < CHARGE_MAXI) ? chargeD32 : CHARGE_MAXI;
                // enregistrement de la nouvelle charge
                FOR_S16_TO(crvDatas, TAI_CRVPS, dst_p, OCP_CHARG0_2) = (int16_t)chargeD32;
                // sortie
                break;
            }
            // Chaine Segment
            case MSG_SEG_1: {
                // charge actuelle
                int16_t chargeD16 = S16_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_CHARG0_2);
                // surcharge
                int32_t chargeD32 = (int32_t)(chargeD16)+param;
                chargeD32 = (chargeD32 > CHARGE_MINI) ? chargeD32 : CHARGE_MINI;
                chargeD32 = (chargeD32 < CHARGE_MAXI) ? chargeD32 : CHARGE_MAXI;
                // enregistrement de la nouvelle charge
                FOR_S16_TO(crvDatas, TAI_CRVPS, dst_p, OCP_CHARG0_2) = (int16_t)chargeD32;
                // extraction de l'activité
                uint8_t activ = U8_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_ACTIVITE_1);
                // réponse de l'activité
                if (dem_u == 0) {
                    ajouter_note_locale(dst_p, MSG_SEG_1A, (uint32_t)activ, dem_p);
                }
                else {
                    ajouter_note_toSend(myPackId, dst_p, MSG_SEG_1A, (uint32_t)activ, dem_u, dem_p);
                }
                // sortie
                break;
            }
            case MSG_SEG_1A: {
                // affectation de l'activité renvoyée
                FOR_U8_TO(crvDatas, TAI_CRVPS, dst_p, OCP_ACTIVITE_1) = (uint8_t)param;
                // sortie
                break;
            }
            // Chaine Extension
            case MSG_EXT_1: {
                // activité de l'antecedent
                uint8_t activAntec8 = U8_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_ACTIVITE_1); // ------------------------- ANT
                // si l'antecedent est actif : déclenchement de tout
                // la refractarité garantit qu'on n'est pas déjà dans une attente de retroaction
                if (activAntec8 != 0) {
                    // réponse de l'activité
                    if (dem_u == 0) {
                        ajouter_note_locale(dst_p, MSG_EXT_1A, (uint32_t)activAntec8, dem_p);
                    }
                    else {
                        ajouter_note_toSend(myPackId, dst_p, MSG_EXT_1A, (uint32_t)activAntec8, dem_u, dem_p);
                    }
                }
                // si l'antecedent n'est pas actif : on se contente de faire évoluer le compteur
                // et on vérifier le retour de retro-action (impossible si on vient de déclencher)
                else {
                    // réponse de l'activité
                    if (dem_u == 0) {
                        ajouter_note_locale(dst_p, MSG_EXT_1B, (uint32_t)activAntec8, dem_p);
                    }
                    else {
                        ajouter_note_toSend(myPackId, dst_p, MSG_EXT_1B, (uint32_t)activAntec8, dem_u, dem_p);
                    }
                }
                // sortie
                break;
            }
            case MSG_EXT_1A: {
                // s'auto-activer
                FOR_U8_TO(crvDatas, TAI_CRVPS, dst_p, OCP_ACTIVITE_1) = 1;
                // destination locale
                uint32_t destinU = U32_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_E_DST_U_4);
                if (destinU == 0) {
                    // présence d'une destination
                    uint32_t destinN = U32_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_E_DST_N_4);
                    if (destinN != 0) {
                        // activer le compteur
                        FOR_U8_TO(crvDatas, TAI_CRVPS, dst_p, OCP_E_CACTT_1) = 1;
                        // surcharge de la destination
                        int8_t surcharge8 = S8_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_E_BSYNV_1);
                        // demande de surcharge simple
                        if (dem_u == 0) {
                            ajouter_note_locale(dst_p, MSG_EXT_1A1, (int32_t)surcharge8, dem_p);
                        }
                        else {
                            ajouter_note_toSend(myPackId, dst_p, MSG_EXT_1A1, (int32_t)surcharge8, dem_u, dem_p);
                        }
                    }
                    else {
                        // générer une pousse ?
                    }
                }
                // sortie
                break;
            }
            case MSG_EXT_1A1: {
                // charge actuelle
                int16_t chargeD16 = S16_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_CHARG0_2);
                // surcharge
                int32_t chargeD32 = (int32_t)(chargeD16)+param;
                chargeD32 = (chargeD32 > CHARGE_MINI) ? chargeD32 : CHARGE_MINI;
                chargeD32 = (chargeD32 < CHARGE_MAXI) ? chargeD32 : CHARGE_MAXI;
                // enregistrement de la nouvelle charge
                FOR_S16_TO(crvDatas, TAI_CRVPS, dst_p, OCP_CHARG0_2) = (int16_t)chargeD32;
                // sortie
                break;
            }
            case MSG_EXT_1B: {
                // s'auto-desactiver
                FOR_U8_TO(crvDatas, TAI_CRVPS, dst_p, OCP_ACTIVITE_1) = 0;
                // évolution du compteur d'activite s'il est actif
                uint8_t comptAct8 = U8_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_E_CACTT_1);
                if (comptAct8 != 0) {
                    comptAct8++;
                    // enregistrement du compteur d'activité incrémenté
                    FOR_U8_TO(crvDatas, TAI_CRVPS, dst_p, OCP_E_CACTT_1) = comptAct8;
                    // rétroaction
                    // destination locale
                    uint32_t destinU = U32_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_E_DST_U_4);
                    if (destinU == 0) {
                        // présence d'une destination
                        uint32_t destinN = U32_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_E_DST_N_4);
                        if (destinN != 0) {
                            // demande d'activité de la destination
                            if (dem_u == 0) {
                                ajouter_note_locale(dst_p, MSG_EXT_1B1, 0, dem_p);
                            }
                            else {
                                ajouter_note_toSend(myPackId, dst_p, MSG_EXT_1B1, 0, dem_u, dem_p);
                            }
                        }
                    }
                }
                // sortie
                break;
            }
            case MSG_EXT_1B1: {
                // activité de la destination
                uint8_t actDest8 = U8_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_ACTIVITE_1); // --------------------- DST
                if (actDest8 != 0) {
                    // auto-modification par rétro-action
                    uint8_t nivActivDest = U8_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_NIVACTMP_1); // ----------- DST
                    // demande d'activité de la destination
                    if (dem_u == 0) {
                        ajouter_note_locale(dst_p, MSG_EXT_1B1A, (uint32_t)nivActivDest, dem_p);
                    }
                    else {
                        ajouter_note_toSend(myPackId, dst_p, MSG_EXT_1B1A, (uint32_t)nivActivDest, dem_u, dem_p);
                    }
                }
                // sortie
                break;
            }
            case MSG_EXT_1B1A: {
                uint8_t comptAct8 = U8_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_E_CACTT_1);
                // modification de la valeur du bloc de boutons
                int8_t valblocb8;
                if (comptAct8 != (uint8_t)param) {
                    valblocb8 = S8_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_E_BSYNV_1);
                    valblocb8 = (valblocb8 <= MINI_VALBB ? MINI_VALBB : valblocb8 - 1);
                }
                else {
                    valblocb8 = S8_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_E_BSYNV_1);
                    valblocb8 = (valblocb8 >= MAXI_VALBB ? MAXI_VALBB : valblocb8 + 1);
                }
                FOR_U8_TO(crvDatas, TAI_CRVPS, dst_p, OCP_E_BSYNV_1) = valblocb8;
                // modification de la masse de bloc de boutons
                uint8_t masblocb8 = U8_FROM(crvDatas, TAI_CRVPS, dst_p, OCP_E_BSYNM_1);
                masblocb8 = (masblocb8 >= MAXI_MASSEBB ? MAXI_MASSEBB : masblocb8 + 1);
                FOR_U8_TO(crvDatas, TAI_CRVPS, dst_p, OCP_E_BSYNM_1) = masblocb8;
                // neutraliser le compteur d'activité
                FOR_U8_TO(crvDatas, TAI_CRVPS, dst_p, OCP_E_CACTT_1) = 0;
                // sortie
                break;
            }


            /* -----------------------------------------------------------------------------
            Pour mémoire :
            void ajouter_note_locale(uint32_t dmdr_prt, uint32_t demande, uint32_t parametre, uint32_t dest_prt);
            void ajouter_note_toSend(uint32_t dmdr_usr, uint32_t dmdr_prt, uint32_t demande, uint32_t parametre, uint32_t dest_usr, uint32_t dest_prt);
            */
        }
    }
    void read_input_file() {
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
        uint16_t maxSize = NOMBRE_MAX_SEGMENTS_TEXTE * LARGEUR_DE_SEGMENT_DE_TEXTE;
        if (fileSize.QuadPart > maxSize) {
            longueur_Texte = maxSize;
        }
        else {
            longueur_Texte = (uint16_t)fileSize.QuadPart;
        }
        rawData_Texte = (uint8_t*)malloc(longueur_Texte);
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
    void load_cervelet() {
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
        if (longueurFichier.QuadPart != NBR_CRVPS * TAI_CRVPS) {
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
        for (uint32_t numPort = 0; numPort < NBR_CRVPS; numPort++) {
            uint8_t TypeDePortion = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_TYPESTEV_1);
            if (TypeDePortion == TYPE_SOURCE) {
                FOR_U64_TO(crvDatas, TAI_CRVPS, numPort, OCP_D_ADDRS_8) = adresse;
            }
        }
    }
    void save_cervelet() {
        // Ouverture du fichier en mode écriture
        HANDLE Filehandle = CreateFileA(
            CERVELET_FILE_NAME, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
        if (Filehandle == INVALID_HANDLE_VALUE) {
            printf("Erreur ouverture fichier pour sauvegarde");
            exit(-1);
        }
        // Écriture du contenu des portions dans le fichier
        DWORD bytesWritten;
        if (!WriteFile(Filehandle, crvDatas, NBR_CRVPS * TAI_CRVPS, &bytesWritten, NULL)) {
            printf("Erreur écriture fichier");
            CloseHandle(Filehandle);
            exit(-1);
        }
        if (bytesWritten != NBR_CRVPS * TAI_CRVPS) {
            printf("Erreur: nombre d'octets écrits incorrect\n");
            CloseHandle(Filehandle);
            exit(-1);
        }
        // Fermeture du fichier
        CloseHandle(Filehandle);
    }
#pragma endregion
#pragma region // ------------------------------------------------------------------------------ OUTILS CONSOLE ET GRAPHIQUE
    // outils console
    void CleanupConsole() {
        FreeConsole();
        printf("Console released successfully.\n");
    }
    void ecrire_ligne_4(HANDLE consoleHandle, const char* texte, int64_t nombre, SHORT positionX, SHORT positionY) {
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
                    printf("Erreur (%d) lors de la recuperation des informations de la console (%p).\n", GetLastError(), consoleHandle);
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
                ligneAEcrire[i + NOMBRE_CARS_AVANT] = texte[i];
                i++;
            }
        }
        // Suffixe
        ligneAEcrire[LONGUEUR_LIGNES - 2] = ' ';
        ligneAEcrire[LONGUEUR_LIGNES - 1] = '#';
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
                ligneAEcrire[LONGUEUR_LIGNES - 3] = '?';
            }
        }
        // Write the line to the console
        DWORD written;
        WriteConsoleA(consoleHandle, ligneAEcrire, (DWORD)strlen(ligneAEcrire), &written, NULL);
    }
    void ecrire_messages_console() {
        // Console
        CONSOLE_SCREEN_BUFFER_INFO csbi;
        HANDLE hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
        // Recuperation des informations de la console
        if (!GetConsoleScreenBufferInfo(hConsole, &csbi)) {
            printf("Erreur (%d) lors de la recuperation des informations de la console (%p).\n", GetLastError(), hConsole);
            return;
        }
        // Position de retour
        SHORT positionDeRetourX = csbi.dwCursorPosition.X;
        SHORT positionDeRetourY = (csbi.dwCursorPosition.Y > HAUTEUR_TABLE - 1) ? csbi.dwCursorPosition.Y : HAUTEUR_TABLE - 1;
        // Positionnement du curseur
        SHORT positionX = POSITION_TABLE_X;
        SHORT hautDeLaTable = positionDeRetourY - (HAUTEUR_TABLE - 1);
        SHORT positionY = hautDeLaTable;
        COORD newCursorPosition = { positionX, positionY };
        SetConsoleCursorPosition(hConsole, newCursorPosition);
        // Ecriture des premieres lignes
        ecrire_ligne_4(hConsole, ligne_de_cadre, -1, positionX, positionY);
        positionY++;
        ecrire_ligne_4(hConsole, ligne_vide, -1, positionX, positionY);
        switch (affichageConsole) {
            case 1: {  // affichage aide 1
                positionY++;
                ecrire_ligne_4(hConsole, help_00, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_01, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_10, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_11, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_12, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_13, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_14, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_15, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_16, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_17, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_18, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_20, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_21, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_22, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_23, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_30, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_31, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_32, -1, positionX, positionY);
                break;
            }
            case 2: { // affichage aide 2
                positionY++;
                ecrire_ligne_4(hConsole, help_50, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_51, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_52, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_53, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_54, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_55, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_56, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_60, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_61, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_62, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_63, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, help_64, -1, positionX, positionY);
                break;
            }
            case 3: { // structurel
                positionY++;
                ecrire_ligne_4(hConsole, texte_reseau_up, net_Okay, positionX, positionY);
                positionY++;
                if (net_Okay) {
                    if (net_Server) {
                        // printf("Mode maitre : ip = %s\n", my_ip_string);
                        ecrire_ligne_4(hConsole, texte_mode_maitre, -1, positionX, positionY);
                    }
                    else {
                        // printf("Mode dependant de %s\n", ip_master_string);
                        ecrire_ligne_4(hConsole, texte_mode_dependant, -1, positionX, positionY);
                    }
                }
                else {
                    // printf("Mode autonome sans reseau\n");
                    ecrire_ligne_4(hConsole, texte_mode_autonome, -1, positionX, positionY);
                }
                positionY++;
                ecrire_ligne_4(hConsole, ligne_vide, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, ligne_vide, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_nombre_de_cibles_validees, ciblesValidees, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_nombre_de_cibles_externes, ciblesExternes, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_nombre_de_cibles_nulles, erreurCibleNulle, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_nombre_de_cibles_incoherentes, erreurCibleIncoherente, positionX, positionY);
                break;
            }
            case 4: { // affichage utilisateurs
                positionY++;
                ecrire_ligne_4(hConsole, texte_nombre_d_utilisateurs, nombreDUtilisateurs, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, ligne_vide, -1, positionX, positionY);
                char toprint_ip_text[INET_ADDRSTRLEN];
                sockaddr_in_t toprint_sock_addr;
                for (int user_num = 0; user_num < nombreDUtilisateurs; user_num++) {
                    positionY++;
                    toprint_sock_addr.sin_addr.s_addr = U32_FROM(netUsers, TAI_USR_NET, user_num, OUN_IPAD_4);
                    DO_UNCONVERT(toprint_sock_addr, toprint_ip_text);
                    ecrire_ligne_4(hConsole, toprint_ip_text, user_num, positionX, positionY);
                }
                break;
            }
            case 5: { // affichage suivi general
                positionY++;
                ecrire_ligne_4(hConsole, texte_numero_de_boucle, numero_de_boucle_lente, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_duree_de_boucle, duree_cycle, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_duree_de_posttrait, dureeDePostTraitement, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, ligne_vide, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_notes_refusees, cumulNotesRefusees, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_erreur_a_voir, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, ligne_vide, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_cpu_usage, cpu_usage_percent, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_memory_usage, memory_usage_percent, positionX, positionY);
                break;
            }
            case 6: { // affichage traitement local données
                positionY++;
                ecrire_ligne_4(hConsole, texte_remplissage_local, remplissagePacksLocaux, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_nombre_de_messages_locaux, localStackIndex[localStackToFill], positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_flux_entrant_sur_pile_0, localStackIndex[0], positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_flux_entrant_sur_pile_1, localStackIndex[1], positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_valeur_de_test, valeurDeTest, positionX, positionY);
                break;
            }
            case 7: { // affichage flux de données externe E/S
                positionY++;
                ecrire_ligne_4(hConsole, texte_envois_actifs, do_send, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_remplissage_tosend, remplissagePacksToSend, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_flux_entrant_sur_pile_0, toSendStackIndex[0], positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_flux_entrant_sur_pile_1, toSendStackIndex[1], positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_nombre_de_messages_a_emettre, cumulNotesATraiter, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, ligne_vide, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_reception_active, do_recv, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_flux_iocp_entrant, cumulFluxIOCPEntrant, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_paquets_udp_refuses, refusedUdp, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_flux_entrant_sur_pile_0, receiveStackIndex[0], positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_flux_entrant_sur_pile_1, receiveStackIndex[1], positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_nombre_de_messages_acceptes, cumulMessagesAcceptes, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_nombre_de_messages_detruits, cumulMessagesDetruits, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_remplissage_received, remplissagePacksReceived, positionX, positionY);

                positionY++;
                ecrire_ligne_4(hConsole, ligne_vide, -1, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_udp_packet_loss, udp_packet_loss_percent, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_socket_queue_usage, socket_queue_usage_percent, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_network_usage, network_usage_percent, positionX, positionY);
                break;
            }
            case 8: { // affichage éléments de la portion pointée selon son type
                // portion en analyse
                uint32_t numPort = xyz_to_u32((id_prt) { visualize_x, visualize_y, visualize_z });
                positionY++;
                ecrire_ligne_4(hConsole, portions_numr_text, numPort, positionX, positionY);
                // position en analyse
                positionY++;
                ecrire_ligne_4(hConsole, texte_position__x, visualize_x, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_position__y, visualize_y, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, texte_position__z, visualize_z, positionX, positionY);
                positionY++;
                ecrire_ligne_4(hConsole, ligne_vide, -1, positionX, positionY);
                // selon type
                uint8_t typestev = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_TYPESTEV_1);
                switch (typestev) {
                    case TYPE_SEGMENT_GERME:
                    case TYPE_SEGMENT_PARTIEL:
                    case TYPE_SEGMENT_COMPLET:
                        // type
                        positionY++;
                        ecrire_ligne_4(hConsole, portions_typeS_text, typestev, positionX, positionY);
                        // temporisation d'evolution de segment
                        positionY++;
                        uint8_t tempoEvo_1U_seg = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_S_TEVOCP_1);
                        ecrire_ligne_4(hConsole, portions_devo_text, tempoEvo_1U_seg, positionX, positionY);
                        // potentiel synaptique restant
                        positionY++;
                        uint16_t tempoEvo_2U_seg = U16_FROM(crvDatas, TAI_CRVPS, numPort, OCP_S_PSYNA_2);
                        ecrire_ligne_4(hConsole, portions_psyn_text, tempoEvo_2U_seg, positionX, positionY);
                        // Niveau activite temporel
                        positionY++;
                        uint8_t niveauActTemp_1U_seg = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_NIVACTMP_1);
                        ecrire_ligne_4(hConsole, portions_ntac_text, niveauActTemp_1U_seg, positionX, positionY);
                        // retro activite
                        positionY++;
                        uint8_t activite_1U_seg = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_ACTIVITE_1);
                        ecrire_ligne_4(hConsole, portions_actv_text, activite_1U_seg, positionX, positionY);
                        // nombre de charges internes utilisées
                        positionY++;
                        uint8_t nombreCharg_1U_seg = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_S_NBRCH_1);
                        ecrire_ligne_4(hConsole, portions_ind1_text, nombreCharg_1U_seg, positionX, positionY);
                        // charge 0
                        positionY++;
                        int16_t charge0_2S_seg = S16_FROM(crvDatas, TAI_CRVPS, numPort, OCP_CHARG0_2);
                        ecrire_ligne_4(hConsole, portions_chrg_text, charge0_2S_seg, positionX, positionY);
                        // charge 1
                        positionY++;
                        int16_t charge1_2S_seg = S16_FROM(crvDatas, TAI_CRVPS, numPort, OCP_S_CHARG1_2);
                        ecrire_ligne_4(hConsole, portions_chrg_text, charge1_2S_seg, positionX, positionY);
                        // charge 2
                        positionY++;
                        int16_t charge2_2S_seg = S16_FROM(crvDatas, TAI_CRVPS, numPort, OCP_S_CHARG2_2);
                        ecrire_ligne_4(hConsole, portions_chrg_text, charge2_2S_seg, positionX, positionY);
                        // charge 3
                        positionY++;
                        int16_t charge3_2S_seg = S16_FROM(crvDatas, TAI_CRVPS, numPort, OCP_S_CHARG3_2);
                        ecrire_ligne_4(hConsole, portions_chrg_text, charge3_2S_seg, positionX, positionY);
                        // numero de segment/neurone suivant / num portion
                        positionY++;
                        uint32_t destinationN_4U_seg = U32_FROM(crvDatas, TAI_CRVPS, numPort, OCP_S_SVT_N_4);
                        ecrire_ligne_4(hConsole, portions_dstn_text, destinationN_4U_seg, positionX, positionY);
                        // numero de segment/neurone suivant / utilisateur
                        positionY++;
                        uint32_t destinationU_4U_seg = U32_FROM(crvDatas, TAI_CRVPS, numPort, OCP_S_SVT_U_4);
                        ecrire_ligne_4(hConsole, portions_dstu_text, destinationU_4U_seg, positionX, positionY);
                        // potentiel de segments restants
                        positionY++;
                        uint8_t potSegRest_1U_seg = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_S_PRSEG_1);
                        ecrire_ligne_4(hConsole, portions_ind1_text, potSegRest_1U_seg, positionX, positionY);
                        // N/U 7o
                        positionY++;
                        ecrire_ligne_4(hConsole, portions_nnux_text, 7, positionX, positionY);
                        break;
                    case TYPE_NEURONE_GERME:
                    case TYPE_NEURONE_PARTIEL:
                    case TYPE_NEURONE_COMPLET:
                        // type
                        positionY++;
                        ecrire_ligne_4(hConsole, portions_typeN_text, typestev, positionX, positionY);
                        // N/U 3
                        positionY++;
                        ecrire_ligne_4(hConsole, portions_nnux_text, 3, positionX, positionY);
                        // Niveau activite temporel
                        positionY++;
                        uint8_t niveauActTemp_1U_neu = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_NIVACTMP_1);
                        ecrire_ligne_4(hConsole, portions_ntac_text, niveauActTemp_1U_neu, positionX, positionY);
                        // activité
                        positionY++;
                        uint8_t activite_1U_neu = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_ACTIVITE_1);
                        ecrire_ligne_4(hConsole, portions_actv_text, activite_1U_neu, positionX, positionY);
                        // compteur refractaire
                        positionY++;
                        uint8_t comptRef_1U = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_N_CREFR_1);
                        ecrire_ligne_4(hConsole, portions_cref_text, comptRef_1U, positionX, positionY);
                        // base decompte refractaire
                        positionY++;
                        uint8_t baseRefr_1U = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_N_BREFR_1);
                        ecrire_ligne_4(hConsole, portions_bref_text, baseRefr_1U, positionX, positionY);
                        // charge
                        positionY++;
                        int16_t charge_2S_neu = S16_FROM(crvDatas, TAI_CRVPS, numPort, OCP_CHARG0_2);
                        ecrire_ligne_4(hConsole, portions_chrg_text, charge_2S_neu, positionX, positionY);
                        // seuil
                        positionY++;
                        uint8_t puisSeuil_1U = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_N_SEUIL_1);
                        ecrire_ligne_4(hConsole, portions_chrs_text, puisSeuil_1U, positionX, positionY);
                        // N/U 1
                        positionY++;
                        ecrire_ligne_4(hConsole, portions_nnux_text, 1, positionX, positionY);
                        // puissance potentiel rayonnant
                        positionY++;
                        uint8_t potRayo_1U = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_N_PRAYO_1);
                        ecrire_ligne_4(hConsole, portions_pray_text, potRayo_1U, positionX, positionY);
                        // puissance potentiel planaire
                        positionY++;
                        uint8_t potPlan_1U = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_N_PPLAN_1);
                        ecrire_ligne_4(hConsole, portions_ppla_text, potPlan_1U, positionX, positionY);
                        // puissance potentiel apical
                        positionY++;
                        uint8_t potApic_1U = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_N_PAPIC_1);
                        ecrire_ligne_4(hConsole, portions_papi_text, potApic_1U, positionX, positionY);
                        // puissance potentiel panier
                        positionY++;
                        uint8_t potPani_1U = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_N_PPANI_1);
                        ecrire_ligne_4(hConsole, portions_ppan_text, potPani_1U, positionX, positionY);
                        // N/U 6
                        positionY++;
                        ecrire_ligne_4(hConsole, portions_nnux_text, 6, positionX, positionY);
                        // puissance potentiel axonal
                        positionY++;
                        uint8_t potAxon_U1 = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_N_PAXON_1);
                        ecrire_ligne_4(hConsole, portions_paxo_text, potAxon_U1, positionX, positionY);
                        // orientation des développements
                        positionY++;
                        uint8_t orient_1U = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_N_ORIENT_1);
                        ecrire_ligne_4(hConsole, portions_ornt_text, orient_1U, positionX, positionY);
                        // N/U 8
                        positionY++;
                        ecrire_ligne_4(hConsole, portions_nnux_text, 8, positionX, positionY);
                        break;
                    case TYPE_EXTENSION_GERME:
                    case TYPE_EXTENSION_PARTIEL:
                    case TYPE_EXTENSION_COMPLET:
                        // type
                        positionY++;
                        ecrire_ligne_4(hConsole, portions_typeE_text, typestev, positionX, positionY);
                        // decompte evolution
                        positionY++;
                        uint8_t decptEvo_1U = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_E_TEVOE_1);
                        ecrire_ligne_4(hConsole, portions_devo_text, decptEvo_1U, positionX, positionY);
                        // N/U 2
                        positionY++;
                        ecrire_ligne_4(hConsole, portions_nnux_text, 2, positionX, positionY);
                        // compteur activite temporelle
                        positionY++;
                        uint8_t comptTAct_1U = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_E_CACTT_1);
                        ecrire_ligne_4(hConsole, portions_ctac_text, comptTAct_1U, positionX, positionY);
                        // activite
                        positionY++;
                        uint8_t activite_1U_ext = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_ACTIVITE_1);
                        ecrire_ligne_4(hConsole, portions_actv_text, activite_1U_ext, positionX, positionY);
                        // valeur du bloc de boutons
                        positionY++;
                        int8_t valBB_1S = S8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_E_BSYNV_1);
                        ecrire_ligne_4(hConsole, portions_blcv_text, valBB_1S, positionX, positionY);
                        // masse du bloc de boutons
                        positionY++;
                        uint8_t masseBB_1U = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_E_BSYNM_1);
                        ecrire_ligne_4(hConsole, portions_blcp_text, masseBB_1U, positionX, positionY);
                        // destination portion
                        positionY++;
                        uint32_t destinN_4U = U32_FROM(crvDatas, TAI_CRVPS, numPort, OCP_E_DST_N_4);
                        ecrire_ligne_4(hConsole, portions_dstn_text, destinN_4U, positionX, positionY);
                        // destination utilisateur
                        positionY++;
                        uint32_t destinU_4U = U32_FROM(crvDatas, TAI_CRVPS, numPort, OCP_E_DST_U_4);
                        ecrire_ligne_4(hConsole, portions_dstu_text, destinU_4U, positionX, positionY);
                        // antecedant portion
                        positionY++;
                        uint32_t antecN_4U = U32_FROM(crvDatas, TAI_CRVPS, numPort, OCP_E_ANT_N_4);
                        ecrire_ligne_4(hConsole, portions_antn_text, antecN_4U, positionX, positionY);
                        // antecedant utilisateur
                        positionY++;
                        uint32_t antecU_4U = U32_FROM(crvDatas, TAI_CRVPS, numPort, OCP_E_ANT_U_4);
                        ecrire_ligne_4(hConsole, portions_antu_text, antecU_4U, positionX, positionY);
                        // potentiel axonal restant
                        positionY++;
                        uint8_t potExtRest_1U = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_E_PREXT_1);
                        ecrire_ligne_4(hConsole, portions_pext_text, potExtRest_1U, positionX, positionY);
                        // N/U 7
                        positionY++;
                        ecrire_ligne_4(hConsole, portions_nnux_text, 7, positionX, positionY);
                        break;
                    case TYPE_SOURCE:
                        // type
                        positionY++;
                        ecrire_ligne_4(hConsole, portions_typeD_text, typestev, positionX, positionY);
                        // adresse
                        positionY++;
                        uint64_t adresse_U8 = U64_FROM(crvDatas, TAI_CRVPS, numPort, OCP_D_ADDRS_8);
                        ecrire_ligne_4(hConsole, portions_adrs_text, adresse_U8, positionX, positionY);
                        // nombre de segments
                        positionY++;
                        uint16_t nbrSeg_U2 = U16_FROM(crvDatas, TAI_CRVPS, numPort, OCP_D_NBSEG_2);
                        ecrire_ligne_4(hConsole, portions_nseg_text, nbrSeg_U2, positionX, positionY);
                        // taille des segments
                        positionY++;
                        uint8_t longSeg_U1 = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_D_LGSEG_1);
                        ecrire_ligne_4(hConsole, portions_tseg_text, longSeg_U1, positionX, positionY);
                        break;
                    case TYPE_LECTEUR:
                        // type
                        positionY++;
                        ecrire_ligne_4(hConsole, portions_typeL_text, typestev, positionX, positionY);
                        // portion acces memoire
                        positionY++;
                        uint32_t portionAccMem_U4 = U32_FROM(crvDatas, TAI_CRVPS, numPort, OCP_L_PAM_N_4);
                        ecrire_ligne_4(hConsole, portions_pamm_text, portionAccMem_U4, positionX, positionY);
                        // index de lecture
                        positionY++;
                        uint16_t indexLect_U2 = U16_FROM(crvDatas, TAI_CRVPS, numPort, OCP_L_INDEX_2);
                        ecrire_ligne_4(hConsole, portions_idxl_text, indexLect_U2, positionX, positionY);
                        // numero de bit a lire
                        positionY++;
                        uint8_t numBit_U1 = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_L_NMBIT_1);
                        ecrire_ligne_4(hConsole, portions_bitl_text, numBit_U1, positionX, positionY);
                        // bloc de boutons
                        positionY++;
                        int8_t blocVal_S1 = S8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_L_BSYNV_1);
                        ecrire_ligne_4(hConsole, portions_blcv_text, blocVal_S1, positionX, positionY);
                        // persistance actuelle
                        positionY++;
                        uint8_t persistAct_U1 = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_L_PERSA_1);
                        ecrire_ligne_4(hConsole, portions_prsa_text, persistAct_U1, positionX, positionY);
                        // persistance de base
                        positionY++;
                        uint8_t persistBase_U1 = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_L_PERSB_1);
                        ecrire_ligne_4(hConsole, portions_prsb_text, persistBase_U1, positionX, positionY);
                        // destination numero portion
                        positionY++;
                        uint32_t destinN_U4 = U32_FROM(crvDatas, TAI_CRVPS, numPort, OCP_L_DST_N_4);
                        ecrire_ligne_4(hConsole, portions_dstn_text, destinN_U4, positionX, positionY);
                        // destination utilisateur
                        positionY++;
                        uint32_t destinU_U4 = U32_FROM(crvDatas, TAI_CRVPS, numPort, OCP_L_DST_U_4);
                        ecrire_ligne_4(hConsole, portions_dstu_text, destinU_U4, positionX, positionY);
                        break;
                    case TYPE_PULSEUR:
                        // type
                        positionY++;
                        ecrire_ligne_4(hConsole, portions_typeP_text, typestev, positionX, positionY);
                        // charge actuelle
                        positionY++;
                        uint16_t chrono_U2 = U16_FROM(crvDatas, TAI_CRVPS, numPort, OCP_P_CHARGI_2);
                        ecrire_ligne_4(hConsole, portions_chro_text, chrono_U2, positionX, positionY);
                        // seuil de charge
                        positionY++;
                        uint8_t pSeuil_U1 = U8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_P_SEUIL_1);
                        ecrire_ligne_4(hConsole, portions_chrl_text, pSeuil_U1, positionX, positionY);
                        // valeur du bloc de boutons
                        positionY++;
                        int8_t surcharge_S1 = S8_FROM(crvDatas, TAI_CRVPS, numPort, OCP_P_BSYNV_1);
                        ecrire_ligne_4(hConsole, portions_blcv_text, surcharge_S1, positionX, positionY);
                        // fin
                        break;
                    default: {
                        uint8_t valeur;
                        for (int n = 0; n < 10; n++) {
                            positionY++;
                            valeur = U8_FROM(crvDatas, TAI_CRVPS, numPort, n);
                            ecrire_ligne_4(hConsole, portions_octt_text, valeur, positionX, positionY);
                        }
                        break;
                    }
            }
                break;
            }
            default: {
                // printf("Aucune action pour affichageConsole = %d\n", affichageConsole);
                break;
            }
        }
        // Remplissage des lignes vides restantes
        while (positionY < hautDeLaTable + HAUTEUR_TABLE - 4) {
            positionY++;
            ecrire_ligne_4(hConsole, ligne_vide, -1, positionX, positionY);
        }
        // Ecriture des dernieres lignes
        positionY++;
        ecrire_ligne_4(hConsole, ligne_vide, -1, positionX, positionY);
        positionY++;
        ecrire_ligne_4(hConsole, ligne_de_cadre, -1, positionX, positionY);
        // Replacement du curseur a sa position initiale
        COORD originalCursorPosition = { positionDeRetourX, positionDeRetourY };
        SetConsoleCursorPosition(hConsole, originalCursorPosition);
    }
    void RefreshSystemStats(SOCKET aSocket) {
        // void CheckUdpPacketLoss() {
            MIB_UDPSTATS stats;
            if (GetUdpStatistics(&stats) == NO_ERROR) {
                if (stats.dwInDatagrams > 0) {
                    udp_packet_loss_percent = (stats.dwInErrors * 100) / stats.dwInDatagrams;
                }
            }
        // void CheckCpuUsage() {
            FILETIME idleTime, kernelTime, userTime;
            GetSystemTimes(&idleTime, &kernelTime, &userTime);
            static ULARGE_INTEGER prevTotalTime = { 0 }, prevIdleTime = { 0 };
            ULARGE_INTEGER kTime, uTime, iTime;
            kTime.LowPart = kernelTime.dwLowDateTime;
            kTime.HighPart = kernelTime.dwHighDateTime;
            uTime.LowPart = userTime.dwLowDateTime;
            uTime.HighPart = userTime.dwHighDateTime;
            iTime.LowPart = idleTime.dwLowDateTime;
            iTime.HighPart = idleTime.dwHighDateTime;
            ULONGLONG totalTime = (kTime.QuadPart + uTime.QuadPart) - prevTotalTime.QuadPart;
            ULONGLONG idleDiff = iTime.QuadPart - prevIdleTime.QuadPart;
            cpu_usage_percent = (int)(100 * (1.0 - ((double)idleDiff / totalTime)));
            prevTotalTime.QuadPart = kTime.QuadPart + uTime.QuadPart;
            prevIdleTime = iTime;
        // void CheckSocketQueueUsage(SOCKET aSocket) {
            u_long pendingBytes = 0;
            ioctlsocket(aSocket, FIONREAD, &pendingBytes);
            socket_queue_usage_percent = (pendingBytes * 100) / (256 * 1024 * 1024); // Comparé à 256 Mo
        // void CheckMemoryUsage() {
            MEMORYSTATUSEX memInfo;
            memInfo.dwLength = sizeof(MEMORYSTATUSEX);
            GlobalMemoryStatusEx(&memInfo);
            memory_usage_percent = (int)memInfo.dwMemoryLoad;
        // void CheckNetworkUsage() {
            MIB_IFROW ifRow;
            ifRow.dwIndex = 1; // Adapter selon la carte réseau
            if (GetIfEntry(&ifRow) == NO_ERROR) {
                network_usage_percent = (ifRow.dwInOctets * 100) / (100L * 1024 * 1024); // Comparé à 100 Mbps
            }
    }
    // outils fenetre graphique
    int RegisterWindowClass(HINSTANCE hInstance) {
        WNDCLASSEXA windowClass = { 0 };
        windowClass.cbSize = sizeof(WNDCLASSEXA);
        windowClass.style = CS_HREDRAW | CS_VREDRAW;
        windowClass.lpfnWndProc = WindowProc;
        windowClass.hInstance = hInstance;
        windowClass.hCursor = LoadCursor(NULL, IDC_ARROW);
        windowClass.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
        windowClass.lpszClassName = windowClassName;
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
            1200, 100,                            // Position (x, y)
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
        bitmapData = (uint8_t*)malloc(bitmapSize);
        if (!bitmapData) {
            printf("Failed to allocate memory for bitmap");
            exit(-1);
        }
        // Clear the bitmap data
        memset(bitmapData, 0, bitmapSize);
    }
    void retracer_la_fenetre() {
        InvalidateRect(gWindowHandle, 0, 0);
        RedrawWindow(gWindowHandle, 0, 0, RDW_INTERNALPAINT);
    }
#pragma endregion
#pragma region // ------------------------------------------------------------------------------ THREADS ET WINDOW PROC
    DWORD WINAPI ProcessThread(LPVOID param) {
        // variables communes
        uint8_t typestev;
        // specifiques à confirer
        // specifiques lecteur
        uint32_t sourceU;
        uint32_t sourceN;
        uint8_t largSegment8;
        uint16_t indexDeLecture16;
        uint8_t bitALire8;
        uint64_t adresse64;
        uint64_t adresseALire;
        uint64_t valeur64;
        uint8_t bit;
        uint32_t destinN;
        int8_t surcharge8;
        uint32_t destinU;
        uint8_t persistActu8;
        uint16_t nbrSeg16;
        // specifique pulseur
        uint16_t chrono16;
        uint32_t chrono32;
        uint8_t pSeuil8;
        int32_t seuil32;
        uint32_t seuilU32;
        // specifique segment
        uint8_t decpteRef8;
        int16_t chargeD16;
        uint8_t baseRefr8;
        // specifique neurone
        uint32_t antecedU;
        uint32_t antecedN;
        uint8_t activAntec8;
        int32_t chargeD32;
        uint8_t comptAct8;
        uint8_t actDest8;
        int8_t valblocb8;
        uint8_t nivActivDest;
        uint8_t masblocb8;
        // specifique extension
        int16_t chargeAUt16;
        int16_t charge116;
        int16_t charge216;
        int16_t charge316;
        uint32_t suivanN;
        uint32_t suivanU;
        // boucle principale
        while (1) {
            // Attente du déclenchement pour traitement des portions
            WaitForSingleObject(startProcessEventHandle, INFINITE);
            if (!ResetEvent(startProcessEventHandle)) {
                printf("Probleme de reset de l'evenement startProcessEventHandle");
            }
            // Traitement portions (création des notes locales/toSend)
            for (uint32_t portion = 0; portion < DIMENSION_Z * DIMENSION_Y * DIMENSION_X; portion++) {
                typestev = U8_FROM(crvDatas, TAI_CRVPS, portion, OCP_TYPESTEV_1);
                switch (typestev) {
                    case TYPE_LECTEUR: {
                        // utilisateur source
                        sourceU = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_L_PAM_U_4);
                        if (sourceU != 0) {
                            printf("Data-source and reader must be in same cervelet");
                            break;
                        }
                        else {
                            // portion source
                            sourceN = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_L_PAM_N_4);
                            if (sourceN != 0) {
                                // parametres de lecture
                                largSegment8 = U8_FROM(crvDatas, TAI_CRVPS, sourceN, OCP_D_LGSEG_1);
                                indexDeLecture16 = U16_FROM(crvDatas, TAI_CRVPS, portion, OCP_L_INDEX_2);
                                bitALire8 = U8_FROM(crvDatas, TAI_CRVPS, portion, OCP_L_NMBIT_1);
                                // adresse de lecture
                                adresse64 = U64_FROM(crvDatas, TAI_CRVPS, sourceN, OCP_D_ADDRS_8);
                                adresseALire = adresse64 + indexDeLecture16 * largSegment8;
                                // lecture du bit
                                valeur64;
                                memcpy(&valeur64, (void*)(uintptr_t)adresseALire, sizeof(uint64_t));
                                bit = (valeur64 >> bitALire8) & 1;
                                // application si bit = 1
                                if (bit) {
                                    // destination
                                    destinN = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_L_DST_N_4);
                                    if (destinN != 0) {
                                        surcharge8 = S8_FROM(crvDatas, TAI_CRVPS, portion, OCP_L_BSYNV_1);
                                        destinU = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_L_DST_U_4);
                                        if (destinU == 0) {
                                            ajouter_note_locale(portion, MSG_PUL_1, (int32_t)surcharge8, destinN);
                                        }
                                        else {
                                            ajouter_note_toSend(myPackId, portion, MSG_PUL_1, (int32_t)surcharge8, destinU, destinN);
                                        }
                                    }
                                }
                                // lire la persistance actuelle
                                persistActu8 = U8_FROM(crvDatas, TAI_CRVPS, portion, OCP_L_PERSA_1);
                                if (persistActu8 == 0) {
                                    // remplacer la persistance actuelle par la base
                                    persistActu8 = U8_FROM(crvDatas, TAI_CRVPS, portion, OCP_L_PERSB_1);
                                    // lire l'index de lecture actuel et l'augmenter
                                    indexDeLecture16 = indexDeLecture16 + 1;
                                    // s'il depasse le nombre de segments, le remettre a 0
                                    nbrSeg16 = U16_FROM(crvDatas, TAI_CRVPS, sourceN, OCP_D_NBSEG_2);
                                    indexDeLecture16 = (indexDeLecture16 < nbrSeg16 ? indexDeLecture16 : 0);
                                    FOR_U16_TO(crvDatas, TAI_CRVPS, portion, OCP_L_INDEX_2) = indexDeLecture16;
                                }
                                FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_L_PERSA_1) = persistActu8 - 1;
                            }
                        }
                        break;
                    }
                    case TYPE_PULSEUR: {
                        // chrono = charge actuelle
                        chrono16 = U16_FROM(crvDatas, TAI_CRVPS, portion, OCP_P_CHARGI_2);
                        // surcharge
                        chrono32 = (uint32_t)chrono16 + INCREMENT_PULSEURS;
                        // limitation word
                        chrono32 = (chrono32 < CHRONO_MAXI) ? chrono32 : CHRONO_MAXI;
                        // seuil
                        pSeuil8 = U8_FROM(crvDatas, TAI_CRVPS, portion, OCP_P_SEUIL_1);
                        seuilU32 = (1 << pSeuil8) - 1;
                        // test
                        if (chrono32 < seuilU32) {
                            // simple enregistrement de la nouvelle charge
                            FOR_U16_TO(crvDatas, TAI_CRVPS, portion, OCP_P_CHARGI_2) = (uint16_t)chrono32;
                        }
                        else {
                            // reinitialisation charge + enregistrement
                            FOR_U16_TO(crvDatas, TAI_CRVPS, portion, OCP_P_CHARGI_2) = CHRONO_INITIAL;
                            // surcharge signee destination
                            surcharge8 = S8_FROM(crvDatas, TAI_CRVPS, portion, OCP_P_BSYNV_1);
                            // surcharge destination
                            #if USE_NOTES
                                destinN = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_P_DST_N_4);
                                if (destinN != 0) {
                                    // user destinataire
                                    destinU = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_P_DST_U_4);
                                    if (destinU == 0) {
                                        ajouter_note_locale(portion, MSG_PUL_1, (int32_t)surcharge8, destinN);
                                    }
                                    else {
                                        ajouter_note_toSend(myPackId, portion, MSG_PUL_1, (int32_t)surcharge8, destinU, destinN);
                                    }
                                }
                            #else
                                // potyion destinataire
                                destinN = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_P_DST_N_4);
                                // charge actuelle
                                int16_t chargeD16 = S16_FROM(crvDatas, TAI_CRVPS, destinN, OCP_CHARG0_2);
                                // surcharge
                                int32_t chargeD32 = (int32_t)(chargeD16) + (int32_t)surcharge8;
                                chargeD32 = (chargeD32 > CHARGE_MINI) ? chargeD32 : CHARGE_MINI;
                                chargeD32 = (chargeD32 < CHARGE_MAXI) ? chargeD32 : CHARGE_MAXI;
                                // enregistrement de la nouvelle charge
                                FOR_S16_TO(crvDatas, TAI_CRVPS, destinN, OCP_CHARG0_2) = (int16_t)chargeD32;
                                // sortie
                            #endif
                        }
                        break;
                    }
                    case TYPE_SEGMENT_GERME:
                    case TYPE_SEGMENT_PARTIEL:
                    case TYPE_SEGMENT_COMPLET: {
                        // mise a 0 de l'activite
                        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_ACTIVITE_1) = 0;
                        // extraction de la charge 0 + mise a 0
                        chargeAUt16 = S16_FROM(crvDatas, TAI_CRVPS, portion, OCP_CHARG0_2);
                        FOR_S16_TO(crvDatas, TAI_CRVPS, portion, OCP_CHARG0_2) = 0;
                        // extraction de la charge 1 et sortie si invalide
                        charge116 = S16_FROM(crvDatas, TAI_CRVPS, portion, OCP_S_CHARG1_2);
                        if (charge116 != CHARGE_INVALIDE) {
                            // transfert charge 0 vers position 1 + charge a utiliser
                            FOR_S16_TO(crvDatas, TAI_CRVPS, portion, OCP_S_CHARG1_2) = chargeAUt16;
                            chargeAUt16 = charge116;
                            // extraction de la charge 2 et sortie si invalide
                            charge216 = S16_FROM(crvDatas, TAI_CRVPS, portion, OCP_S_CHARG2_2);
                            if (charge216 != CHARGE_INVALIDE) {
                                // transfert charge 1 vers position 2 + charge a utiliser
                                FOR_S16_TO(crvDatas, TAI_CRVPS, portion, OCP_S_CHARG2_2) = chargeAUt16;
                                chargeAUt16 = charge216;
                                // extraction de la charge 3 et sortie si invalide
                                charge316 = S16_FROM(crvDatas, TAI_CRVPS, portion, OCP_S_CHARG3_2);
                                if (charge316 != CHARGE_INVALIDE) {
                                    // transfert charge 2 vers position 3 + charge a utiliser
                                    FOR_S16_TO(crvDatas, TAI_CRVPS, portion, OCP_S_CHARG3_2) = chargeAUt16;
                                    chargeAUt16 = charge316;
                                }
                            }
                        }
                        suivanN = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_S_SVT_N_4);
                        if (suivanN != 0) {
                            suivanU = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_S_SVT_U_4);
#if USE_NOTES
                            if (suivanU == 0) {
                                ajouter_note_locale(portion, MSG_SEG_1, (int32_t)chargeAUt16, suivanN);
                            }
                            else {
                                ajouter_note_toSend(myPackId, portion, MSG_SEG_1, (int32_t)chargeAUt16, suivanU, suivanN);
                            }
#else
                            if (suivanU == 0) {
                                // doApplicate MSG_SEG_1 :
                                    // utilisation de chargeAUt16
                                int16_t chargeD16 = S16_FROM(crvDatas, TAI_CRVPS, suivanN, OCP_CHARG0_2);
                                // surcharger_destination();
                                int32_t chargeD32 = (int32_t)(chargeD16)+chargeAUt16;
                                chargeD32 = (chargeD32 > CHARGE_MINI) ? chargeD32 : CHARGE_MINI;
                                chargeD32 = (chargeD32 < CHARGE_MAXI) ? chargeD32 : CHARGE_MAXI;
                                FOR_S16_TO(crvDatas, TAI_CRVPS, suivanN, OCP_CHARG0_2) = (int16_t)chargeD32;
                                // transfert de l'activite du suivant vers l'actuel
                                uint8_t actSuiv8 = U8_FROM(crvDatas, TAI_CRVPS, suivanN, OCP_ACTIVITE_1);
                                // doApplicate MSG_SEG_1A
                                FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_ACTIVITE_1) = actSuiv8;
                            }
#endif
                        }
                        else{
                            // destination invalide donc passer en mode germe ?
                        }
                        break;
                    }
                    case TYPE_NEURONE_GERME:
                    case TYPE_NEURONE_PARTIEL:
                    case TYPE_NEURONE_COMPLET: {
                        // auto-désactivation
                        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_ACTIVITE_1) = 0;
                        // décompte réfractaire
                        decpteRef8 = U8_FROM(crvDatas, TAI_CRVPS, portion, OCP_N_CREFR_1);
                        // si le neurone n'est plus refractaire
                        if (decpteRef8 == 0) {
                            // charge actuelle 16 bits signée
                            chargeD16 = S16_FROM(crvDatas, TAI_CRVPS, portion, OCP_CHARG0_2);
                            // puissance de seuil
                            pSeuil8 = U8_FROM(crvDatas, TAI_CRVPS, portion, OCP_N_SEUIL_1);
                            // seuil de 1->1 à 15->32767
                            seuil32 = (1 << pSeuil8) - 1;
                            // si déclenchement
                            if (chargeD16 >= seuil32) {
                                // auto-activation
                                FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_ACTIVITE_1) = 1;
                                // base réfractaire pour réinitialisation
                                baseRefr8 = U8_FROM(crvDatas, TAI_CRVPS, portion, OCP_N_BREFR_1);
                                // lancement compteur refractaire
                                FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_N_CREFR_1) = baseRefr8;
                            }
                            // si le seuil n'est pas encore atteint
                            else {
                                // lente décharge
                                chargeD16 -= VITESSE_DECHARGE;
                                chargeD16 = (chargeD16 < 0 ? 0 : chargeD16);
                                FOR_S16_TO(crvDatas, TAI_CRVPS, portion, OCP_CHARG0_2) = chargeD16;
                            }
                        }
                        // si le neurone est encore refractaire
                        else {
                            // décompte réfractaire
                            decpteRef8 -= 1;
                            FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_N_CREFR_1) = decpteRef8;
                            if (decpteRef8 == 0) {
                                // réinitialisation de la charge
                                FOR_U16_TO(crvDatas, TAI_CRVPS, portion, OCP_CHARG0_2) = 0;
                            }
                        }
                        break;
                    }
                    case TYPE_EXTENSION_GERME:
                    case TYPE_EXTENSION_PARTIEL:
                    case TYPE_EXTENSION_COMPLET: {
                        antecedN = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_ANT_N_4);
                        if (antecedN == 0) {
                            // extension orpheline
                            // auto-destruction ?
                        }
                        else {
#if USE_NOTES
                            antecedU = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_ANT_U_4);
                            if (antecedU == 0) {
                                ajouter_note_locale(portion, MSG_EXT_1, 0, antecedN);
                            }
                            else {
                                ajouter_note_toSend(myPackId, portion, MSG_EXT_1, 0, antecedU, antecedN);
                            }
#else
                            antecedU = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_ANT_U_4);
                            // antécédent local
                            if (antecedU == 0) {
                                // activité de l'antecedent
                                activAntec8 = U8_FROM(crvDatas, TAI_CRVPS, antecedN, OCP_ACTIVITE_1); // ------------------------- ANT
                                // si l'antecedent est actif : déclenchement de tout
                                // la refractarité garantit qu'on n'est pas déjà dans une attente de retroaction
                                if (activAntec8 != 0) {
                                    // s'auto-activer
                                    FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_ACTIVITE_1) = 1;
                                    // destination locale
                                    destinU = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_DST_U_4);
                                    if (destinU == 0) {
                                        // présence d'une destination
                                        destinN = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_DST_N_4);
                                        if (destinN != 0) {
                                            // activer le compteur
                                            FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_E_CACTT_1) = 1;
                                            // surcharge de la destination
                                            surcharge8 = S8_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_BSYNV_1);
                                            chargeD16 = S16_FROM(crvDatas, TAI_CRVPS, destinN, OCP_CHARG0_2); // ----------------- DST
                                            chargeD32 = (int32_t)(chargeD16) + surcharge8;
                                            chargeD32 = (chargeD32 > CHARGE_MINI) ? chargeD32 : CHARGE_MINI;
                                            chargeD32 = (chargeD32 < CHARGE_MAXI) ? chargeD32 : CHARGE_MAXI;
                                            FOR_S16_TO(crvDatas, TAI_CRVPS, destinN, OCP_CHARG0_2) = (int16_t)chargeD32; // ------ DST
                                        }
                                        else {
                                            // générer une pousse ?
                                        }
                                    }
                                }
                                // si l'antecedent n'est pas actif : on se contente de faire évoluer le compteur
                                // et on vérifier le retour de retro-action (impossible si on vient de déclencher)
                                else {
                                    // s'auto-desactiver
                                    FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_ACTIVITE_1) = 0;
                                    // évolution du compteur d'activite s'il est actif
                                    comptAct8 = U8_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_CACTT_1);
                                    if (comptAct8 != 0) {
                                        comptAct8 ++;
                                        // rétroaction
                                        // destination locale
                                        destinU = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_DST_U_4);
                                        if (destinU == 0) {
                                            // présence d'une destination
                                            destinN = U32_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_DST_N_4);
                                            if (destinN != 0) {
                                                // activité de la destination
                                                actDest8 = U8_FROM(crvDatas, TAI_CRVPS, destinN, OCP_ACTIVITE_1); // --------------------- DST
                                                if (actDest8 != 0) {
                                                    // auto-modification par rétro-action
                                                    comptAct8 = U8_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_CACTT_1);
                                                    nivActivDest = U8_FROM(crvDatas, TAI_CRVPS, destinN, OCP_NIVACTMP_1); // ------------- DST
                                                    // modification de la valeur du bloc de boutons
                                                    if (comptAct8 != nivActivDest) {
                                                        valblocb8 = S8_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_BSYNV_1);
                                                        valblocb8 = (valblocb8 <= MINI_VALBB ? MINI_VALBB : valblocb8 - 1);
                                                    }
                                                    else {
                                                        valblocb8 = S8_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_BSYNV_1);
                                                        valblocb8 = (valblocb8 >= MAXI_VALBB ? MAXI_VALBB : valblocb8 + 1);
                                                    }
                                                    FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_E_BSYNV_1) = valblocb8;
                                                    // modification de la masse de bloc de boutons
                                                    masblocb8 = U8_FROM(crvDatas, TAI_CRVPS, portion, OCP_E_BSYNM_1);
                                                    masblocb8 = (masblocb8 >= MAXI_MASSEBB ? MAXI_MASSEBB : masblocb8 + 1);
                                                    FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_E_BSYNM_1) = masblocb8;
                                                    // neutraliser le compteur d'activité
                                                    comptAct8 = 0;
                                                }
                                            }
                                        }
                                        // enregistrement du compteur d'activité (incrémenté ou neutralisé)
                                        FOR_U8_TO(crvDatas, TAI_CRVPS, portion, OCP_E_CACTT_1) = comptAct8;
                                    }
                                }
                            }
#endif
                        }
                        break;
                    }
                }
            }
            cumulNotesATraiter += toSendStackIndex[toSendStackToFill];
            // Signalement de fin de traitement des portions
            if (!SetEvent(processEndedEventHandle)) {
                printf("Erreur lors de l'activation de l'evenement de fin de process. Code : %lu\n", GetLastError());
                return 1;
            }
        }
    }
    DWORD WINAPI DirectThread(LPVOID param) {
        // en entrée :
        // nombre total de notes à traiter en local = localStackIndex[localStackToFill]
        // nombre de notes par destinataire dans localDests[localStackToRead]
        // toutes lotes à traiter en local dans localNotesStack[localStackToFill]
        int localStackToRead;
        uint32_t nombreDePaquetsATraiter;
        uint32_t pointeurDUnite; // position de depart dans localMessagesPacks (utilisé deux fois)
        uint32_t portDest;
        uint32_t nombreDeNotesLocal;  // maxi 2^^32-1 car ne peut pas dépasser le nombre de portions ?
        uint64_t note;
        uint32_t portionDestinataire;
        uint32_t emplacementLocMsg;
        uint32_t longueurPaquetsToApply;
        uint32_t emetteur;
        uint32_t demande;
        uint32_t parametres;
        // boucle principale
        while (1) {
            // Attente du déclenchement pour traitement des portions
            WaitForSingleObject(startDirectEventHandle, INFINITE);
            if (!ResetEvent(startDirectEventHandle)) {
                printf("Probleme de reset de l'evenement startProcessEventHandle");
            }
            // Changement de buffer
            localStackToFill = 1 - localStackToFill;
            localStackIndex[localStackToFill] = 0;
            localStackToRead = 1 - localStackToFill;
            // Initialisation des paquets de localPacks (par portion de destination)
            nombreDePaquetsATraiter = 0;
            pointeurDUnite = 0;
            // A_FAIRE : prendre en compte la place prise par les entetes => > uint32_t !!!! mais peu probable car source locale
            for (portDest = 0; portDest < NBR_CRVPS; portDest++) {
                nombreDeNotesLocal = U32_FROM(localDests[localStackToRead], TAI_LOC_DST, portDest, OLD_NMSGS_4); // Nombre de notes pour ce paquet
                if (nombreDeNotesLocal != 0) {
                    // remplissage de l'en-tete
                    FOR_U32_TO(localMessagesPacks, TAI_LOC_MSG, pointeurDUnite, OLM_P_DEST_4) = portDest; // portion destinataire
                    FOR_U32_TO(localMessagesPacks, TAI_LOC_MSG, pointeurDUnite, OLM_P_TAIP_4) = nombreDeNotesLocal; // Nombre de notes pour ce paquet
                    pointeurDUnite++; // Position pour la premiere note de ce paquet
                    FOR_U32_TO(localDests[localStackToRead], TAI_LOC_DST, portDest, OLD_POSIT_4) = pointeurDUnite; // Position ou ecrire la premiere note
                    pointeurDUnite += nombreDeNotesLocal; // Position du debut du prochain paquet
                    FOR_U32_TO(localDests[localStackToRead], TAI_LOC_DST, portDest, OLD_NMSGS_4) = 0; // mise a zero du nombre de notes (pour la fois suivante)
                    nombreDePaquetsATraiter++;
                }
            }
            // Pour suivi
            remplissagePacksLocaux = (remplissagePacksLocaux > pointeurDUnite) ? remplissagePacksLocaux : pointeurDUnite;
            // Dépilage des messages vers les paquets de localMessagesPacks
            // en faire une fonction depilage(...)
            if (nombreDePaquetsATraiter != 0) {
                for (note = 0; note < localStackIndex[localStackToRead]; note++) {
                    portionDestinataire = U32_FROM(localNotesStack[localStackToRead], TAI_LOC_NOT, note, OLN_DST_4);
                    // numero de message en cours pour cet utilisateur
                    emplacementLocMsg = U32_FROM(localDests[localStackToRead], TAI_LOC_DST, portionDestinataire, OLD_POSIT_4);
                    // enregistrement du message
                    FOR_U32_TO(localMessagesPacks, TAI_LOC_MSG, emplacementLocMsg, OLM_M_EMM_4) = U32_FROM(localNotesStack[localStackToRead], TAI_LOC_NOT, note, OLN_EMM_4);
                    FOR_U32_TO(localMessagesPacks, TAI_LOC_MSG, emplacementLocMsg, OLM_M_DEMC_4) = U32_FROM(localNotesStack[localStackToRead], TAI_LOC_NOT, note, OLN_DEMC_4);
                    FOR_U32_TO(localMessagesPacks, TAI_LOC_MSG, emplacementLocMsg, OLM_M_DEMP_4) = U32_FROM(localNotesStack[localStackToRead], TAI_LOC_NOT, note, OLN_DEMP_4);
                    // calage sur suivant pour cet utilisateur
                    FOR_U32_TO(localDests[localStackToRead], TAI_LOC_DST, portionDestinataire, OLD_POSIT_4) = emplacementLocMsg + 1;
                }
            }
            // --------------------- réinitialisation de la pile de messages ?



            // --------------------- application des packs de localPacks (par portions de destination)
            // application a une partie des portions
            // parcourir localMessagesPacks et appliquer par portion destinataire
            // dans un second temps, le faire par paquets de X portions
            pointeurDUnite = 0;
            for (int pack = 0; pack < nombreDePaquetsATraiter; pack++) {
                portionDestinataire = U32_FROM(localMessagesPacks, TAI_LOC_MSG, pointeurDUnite, OLM_P_DEST_4);
                longueurPaquetsToApply = U32_FROM(localMessagesPacks, TAI_LOC_MSG, pointeurDUnite, OLM_P_TAIP_4);
                pointeurDUnite++;
                for (int i = 0; i < longueurPaquetsToApply; i++) {
                    emetteur = U32_FROM(localMessagesPacks, TAI_LOC_MSG, pointeurDUnite, OLM_M_EMM_4);
                    demande = U32_FROM(localMessagesPacks, TAI_LOC_MSG, pointeurDUnite, OLM_M_DEMC_4);
                    parametres = U32_FROM(localMessagesPacks, TAI_LOC_MSG, pointeurDUnite, OLM_M_DEMP_4);
                    // traitement du messages
                    doApplicate(0, emetteur, demande, parametres, portionDestinataire);
                    pointeurDUnite++;
                }
            }
            // Signalement de fin de traitement des portions
            if (!SetEvent(directEndedEventHandle)) {
                printf("Erreur lors de l'activation de l'evenement localEndedEventHandle. Code : %lu\n", GetLastError());
                return 1;
            }
        }
    }
    DWORD WINAPI SendThread(LPVOID param) {
        int toSendStackToRead;
        uint32_t nombreDePaquetsAEmettre; // maxi 2^^32-1 car ne peut pas dépasser le nombre de messages
        uint64_t pointeurDUniteToSend; // position de depart dans toSendMessagesPacks
        uint64_t nombreDeNotesToSend; // maxi 2^^32-1 car ne peut pas dépasser le nombre de portions
        uint16_t utilisateurDestinataire;
        uint64_t nombreDeNotesDuPaquet;
        uint32_t destinataireNote;
        uint64_t emplacementMessage;
        uint16_t comptagePartiel;
        uint32_t longueurPaquetsToSend;
        int note;
        uint32_t paquet;
        // boucle principale
        while (1) {
            // --------------------- Attente
            WaitForSingleObject(sendStartEventHandle, INFINITE);
            if (!ResetEvent(sendStartEventHandle)) {
                printf("Probleme de reset de l'evenement d'envoi des messages");
            }
            // Changement de buffer
            toSendStackToFill = 1 - toSendStackToFill;
            toSendStackIndex[toSendStackToFill] = 0;
            toSendStackToRead = 1 - toSendStackToFill;
            // --------------------- Initialisation des paquets
            nombreDePaquetsAEmettre = 0;
            pointeurDUniteToSend = 0;
            for (utilisateurDestinataire = 0; utilisateurDestinataire < nombreDUtilisateurs; utilisateurDestinataire++) {
                // Initialisation du compte partiel
                FOR_U16_TO(toSendUsers[toSendStackToRead], TAI_SND_USR, utilisateurDestinataire, OSU_DECM_2) = 0;
                // Nombre de notes pour ce destinataire
                nombreDeNotesToSend = U64_FROM(toSendUsers[toSendStackToRead], TAI_SND_USR, utilisateurDestinataire, OSU_NMES_8);
                // Position ou ecrire la premiere note des paquets pour ce destinataire
                FOR_U64_TO(toSendUsers[toSendStackToRead], TAI_SND_USR, utilisateurDestinataire, OSU_ADRP_8) = pointeurDUniteToSend + 1;
                // initialisation des paquets pour cet utilisateur
                while (nombreDeNotesToSend != 0) {
                    nombreDeNotesDuPaquet = (nombreDeNotesToSend < MAX_UNITS_PER_UDP_PAQUET) ? nombreDeNotesToSend : MAX_UNITS_PER_UDP_PAQUET;
                    // remplissage de l'en-tete
                    FOR_U32_TO(toSendMessagesPacks, TAI_SND_MSG, pointeurDUniteToSend, OSM_P_IPAD_4) = U32_FROM(netUsers, TAI_USR_NET, utilisateurDestinataire, OUN_IPAD_4);
                    FOR_U16_TO(toSendMessagesPacks, TAI_SND_MSG, pointeurDUniteToSend, OSM_P_PORT_2) = U16_FROM(netUsers, TAI_USR_NET, utilisateurDestinataire, OUN_PORT_2);
                    FOR_U16_TO(toSendMessagesPacks, TAI_SND_MSG, pointeurDUniteToSend, OSM_P_VIDE_2) = 0;
                    FOR_U64_TO(toSendMessagesPacks, TAI_SND_MSG, pointeurDUniteToSend, OSM_P_TAIP_8) = nombreDeNotesDuPaquet;
                    // printf(".n=%d.", (int)nombreDeNotesDuPaquet);
                    // Position du debut du prochain paquet
                    pointeurDUniteToSend += (1 + nombreDeNotesDuPaquet);
                    // décomptes
                    nombreDePaquetsAEmettre++;
                    nombreDeNotesToSend -= nombreDeNotesDuPaquet;
                }
                // mise a zero du nombre de notes (pour la fois suivante)
                FOR_U64_TO(toSendUsers[toSendStackToRead], TAI_SND_USR, utilisateurDestinataire, OSU_NMES_8) = 0;
            }
            // pour suivi
            remplissagePacksToSend = (remplissagePacksToSend > pointeurDUniteToSend) ? remplissagePacksToSend : pointeurDUniteToSend;
            // --------------------- Transfert des messages
            if (nombreDePaquetsAEmettre != 0) {
                for (note = 0; note < toSendStackIndex[toSendStackToRead]; note++) {
                    destinataireNote = U32_FROM(toSendNotesStack[toSendStackToRead], TAI_SND_NOT, note, OSN_DESU_4);
                    // numero de message en cours pour cet utilisateur
                    emplacementMessage = U64_FROM(toSendUsers[toSendStackToRead], TAI_SND_USR, destinataireNote, OSU_ADRP_8);
                    // enregistrement du message
                    FOR_U32_TO(toSendMessagesPacks, TAI_SND_MSG, emplacementMessage, OSM_M_EMMP_4) = U32_FROM(toSendNotesStack[toSendStackToRead], TAI_SND_NOT, note, OSN_EMMP_4);
                    FOR_U32_TO(toSendMessagesPacks, TAI_SND_MSG, emplacementMessage, OSM_M_EMMU_4) = U32_FROM(toSendNotesStack[toSendStackToRead], TAI_SND_NOT, note, OSN_EMMU_4);
                    FOR_U32_TO(toSendMessagesPacks, TAI_SND_MSG, emplacementMessage, OSM_M_DESP_4) = U32_FROM(toSendNotesStack[toSendStackToRead], TAI_SND_NOT, note, OSN_DESP_4);
                    FOR_U32_TO(toSendMessagesPacks, TAI_SND_MSG, emplacementMessage, OSM_M_DESU_4) = U32_FROM(toSendNotesStack[toSendStackToRead], TAI_SND_NOT, note, OSN_DESU_4);
                    FOR_U32_TO(toSendMessagesPacks, TAI_SND_MSG, emplacementMessage, OSM_M_DEMC_4) = U32_FROM(toSendNotesStack[toSendStackToRead], TAI_SND_NOT, note, OSN_DEMC_4);
                    FOR_U32_TO(toSendMessagesPacks, TAI_SND_MSG, emplacementMessage, OSM_M_DEMP_4) = U32_FROM(toSendNotesStack[toSendStackToRead], TAI_SND_NOT, note, OSN_DEMP_4);
                    // printf("-sent%u", U32_FROM(toSendMessagesPacks, TAI_SND_MSG, emplacementMessage, OSM_M_DEMP_4));
                    // calage et sur-calage sur suivant pour changement de paquet pour cet utilisateur
                    comptagePartiel = U16_FROM(toSendUsers[toSendStackToRead], TAI_SND_USR, destinataireNote, OSU_DECM_2) + 1;
                    if (comptagePartiel < MAX_UNITS_PER_UDP_PAQUET) {
                        FOR_U16_TO(toSendUsers[toSendStackToRead], TAI_SND_USR, destinataireNote, OSU_DECM_2) = comptagePartiel;
                        FOR_U64_TO(toSendUsers[toSendStackToRead], TAI_SND_USR, destinataireNote, OSU_ADRP_8) = emplacementMessage + 1;
                    }
                    else {
                        FOR_U16_TO(toSendUsers[toSendStackToRead], TAI_SND_USR, destinataireNote, OSU_DECM_2) = 0;
                        FOR_U64_TO(toSendUsers[toSendStackToRead], TAI_SND_USR, destinataireNote, OSU_ADRP_8) = emplacementMessage + 2;
                    }
                }
            }
            // --------------------- Envoi des messages
            pointeurDUniteToSend = 0;
            // printf("#%d#", nombreDePaquetsAEmettre);
            for (paquet = 0; paquet < nombreDePaquetsAEmettre; paquet++) {
                toDestSockAddr.sin_family = AF_INET;
                toDestSockAddr.sin_addr.s_addr = U32_FROM(toSendMessagesPacks, TAI_SND_MSG, pointeurDUniteToSend, OSM_P_IPAD_4); // Adresse cible (ou inet_addr("192.168.1.1"))
                toDestSockAddr.sin_port = U16_FROM(toSendMessagesPacks, TAI_SND_MSG, pointeurDUniteToSend, OSM_P_PORT_2); // Port cible (ou htons(8080))
                longueurPaquetsToSend = (uint32_t)U64_FROM(toSendMessagesPacks, TAI_SND_MSG, pointeurDUniteToSend, OSM_P_TAIP_8); // Longueur du paquet en unites
                pointeurDUniteToSend++;
                int result = sendto(
                    sendSocketHandle,
                    &((uint8_t*)toSendMessagesPacks)[pointeurDUniteToSend * TAI_SND_MSG],
                    longueurPaquetsToSend * TAI_SND_MSG,
                    0,
                    (struct sockaddr*)&toDestSockAddr,
                    SOCKET_ADDRESS_LEN);
                if (result == SOCKET_ERROR) {
                    printf("Probleme d'envoi de paquet. Code : %lu\n", WSAGetLastError());
                    // printf(".%d.", longueurPaquetsToSend * TAI_SND_MSG);
                }
                pointeurDUniteToSend += longueurPaquetsToSend;
                // printf(".");
            }
            // Signalement de fin d'envoi
            if (!SetEvent(sendEndedEventHandle)) {
                printf("Erreur lors de l'activation de l'evenement sendEndedEventHandle. Code : %lu\n", GetLastError());
                return 1;
            }
        }
        return 0;
    }
    void InitIncomeIocp() {
        // Création du port IOCP à vide
        incomeIocpHandle = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0);
        if (!incomeIocpHandle) {
            printf("Erreur de la creation IOCP\n");
            exit(1);
        }
        // Associer le socket au Completion Port
        if(CreateIoCompletionPort((HANDLE)incomeSocketHandle, incomeIocpHandle, (ULONG_PTR)incomeSocketHandle, 0) == NULL) {
            printf("Erreur lors de l'association du socket au port IOCP: %d\n", GetLastError());
            exit(1);
        }
        // Fixe le mode asynchrone
        u_long mode = 1;  // mode asynchrone
        if (ioctlsocket(incomeSocketHandle, FIONBIO, &mode) != 0) {
            printf("Erreur lors du réglage du mode non-bloquant\n");
            exit(1);
        }
    }
    void PostIncomeRecv(IOCP_UDP_PACKET* packet) {
        DWORD incomeFlags = 0;
        packet->incomeAddressLen = sizeof(packet->incomeClientAddress);
        packet->incomeWsaBuffer.buf = packet->incomeBuffer;
        packet->incomeWsaBuffer.len = sizeof(packet->incomeBuffer);
        int resultat = WSARecvFrom(
            incomeSocketHandle,
            &packet->incomeWsaBuffer,
            1,
            NULL,
            &incomeFlags,
            (struct sockaddr*)&packet->incomeClientAddress,
            &packet->incomeAddressLen,
            &packet->incomeOverlapped,
            NULL);
        if (resultat == SOCKET_ERROR) {
            int err = WSAGetLastError();
            if (err != WSA_IO_PENDING) {
                printf("Erreur WSARecvFrom: %d\n", err);
            }
            else {
                // printf("WSARecvFrom en attente IOCP\n");
            }
        }
        else {
            printf("WSARecvFrom terminé immédiatement\n");
        }
    }
    DWORD WINAPI FillingThread(LPVOID param) {
        BOOL success;
        DWORD bytesTransferred;
        ULONG_PTR key;
        IOCP_UDP_PACKET* packet;
        DWORD dwError;
        // boucle principale
        while (1) {
            success = GetQueuedCompletionStatus(incomeIocpHandle, &bytesTransferred, &key, (LPOVERLAPPED*)&packet, INFINITE);
            if (!success) {
                dwError = GetLastError();
                if (dwError == ERROR_IO_PENDING) {
                    // L'opération est en cours, ne rien faire
                    // Continuer à attendre : retour au début de la boucle
                    continue;
                }
                else {
                    printf("Erreur GetQueuedCompletionStatus, code d'erreur : %lu\n", dwError);
                    // Gérer l'erreur si nécessaire
                    // Recommencer à attendre : retour au début de la boucle
                    continue;
                }
            }
            // printf("Pointeur : %lu\n", receiveStackIndex[receiveStackToFill]);
            // printf("Recus : %lu\n", bytesTransferred);
            // printf("Taille : %lu\n", sizeof(receiveNotesStack[0]));
            // Vérification de la recevabilité du paquet udp
            if ((bytesTransferred % TAI_RCV_NOT == 0) && (receiveStackIndex[receiveStackToFill] * TAI_RCV_NOT + bytesTransferred <= sizeof(receiveNotesStack[0]))) {
                // Entrée dans de la section critique
                EnterCriticalSection(&incomeCriticalSection);
                // sur option : lister les messages et incrementer les destinataires

                // LIMITER

                // remplir la pile
                memcpy(receiveNotesStack[receiveStackToFill][receiveStackIndex[receiveStackToFill]], packet->incomeBuffer, bytesTransferred);
                // avance du pointeur
                receiveStackIndex[receiveStackToFill] += bytesTransferred / TAI_RCV_NOT;
                // pour suivi
                cumulFluxIOCPEntrant += bytesTransferred;
                // Sortie de la section critique
                LeaveCriticalSection(&incomeCriticalSection);
                // Libérer la mémoire après traitement ??? NON : réutilisation
                // free(packet);
            }
            else {
                refusedUdp += 1;
            }
            // Relancer une réception
            PostIncomeRecv(packet);
        }
        //
        printf("SORTIE DU WORKER THREAD");
        return 0;
    }
    DWORD WINAPI ReceiveThread(LPVOID param) {
        // pour decompte
        int receiveStackToRead;
        int nombreDeMessagesALire;
        int numeroDeMessage;
        uint32_t portionDestinataire;
        uint32_t nombreDeMessages;
        // pour dépilage
        uint32_t pointeurDUnite;
        uint32_t nombreDeDestsATraiter;
        uint32_t nombreDeNotesPourLocal;
        // pour traitement
        int numeroDePaquet;
        uint32_t portionEmettrice;
        uint32_t utilisateurEmetteur;
        uint32_t commande;
        uint32_t parametres;
        int16_t chargeD16;
        int32_t chargeD32;
        // boucle principale
        while (1) {
            // attendre la demande de traitement du thread principal
            WaitForSingleObject(receiveStartEventHandle, INFINITE);
            if (!ResetEvent(receiveStartEventHandle)) {
                printf("Probleme de reset de l'evenement receiveStartEventHandle");
            }
            // Changer de buffer de réception IOCP
            EnterCriticalSection(&incomeCriticalSection);
            receiveStackToFill = 1 - receiveStackToFill;
            receiveStackIndex[receiveStackToFill] = 0;
            LeaveCriticalSection(&incomeCriticalSection);
            receiveStackToRead = 1 - receiveStackToFill;
            // Décompte des messages par destinataire (dans receiveNotesStack[receiveStackToRead])
            nombreDeMessagesALire = receiveStackIndex[receiveStackToRead];
            for (numeroDeMessage = 0; numeroDeMessage < nombreDeMessagesALire; numeroDeMessage++) {
                portionDestinataire = U32_FROM(receiveNotesStack[receiveStackToRead], TAI_SND_MSG, numeroDeMessage, OSM_M_DESP_4);
                nombreDeMessages = U32_FROM(receiveDests, TAI_RCV_DST, portionDestinataire, ORD_NMSGS_4);
                FOR_U32_TO(receiveDests, TAI_RCV_DST, portionDestinataire, ORD_NMSGS_4) = nombreDeMessages + 1;
            }
            // Initialisation des paquets de receiveMessagesPacks (par portion de destination)
            pointeurDUnite = 0;
            nombreDeDestsATraiter = 0;
            for (portionDestinataire = 0; portionDestinataire < NBR_CRVPS; portionDestinataire++) {
                nombreDeNotesPourLocal = U32_FROM(receiveDests, TAI_RCV_DST, portionDestinataire, ORD_NMSGS_4);
                if (nombreDeNotesPourLocal != 0) {
                    // Position ou ecrire la premiere note
                    FOR_U32_TO(receiveDests, TAI_RCV_DST, portionDestinataire, ORD_POSIT_4) = pointeurDUnite;
                    // Position du debut du prochain paquet
                    pointeurDUnite += nombreDeNotesPourLocal;
                    // mise a zero du nombre de notes (pour la fois suivante)
                    FOR_U32_TO(receiveDests, TAI_RCV_DST, portionDestinataire, ORD_NMSGS_4) = 0;
                    nombreDeDestsATraiter++;
                }
            }
            // nombreDeNotesPourLocal = pointeurDUnite; // Si on doit supprimer des messages ?
            // Pour suivi du taux de remplissage
            remplissagePacksReceived = (remplissagePacksReceived > pointeurDUnite) ? remplissagePacksReceived : pointeurDUnite;
            // Dépilage des messages de receiveNotesStack[ receiveStackToRead ] vers receiveMessagesPacks
            if (nombreDeDestsATraiter != 0) {
                // printf("\n");
                for (numeroDeMessage = 0; numeroDeMessage < nombreDeMessagesALire; numeroDeMessage++) {
                    portionDestinataire = U32_FROM(receiveNotesStack[receiveStackToRead], TAI_RCV_NOT, numeroDeMessage, ORN_DSTP_4);
                    // numero de message en cours pour cet utilisateur
                    pointeurDUnite = U32_FROM(receiveDests, TAI_RCV_DST, portionDestinataire, ORD_POSIT_4);
                    // enregistrement du message
                    FOR_U32_TO(receiveMessagesPacks, TAI_RCV_MSG, pointeurDUnite, ORM_M_DSTP_4) = portionDestinataire;
                    FOR_U32_TO(receiveMessagesPacks, TAI_RCV_MSG, pointeurDUnite, ORM_M_EMMP_4) = U32_FROM(receiveNotesStack[receiveStackToRead], TAI_RCV_NOT, numeroDeMessage, ORN_EMMP_4);
                    FOR_U32_TO(receiveMessagesPacks, TAI_RCV_MSG, pointeurDUnite, ORM_M_EMMU_4) = U32_FROM(receiveNotesStack[receiveStackToRead], TAI_RCV_NOT, numeroDeMessage, ORN_EMMU_4);
                    FOR_U32_TO(receiveMessagesPacks, TAI_RCV_MSG, pointeurDUnite, ORM_M_DEMC_4) = U32_FROM(receiveNotesStack[receiveStackToRead], TAI_RCV_NOT, numeroDeMessage, ORN_DEMC_4);
                    FOR_U32_TO(receiveMessagesPacks, TAI_RCV_MSG, pointeurDUnite, ORM_M_DEMP_4) = U32_FROM(receiveNotesStack[receiveStackToRead], TAI_RCV_NOT, numeroDeMessage, ORN_DEMP_4);
                    // calage sur suivant pour cet utilisateur
                    FOR_U32_TO(receiveDests, TAI_RCV_DST, portionDestinataire, ORD_POSIT_4) = pointeurDUnite + 1;
                    // printf("-recv%u", U32_FROM(receiveNotesStack[receiveStackToRead], TAI_RCV_NOT, numeroDeMessage, ORN_EMMP_4));
                }
                // printf("\n");
            }
            // Appliquer les messages
            for (numeroDeMessage = 0; numeroDeMessage < nombreDeMessagesALire; numeroDeMessage++) {
                portionDestinataire = U32_FROM(receiveMessagesPacks, TAI_RCV_MSG, numeroDeMessage, ORM_M_DSTP_4);
                portionEmettrice = U32_FROM(receiveMessagesPacks, TAI_RCV_MSG, numeroDeMessage, ORM_M_EMMP_4);
                utilisateurEmetteur = U32_FROM(receiveMessagesPacks, TAI_RCV_MSG, numeroDeMessage, ORM_M_EMMU_4);
                commande = U32_FROM(receiveMessagesPacks, TAI_RCV_MSG, numeroDeMessage, ORM_M_DEMC_4);
                parametres = U32_FROM(receiveMessagesPacks, TAI_RCV_MSG, numeroDeMessage, ORM_M_DEMP_4);
                // traitement du message
                doApplicate(utilisateurEmetteur, portionEmettrice, commande, parametres, portionDestinataire);
            }
            // printf("\n");
            // Dire qu'on a fini
            if (!SetEvent(receiveFinishedEventHandle)) {
                printf("Erreur lors de l'activation de receiveFinishedEventHandle. Code : %lu\n", GetLastError());
                return 1;
            }
        }
        return 0;
    }
    LRESULT CALLBACK WindowProc(HWND windowHandle, UINT uMsg, WPARAM wParam, LPARAM lParam) {
        switch (uMsg) {
        case WM_KEYDOWN: {
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
                visualize_x = (visualize_x < (DIMENSION_X - 1)) ? visualize_x + 1 : 0;
                return 0;
            }
            if (wParam == 'C' || wParam == 'c') { // y-1
                visualize_y = (visualize_y < (DIMENSION_Y - 1)) ? visualize_y + 1 : 0;
                return 0;
            }
            if (wParam == 'D' || wParam == 'd' || wParam == 'f' || wParam == 'F') { // y+1
                visualize_y = (visualize_y > 0) ? visualize_y - 1 : DIMENSION_Y - 1;
                return 0;
            }
            if (wParam == 'W' || wParam == 'w') { // z-1
                visualize_z = (visualize_z > 0) ? visualize_z - 1 : DIMENSION_Z - 1;
                return 0;
            }
            if (wParam == 'Q' || wParam == 'q' || wParam == 's' || wParam == 'S') { // z+1
                visualize_z = (visualize_z < (DIMENSION_Z - 1)) ? visualize_z + 1 : 0;
                return 0;
            }
            // Affichage graphique
            /* Assembly
                cas_WM_KEYDOWN_G: ; touche g
                    mov al, [affichageGraphique]
                    cmp al, 0
                    sete al
                    mov [affichageGraphique], al
                    jmp cas_WM_KEYDOWN_Sortie
                */
            // Fonctionnalités directes
            if (wParam == 'K' || wParam == 'k') {
                do_direct = 1 - do_direct;
            }
            // Fonctionnalités d'envoi
            if (wParam == 'L' || wParam == 'l') {
                if (!net_Okay) {
                    do_send = FALSE;
                }
                else {
                    do_send = 1 - do_send;
                }
                return 0;
            }
            // Fonctionnalités de réception
                if (wParam == 'M' || wParam == 'm') {
                    if (!net_Okay) {
                        do_recv = FALSE;
                    }
                    else {
                        do_recv = 1 - do_recv;
                    }
                    return 0;
                }
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
            // Affichage console
            if (wParam == 'H' || wParam == 'h') {
                affichageConsole = (affichageConsole + 1) % 9;
                return 0;
            }
            // Load / save (en mode pas a pas seulement)
            if (wParam == 'I' || wParam == 'i') {
                if (mode_pas_a_pas == 1) {
                    load_cervelet();
                }
                return 0;
            }
            if (wParam == 'O' || wParam == 'o') {
                if (mode_pas_a_pas == 1) {
                    save_cervelet();
                }
                return 0;
            }
            if (wParam == 'P' || wParam == 'p') {
                printf("Key P pressed, exiting...");
                PostQuitMessage(0);
                return 0;
            }
            return 0;
        }
                       /* Assembly
                           cas_WM_SIZE: ; ---------------------------------------------------
                               ; rax et sortie
                                   xor rax, rax
                                   jmp fin_WindowProc
                               */
        case WM_PAINT: {
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
            bitMapInfos.bmiHeader.biBitCount = NOMBRE_OCTET_PAR_POINT * 8;
            bitMapInfos.bmiHeader.biCompression = BI_RGB;
            void* pBitDataAdress = NULL;
            HBITMAP pBitDataHandle = CreateDIBSection(DrawingCtxHandle2, &bitMapInfos, DIB_RGB_COLORS, &pBitDataAdress, NULL, 0);
            if ((pBitDataHandle == 0) || (pBitDataAdress == 0)) {
                goto cWP_no_dibs;
            }
            HGDIOBJ OldBitmapHandle2 = SelectObject(DrawingCtxHandle2, pBitDataHandle);
            GdiFlush();
            unsigned char* pixelData = (unsigned char*)pBitDataAdress;
            for (uint16_t lig = 0; lig < HAUTEUR_FENETRE; lig++) {
                for (uint16_t col = 0; col < LARGEUR_FENETRE; col++) {
                    switch (modeTracage) {
                    case 0:
                        /* Assembly
                            cWP_tracage_mode_A0: ; mode A0 / type de portion ------------------------------
                                ; type
                                mov dl, [r10+OCP_TYPESTEV_1]
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
                                mov dl, [r10+OCP_E_BSYNV_1]
                                mov [r11+VERT], dl
                                jmp cWP_boucle_tracage_fin
                            */
                        break;
                    case 2: {
                        uint16_t charge = 0;
                        switch (crvDatas[visualize_z][lig][col][OCP_TYPESTEV_1]) {
                        case TYPE_PULSEUR: {
                            uint8_t* baseAddress = &crvDatas[visualize_z][lig][col][OCP_P_CHARGI_2];
                            charge = *(uint16_t*)baseAddress;
                            break;
                        }
                        case TYPE_SEGMENT_COMPLET:
                        case TYPE_NEURONE_COMPLET: {
                            uint8_t* baseAddress = &crvDatas[visualize_z][lig][col][OCP_P_CHARGI_2];
                            charge = *(uint16_t*)baseAddress;
                            break;
                        }
                        }
                        if (charge > 255) { // grand => vert
                            uint8_t vert = (uint8_t)(32 + charge / 293);
                            pixelData[NOMBRE_OCTET_PAR_POINT * (lig * (LARGEUR_FENETRE + COMPLEMENT_LIGNE_DWORD) + col) + VERT] = vert;
                        }
                        else { // petit => rouge
                            uint8_t rouge = (uint8_t)charge;
                            pixelData[NOMBRE_OCTET_PAR_POINT * (lig * (LARGEUR_FENETRE + COMPLEMENT_LIGNE_DWORD) + col) + ROUG] = rouge;
                        }
                    }
                    case 3:
                        /* Assembly
                            cWP_tracage_mode_R3: ; mode R3 / activité + retroactivité ------------------------------
                                ; type
                                mov dl, [r10+OCP_TYPESTEV_1]
                                and dl, MASQUE_TYPE_TSx
                                comparer_et_jump_si_egal dl, TYPE_SEGMENT_GERME, cWP_btr3_traiter
                                comparer_et_jump_si_egal dl, TYPE_NEURONE_GERME, cWP_btr3_traiter
                                comparer_et_jump_si_egal dl, TYPE_EXTENSION_GERME, cWP_btr3_traiter
                                jmp cWP_boucle_tracage_fin
                                ; affichage cible retroactive (forcement simple ou liaison) => bleu
                                cWP_btr3_traiter:
                                mov dl, [r10+OCP_ACTIVITE_1]
                                comparer_et_jump_si_egal dl, 0, cWP_boucle_tracage_fin
                                mov [r11+BLEU], byte 255
                                ; jmp cWP_boucle_tracage_fin
                                ; fin des options ----------------------
                            */
                        break;
                    }
                }
                // Le complement de ligne DWORD n'est pas la car il est integre au calcul d'adresse
            }
            // Faire ici le tracage d'un point simple
            pixelData[NOMBRE_OCTET_PAR_POINT * (visualize_y * (LARGEUR_FENETRE + COMPLEMENT_LIGNE_DWORD) + visualize_x) + ROUG] = 255;
            pixelData[NOMBRE_OCTET_PAR_POINT * (visualize_y * (LARGEUR_FENETRE + COMPLEMENT_LIGNE_DWORD) + visualize_x) + VERT] = 255;
            pixelData[NOMBRE_OCTET_PAR_POINT * (visualize_y * (LARGEUR_FENETRE + COMPLEMENT_LIGNE_DWORD) + visualize_x) + BLEU] = 255;
            // copie de ce qui a ete trace vers le contexte de la fenetre
            BitBlt(DrawingCtxHandle, 0, 0, LARGEUR_FENETRE, HAUTEUR_FENETRE, DrawingCtxHandle2, 0, 0, MERGECOPY); // devrait etre SRCCOPY
            // recuperation de l'ancien objet bitmap 2
            SelectObject(DrawingCtxHandle2, OldBitmapHandle2);
            // destruction de l'objet bitmap 2
            DeleteObject(pBitDataHandle);
            // destruction du DIB ?
            DeleteDC(DrawingCtxHandle2);
            // cloture du tracage
        cWP_no_dibs:
            EndPaint(windowHandle, &paintStructure);
            return 0;
        }
        case WM_DESTROY:
            PostQuitMessage(0);
            return 0;
        default:
            return DefWindowProcA(windowHandle, uMsg, wParam, lParam);
        }
    }
#pragma endregion