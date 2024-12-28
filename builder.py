if True: # =========================================================================================== GPL LICENCE & IMPORTS
    # Copyright (C) 2024 Philippe BLATIERE
    # This program is free software: you can redistribute it and/or modify
    # it under the terms of the GNU General Public License as published by
    # the Free Software Foundation, either version 3 of the License, or
    # (at your option) any later version.
    # This program is distributed in the hope that it will be useful,
    # but WITHOUT ANY WARRANTY; without even the implied warranty of
    # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    # GNU General Public License for more details.
    # You should have received a copy of the GNU General Public License
    # along with this program. If not, see <https://www.gnu.org/licenses/>.
    import pygame
    import numpy as np
    import pickle
    import os
    import ast
    import operator
if True: # Constantes, variables, tableaux généraux
    # Paramètres mémoire
    size_x = 1
    size_y = 1
    size_z = 1
    PORTION_SIZE = 32
    DATA_PILOTE = 0
    # Minis
    size_x_min = 512
    size_y_min = 128
    size_z_min = 1
    # Maxis
    size_x_max = 2048
    size_y_max = 512
    size_z_max = 64
    # Paramètres graphiques
    BORDER = 5
    INFO_WIDTH = 350
    CTR_BUTTONS_WIDTH = 50
    CTR_BUTTONS_HEIGHT = 50
    GRAPH_OFFSET_LEFT = BORDER + BORDER
    GRAPH_OFFSET_TOP = BORDER + CTR_BUTTONS_HEIGHT + BORDER + BORDER
    POS_BUTTONS_WIDTH = int ((INFO_WIDTH - 4 * BORDER) / 6)
    POS_BUTTONS_HEIGHT = CTR_BUTTONS_HEIGHT
    WRAP_EDIT_LEFT = 390
    INTERFACE_HEIGHT = 400
    WRAPS_BUTTONS_WIDTH = 130
    window_width = 0
    window_height = 0
    # Listes d'objets
    wraps = [] # Liste des wraps (objets wrapBuilder)
    sorted_w_list = [] # Liste des wraps triée (liste seulement)
    compiled_formulas = {} # Liste des formules validées et compilées
if True: # Paramètres visuels et interface
    # Interface - positions
    font_size = 20
    line_height = 15
    control_buttons = []
    point_buttons = []
    wraps_buttons = []
    delete_buttons = []
    BUTTON_PADDING = 5
    # Interface - couleurs
    BORDER_COLOR = (255, 0, 0) # rouge
    CIRCLE_COLOR = (255, 0, 0) # rouge
    TEXT_COLOR = (255, 255, 255) # blanc
    HIGHLIGHT_COLOR = (100, 100, 255) # mauve
    BUTTON_TEXT_COLOR = (255, 255, 255) # blanc
    # Message de feedback
    feedback_message = ''
    feedback_timer = 0  # Timer pour afficher le message pendant quelques secondes
if True: # Pré-définition et fonctionnels
    # modes
    mode_test = False
    doApplies = False
    editMode = False
    # wraps
    selected_wrap_index = 0
    selected_input = 0
    input_values = []
    replace_value = True
    EDITABLE_INPUTS_OFFSET = 1
    DATAS_INPUTS_OFFSET = 2
    # point selectionné
    selected_x = 0
    selected_y = 0
    selected_z = 0
# ==================================================================================================== DICTIONNAIRES ET LISTES
# si texte ="Code"=>codePortion / ="P."=>numeroPortion / ...
wraps_datas = { # Dictionnaire des données pour les wraps
    'CONFIG': {
        'type': 'CONFIG',
        'nom': '',
        'couleur': (0,127,0),
        'textes': ("Code", "Dimension x", "Dimension y", "Dimension z", "N/D", "Pack n°", "N/D", "N/D", "N/D"),
        'initial': [  255,             1,             1,             1,     0,         0,     0,     0,     0],
        'tailles': (    1,             2,             2,             2,     1,         4,     4,     8,     8)
    },
    'SOURCE': {
        'type': 'PORTION',
        'nom': '',
        'couleur': (127,127,0),
        'textes': ("Code", "N/D", "Largeur de segment *", "N/D", "Nombre de segments *", "N/D", "Adresse *", "N/D"),
        'initial': [  162,     0,                      1,     0,                      0,     0,           0,     0],
        'tailles': (    1,     3,                      1,     3,                      2,     6,           8,     8)
    },
    'LECTEUR': {
        'type': 'PORTION',
        'nom': '',
        'couleur': (0,127,127),
        'textes': ("Code", "N/D", "Persistance actuelle", "Persistance de base", "Index de lecture", "Numéro de bit", "Valeur du bloc de boutons", "P. Source", "N/D", "P. Destination", "N/D", "N/D"),
        'initial': [  160,     0,                      0,                     1,                  0,               0,                         127,        'p1',     0,       '*prt_z+1',     0,     0],
        'tailles': (    1,     1,                      1,                     1,                  2,               1,                           1,           6,     2,                6,     2,     8)
    },
    'PULSEUR': {
        'type': 'PORTION',
        'nom': '',
        'couleur': (127,127,127),
        'textes': ("Code", "N/D", "Charge générée", "Puissance du seuil", "Valeur du bloc de boutons", "N/D", "P. Destination", "N/D", "N/D"),
        'initial': [  128,     0,                0,                   11,                         127,     0,       '*prt_z+1',     0,     0],
        'tailles': (    1,     7,                2,                    1,                           1,     4,                                6,     2,     8)
    },
    'SEGMENT': {
        'type': 'PORTION',
        'nom': '',
        'couleur': (0,127,0),
        'textes': ("Code", "Tempo évolution", "Potentiel synaptique disponible", "Niveau TAC", "N/D", "Charge 4", "Charge reçue", "Charge 1", "Charge 2", "Charge 3", "P. Suivant", "Potentiel de segments restants", "N/D", "N/D"),
        'initial': [   48,                 0,                                 0,            0,     0,     -32768,              0,     -32768,     -32768,     -32768,   '*prt_z+1',                                3,     0,     0],
        'tailles': (    1,                 1,                                 2,            1,     1,          2,              2,          2,          2,          2,                                   6,                                1,     1,     8)
    },
    'NEURONE': {
        'type': 'PORTION',
        'nom': '',
        'couleur': (0,0,255),
        'textes': ("Code", "N/D", "Niveau TAC", "N/D", "Decompte réfractaire", "Base réfractaire", "Charge reçue", "Puissance de seuil", "N/D", "Potention rayonnant", "Potentiel planaire", "Potentiel apical", "Potentiel panier", "N/D", "Potentiel axonal", "Orientation", "N/D"),
        'initial': [  112,     0,            0,     0,                      0,                 15,              0,                    8,     0,                     0,                    0,                  0,                  0,     0,                  0,             0,     0],
        'tailles': (    1,     3,            1,     1,                      1,                  1,              2,                    1,     1,                     1,                    1,                  1,                  1,     6,                  1,             1,     8)
    },
    'EXTENSION': {
        'type': 'PORTION',
        'nom': '',
        'couleur': (127,0,0),
        'textes': ("Code", "Temporisation évolution", "N/D", "Compteur TAC", "N/D", "Valeur du bloc de boutons", "Masse du bloc de boutons", "P. Destination", "N/D", "P. Antécédante", "Potentiel axonal restant", "N/D", "N/D"),
        'initial': [   80,                         3,     0,              2,     0,                         115,                          0,       '*prt_z+1',     0,       '*prt_z-1',                          5,     0,     0],
        'tailles': (    1,                         1,     2,              1,     1,                           1,                          1,                                6,     2,                                6,                          1,     1,     8)
    },
    'CHAINE': {
        'type': 'CHAINE',
        'nom': '',
        'couleur': (64,64,64),
        'textes': ("Paramètre 1", "Paramètre 2", "Paramètre 3", "position x", "position y", "position z", "direction", "numéro de wrap 1", "numéro de wrap 2", "numéro de wrap 3", "numéro de wrap 4", "numéro de wrap 5", "numéro de wrap 6", "numéro de wrap 7", "numéro de wrap 8", "numéro de wrap 9", "numéro de wrap 10"),
        'initial': [           0,             0,             0,            0,            0,            0,        '_z',                  0,                  0,                  0,                  0,                  0,                  0,                  0,                  0,                  0,                   0]
    },
    'RESEAU': {
        'type': 'RESEAU',
        'nom': '',
        'couleur': (128,128,128),
        'textes': ("Paramètre 1", "Paramètre 2", "Paramètre 3", "Wrap à démultiplier", "départ x", "départ y", "départ z", "nombre sur x", "nombre sur y", "nombre sur z", "pas sur x", "pas sur y", "pas sur z"),
        'initial': [      'test',             0,             0,                     0,        100,        100,          0,             30,             30,              1,           1,           1,           1]
    }
    }
formulas = { # Formules pré-définies
    '*prt_z-1': 'x+y*size_x+(z-1)*size_x*size_y',
    '*prt_z+1': 'x+y*size_x+(z+1)*size_x*size_y',
    '*prt_123': 'p1+p2*size_x+p3*size_x*size_y'
    }
ALLOWED_OPERATORS = { # Opérateurs autorisés
    ast.Add: operator.add,
    ast.Sub: operator.sub,
    ast.Mult: operator.mul,
    ast.Div: operator.truediv,
    ast.Pow: operator.pow,
    ast.USub: operator.neg,
    }
ALLOWED_VARIABLES = { # Variables autorisées
    "x",
    "y",
    "z",
    "p1",
    "p2",
    "p3",
    "n",
    "size_x",
    "size_y",
    "size_z"
    }
# ==================================================================================================== FONCTIONS
# ------------------- Tailles fenetre et mémoire
def calc_windows_width():
    ww = GRAPH_OFFSET_LEFT + size_x + BORDER + INFO_WIDTH
    return ww
def calc_windows_height():
    wh = GRAPH_OFFSET_TOP + size_y + BORDER + BORDER + INTERFACE_HEIGHT
    return wh
def resize_memories(): # redimensionnement des zones mémoires
    global savedMem, drawnMem
    global size_x, size_y, size_z
    global selected_x, selected_y, selected_z
    # récupération des dimensions voulues (pour la fonction et le reste du programme)
    size_x = wraps[0].dataset[1]
    size_y = wraps[0].dataset[2]
    size_z = wraps[0].dataset[3]
    # dimensions actuelles
    dimensions = savedMem.shape
    old_size_z = dimensions[0]
    old_size_y = dimensions[1]
    old_size_x = dimensions[2]
    # dimensions intermédiaires
    temp_z = min(old_size_z, size_z)
    temp_y = min(old_size_y, size_y)
    temp_x = min(old_size_x, size_x)
    # Réduction des dimensions à réduire
    temp_tab = np.resize(savedMem, (temp_z, temp_y, temp_x, PORTION_SIZE))
    # Agrandissement des dimensions à agrandir
    savedMem = np.pad(temp_tab, pad_width=((0, size_z - temp_z), (0, size_y - temp_y), (0, size_x - temp_x), (0, 0)), mode='constant', constant_values=0)
    drawnMem = np.zeros((size_z, size_y, size_x, PORTION_SIZE), dtype=np.uint8)
    # Replacement du point selectionné
    dimensions = drawnMem.shape
    selected_x = min(dimensions[2] - 1, selected_x)
    selected_y = min(dimensions[1] - 1, selected_y)
    selected_z = min(dimensions[0] - 1, selected_z)
def resize_window(): # redimensionnement de la fenetre
    global screen
    global window_width, window_height
    global size_x, size_y, size_z
    # redimensionnement
    window_width = calc_windows_width()
    window_height = calc_windows_height()
    screen = pygame.display.set_mode((window_width, window_height), pygame.RESIZABLE)
# ------------------- Tracages
def refresh_mem_to_draw(): # Construction de la memoire à tracer
    global selected_values
    global compiled_formulas
    # remplissage de la mémoire à tracer
    drawnMem[:] = savedMem
    # construction de toutes les formules validées
    for wrap in wraps:
        for dat in wrap.dataset:
            if not isinstance(dat, (int)):
                if not dat in compiled_formulas:
                    # déterminer la bonne formule
                    if dat == '':
                        formula = '0+0'
                    elif dat[0] == '*':
                        if dat in formulas:
                            formula = formulas[dat]
                        else:
                            formula = '0+0'
                    elif dat[0] == '_':
                        formula = '0+0'
                    else:
                        formula = dat
                    # validation
                    is_valid, message = validate_formula(formula)
                    if not is_valid:
                        print("Erreur : ", message)
                        formula = '0+0'
                    # affectation
                    compiled_formulas[dat] = compile_formula(formula)
                    print("Formule ajoutée : ", dat, " > ", formula)
    # Appliquer les wraps à la zone de mémoire
    for wrap in wraps:
        wrap.apply_on_mem(0, 0, 0, 0, 0, 0, 0)
    selected_values = read_from_mem_to_draw(selected_x, selected_y, selected_z)
def draw_memory_layer(screen, layer_index): # Traçage d'une couche de mémoire
    # tracage
    for y in range(size_y):
        for x in range(size_x):
            # Utiliser le premier octet de la portion pour déterminer la couleur
            first_byte = drawnMem[layer_index, y, x, 0]
            color = byte_to_color(first_byte)
            screen.set_at((GRAPH_OFFSET_LEFT + x, GRAPH_OFFSET_TOP + y), color)
def byte_to_color(byte): # Convertion d'un octet en couleur RGB
    return (byte, byte, byte)
def read_from_mem_to_draw(x, y, z):
    dimensions = drawnMem.shape
    if z < dimensions[0] and y < dimensions[1] and x < dimensions[2]:
        return drawnMem[z, y, x]
    else:
        return [0] * PORTION_SIZE
        print(f"Tentative de lire au delà des limites : {x}/{dimensions[2]} ou {y}/{dimensions[1]} ou {z}/{dimensions[0]}")
def put_on_mem_to_draw(x, y, z, s, l, b):
    dimensions = drawnMem.shape
    if z < dimensions[0] and y < dimensions[1] and x < dimensions[2]:
        drawnMem[z, y, x, s:s+l] = b
    else:
        print(f"Tentative d'écrire au delà des limites : {x}/{dimensions[2]} ou {y}/{dimensions[1]} ou {z}/{dimensions[0]}")
# ------------------- Chargements sauvegardes
def save_memory_to_bin(filename):
    try:
        with open(filename, 'wb') as file:
            file.write(drawnMem.tobytes())
        print(f"Mémoire sauvegardée avec succès dans {filename}")
    except Exception as e:
        print(f"Erreur lors de la sauvegarde: {e}")
def load_memory_from_bin(filename): # VERIFIER QUE CA SAUVEGARDE LA BONNE MEMOIRE
    global savedMem
    # A_FAIRE : faire un reshape de la zone memoire avant !!!
    try:
        with open(filename, 'rb') as f:
            data = np.frombuffer(f.read(), dtype=np.uint8)
            savedMem = data.reshape(size_z, size_y, size_x, PORTION_SIZE)
        print(f"Mémoire chargée avec succès depuis {filename}")
    except Exception as e:
        print(f"Erreur lors du chargement: {e}")
def save_wraps_to_file(filename): # Sauvegarde des wraps
    if os.path.exists(filename): # si le fichier existe déjà
        base, ext = os.path.splitext(filename) # Séparer le nom de base et l'extension
        new_name = f"{base}_backup{ext}" # Ajouter "_backup" avant l'extension
        counter = 1 # S'assurer que le nouveau nom n'existe pas déjà
        while os.path.exists(new_name):
            new_name = f"{base}_backup_{counter}{ext}"
            counter += 1
        os.rename(filename, new_name) # Renommer l'ancien fichier
    try:
        with open(filename, 'wb') as f:
            pickle.dump(wraps, f)
        print(f"Wraps sauvegardés avec succès dans {filename}")
    except Exception as e:
        print(f"Erreur lors de la sauvegarde des wraps : {e}")
def load_wraps_from_file(filename): # Chargement des wraps
    global wraps
    try:
        with open(filename, 'rb') as f:
            wraps = pickle.load(f)
            rebuild_sorted_list()
        print(f"Wraps chargés avec succès depuis {filename}")
    except Exception as e:
        print(f"Erreur lors du chargement des wraps : {e}")
# ------------------- Edition
def make_button(butts, xx, yy, ll, hh, nom, texte, couleur, txtcol):
    text_width, text_height = font.size(texte)
    # horizontalement
    if ll == 1 or ll == -1:
        larg = text_width + 2 * BUTTON_PADDING
    else:
        larg = abs(ll)
    if ll < 0:
        xx -= larg
    # verticalement
    if hh == 1 or hh == -1:
        haut = text_height + 2 * BUTTON_PADDING
    else:
        haut = abs(hh)
    if hh < 0:
        yy -= haut
    # traçage et enregistrement
    pygame.draw.rect(screen, couleur, (xx, yy, larg, haut))
    screen.blit(font.render(texte, True, txtcol), (xx + larg / 2 - text_width / 2, yy + haut / 2 - text_height / 2))
    if not nom == "":
        butts.append([xx, yy, xx + larg - 1, yy + haut -1, nom, texte, couleur])
    return (larg, haut)
def find_wrap(id): # Rechercher d'un wrap
    try:
        p = wraps[id]
    except:
        i = -1
    else:
        i = id
    finally:
        return i
def select_wrap(selected_wrap_index): # Selection d'un wrap
    if selected_wrap_index is not None:
        global input_values
        input_values = []
        selected_wrap = wraps[selected_wrap_index]
        wd_key = selected_wrap.wd_key
        input_values.append(wd_key)
        nom = selected_wrap.nom
        input_values.append(nom)
        dataset = selected_wrap.dataset
        for d in dataset:
            input_values.append(str(d))
def rebuild_sorted_list():
    global sorted_w_list
    indexed_w_list = [(index, wrap.nom) for index, wrap in enumerate(wraps)]
    sorted_w_list = sorted(indexed_w_list, key=lambda x: x[1])
def save_wrap(selected_wrap_index):
    if selected_wrap_index is not None:
        global wraps
        wrap = wraps[selected_wrap_index]
        wd_key = input_values[0]
        nom = input_values[1]
        wrap.wd_key = wd_key
        wrap.nom = nom
        for i, d in enumerate(wrap.dataset):
            val = input_values[i+DATAS_INPUTS_OFFSET]
            try:
                wrap.dataset[i] = int(val)
            except:
                wrap.dataset[i] = val
        print(f"Packet {selected_wrap_index} modifié : {wrap}")
# ------------------- Formules
def validate_formula(formula):
    try:
        tree = ast.parse(formula, mode='eval')
        validator = FormulaValidator()
        validator.visit(tree)
        if not validator.is_valid:
            return False, validator.error_message
        return True, "Formule valide."
    except Exception as e:
        return False, f"Erreur de syntaxe : {str(e)}"
def compile_formula(formula):
    tree = ast.parse(formula, mode='eval')
    # Transformer le code AST en une fonction Python sécurisée
    code = compile(tree, filename="<ast>", mode="eval")
    def eval_formula(**variables):
        return eval(code, {"__builtins__": {}}, variables)
    return eval_formula
def give_value(var,x,y,z,p1,p2,p3,n):
    if isinstance(var, (int)):
        return var
    elif var == "":
        return 0
    elif var[0] == "*":
        if var in formulas:
            return compiled_formulas[var](x=x,y=y,z=z,p1=p1,p2=p2,p3=p3,n=n,size_x=size_x,size_y=size_y,size_z=size_z)
        else:
            return 0
    else:
        return compiled_formulas[var](x=x,y=y,z=z,p1=p1,p2=p2,p3=p3,n=n,size_x=size_x,size_y=size_y,size_z=size_z)
# ==================================================================================================== CLASSES ET OBJETS
class wrapBuilder: # Classe pour wraps
    def __init__(self, wd_key, nom, dataset):
        self.wd_key = wd_key
        self.nom = nom
        self.dataset = dataset
    def is_deletable(self):
        # A_FAIRE : listage des paquets pour savoir si le wrap est référencé quelque part
        return True
    def apply_on_mem(self, i, j, k, q1, q2, q3, m):
        wd_key = self.wd_key
        dataset = self.dataset
        typ = wraps_datas[wd_key]['type']
        if typ == 'CONFIG':
            if size_x != wraps[0].dataset[1] or size_y != wraps[0].dataset[2] or size_z != wraps[0].dataset[3]:
                resize_memories()
                resize_window()
            code = 255
            size = 1
            bytes = np.frombuffer(code.to_bytes(1, byteorder='little', signed=False), dtype=np.uint8)
            put_on_mem_to_draw(0, 0, 0, 0, size, bytes)
            size = 2
            bytes = np.frombuffer(wraps[0].dataset[1].to_bytes(size, byteorder='little', signed=False), dtype=np.uint8)
            put_on_mem_to_draw(0, 0, 0, 1, size, bytes)
            bytes = np.frombuffer(wraps[0].dataset[2].to_bytes(size, byteorder='little', signed=False), dtype=np.uint8)
            put_on_mem_to_draw(0, 0, 0, 3, size, bytes)
            bytes = np.frombuffer(wraps[0].dataset[3].to_bytes(size, byteorder='little', signed=False), dtype=np.uint8)
            put_on_mem_to_draw(0, 0, 0, 5, size, bytes)
        if typ == 'PORTION':
            # PORTION : d/f,d/f,d/f,d/f,...
            # => on ecrit les données/fonctions(p1,p2,p3) à l'emplacement i,j,k
            if (i+j+k != 0): # si le wrap est appelé par un wrap/reseau
                start_byte = 0
                sizes = wraps_datas[wd_key]['tailles']
                for idx, dat in enumerate(dataset):
                    the_size = sizes[idx]
                    the_value = give_value(dat,i,j,k,q1,q2,q3,m)
                    bytes = np.frombuffer(the_value.to_bytes(the_size, byteorder='little', signed=False), dtype=np.uint8)
                    put_on_mem_to_draw(i, j, k, start_byte, the_size, bytes)
                    start_byte += the_size
            return m+1
        if typ == 'CHAINE': # .data contient une liste de portions PRT_
            # CHAINE : orient,nump,nump,nump,nump,...
            #   => on fait les appels aux wraps avec les decalages
            n = m
            # calcul des nouvelles valeurs x, y, z
            # A_FAIRE : interprêter datatset[..]
            xx = i + give_value(dataset[3],i,j,k,q1,q2,q3,m)
            yy = j + give_value(dataset[4],i,j,k,q1,q2,q3,m)
            zz = k + give_value(dataset[5],i,j,k,q1,q2,q3,m)
            if (xx+yy+zz != 0): # si c'est un reseau autonome ou si il est appelé par un reseau
                # récupération des formules / nouvelles valeurs p1, p2, p3, axe d'empilage
                p1 = give_value(dataset[0],i, j, k, q1, q2, q3, m)
                p2 = give_value(dataset[1],i, j, k, q1, q2, q3, m)
                p3 = give_value(dataset[2],i, j, k, q1, q2, q3, m)
                axe = dataset[6]
                decalage = 0
                for numero in dataset[7:]: # traitement de chaque élément de la chaine
                    id = find_wrap(numero)
                    if id != -1 and id != 0:
                        rec_wrap = wraps[id]
                        if axe == "_x":
                            rec_wrap.apply_on_mem(xx + decalage, yy, zz, p1, p2, p3, n)
                        elif axe == "_y":
                            rec_wrap.apply_on_mem(xx, yy + decalage, zz, p1, p2, p3, n)
                        elif axe == "_z":
                            rec_wrap.apply_on_mem(xx, yy, zz + decalage, p1, p2, p3, n)
                    decalage += 1
                n += 1
            return n
        if typ == 'RESEAU': # .data contient le wrap et les valeurs de position et multiplicatives
            # RESEAU : p1,p2,p3,numeroWrap,x,y,z,dx,dy,dz,nx,ny,nz
            # => on fait les appels au wrap avec les coordonnées et paramètres
            n = m
            id = find_wrap(dataset[3])
            if id != -1:
                xx = i + give_value(dataset[4],i,j,k,q1,q2,q3,m)
                yy = j + give_value(dataset[5],i,j,k,q1,q2,q3,m)
                zz = k + give_value(dataset[6],i,j,k,q1,q2,q3,m)
                if (xx+yy+zz != 0): # si c'est un reseau autonome ou si il est appelé par un reseau
                    p1 = give_value(dataset[0],i,j,k,q1,q2,q3,m)
                    p2 = give_value(dataset[1],i,j,k,q1,q2,q3,m)
                    p3 = give_value(dataset[2],i,j,k,q1,q2,q3,m)
                    nx = give_value(dataset[7],i,j,k,q1,q2,q3,m)
                    ny = give_value(dataset[8],i,j,k,q1,q2,q3,m)
                    nz = give_value(dataset[9],i,j,k,q1,q2,q3,m)
                    dx = give_value(dataset[10],i,j,k,q1,q2,q3,m)
                    dy = give_value(dataset[11],i,j,k,q1,q2,q3,m)
                    dz = give_value(dataset[12],i,j,k,q1,q2,q3,m)
                    rec_wrap = wraps[id]
                    for z in range(zz, zz+nz*dz, dz):
                        for y in range(yy, yy+ny*dy, dy):
                            for x in range(xx, xx+nx*dx, dx):
                                n = rec_wrap.apply_on_mem(x, y, z, p1, p2, p3, n)
            return n
class FormulaValidator(ast.NodeVisitor):
    def __init__(self):
        self.is_valid = True
        self.error_message = ""
    def visit_Name(self, node):
        if node.id not in ALLOWED_VARIABLES:
            self.is_valid = False
            self.error_message = f"Variable '{node.id}' non autorisée."
        self.generic_visit(node)
    def visit_BinOp(self, node):
        if type(node.op) not in ALLOWED_OPERATORS:
            self.is_valid = False
            self.error_message = f"Opérateur '{type(node.op).__name__}' non autorisé."
        self.generic_visit(node)
    def visit_UnaryOp(self, node):
        if type(node.op) not in ALLOWED_OPERATORS:
            self.is_valid = False
            self.error_message = f"Opérateur '{type(node.op).__name__}' non autorisé."
        self.generic_visit(node)
    def visit_Call(self, node):
        self.is_valid = False
        self.error_message = "Les appels de fonction ne sont pas autorisés."
    def visit(self, node):
        if not self.is_valid:
            return  # Arrête l'exploration si déjà invalide
        super().visit(node)
# ==================================================================================================== CODE
# Initialisation des dimensions de la fenêtre
window_width = calc_windows_width()
window_height = calc_windows_height()
# Initialisation des zones mémoire
savedMem = np.zeros((size_z, size_y, size_x, PORTION_SIZE), dtype=np.uint8)
drawnMem = np.zeros((size_z, size_y, size_x, PORTION_SIZE), dtype=np.uint8)
# Initialisation du point selectionné
selected_values = drawnMem[selected_z, selected_y, selected_x]
# Initialisation des wraps
num = len(wraps)
wrap = wrapBuilder('CONFIG'," Config",[255, 512, 256, 32, 0, 0, 0, 0]) # Configuration
wraps.append(wrap)
rebuild_sorted_list()
select_wrap(selected_wrap_index)
if mode_test: # Pré-construction wraps pour tests
    # Wraps par défaut
    wrap = wrapBuilder(wd_key='NEURONE', dataset=[96, 1, 2, 3, 4, 'p1', 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]) # Wrap 1
    wraps.append(wrap)
    wrap = wrapBuilder(wd_key='EXTENSION', dataset=[64, 0, 'f_x', 'f_y', 'f_z', 0, 0, 'f_z+1_6', 0, 'f_z-1_6', 0, 0]) # Wrap 2
    wraps.append(wrap)
    wrap = wrapBuilder(wd_key='CHAINE', dataset=['p1', 0, 0, 0, 0, 0, "z", 1, 2, 1, 2]) # Wrap 3
    wraps.append(wrap)
    wrap = wrapBuilder(wd_key='RESEAU', dataset=['p1', 0, 0, 3, 0, 0, 0, 20, 20, 1, 1, 1, 1]) # Wrap 4
    wraps.append(wrap)
    wrap = wrapBuilder(wd_key='RESEAU', dataset=[10, 0, 0, 4, 10, 10, 0, 4, 2, 1, 40, 30, 1]) # Wrap 5
    wraps.append(wrap)
# ------------------- Initialisation de Pygame et fenetre
pygame.init()
screen = pygame.display.set_mode((window_width, window_height))
pygame.display.set_caption("Cervelet builder")
# ==================================================================================================== BOUCLE PRINCIPALE
# Boucle principale
doApplies = True
running = True
while running:
    for event in pygame.event.get(): # =============================================================== Gestion des événements
        if event.type == pygame.QUIT: # -------------------------------------- Evenement fermeture
            running = False
        if event.type == pygame.VIDEORESIZE: # -------------------------------- Evenement redimensionnement
            window_width = calc_windows_width()
            window_height = event.h
            screen = pygame.display.set_mode((window_width, window_height), pygame.RESIZABLE)
        if event.type == pygame.KEYDOWN: # --------------------------------- Evénement clavier
            if editMode == False:
                # ----- Sortie
                if event.key == pygame.K_p:
                    running = False
                # ----- Navigation entre les wraps + validation
                elif event.key == pygame.K_DOWN:
                    if selected_wrap_index is not None:
                        sorted_wrap_index = next(i for i, (first, _) in enumerate(sorted_w_list) if first == selected_wrap_index)
                        sorted_wrap_index = (sorted_wrap_index + 1) % len(sorted_w_list)
                        selected_wrap_index = sorted_w_list[sorted_wrap_index][0]
                        select_wrap(selected_wrap_index)
                elif event.key == pygame.K_UP:
                    if selected_wrap_index is not None:
                        sorted_wrap_index = next(i for i, (first, _) in enumerate(sorted_w_list) if first == selected_wrap_index)
                        sorted_wrap_index = (sorted_wrap_index - 1) % len(sorted_w_list)
                        selected_wrap_index = sorted_w_list[sorted_wrap_index][0]
                        select_wrap(selected_wrap_index)
                elif event.key == pygame.K_KP_ENTER or event.key == pygame.K_RETURN:
                    # Affiche le détail
                    select_wrap(selected_wrap_index)
                    editMode = True
                    selected_input = DATAS_INPUTS_OFFSET
                    replace_value = True
            else: # editMode = True
                # ----- Modification des champs et validation
                if event.key == pygame.K_ESCAPE:
                    editMode = False
                elif event.key == pygame.K_UP:
                    selected_input = (selected_input - 1) % len(input_values)
                    replace_value = True
                elif event.key == pygame.K_DOWN or event.key == pygame.K_TAB:
                    selected_input = (selected_input + 1) % len(input_values)
                    replace_value = True
                elif event.key == pygame.K_RIGHT:
                    replace_value = False
                elif event.key == pygame.K_BACKSPACE:
                    if replace_value == True:
                        input_values[selected_input] = ''
                    else:
                        input_values[selected_input] = input_values[selected_input][:-1]
                elif event.key == pygame.K_KP_ENTER or event.key == pygame.K_RETURN:
                    save_wrap(selected_wrap_index)
                    rebuild_sorted_list()
                    editMode = False
                else:
                    char = event.unicode
                    if char.isdigit() or char.islower() or char in "_-+/*%()":
                        if replace_value:
                            input_values[selected_input] = char
                            replace_value = False
                        else:
                            input_values[selected_input] += char
        if event.type == pygame.MOUSEBUTTONDOWN: # ------------------------- Evénement bouton souris
            if event.button == 1: # Si clic gauche
                mouse_x, mouse_y = event.pos
                if not editMode and (GRAPH_OFFSET_LEFT <= mouse_x < GRAPH_OFFSET_LEFT + size_x) and (GRAPH_OFFSET_TOP <= mouse_y < GRAPH_OFFSET_TOP + size_y):
                    # Si le clic est dans la zone graphique
                    selected_x = mouse_x - GRAPH_OFFSET_LEFT
                    selected_y = mouse_y - GRAPH_OFFSET_TOP
                    selected_values = drawnMem[selected_z, selected_y, selected_x]
                if not editMode and delete_buttons:
                    # Si le clic est dans la zone du bouton (et qu'il y a des boutons)
                    for b in delete_buttons:
                        numero = int(b[4])
                        if b[0] <= mouse_x <= b[2] and b[1] <= mouse_y <= b[3] -1 :
                            if wraps[numero].is_deletable():
                                print(f"Wrap {str(numero)} à supprimer (codage à faire)")
                                rebuild_sorted_list()
                if not editMode and wraps_buttons:
                    # Si le clic est dans la zone du bouton (et qu'il y a des boutons)
                    for b in wraps_buttons:
                        typ = b[4]
                        wd_key = b[5]
                        if b[0] <= mouse_x <= b[2] and b[1] <= mouse_y <= b[3] -1 :
                            dataset = wraps_datas[wd_key]['initial'][:]
                            num = len(wraps)
                            new_wrap = wrapBuilder(wd_key,str(num),dataset)
                            wraps.append(new_wrap)
                            selected_wrap_index = len(wraps) - 1
                            print(f"Wrap {selected_wrap_index} ({wd_key}) créé :", new_wrap)
                            rebuild_sorted_list()
                if control_buttons:
                    for b in control_buttons:
                        if b[0] <= mouse_x <= b[2] and b[1] <= mouse_y <= b[3] :
                            nom = b[4]
                            # changement ecritures
                            if nom == "size-":
                                font_size = max(font_size - 5, 15)
                                line_height = max(line_height - 5, 10)
                            if nom == "size+":
                                font_size = min(font_size + 5, 25)
                                line_height = min(line_height + 5, 20)
                            # contrôles
                            if nom == "startOrStop":
                                doApplies = not doApplies
                            if nom == "wrp_load":
                                load_wraps_from_file('structure.wrp')
                            if nom == "wrp_save":
                                save_wraps_to_file('structure.wrp')
                            if nom == "crv_load":
                                # load_memory_from_bin('structure.crv')
                                print (f"Loading raw structure not implemented")
                            if nom == "crv_save":
                                save_memory_to_bin('structure.crv')
                if point_buttons:
                    for b in point_buttons:
                        if b[0] <= mouse_x <= b[2] and b[1] <= mouse_y <= b[3] :
                            nom = b[4]
                            # changement ecritures
                            if nom == "sel_x-":
                                selected_x = max(selected_x - 1, 0)
                                selected_values = drawnMem[selected_z, selected_y, selected_x]
                            if nom == "sel_x+":
                                selected_x = min(selected_x + 1, size_x - 1)
                                selected_values = drawnMem[selected_z, selected_y, selected_x]
                            if nom == "sel_y-":
                                selected_y = max(selected_y - 1, 0)
                                selected_values = drawnMem[selected_z, selected_y, selected_x]
                            if nom == "sel_y+":
                                selected_y = min(selected_y + 1, size_y - 1)
                                selected_values = drawnMem[selected_z, selected_y, selected_x]
                            if nom == "sel_z-":
                                selected_z = (selected_z - 1) % size_z
                                selected_values = drawnMem[selected_z, selected_y, selected_x]
                            if nom == "sel_z+":
                                selected_z = (selected_z + 1) % size_z
                                selected_values = drawnMem[selected_z, selected_y, selected_x]
    # ================================================================================================ Traçage et ecritures
    control_buttons = []
    point_buttons = []
    delete_buttons = []
    wraps_buttons = []
    if True: # Dessin de la couche actuelle + cadre + cercle autour du point selectionné
        screen.fill((0, 0, 0))
        pygame.draw.rect(screen, BORDER_COLOR, (GRAPH_OFFSET_LEFT - BORDER, GRAPH_OFFSET_TOP - BORDER, size_x + 2 * BORDER, size_y + 2 * BORDER))
        if doApplies:
            refresh_mem_to_draw()
            draw_memory_layer(screen, selected_z)
            pygame.draw.circle(screen, CIRCLE_COLOR, (GRAPH_OFFSET_LEFT + selected_x, GRAPH_OFFSET_TOP + selected_y), 5, 2)
        # Variables communes
        font = pygame.font.SysFont(None, font_size)
    if True: # Affichage des boutons de contrôle
        pos_y0 = BORDER
        # boutons de dimension de texte (alignement à droite)
        pos_y = pos_y0
        pos_x = GRAPH_OFFSET_LEFT + BORDER + size_x - BORDER
        (larg, haut) = make_button(control_buttons, pos_x, pos_y, -50, 1, 'size+', "Txt+", (127,127,127), BUTTON_TEXT_COLOR)
        pos_y = pos_y + CTR_BUTTONS_HEIGHT
        (larg, haut) = make_button(control_buttons, pos_x, pos_y, -50, -1, 'size-', "Txt-", (63,63,63), BUTTON_TEXT_COLOR)
        # boutons principal (à gauche)
        pos_y = pos_y0
        pos_x = GRAPH_OFFSET_LEFT
        texte = "Running" if doApplies else "Stop"
        couleur = (0,127,0) if doApplies else (191,0,0)
        (larg, haut) = make_button(control_buttons, pos_x, pos_y, 2 * CTR_BUTTONS_WIDTH, CTR_BUTTONS_HEIGHT, 'startOrStop', texte, couleur, BUTTON_TEXT_COLOR)
        pos_x += larg + BORDER
        pos_y1 = pos_y0 + haut
        # boutons fichier wrp
        pos_y = pos_y0
        (larg, haut) = make_button(control_buttons, pos_x, pos_y, 2 * CTR_BUTTONS_WIDTH, 1, '', "wraps file", (0,0,0), BUTTON_TEXT_COLOR)
        pos_y += haut
        pos_y = pos_y1
        (larg, haut) = make_button(control_buttons, pos_x, pos_y, CTR_BUTTONS_WIDTH, -1, 'wrp_load', "load", (63,63,63), BUTTON_TEXT_COLOR)
        pos_x += larg
        (larg, haut) = make_button(control_buttons, pos_x, pos_y, CTR_BUTTONS_WIDTH, -1, 'wrp_save', "save", (127,127,127), BUTTON_TEXT_COLOR)
        pos_x += larg + BORDER
        # boutons fichier crv
        pos_y = pos_y0
        (larg, haut) = make_button(control_buttons, pos_x, pos_y, 2 * POS_BUTTONS_WIDTH, 1, '', "raw file", (0,0,0), BUTTON_TEXT_COLOR)
        pos_y += haut
        pos_y = pos_y1
        (larg, haut) = make_button(control_buttons, pos_x, pos_y, POS_BUTTONS_WIDTH, -1, 'crv_load', "----", (63,63,63), BUTTON_TEXT_COLOR)
        pos_x += larg
        (larg, haut) = make_button(control_buttons, pos_x, pos_y, POS_BUTTONS_WIDTH, -1, 'crv_save', "save", (127,127,127), BUTTON_TEXT_COLOR)
        pos_x += larg + BORDER
    if True: # Affichage de la liste des wraps + boutons de suppression
        pos_x = 10
        pos_y = GRAPH_OFFSET_TOP + size_y + BORDER + BORDER
        screen.blit(font.render("Wraps existants (use arrows & enter)", True, TEXT_COLOR), (pos_x, pos_y))
        pos_y += line_height + BUTTON_PADDING
        # Affichage de la liste des wraps
        for (num, nom) in sorted_w_list:
            wrap = wraps[num]
            if num == 0 or not wrap.is_deletable(): # non-cliquable
                click_id = ""
                puce = "o"
            else:
                click_id = str(num)
                puce = "x"
            strnum = str(num)
            wd_key = wrap.wd_key
            nom = wrap.nom
            dataset = wrap.dataset
            typ = wraps_datas[wd_key]['type']
            # définition du texte à afficher
            entete = "- " + nom + " ( " + str(wd_key).lower() + " / " + strnum + " ) > "
            if (typ == 'CONFIG'):
                wrap_info = f"{entete} dim:{dataset[1]},{dataset[2]},{dataset[3]} - packid:{dataset[5]} ..."
            elif (typ == 'PORTION'):
                wrap_info = f"{entete} {dataset}"
            elif (typ == 'CHAINE'):
                wrap_info = f"{entete} pos:{dataset[3]},{dataset[4]},{dataset[5]} - axe:{dataset[6]} - prt:{dataset[7]} ..."
            elif (typ == 'RESEAU'):
                wrap_info = f"{entete} prt:{dataset[3]} - pos:{dataset[4]},{dataset[5]},{dataset[6]} - nbr:{dataset[7]},{dataset[8]},{dataset[9]} - pas:{dataset[10]},{dataset[11]},{dataset[12]}"
            else:
                wrap_info = f"- Type non identifié"
            # affichage coloré ou non
            if num == selected_wrap_index:
                # Ecrire en vert le wrap sélectionné
                (larg, haut) = make_button(delete_buttons, pos_x, pos_y, 1, line_height - 3, click_id, puce, (127,0,0), BUTTON_TEXT_COLOR)
                screen.blit(font.render(wrap_info, True, (127, 255, 127)), (pos_x + larg, pos_y))
            else:
                # Ecrire en blanc les autres wraps
                (larg, haut) = make_button(delete_buttons, pos_x, pos_y, 1, line_height - 3, click_id, puce, (0,0,0), BUTTON_TEXT_COLOR)
                screen.blit(font.render(wrap_info, True, TEXT_COLOR), (pos_x + larg, pos_y))
            pos_y += line_height
        pos_y += BUTTON_PADDING
    if not editMode: # Affichage des boutons de création de wrap
        # texte
        pos_x = WRAP_EDIT_LEFT
        pos_y = GRAPH_OFFSET_TOP + size_y + BORDER + 3 * BORDER
        (larg, haut) = make_button(wraps_buttons, pos_x, pos_y, WRAPS_BUTTONS_WIDTH, 1, "", "Créer nouveau :", (0,0,0), BUTTON_TEXT_COLOR)
        pos_y += haut
        # boutons
        for wd_key, datas in wraps_datas.items():
            if not datas['type'] == 'CONFIG':
                (larg, haut) = make_button(wraps_buttons, pos_x, pos_y, WRAPS_BUTTONS_WIDTH, 1, datas['type'], wd_key, datas['couleur'], BUTTON_TEXT_COLOR)
                pos_y += haut
            # décalages pour séparation visuelle
            if (wd_key == 'PULSEUR' or wd_key == 'EXTENSION'):
                pos_y += BORDER
    if True: # Affichage des informations du point sur la couche + boutons
        # Nettoyage de la zone
        pos_x0 = GRAPH_OFFSET_LEFT + size_x + BORDER
        pos_y0 = 0
        pygame.draw.rect(screen, (0,0,0), (pos_x0, pos_y0, window_width - pos_x0, window_height - pos_y0))
        pygame.draw.rect(screen, BORDER_COLOR, (pos_x0 - BORDER, GRAPH_OFFSET_TOP + size_y + BORDER + BORDER, BORDER, window_height - (GRAPH_OFFSET_TOP + size_y + 10)))
        pos_x0 += BORDER
        pos_y0 += BORDER
        # Boutons de positionnement
        # position x
        pos_x = pos_x0
        pos_y = pos_y0
        (larg, haut) = make_button(point_buttons, pos_x, pos_y, 2 * POS_BUTTONS_WIDTH, 1, '', "x=" + str(selected_x), (63,63,63), BUTTON_TEXT_COLOR)
        pos_y += POS_BUTTONS_HEIGHT
        (larg, haut) = make_button(point_buttons, pos_x, pos_y, POS_BUTTONS_WIDTH, -1, 'sel_x-', "x-", (63,63,63), BUTTON_TEXT_COLOR)
        pos_x += larg
        (larg, haut) = make_button(point_buttons, pos_x, pos_y, POS_BUTTONS_WIDTH, -1, 'sel_x+', "x+", (63,63,63), BUTTON_TEXT_COLOR)
        # position y
        pos_x += larg + BORDER
        pos_y = pos_y0
        (larg, haut) = make_button(point_buttons, pos_x, pos_y, 2 * POS_BUTTONS_WIDTH, 1, '', "y=" + str(selected_y), (63,63,63), BUTTON_TEXT_COLOR)
        pos_y += POS_BUTTONS_HEIGHT
        (larg, haut) = make_button(point_buttons, pos_x, pos_y, POS_BUTTONS_WIDTH, -1, 'sel_y-', "y-", (63,63,63), BUTTON_TEXT_COLOR)
        pos_x += larg
        (larg, haut) = make_button(point_buttons, pos_x, pos_y, POS_BUTTONS_WIDTH, -1, 'sel_y+', "y+", (63,63,63), BUTTON_TEXT_COLOR)
        # position z
        pos_x += larg + BORDER
        pos_y = pos_y0
        (larg, haut) = make_button(point_buttons, pos_x, pos_y, 2 * POS_BUTTONS_WIDTH, 1, '', "z=" + str(selected_z), (63,63,63), BUTTON_TEXT_COLOR)
        pos_y += POS_BUTTONS_HEIGHT
        (larg, haut) = make_button(point_buttons, pos_x, pos_y, POS_BUTTONS_WIDTH, -1, 'sel_z-', "z-", (63,63,63), BUTTON_TEXT_COLOR)
        pos_x += larg
        (larg, haut) = make_button(point_buttons, pos_x, pos_y, POS_BUTTONS_WIDTH, -1, 'sel_z+', "z+", (63,63,63), BUTTON_TEXT_COLOR)
        # Ecritures du numéro
        pos_y += haut + BORDER
        pos_x = pos_x0
        screen.blit(font.render(f"Numéro de portion : {selected_x + selected_y * size_x + selected_z * size_x * size_y}", True, TEXT_COLOR), (pos_x, pos_y))
        # Ecritures des valeurs
        if selected_x is not None and selected_y is not None and selected_values is not None:
            pos_y += line_height
            screen.blit(font.render(f"------------------------------------", True, TEXT_COLOR), (pos_x, pos_y))
            pos_y += line_height
            # Récupération des textes et tailles
            pilote = selected_values[DATA_PILOTE]
            this_texts_set = ("?",) * PORTION_SIZE
            this_sizes_set = (1,) * PORTION_SIZE
            # listage des modeles de wrap
            for datas in wraps_datas.values():
                if (datas['type'] == 'PORTION' or datas['type'] == 'CONFIG'):
                    code = 0
                    num = 0
                    # recherche de code pilote
                    for i, t in enumerate(datas['tailles']):
                        if num == DATA_PILOTE:
                            liste = datas['initial']
                            code = liste[i]
                            break
                        num += t
                    # récupération des listes
                    if code != 0:
                        if code == pilote:
                            this_texts_set = datas['textes']
                            this_sizes_set = datas['tailles']
                            break
            # Vérification de la cohérence des tailles
            if sum(this_sizes_set) != PORTION_SIZE:
                print(f"Erreur: La somme des tailles ne correspond pas à {PORTION_SIZE} octets pour le bloc sélectionné.")
                continue
            # Ecriture des valeurs selon la liste des tailles
            i = 0  # index dans la liste des tailles
            b = 0  # octet de début en traitement
            while b < PORTION_SIZE:
                titre = this_texts_set[i]
                taille = this_sizes_set[i]
                valeur = int.from_bytes(selected_values[b:b + taille], byteorder='little')
                # Afficher les octets dans l'ordre, y compris les octets combinés en 64 bits
                texte_comp = ""
                if titre == "Code":
                    texte_comp = "(" + "Code" + ")"
                if titre[:2] == "P.":
                    x = valeur % size_x
                    y = int((valeur - x) / size_x) % size_y
                    z = int((valeur - y * size_x - x) / (size_x * size_y)) % size_z
                    texte_comp = "(" + str(x) + "," + str (y) + "," + str(z) + ")"
                screen.blit(font.render(f"> {titre} ({b}/{taille}) : {valeur} {texte_comp}", True, TEXT_COLOR), (pos_x, pos_y))
                pos_y += line_height
                b += taille
                i += 1
                if (b % 8) == 0:
                    screen.blit(font.render(f"--------------------", True, TEXT_COLOR), (pos_x, pos_y))
                    pos_y += line_height
    if editMode: # Affichage de l'interface de détail du wrap
        pos_x = WRAP_EDIT_LEFT
        pos_y = GRAPH_OFFSET_TOP + size_y + BORDER
        pygame.draw.rect(screen, (0,0,0), (pos_x, pos_y, window_width - pos_x, window_height - pos_y))
        pygame.draw.rect(screen, (0,255,0), (pos_x, pos_y, window_width - pos_x, window_height - pos_y), 5)
        pos_x += 10
        pos_y += 10
        screen.blit(font.render("Détails du wrap (use tab, numpad & enter)", True, TEXT_COLOR), (pos_x, pos_y))
        pos_y += line_height
        screen.blit(font.render("-----------------------------------------", True, TEXT_COLOR), (pos_x, pos_y))
        pos_y += line_height
        if input_values:
            wd_key = input_values[0]
            nom = input_values[1]
            typ = wraps_datas[wd_key]['type']
            edit_set = wraps_datas[wd_key]['textes']

            text_surface = font.render(f"Nom : {nom}", True, TEXT_COLOR)
            if 1 == selected_input: # affichage d'un rectangle avant le texte
                rect = text_surface.get_rect(topleft=(pos_x, pos_y))
                pygame.draw.rect(screen, HIGHLIGHT_COLOR, rect)
            screen.blit(text_surface, (pos_x, pos_y))
            pos_y += line_height

            for i, d in enumerate(input_values[DATAS_INPUTS_OFFSET:], start=DATAS_INPUTS_OFFSET):
                text_surface = font.render(f"{edit_set[i-DATAS_INPUTS_OFFSET]} : {d}", True, TEXT_COLOR)
                if i == selected_input: # affichage d'un rectangle avant le texte
                    rect = text_surface.get_rect(topleft=(pos_x, pos_y))
                    pygame.draw.rect(screen, HIGHLIGHT_COLOR, rect)
                screen.blit(text_surface, (pos_x, pos_y))
                pos_y += line_height
    if True: # Affichage de la zone de feedback
        if feedback_message and pygame.time.get_ticks() - feedback_timer < 3000:  # Affichage pendant 3 secondes
            screen.blit(font.render(feedback_message, True, (255, 0, 0)), (10, window_height - 20))
    pygame.display.flip()
pygame.quit()