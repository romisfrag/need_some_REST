(* Interface du fichier de zipper réduite au minmum *)

type location
type inTm




(* Fonctions retournant certaines informations contenues dans les noeuds du zipper *)

(* Fonction qui a retourne le terme d'un Item qu'il contiennent :
-une définition compète
-une définition incomplète 
-un noeud intermedière  *)
val get_terme_item : location -> inTm

(* Identique à la fonction get_terme_item mais celle ci retourne le type *)
val get_type_item : location -> inTm

(* A partir d'un arbre retourne le numéro contenue dans un noeud Intermediaire.
Renvoie 0 si le noeud n'est pas une définition intermediaire *)
val get_num_Inter : location -> int


(* Permet de savoir si le noeud courant est un noeud Intermediaire,
Retourne false sinon*)
val know_def_inter : location -> bool 



(* ------------------------ Fonctions de navigation ------------------- *)
(* je pense que je n'ai pas besoin de fournir les fonctions permettant de naviguer simplement
Il faut que je donne seulement les fonctions du style proof_up ect .... *)


(* Retourne la location en se déplaçant vers le n-ieme fils du noeud en cour*)
val go_n_son : location -> int -> location

(* Retourne le nombre de fils du noeud en cours 
Si Top retourne 0*)
val count_son : location -> int 

(* Permet de remonter dans l'arbre de preuves (ne s'arretes pas sur les variables) *)
val proof_up : location -> location

(* Fonction permettant de retourner la liste de l'ensemble de définitions complètes présentes dans le zipper 
le formatage de la liste de retour est le suivant (name,typ,terme) *)
val liste_def : location -> (string * inTm * inTm) list

(* Retourne le type d'une variable a partir d'une location ainsi que de son nom *)
val get_var_type: location -> string -> inTm


(* Dexux fonctions permettants respectivements de remplacer le type ou le terme de l'item courant 
Retourne une exeption si utilisée sur une section (surement changer ça)*)
val replace_terme_item : location -> inTm -> location
val replace_type_item : location -> inTm -> location




