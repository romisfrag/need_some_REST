(* NOTES :
J'aurais forcément besoin de lambda.ml pour ce fichier étant donné que je vais devoir afficher les termes ect...
Mais il faut que je fasse la liste des fonctions qui me sont utiles:
- pretty_print_inTm 


Je pense que c'est une meilleur stratégie de ne pas passer le save dans ces fonctions car ce n'est pas leur role 
 *)
(* faux type pour voir comment va se comporter le logiciel quand je changerais cette definition  *)
type inTm = string

(* replace some string by inTm CF the repository *)
type definition =
(* The  first arg is the typ and the second the terme *)
  | Complete of inTm * inTm (* replace with inTm * inTm *) 
  | Incomplete of inTm * inTm (* replace with string * inTm *)

type noeud =  
(* the first arg is the name of the var and the second his type *)
  | Variable of string * inTm 
  | Definition of string * definition 
(* the first inTm is the type and the second one is the terme *)
  | Intermediaire of int * inTm * inTm   (* replace with int * inTm * inTm * string  *)

type tree =
  | Item of noeud 
  | Section of tree list

type path =
  | Top
  | Node of tree list * path * tree list
  
type location = Loc of (tree * path)



(* -------------------Fonctions permettant d'acceder aux différents termes contenus dans le zipper -------------------- *)
let get_terme_item (Loc(t,p)) = 
  match Loc(t,p) with 
  | Loc(Item(Variable(name,typ)),t) -> failwith ("get_terme_item : this is a variable it don't have terme" ^ name)
  | Loc(Item(Definition(name,Incomplete(typ,terme))),t) -> terme     
  | Loc(Item(Intermediaire(n,typ,terme)),t) -> terme     
  | Loc(Item(Definition(name,Complete(typ,terme))),t) -> terme     
  | _ -> failwith "get item : it's not possible to get this..." 


let get_type_item (Loc(t,p)) = 
  match Loc(t,p) with 
  | Loc(Item(Variable(name,typ)),t) -> typ
  | Loc(Item(Definition(name,Incomplete(typ,terme))),t) -> typ
  | Loc(Item(Intermediaire(n,typ,terme)),t) -> typ
  | Loc(Item(Definition(name,Complete(typ,terme))),t) -> typ
  | _ -> failwith "get_type_item : it's not possible to get this..." 


let get_num_Inter (Loc(t,p)) = 
  match Loc(t,p) with 					
  | Loc(Item(Intermediaire(n,typ,terme)),t) -> n
  | _ -> 0


let know_def_inter (Loc(t,p)) = 
  match Loc(t,p) with 
  | Loc(Item(Intermediaire(n,typ,terme)),t) -> true
  | _ -> false




(* ----------------------Fonctions de navigation dans le zipper ------------------------ *)
(* L'ensemble des fonctions de navigations retournes la location sans modification si celle si ne peut pas s'effectuer *)

(* Fonctions de navigations brutes *)
let go_left (Loc(t,p)) = 
  match p with
  | Top -> (Loc(t,p))
  | Node(l::left,up,right) -> (Loc(l,Node(left,up,t::right)))
  | Node([],up,right) -> (Loc(t,p))

let go_right (Loc(t,p)) = 
  match p with
  | Top -> failwith "right of top"
  | Node(left,up,r::right) -> (Loc(r,Node(t::left,up,right)))
  | _ -> (Loc(t,p)) 
		  		  
let go_up (Loc(t,p)) = 
  match p with
  | Top -> failwith "up of top"
  | Node(left,up,right) -> (Loc(Section((List.rev left) @ (t::right)),up))
			      
let go_down (Loc(t,p)) = match t with
    Item(_) -> (Loc(t,p)) 
  | Section(t1::trees) -> (Loc(t1,Node([],p,trees)))
  | _ -> (Loc(t,p)) 

(* Fonctions de navigations plus abstraites *)

let rec go_n_son (Loc(t,p)) n = match n with    
  | 0 -> go_down (Loc(t,p))
  | n -> go_n_son (go_right (Loc(t,p))) (n-1)

let rec go_full_left (Loc(t,p)) = 
  match p with 
  | Node([],up,right) -> (Loc(t,p))
  | Node(left,up,right) -> go_full_left (go_left (Loc(t,p)))
  | Top -> failwith "go_full_left : can't go left of a top"  

let rec go_right_count (Loc(t,p)) n = 
  match p with 
  | Node(left,up,[]) -> n
  | Node(left,up,right) -> go_right_count (go_right (Loc(t,p))) (n + 1)
  | Top -> 0
and count_son (Loc(t,p),d) =   
  go_right_count (go_full_left (Loc(t,p))) 0


(* Fonction permettant de remonter tant que le noeud en cour n'est pas un Inter ou une Definition *)
let rec stop_when_def_inter (Loc(t,p)) =   
  match (Loc(t,p)) with 
  | Loc(_,Top) -> (Loc(t,p))
  | Loc(Item(Variable(name,terme)),p) -> stop_when_def_inter (go_up (Loc(t,p)))
  | Loc(Item(Definition(name,terme)),p) -> (Loc(t,p))
  | Loc(Item(Intermediaire(n,name,terme)),p) -> (Loc(t,p))
  | Loc(Section(x),p) -> stop_when_def_inter (go_full_left (Loc(t,p)))

let proof_up (Loc(t,p)) = 
  let arbre = go_up (Loc(t,p)) in 
  stop_when_def_inter arbre



(* ---------------------Fonctions de parcours de l'arbre afin de récupérer des informations------------------------ *)

(* Fonctions nécéssaires pour retrouver l'ensemble des définitions complètes du contexte *)
let rec get_def_item it env = 
  match it with 
  | Definition(name,Complete(typ,terme)) -> ((name,typ,terme) :: env)
  | _ -> env 
and get_def_tree_liste tree_liste env = 
  match tree_liste with 
  | [] -> env 
  | Item(Definition(name,Complete(typ,terme))) :: [] -> ((name,typ,terme) :: env)
  | Item(Definition(name,Complete(typ,terme))) :: suite -> get_def_tree_liste suite ((name,typ,terme) :: env)
  | other :: suite -> get_def_tree_liste suite env
and get_def (Loc(t,p)) env = 
  match t,p with 
  | (Section(x),Top) -> get_def_tree_liste x env
  | (Item(x),Top) -> get_def_item x env
  | (Section(x),p) -> get_def (go_up(Loc(t,p))) (get_def_tree_liste x env)
  | (Item(x),p) -> get_def (go_up(Loc(t,p))) (get_def_item x env)

let liste_def (Loc(t,p)) = 
  get_def (Loc(t,p)) []




(* Fonctions nécéssaires pour retrouver l'ensemble des variables contenus dans le contexte *)
let rec get_env_item it env= 
  match it with 
  | Variable(name,typ) -> ((name,typ) :: env)
  | _ -> env
and get_env_tree_liste tree_liste env = 
  match tree_liste with 
  | (Item(Variable(name,typ))) :: [] -> ((name,typ) :: env)
  | (Item(Variable(name,typ))) :: suite -> get_env_tree_liste suite ((name,typ) :: env) 
  | other :: suite -> get_env_tree_liste suite env
  | [] -> env
and get_env (Loc(t,p)) env =
  match t,p with 
  | (Section(x),Top) -> get_env_tree_liste x env
  | (Item(x),Top) -> get_env_item x env
  | (Section(x),p) -> get_env (go_up (Loc(t,p))) (get_env_tree_liste x env)
  | (Item(x),p) -> get_env (go_up (Loc(t,p))) (get_env_item x env)


let rec return_type_var_env env var = 
  match env with 
  | [] -> failwith "returne_type_var_env : la variable ne fait pas partis de l'environement" 
  | (name,typ) :: suite -> if name = var then typ else return_type_var_env suite var

let get_var_type (Loc(t,p)) var = 
  let env = get_env (Loc(t,p)) [] in
  return_type_var_env env var





(* -----------------------------------Fonctions de modifications du zipper-----------------------------------*)

let replace_item (Loc(t,p),d) tsub = 
  match t with 
  | Item(_) -> (Loc(tsub,p),d)
  | _ -> failwith "replac_item : you are supposed to change an item" 

let replace_terme_item (Loc(t,p)) terme = 
  match t with 
  | Item(Intermediaire(num,typ,_)) -> Loc(Item(Intermediaire(num,typ,terme)),p)
  | Item(Definition(name,(Complete(typ,_)))) -> Loc(Item(Definition(name,(Complete(typ,terme)))),p)
  | Item(Definition(name,(Incomplete(typ,_)))) -> Loc(Item(Definition(name,(Incomplete(typ,terme)))),p)
  | _ -> failwith "replace_terme_item : can't replace in other things than item "


let replace_type_item (Loc(t,p)) typ = 
  match t with 
  | Item(Intermediaire(num,_,terme)) -> Loc(Item(Intermediaire(num,typ,terme)),p)
  | Item(Definition(name,(Complete(_,terme)))) -> Loc(Item(Definition(name,(Complete(typ,terme)))),p)
  | Item(Definition(name,(Incomplete(_,terme)))) -> Loc(Item(Definition(name,(Incomplete(typ,terme)))),p)
  | _ -> failwith "replace_terme_item : can't replace in other things than item" 













(* I will put every pretty_print at the end of the file *)
(* 

let rec pretty_print_env env = 
  match env with 
  | [] -> ""
  | (name,typ) :: suite -> "(" ^ name ^ " : " ^ pretty_print_inTm typ [] ^ ") " ^ print_env suite 
let get_and_print_env (Loc(t,p)) = 
  let env = get_env (Loc(t,p)) [] in 
  pretty_print_env env
*)
