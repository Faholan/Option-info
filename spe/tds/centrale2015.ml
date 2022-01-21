(*Sujet de Centrale 2015*)

(*Partie I*)

let conflit (a, b) (c, d) =
  (c <= a && a <= d) || (a <= c && c <= b)
;;

(*graphe : int list array*)

let construit_graphe (arr: (int * int) array) =
  let result = Array.make (Array.length arr) [] in
    for i = 0 to (Array.length arr - 1) do
      for j = i + 1 to (Array.length arr - 1) do
        if conflit arr.(i) arr.(j) then (
          result.(i) <- j::result.(i) ;
          result.(j) <- i::result.(j)
        )
      done ;
    done ;
    result
;;

let rec appartient lst (x: int) = match lst with
  | [] -> false
  | h::t when h = x -> true
  | _::t -> appartient t x
;;

let plus_petit_absent lst =
  let rec _test_petit x =
    if appartient lst x then
      _test_petit (x + 1)
    else
      x
  in _test_petit 0
;;

let couleur_voisins aretes (couleurs: int array) i =
  let rec get_colors = function
    | [] -> []
    | h::t -> couleurs.(h)::(get_colors t)
  in get_colors aretes.(i)
;;

let couleur_disponible aretes couleurs i = plus_petit_absent (couleur_voisins aretes couleurs i) ;;

let rec est_clique aretes = function
  | [] -> true
  | h::t when partial_clique aretes h t -> est_clique aretes t
  | _ -> false
and partial_clique aretes s = function
  | [] -> true
  | h::t when h = s -> partial_clique aretes s t
  | h::t when appartient aretes.(s) h -> partial_clique aretes s t
  | _ -> false
;;


let coloration segments aretes =
  let couleurs = Array.make (Array.length segments) (-1) in
    for i = 0 to Array.length segments - 1 do
      couleurs.(i) <- couleur_disponible aretes couleurs i
    done ;
    couleurs
;;

let voisins_inferieurs aretes x =
  let rec _get_voisins = function
    | [] -> []
    | h::t when h < x -> h::(_get_voisins t)
    | _::t -> _get_voisins t
  in _get_voisins aretes.(x)
;;

let est_ordre_parfait aretes =
  let rec test_parfait = function
    | x when x = Array.length aretes -> true
    | x when not (est_clique aretes (voisins_inferieurs aretes x)) -> false
    | x -> test_parfait (x + 1)
  in test_parfait 0
;;

let colore aretes =
  let couleurs = Array.make (Array.length aretes) (-1) in
    for i = 0 to Array.length aretes - 1 do
      couleurs.(i) <- couleur_disponible aretes couleurs i
    done ;
    couleurs
;;
