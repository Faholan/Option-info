(*
Questionnaire de rentrée d'option Info

Copyright (C) 2021  Faholan <https://github.com/Faholan>
*)

let myname = "Florian Chivé" ;;

(*Question 1*)

(*a*)
let rec pgcd a b =
  if a > b then
    pgcd b a
  else
    let r = b mod a in
      match r with
        | 0 -> a
        | _ -> pgcd r a
;;

(*b*)
let rec minimum = function
  | [] -> failwith "Empty list"
  | [t] -> t
  | h::t -> min h (minimum t)
;;

let minimum_iter l =
  let lref = ref (List.tl l) and cur_min = ref (List.hd l) in
    while !lref <> [] do
      cur_min := min !cur_min (List.hd !lref) ;
      lref := List.tl !lref
    done ;
    !cur_min
;;

(*c*)
let rec somme = function
  | [] -> 0
  | h::t -> h + somme t
;;

(*d*)
let minimum_arr arr =
  if Array.length arr = 0 then
    failwith "Emtpy array"
  else (
    let cur = ref arr.(0) in
      for i = 1 to Array.length arr - 1 do
        if arr.(i) < !cur then
          cur := arr.(i)
      done ;
      !cur
  )
;;

let somme_arr arr =
  if Array.length arr = 0 then
    failwith "Emtpy array"
  else (
    let cur = ref arr.(0) in
      for i = 1 to Array.length arr - 1 do
        cur := !cur + arr.(i)
      done ;
      !cur
  )
;;

(*e*)
let hamming a =
  let rec divide_by a d = match a mod d with
    | 0 when a = 0 -> 0
    | 0 -> divide_by (a/d) d
    | _ -> a
  in
    divide_by (divide_by (divide_by a 2) 3) 5 == 1
;;

(*Partie I*)

(*Question 2*)
let rec appartient l a = match l with
  | [] -> false
  | h::t -> if h = a then true else appartient t a
;;

(*Question 3*)
let rec map f l = match l with
  | [] -> []
  | h::t -> (f h)::(map f t)
;;

(*Question 4*)
let rec concat l1 l2 = match l1 with
  | h::t -> h::(concat t l2)
  | [] -> l2
;;

(*Question 5*)
let rec filter f = function
  | [] -> []
  | h::t when f h -> h::(filter f t)
  | _::t -> filter f t
;;

let filter2 f =
  let rec filter_rec beginning = function
  | [] -> beginning
  | h::t when f h -> filter_rec (h::beginning) t
  | _::t -> filter_rec beginning t
  in filter_rec []
;;

(*Question 6*)
let rec fold_left f a = function
  | [] -> a
  | h::t -> fold_left f (f a h) t
;;

(*Partie II*)

(*Question 7*)
let positifs = filter (fun x -> x > 0) ;;

(*Question 8*)
let somme = fold_left (fun cur e -> cur + e) 0 ;;

(*Question 9*)
let flatten = fold_left concat [] ;;

(*Question 10*)
let max_list l = fold_left max (List.hd l) (List.tl l) ;;

(*Question 11*)
let len = fold_left (fun cur _ -> cur + 1) 0 ;;

(*Question 12*)
let appartient l a = fold_left (fun cur x -> cur || x = a) false l ;;

(*Partie III*)
type tuile = Trou | Noeud of tuile list ;;

(*Question 13*)
let rec arite = function
  | Trou -> 1
  | Noeud(l) -> fold_left (fun cur tuile -> cur + arite tuile) 0 l
;;

(*Question 14*)
let rec profondeur = function
  | Trou -> -1
  | Noeud(l) -> 1 + fold_left (fun cur tuile -> max cur (profondeur tuile)) 0 l
;;

(*Question 15*)
let rec nb_noeuds = function
  | Trou -> 0
  | Noeud(l) -> 1 + fold_left (fun cur tuile -> cur + nb_noeuds tuile) 0 l
;;

(*Question 16*)
let fold_left2 f a default = function
  | [], [] -> a
  | [], _ -> default
  | _, [] -> default
  | h::t, h'::t' -> let rec rest_list cur = function
    | [], [] -> cur
    | [], _ -> default
    | _, [] -> default
    | (h::t, h'::t') -> rest_list (f cur h h') (t,t')
  in rest_list (f a h h') (t,t')
;;

let rec egalite = function
  | Trou, Trou -> true
  | Noeud(_), Trou -> false
  | Trou, Noeud(_) -> false
  | Noeud(l1), Noeud(l2) -> fold_left2 (fun cur a b -> if cur then egalite (a,b) else false) true false (l1, l2)
;;

(*Question 17*)
let rec fold_right f a = function
  | [] -> a
  | h::t -> f h (fold_right f a t)
;;

let rec replace_all t1 t2 = match t1 with
  | Trou -> t2
  | Noeud(l1) -> Noeud(fold_right (fun x r -> (if x = Trou then t2 else replace_all x t2)::r) [] l1)
;;

(*Question 18*)
let replace tuile liste =
  if arite tuile = len liste then
    let lref = ref liste in
      match tuile with
        | Trou -> List.hd !lref
        | Noeud(l) -> let rec replacer = function
          (*Liste_noeud, liste_externe*)
          | []-> []
          | Trou::t -> let h = List.hd !lref in (
            lref := List.tl !lref ;
            h::(replacer t)
          )
          | Noeud(l2)::t -> let h = Noeud(replacer l2) in
            h::(replacer t)
        in Noeud(replacer l)
  else
    tuile
;;

(*Question 19*)
let replace_only t1 k0 t2 =
  let k = ref k0 in match t1 with
    | Trou when k0 = 1 -> t2
    | Trou -> Trou
    | Noeud(l) -> let rec replacer = function
      | [] -> []
      | Trou::t when !k = 0 -> k := -1 ; t2::t
      | Trou::t -> k := !k - 1; Trou::(replacer t)
      | Noeud(l)::t -> let h = Noeud(replacer l) in
        h::(replacer t)
      in Noeud(replacer l)
;;

(*Partie IV*)

(*Question 20*)
let n_max = 5 ;;

let rec est_valide = function
  | Trou -> true
  | Noeud(l) when len l > n_max -> false
  | Noeud(l) -> fold_left (fun cur tuile -> if cur then est_valide tuile else false) true l
;;

(*Partie V*)

(*Question 21*)

(*
Pour paver toute tuile valide, il est nécessaire de considérer la tuile Trou, ainsi que des Noeuds comportant entre 0 et n_max fils Trou

Cette liste est complète : en effet, tout noeud admet entre 0 et n_max fils, et nous avons bien des noeuds ayant entre 0 et n fils. En utilisant replace_only, on peut créer n'importe quelle liste de noeuds ou de trous fils.
Elle est minimale : Supposons par l'absurde qu'elle ne le soit pas.
Or notre liste est en bijection avec l'ensemble [|-1, n_max|], la numérotation correspondant au nombre de noeuds fils.
Soit donc une liste complète incluse dans la nôtre mais distincte. Il existe donc un index correspondant à une tuile n'étant pas présente dans notre nouvelle liste.
Or, cette tuile est elle-même valide, et ne peut être reconstituée en combinant les autres tuiles, donc la nouvelle liste n'est pas complète, d'où l'absurdité.

Quant à l'unicité, elle est due à deux faits :
1) Tout noeud admet entre 0 et n fils. Notre liste complète doit donc comporter au moins n+1 noeuds, au moins 1 par nombre de fils.
2) On doit considérer les noeuds n'admettant que des trous comme fils : en effet, ceux-ci sont valides, mais ne peuvent être retrouvés à partir de noeuds comportant des noeuds comme fils. De plus ils sont suffisants

D'où l'unicité
*)

(*Question 22*)
let rec elementaires = function
  | -1 -> [Trou]
  | n -> let rec make_list_trou = function
    | 0 -> []
    | k -> Trou::(make_list_trou (k - 1))
    in Noeud(make_list_trou n)::(elementaires (n - 1))
;;

(*Partie VI*)

type arraylist = {
  mutable size: int ;
  mutable data: int array ;
} ;;

let new_arraylist n = {size = 1; data = [|n|]} ;;

(*Question 25*)
let nth arr n =
  if n > arr.size then
    failwith "Element doesn't exist"
  else
    arr.data.(n - 1)
;;

(*Question 26*)
let pop arr =
  if arr.size = 0 then
    failwith "Empty array"
  else
    arr.size <- arr.size - 1 ;
    arr.data.(arr.size)
;;

(*Question 27*)

let push n arr =
  if arr.size == Array.length arr.data then (
    let data = Array.make (2 * (Array.length arr.data)) 0 in
      for i = 0 to arr.size - 1 do
        data.(i) <- arr.data.(i)
      done ;
      arr.data <- data
  ) ;
  arr.data.(arr.size) <- n ;
  arr.size <- arr.size + 1
;;

(*Séance 1*)
type 'a arbre =
  | Vide
  | Noeud of 'a arbre * 'a * 'a arbre
;;

let rec parcours_infixe = function
  | Vide -> []
  | Noeud(left, name, right) -> concat (parcours_infixe left) (name::(parcours_infixe right))
;;

let parcours_infixe2 =
  let rec concat_right current = function
    | Vide -> current
    | Noeud(left, value, right) -> concat_right (value::(concat_right current right)) left
  in concat_right []
;;

let parcours_infixe3 arbre =
  let res = ref [] in
    let rec concat_right = function
      | Vide -> ()
      | Noeud(left, value, right) -> (
          concat_right right ;
          res := value::!res ;
          concat_right left
        )
    in concat_right arbre;
    !res
;;
