(*
Algorithmes de tri rapide

Copyright (C) 2021  Faholan <https://github.com/Faholan>
*)

type 'a atho =
  | F_atho of 'a
  | N_atho of 'a * 'a atho array
;;

let rec hauteur_atho = function
  | F_atho(_) -> 0
  | N_atho(_, foret) -> 1 + hauteur_atho_foret foret
and hauteur_atho_foret foret = let result = ref 0 in
  for i = 0 to Array.length foret - 1 do
    let nb = hauteur_atho foret.(i) in
      if nb > !result then
        result := nb
  done ;
  !result
;;

let rec nb_noeuds_atho = function
  | F_atho(_) -> 1
  | N_atho(_, foret) -> nb_noeuds_atho_foret foret
and nb_noeuds_atho_foret foret = let result = ref 1 in
  for i = 0 to Array.length foret - 1 do
    result := !result + nb_noeuds_atho foret.(i)
  done ;
  !result
;;

let rec nb_feuilles_atho = function
  | F_atho(_) -> 1
  | N_atho(_, foret) -> nb_feuilles_atho_foret foret
and nb_feuilles_atho_foret foret = let result = ref 0 in
  for i = 0 to Array.length foret - 1 do
    result := !result + nb_feuilles_atho foret.(i)
  done ;
  !result
;;

type 'a alho =
  | F_alho of 'a
  | N_alho of 'a * 'a alho list
;;

let rec hauteur_alho = function
  | F_alho(_) -> 0
  | N_alho(_, lst) -> 1 + List.fold_left (fun cur noeud -> max cur (hauteur_alho noeud)) 0 lst
  (*1 + List.fold_left max 0 (List.map hauteur_alho lst)*)
;;

let rec nb_noeuds_alho = function
  | F_alho(_) -> 1
  | N_alho(_, lst) -> List.fold_left (fun cur noeud -> cur + nb_noeuds_alho noeud) 1 lst
;;

let rec nb_feuilles_alho = function
  | F_alho(_) -> 1
  | N_alho(_, lst) -> List.fold_left (fun cur noeud -> cur + nb_feuilles_alho noeud) 0 lst
;;

let rec parcours_prefixe_alho = function
  | F_alho(a) -> [a]
  | N_alho(a, foret) -> a::(parcours_prefixe_alho_foret foret)
and parcours_prefixe_alho_foret = function
  | [] -> []
  | h::t -> (parcours_prefixe_alho h) @ (parcours_prefixe_alho_foret t)
;;

let prefixe =
  let rec parcours_noeud current = function
    | F_alho(a) -> a::current
    | N_alho(a, foret) -> a::(parcours_foret current foret)
  and parcours_foret current = function
    | [] -> current
    | h::t -> parcours_foret (parcours_noeud current h) t
  in parcours_noeud []
;;
