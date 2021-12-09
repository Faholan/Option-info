(*
TD Graphes

Copyright (C) 2021  Faholan <https://github.com/Faholan>
*)

type arete = {a: int; b: int} ;;

type graphe = {n: int; aretes: arete list} ;;

(*Question 1*)
let gex1 = {
  n = 5 ;
  aretes = [
    {a = 0; b = 4};
    {a = 4; b = 3};
    {a = 0; b = 3};
    {a = 1; b = 3};
    {a = 0; b = 3};
    {a = 3; b = 0}
  ]
} ;;

(*Question 2*)
let gex2 = {
  n = 6 ;
  aretes = [
    {a = 0; b = 3};
    {a = 3; b = 4};
    {a = 4; b = 1};
    {a = 1; b = 2};
    {a = 2; b = 5};
    {a = 5; b = 0}
  ]
} ;;

(*Question 3*)
let insere lst i =
  let rec inserator = function
    | [] -> [i]
    | h::t when h = i -> h::t
    | h::t when h > i -> i::h::t
    | h::t -> h::(inserator t)
  in inserator lst
;;

(*Question 4*)
let rec voisins grph i = _make_voisins [] i grph.aretes
and _make_voisins cur sommet = function
  | [] -> cur
  | h::t when h.a = sommet -> _make_voisins (insere cur h.b) sommet t
  | h::t when h.b = sommet -> _make_voisins (insere cur h.a) sommet t
  | h::t -> _make_voisins cur sommet t
;;

(*Question 5*)
let bonne_coloration grph couleur =
  let rec verify_color = function
    | [] -> true
    | h::_ when couleur.(h.a) = couleur.(h.b) -> false
    | _::t -> verify_color t
  in verify_color grph.aretes
;;

(*Question 8*)
let rec coloration grph =
  let result = Array.make grph.n 0 in
    for i = 0 to grph.n - 1 do
      result.(i) <- _gen_coloration result [] (voisins grph i)
    done ;
    result
and _gen_coloration couleur current = function
  | [] -> _find_min_color current
  | h::t -> _gen_coloration couleur (insere current couleur.(h)) t
and _find_min_color = function
  | [] -> 1
  | 0::t -> _find_min_color t
  | [i] -> i + 1
  | h::t when h = List.hd t - 1 -> _find_min_color t
  | h::t -> h + 1
;;

(*Question 9*)
let prem_voisin grph i = List.hd (voisins grph i) ;;

(*Question 10*)
let rec prem_ni grph = List.hd (_find_ni [] grph.aretes)
and _find_ni current = function
  | [] -> current
  | h::t -> _find_ni (insere (insere current h.a) h.b) t
;;

(*Question 11*)
let rec h grph =
  let s1 = prem_ni grph in
    let s2 = prem_voisin grph s1 in
      {n=grph.n; aretes=_remove_aretes s1 s2 grph.aretes}
and _remove_aretes i j = function
  | [] -> []
  | h::t when h.a = i && h.b = j -> _remove_aretes i j t
  | h::t when h.b = i && h.a = j -> _remove_aretes i j t
  | h::t -> h::(_remove_aretes i j t)
;;

(*Question 12*)
let rec k grph =
  let hshed = h grph and s1 = prem_ni grph in
    let s2 = prem_voisin grph s1 in
      {n=grph.n - 1; aretes=_k_aretes s1 s2 hshed.aretes}
and _k_aretes s1 s2 = function
  | [] -> []
  | h::t -> {a=_k_renumerote s1 s2 h.a; b=_k_renumerote s1 s2 h.b}::(_k_aretes s1 s2 t)
and _k_renumerote s1 s2 i =
  if i < s2 then
    i
  else
    if i = s2 then
      s1
    else
      i - 1
;;

(*Question 13*)
(*
Si le graphe g ne possède aucune arête,
fc(g, p) = p^n
*)

(*Question 14*)
let rec fc grph p =
  if grph.aretes = [] then
    _to_power p grph.n
  else
    (fc (h grph) p) - (fc (k grph) p)
and _to_power p = function
  | 0 -> 1
  | k -> p * (_to_power p (k - 1))
;;

(*Question 15*)
let rec nombre_chromatique grph = _chroma_rec grph 1
and _chroma_rec grph p = match fc grph p with
  | 0 -> _chroma_rec grph (p + 1)
  | _ -> p
;;

(*Question 17*)
let difference p1 p2 =
  let result = Array.copy p1 in
    for i = 0 to Array.length p2 - 1 do
      result.(i) <- result.(i) - p2.(i)
    done;
    result
;;

(*Question 18*)
let rec polynome_chromatique grph = match grph.aretes with
  | [] -> _basic_chroma grph.n
  | _ -> difference (polynome_chromatique (h grph)) (polynome_chromatique (k grph))
and _basic_chroma n =
  let result = Array.make (n + 1) 0 in
    result.(n) <- 1 ;
    result
;;

(*Question 19*)
let evalue_polynome poly x =
  let result = ref 0 and power = ref 1 in
    for i = 0 to Array.length poly - 1 do
      result := !result + poly.(i) * !power ;
      power := x * !power
    done ;
    !result
;;

(*Question 20*)
let rec nombre_chromatique_bis grph = _find_chroma_bis (polynome_chromatique grph) 1
and _find_chroma_bis poly p = match evalue_polynome poly p with
  | 0 -> _find_chroma_bis poly (p + 1)
  | _ -> p
;;

(*Question 21*)
(*Trouver une coloration minimale*)

let rec _valid_color color colors = function
  | [] -> true
  | h::_ when colors.(h) = color -> false
  | _::t -> _valid_color color colors t
;;

let find_coloration_minimale grph =
  let mini = nombre_chromatique grph and result = Array.make grph.n 0 and cur_sommet = ref 0 in
  let rec placer color =
    if color > mini then
      backtrack ()
    else
      if _valid_color color result (voisins grph !cur_sommet) then (
        result.(!cur_sommet) <- color ;
        cur_sommet := !cur_sommet + 1
      )
      else
        placer (color + 1)
  and backtrack () =
    let color = result.(!cur_sommet - 1) in (
      cur_sommet := !cur_sommet - 1 ;
      result.(!cur_sommet) <- 0 ;
      if color = mini then
        backtrack ()
      else
        placer (color + 1)
    )
  in
    while !cur_sommet < grph.n do
      placer 1
    done ;
    result
;;

(*

Question 22

Résolution d'un Sudoku à l'aide de ça
*)

(*
Un Sudoku sera représenté par une matrice 9x9
*)

let pile_vide () = ref [] ;;

let est_pile_vide p = !p = [] ;;

let depile p =
  let e = List.hd !p in
    p := List.tl !p ;
    e
;;

let empile p e =
  p := e::!p
;;

let graphe_sudoku () =
  let aretes = ref [] in
    for i = 0 to 8 do
      for j = 0 to 8 do
        for k = j + 1 to 8 do
          aretes := {a=9*i+j; b=9*i+k}::{a=9*j+i;b=9*k+i}::!aretes
        done ;
      done ;
    done ;
    for i = 0 to 2 do
      for j = 0 to 2 do
        for k = 0 to 8 do
          for l = k + 1 to 8 do
            aretes :={a=27 * i+3*j + 9 * (k / 3) + (k mod 3); b=27*i + 3*j + 9 * (l / 3) + l mod 3}::!aretes
          done ;
        done ;
      done ;
    done ;
    {n=9 * 9; aretes = !aretes}
;;


let sudoku_to_mat sudoku =
  let result = Array.make (9 * 9) 0 in
    for i = 0 to 8 do
      for j = 0 to 8 do
        result.(9 * i + j) <- sudoku.(i).(j)
      done ;
    done ;
    result
;;

let mat_to_sudoku mat =
  let result = Array.make_matrix 9 9 0 in
    for i = 0 to 8 do
      for j = 0 to 8 do
        result.(i).(j) <- mat.(9 * i + j)
      done ;
    done ;
    result
;;

let find_empty mat =
  let rec finder = function
    | i when i = Array.length mat -> -1
    | i when mat.(i) = 0 -> i
    | i -> finder (i + 1)
  in finder 0
;;

let find_sudoku sudoku =
  let grph = graphe_sudoku () and result = sudoku_to_mat sudoku and historique = pile_vide () in
  let rec placer color cur_sommet =
    if color > 9 then
      backtrack ()
    else
      if _valid_color color result (voisins grph cur_sommet) then (
        result.(cur_sommet) <- color ;
        empile historique cur_sommet
      )
      else
        placer (color + 1) cur_sommet
  and backtrack () =
    let cur_sommet = depile historique in
      let color = result.(cur_sommet) in (
        result.(cur_sommet) <- 0 ;
        if color = 9 then
          backtrack ()
        else
          placer (color + 1) cur_sommet
      )
  in
    let cur_sommet = ref (find_empty result) in
      while !cur_sommet <> -1 do
        placer 1 !cur_sommet
      done ;
      mat_to_sudoku result
;;

let sudo = [|
  [|0; 1; 0; 0; 5; 2; 0; 0; 7|];
  [|0; 8; 0; 7; 0; 0; 0; 1; 0|];
  [|9; 0; 2; 0; 6; 1; 5; 0; 0|];
  [|4; 0; 0; 2; 0; 0; 7; 0; 0|];
  [|8; 0; 1; 0; 7; 0; 2; 0; 4|];
  [|0; 0; 7; 0; 0; 5; 0; 0; 9|];
  [|0; 0; 6; 5; 3; 0; 9; 0; 8|];
  [|0; 9; 0; 0; 0; 4; 0; 5; 0|];
  [|2; 0; 0; 9; 8; 0; 0; 7; 0|]
|];;
