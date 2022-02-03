(*
TD algorithme de Prime

Copyright (C) 2022  Faholan <https://github.com/Faholan>
*)

(* Question 1

A chaque étape de l'algorithme, on rajoute un sommet ne s'y trouvant pas à l'arbre construit.
Comme le graphe de départ contient un nombre fini de sommets, l'algorithme se termine
*)

let graphe1 = [|
  [(1, 4); (7, 8)] ;
  [(0, 4); (2, 8); (7, 11)];
  [(1, 8); (3, 7); (5, 4); (8, 2)];
  [(2, 7); (4, 9); (5, 14)];
  [(3, 9); (5, 10)];
  [(4, 10); (3, 14); (2, 4); (6, 2)];
  [(5, 2); (8, 6); (7, 1)];
  [(6, 1); (8, 7); (1, 11); (0, 8)];
  [(2, 2); (6, 6); (7, 7)];
|] ;;


let compare_arr (((i: int), (j: int)), (p1: int)) (((i2: int), (j2: int)), p2) = p1 <= p2 ;;

let rec insere elem lst compare = match lst with
  | h::t when compare elem h -> elem::h::t
  | h::t -> h::(insere elem t compare)
  | [] -> [elem]
;;

let extrait_min = function
  | [] -> failwith "Empty list"
  | h::t -> h, t
;;
(*La complexité de insere est en O(n), extrait_min est en O(1)*)

(*Question 3*)
let maj_accessibles grph a lst compare =
  let rec insertor lst = function
    | (s, p)::t -> insertor (insere ((a, s), p) lst compare) t
    | [] -> lst
  in insertor lst grph.(a)
;;

let any_empty arr =
  let rec tester = function
    | i when i = Array.length arr -> false
    | i when arr.(i) = [] -> true
    | i -> tester (i + 1)
  in tester 0
;;

let prim1 grph compare =
  let accessibles = ref (maj_accessibles grph 0 [] compare) and result = Array.make (Array.length grph) [] in
    while any_empty result do
      let ((i, j), p), newacc = extrait_min !accessibles in
        if result.(j) = [] then (
          result.(j) <- [i] ;
          result.(i) <- j::result.(i) ;
          accessibles := maj_accessibles grph j newacc compare
        )
        else
          accessibles := newacc
    done ;
    result
;;

(*maj_accessibles est en O(n²)*)
(*Il y a au plus n² arètes qui constituent le graphe qui nous intéresse*)
(*prim1 est donc en O(n**4)*)

prim1 graphe1 compare_arr ;; (*ok*)

type tas = {mutable taille: int; mutable elements: ((int * int) * int) array}

(*Question 5*)
let rec entasser tas i =
  if 2 * i + 1 >= tas.taille then
    ()
  else
    let e = tas.elements.(i) and f1 = tas.elements.(2 * i + 1) in
      if 2 * i + 2 = tas.taille then (
        if compare_arr f1 e then (
          tas.elements.(i) <- f1 ;
          tas.elements.(2 * i + 1) <- e
        )
      ) else (
        let f2 = tas.elements.(2 * i + 1) in
          if compare_arr f1 f2 then (
            if compare_arr f1 e then (
              tas.elements.(i) <- f1 ;
              tas.elements.(2 * i + 1) <- e ;
              entasser tas (2 * i + 1)
            )
          ) else (
            if compare_arr f2 e then (
              tas.elements.(i) <- f2 ;
              tas.elements.(2 * i + 2) <- e ;
              entasser tas (2 * i + 2)
            )
          )
      )
;;
(*La complexité de entasser est en O(logn)*)

(*Question 6*)
let pere i = (i - 1) / 2 ;;

let insere_tas tas e =
  if tas.taille = Array.length tas.elements then (
    let newelems = Array.make (2 * tas.taille) ((0, 0), 0) in
      for i = 0 to tas.taille - 1 do
        newelems.(i) <- tas.elements.(i)
      done ;
      tas.elements <- newelems
  ) ;
  tas.elements.(tas.taille) <- e ;
  tas.taille <- tas.taille + 1 ;
  let rec percolate = function
    | 0 -> ()
    | i when compare_arr tas.elements.(i) tas.elements.(pere i) -> (
      let elem = tas.elements.(i) in
        tas.elements.(i) <- tas.elements.(pere i) ;
        tas.elements.(pere i) <- elem ;
        percolate (pere i)
    )
    | _ -> ()
  in percolate (tas.taille - 1)
;;
(*Cette fonction est en O(n) en cas d'augmentation de la taille, O(log n) sinon.
Comme on n'augmente pas la taille du tas souvent, on peut l'appromximer par 0(log n)
*)

let extrait_min_tas tas =
  let e = tas.elements.(0) in
    tas.taille <- tas.taille - 1 ;
    tas.elements.(0) <- tas.elements.(tas.taille) ;
    entasser tas 0 ;
    e
;;
(*La complexité de cette fonction est en O(log n)*)

let prim2 grph =
  let tas = {taille=0; elements=[|((0, 0), 0)|]} and result = Array.make (Array.length grph) [] in
    let rec adder i = function
      | (j, p)::t -> insere_tas tas ((i, j), p); adder i t
      | _ -> ()
    in
      adder 0 grph.(0) ;
      while tas.taille <> 0 do
        let ((i, j), p) = extrait_min_tas tas in
          if result.(j) = [] then (
            result.(i) <- j::result.(i) ;
            result.(j) <- [i] ;
            adder j grph.(j)
          )
      done ;
      result
;;
(*prim2 est donc en O(n²log n)*)

prim2 graphe1 ;;

type tas_tri = {mutable taille: int; mutable elements: int array} ;;

let rec entasser_tri tas i =
  if 2 * i + 1 >= tas.taille then
    ()
  else
    let e = tas.elements.(i) and f1 = tas.elements.(2 * i + 1) in
      if 2 * i + 2 = tas.taille then (
        if f1 <= e then (
          tas.elements.(i) <- f1 ;
          tas.elements.(2 * i + 1) <- e
        )
      ) else (
        let f2 = tas.elements.(2 * i + 1) in
          if f1 <= f2 then (
            if f1 <= e then (
              tas.elements.(i) <- f1 ;
              tas.elements.(2 * i + 1) <- e ;
              entasser_tri tas (2 * i + 1)
            )
          ) else (
            if f2 <= e then (
              tas.elements.(i) <- f2 ;
              tas.elements.(2 * i + 2) <- e ;
              entasser_tri tas (2 * i + 2)
            )
          )
      )
;;

let insere_tri tas e =
  if tas.taille = Array.length tas.elements then (
    let newelems = Array.make (2 * tas.taille) 0 in
      for i = 0 to tas.taille - 1 do
        newelems.(i) <- tas.elements.(i)
      done ;
      tas.elements <- newelems
  ) ;
  tas.elements.(tas.taille) <- e ;
  tas.taille <- tas.taille + 1 ;
  let rec percolate = function
    | 0 -> ()
    | i when tas.elements.(i) <= tas.elements.(pere i) -> (
      let elem = tas.elements.(i) in
        tas.elements.(i) <- tas.elements.(pere i) ;
        tas.elements.(pere i) <- elem ;
        percolate (pere i)
    )
    | _ -> ()
  in percolate (tas.taille - 1)
;;

let extrait_min_tri tas =
  let e = tas.elements.(0) in
    tas.taille <- tas.taille - 1 ;
    tas.elements.(0) <- tas.elements.(tas.taille) ;
    entasser_tri tas 0 ;
    e
;;

let tri_tas lst =
  let tas = {taille=0; elements=[|0|]} in
    let rec inserter = function
      | h::t -> insere_tri tas h; inserter t
      | [] -> ()
    in
      inserter lst ;
      let rec fetcher () = match tas.taille with
        | 0 -> []
        | _ -> let e = extrait_min_tri tas in
          e::(fetcher ())
      in List.rev (fetcher ())
;;
