(*TD Kruskal

Copyright (C) 2022  Faholan <https://github.com/Faholan>
*)

if false then
  if true then print_string "Le chat est vivant."
else
  print_string "Le chat est mort."
;;

(*
Question 1

trouver a une complexité dans le pire des cas en O(n).
unir a une complexité en O(n) (on doit faire un appel à trouver)
*)


type unionfind = {pere: int array; rang: int array} ;;

(*Question 2*)
let initialise n =
  let pere = Array.make n 0 in
    for i = 1 to n - 1 do
      pere.(i) <- i
    done ;
    {pere = pere; rang = Array.make n 1}
;;

(*Question 3*)
let rec trouver uf i = trouve_rec [] uf i
and trouve_rec sommets uf i = match uf.pere.(i) with
  | k when k = i -> compression uf i sommets
  | k -> trouve_rec (i::sommets) uf k
and compression uf i = function
  | [] -> i
  | h::t -> uf.pere.(h) <- i ; compression uf i t
;;

(*Question 4*)
let unir uf i j =
  let p1 = trouver uf i and p2 = trouver uf j in
    if p1 <> p2 then (
      if uf.rang.(p1) < uf.rang.(p2) then
        uf.pere.(p1) <- p2
      else (
        if uf.rang.(p1) > uf.rang.(p2) then
          uf.pere.(p2) <- p1
        else (
          uf.pere.(p1) <- p2 ;
          uf.rang.(p2) <- uf.rang.(p2) + 1
        )
      )
    )
;;

(*La fonction trouver est de complexité O(n), unir est en O(n) également*)

let composantes_connexe grph =
  let n = Array.length grph and result = ref 0 in
    let uf = initialise n and seen = Array.make n false in
      let rec uf_join i = function
        | [] -> ()
        | h::t -> unir uf i h ; uf_join i t
      in
        for i = 0 to n - 1 do
          uf_join i grph.(i)
        done ;
        for i = 0 to n - 1 do
          let pere = trouver uf i in
            if not seen.(pere) then (
              seen.(pere) <- true ;
              result := !result + 1
            )
        done ;
        !result
;;

(*Question 7

L'algorithme de Kruskal correspond à une boucle sur l'ensemble des arêtes, qui est fini, donc l'algorithme termine.
*)

(*Question 8*)

let rec _get_extreme cur_min cur_max = function
  | [] -> cur_min, cur_max
  | (a, b, _)::t -> _get_extreme (min a (min cur_min b)) (max a (max cur_max b)) t
;;

let get_extreme = function
  | [] -> 0, 0
  | (a, b, _)::t -> _get_extreme (min a b) (max a b) t
;;

let rec fill_mat grph_min mat = function
  | [] -> mat
  | (a, b, _)::t -> mat.(a - grph_min) <- true ; mat.(b - grph_min) <- true ; fill_mat grph_min mat t
;;

let count_true mat =
  let result = ref 0 in
    for i = 0 to Array.length mat - 1 do
      if mat.(i) then
        result := !result + 1
    done ;
    !result
;;

let count_sommets lst =
  let grph_min, grph_max = get_extreme lst in
    count_true (fill_mat grph_min (Array.make (grph_max - grph_min + 1) false) lst)
;;

(*Question 9*)

let compare_arete (a, b, c) (d, e, f) = c - f ;;

let rec kruskal lst =
  let sorted = List.sort compare_arete lst and n = count_sommets lst in
    kruskal_rec (initialise n) [] sorted
and kruskal_rec uf result = function
  | [] -> result
  | (a, b, p)::t when trouver uf a = trouver uf b -> kruskal_rec uf result t
  | (a, b, p)::t -> unir uf a b ;
    kruskal_rec uf ((a, b, p)::result) t
;;

(*La complexité de kruskal est en O(n^3) car il y a moins de O(n²) arêtes*)

(*Labyrinthe*)
Random.self_init () ;;

let rec une_composante_connexe uf n = _une_rec uf (trouver uf 0) (n * n - 1)
and _une_rec uf pere = function
  | -1 -> true
  | i when trouver uf i = pere -> _une_rec uf pere (i - 1)
  | _ -> false
;;

let find_next i j = function
  | 0 -> i - 1, j (*N*)
  | 1 -> i, j + 1 (*E*)
  | 2 -> i + 1, j (*S*)
  | 3 -> i, j - 1 (*W*)
  | _ -> failwith "find_next: Wrong direction"
;;

let to_lin n i j = n * i + j ;;

let reverse = function (*Reverse the direction*)
  | 0 -> 2
  | 1 -> 3
  | 2 -> 0
  | 3 -> 1
  | _ -> failwith "reverse: Wrong direction"
;;

let gen_lab n =
  let result = Array.make_matrix n n [||] in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        result.(i).(j) <- [|false; false; false; false|]
      done ;
    done ;
    result
;;

let create_labyrinthe n = (*Create a n x n labyrinth.*)
  (*true si il n'y a pas de mur dans cette direction - NSEW*)
  let result = gen_lab n and uf = initialise (n * n) in
    while not (une_composante_connexe uf n) do
      let i = Random.int n and j = Random.int n and dir = Random.int 4 in
        if not ((dir = 0 && i = 0) || (dir = 1 && j = n - 1) || (dir = 2 && i = n - 1) || (dir = 3 && j = 0)) then
          let i', j' = find_next i j dir in
            if trouver uf (to_lin n i j) <> trouver uf (to_lin n i' j') then (
              result.(i).(j).(dir) <- true ;
              result.(i').(j').(reverse dir) <- true ;
              unir uf (to_lin n i j) (to_lin n i' j')
            )
    done ;
    result
;;

let print_middle lab n i j = print_string (
  (if lab.(i).(j).(3) then " " else "#") ^ "   " ^ (if j = n - 1 then "#" else "")
)
;;

let print_upper lab n i j = print_string (
  "##" ^ (if lab.(i).(j).(0) then " " else "#") ^ "#" ^ (if j = n - 1 then "#" else "")
)

let print_neutral n =
  print_string (String.init (4 * n + 1) (fun x -> if x mod 4 = 0 then '#' else ' '))
;;

let print_labyrinthe lab =
  let n = Array.length lab in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        print_upper lab n i j
      done ;
      print_newline () ;
      print_neutral n ;
      print_newline () ;
      for j = 0 to n - 1 do
        print_middle lab n i j
      done ;
      print_newline () ;
      print_neutral n ;
      print_newline () ;
    done ;
    print_string (String.make (4 * n + 1) '#') ;
    print_newline () ;
;;

let do_lab n = print_labyrinthe (create_labyrinthe n) ;;
