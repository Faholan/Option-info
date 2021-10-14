(*
Premier TD noté

Copyright (C) 2021  Faholan <https://github.com/Faholan>
*)

let myname = "Florian Chivé" ;;

let u0 = 115 + 24 + 07 ;;

(*Question 1*)
let rec u_n cur = function
    | 0 -> cur
    | n -> u_n ((15091 * cur) mod 64007) (n - 1)
;;

let fill_un u0 length =
    (*An array with u_n values*)
    let res = Array.make length u0 in
        for i = 1 to length - 1 do
            res.(i) <- (15091 * res.(i - 1)) mod 64007
        done ;
        res
;;

let un_values = fill_un u0 10000 ;;

(*Question 2*)
let rec create_sequence k l =
    (*Create the Sk,l sequence*)
    let result = Bytes.make l '0' in
        for i = 0 to l - 1 do
            Bytes.set result i (getchar (un_values.(i + k)))
        done ;
        result
and getchar i = match i mod 4 with
    | 0 -> 'A'
    | 1 -> 'T'
    | 2 -> 'G'
    | _ -> 'C'
;;

type arbre =
    | Feuille of char
    | Racine of arbre list
    | Noeud of char * arbre list
;;

(*Liste de séquence*)
let liste_sequence k n l =
    (*Lk,n,l*)
    let rec make_list = function
        | i when i = n-> []
        | i -> (create_sequence (k + i * l) l)::(make_list (i + 1))
    in make_list 0
;;

let rec create_arbre elem_list =
    create_rec (Racine([])) elem_list
and create_rec arb = function
    | [] -> arb
    | h::t when Bytes.length h = 0 -> create_rec arb t
    | h::t -> create_rec (inserter h arb) t
and inserter elem = function
    | Racine(elems) -> Racine(insert_in_list elem 0 elems)
    | _ -> failwith "Feuille dans insert_rec"
and insert_in_list elem index = function
    | Feuille(char)::t when char=(Bytes.get elem index) && index=(Bytes.length elem - 1) -> Feuille(char)::t
    | Noeud(char, elems)::t when char=(Bytes.get elem index) && index < (Bytes.length elem - 1) -> Noeud(char, insert_in_list elem (index + 1) elems)::t
    | Racine(_)::t -> failwith "Missplaced root"
    | h::t -> h::(insert_in_list elem index t)
    | [] when index=(Bytes.length elem - 1) -> [Feuille((Bytes.get elem index))]
    | [] -> [Noeud((Bytes.get elem index), insert_in_list elem (index + 1) [])]
;;

let count_letter letter k n l =
    let rec counter = function
        | Racine(elems) -> let noeuds, lettres = counter_list elems in
            (noeuds + 1), lettres
        | Feuille(char) when char=letter -> 1, 1
        | Feuille(_) -> 1, 0
        | Noeud(char, elems) when char=letter -> let noeuds, lettres = counter_list elems in
            noeuds + 1, lettres + 1
        | Noeud(_, elems) -> let noeuds, lettres = counter_list elems in
            noeuds + 1, lettres
    and counter_list = function
        | h::t -> let noeuds_h, lettres_h = counter h and noeuds_t, lettres_t = counter_list t in
            noeuds_h + noeuds_t, lettres_h + lettres_t
        | [] -> 0, 0
    in counter (create_arbre (liste_sequence k n l))
;;

(*Question 4*)
(*On utilise l'algorithme de tri rapide*)
let rec lexico_compare b1 b2 = rec_compare b1 b2 0
(*b1 < b2*)
and rec_compare b1 b2 = function
    | i when i = Bytes.length b1 -> Bytes.length b2 > i
    | i when i = Bytes.length b2 -> false
    | i when lexico_less (Bytes.get b1 i) (Bytes.get b2 i) -> true
    | i when (Bytes.get b1 i) = (Bytes.get b2 i) -> rec_compare b1 b2 (i + 1)
    | _ -> false
and lexico_less c1 = function
    | 'A' -> false
    | 'T' -> c1 = 'A'
    | 'G' -> c1 = 'A' || c1 = 'T'
    | _ -> c1 <> 'C'
;;


let echange i j tab =
  let elem = tab.(i) in
    tab.(i) <- tab.(j) ;
    tab.(j) <- elem
;;

let separation tab i1 i2 =
  let pos_pivot = ref i1 and pivot = ref tab.(i1) in
    for i = (i1 + 1) to i2 do
      if lexico_compare tab.(i) !pivot then (
          echange i !pos_pivot tab ;
          pos_pivot := !pos_pivot + 1 ;
          echange i !pos_pivot tab
        )
    done ;
    !pos_pivot
;;

let tri_rapide tab =
  let rec tri_recursif i1 i2 =
    if i1 >= i2 || i1 < 0 then
      ()
    else
      let pivot = separation tab i1 i2 in
        tri_recursif i1 (pivot - 1) ;
        tri_recursif (pivot + 1) i2
  in tri_recursif 0 (Array.length tab - 1)
;;

let get_pos_elem pos k n l =
    let sequences = Array.of_list (liste_sequence k n l) in
        tri_rapide sequences ;
        sequences
;;

let counter index length tab =
    let result = ref 0 and elem = Bytes.sub (tab.(index)) 0 length in
        for i = index + 1 to (Array.length tab - 1) do
            if Bytes.sub (tab.(i)) 0 length = elem then
                result := !result + 1
        done ;
        !result + 1
;;

let plus_long_prefixe commun k n l =
    let plus_long = ref (Bytes.make l 'C') and sequences = Array.of_list (liste_sequence k n l) and found = ref false in
        let rec find_prefix length =
            for i = 0 to (Array.length sequences - 1) do
                if counter i length sequences >= commun then (
                    if !found then (
                        if lexico_compare (Bytes.sub (sequences.(i)) 0 length) !plus_long then
                            plus_long := Bytes.sub (sequences.(i)) 0 length
                    ) else
                        plus_long := Bytes.sub (sequences.(i)) 0 length ;
                        found := true
                )
            done ;
            if !found then
                !plus_long
            else
                if length = 0 then
                    Bytes.make 0 'a'
                else
                    find_prefix (length - 1)
        in find_prefix l
;;

(*Question 6*)
let rec find_shorter k n l =
    find_short_length (Array.of_list (liste_sequence k n l)) 1
and find_short_length sequences length =
    let result = find_rec_prefix sequences (gen_sequences length) in
        if result <> (Bytes.of_string "") then
            result
        else
            find_short_length sequences (length + 1)
and find_rec_prefix sequences = function
    | h::_ when prefix_not_found h sequences 0 -> h
    | _::t -> find_rec_prefix sequences t
    | [] -> Bytes.of_string ""
and prefix_not_found prefix sequences = function
    | i when i = Array.length sequences -> true
    | i when prefix = Bytes.sub (sequences.(i)) 0 (Bytes.length prefix) -> false
    | i -> prefix_not_found prefix sequences (i + 1)
and gen_sequences = function
    (*Generate all sequences of the given length*)
    | 1 -> [Bytes.of_string "A"; Bytes.of_string "T"; Bytes.of_string "G"; Bytes.of_string "C"]
    | n -> let preced = gen_sequences (n - 1) in
        (appender "A" preced) @ (appender "T" preced) @ (appender "G" preced) @ (appender "C" preced)
and appender chr = function
    | [] -> []
    | h::t -> (Bytes.of_string (chr ^ (Bytes.to_string h)))::(appender chr t)
;;

let rec create_arbre_suffixe k l =
    create_rec (Racine([])) (get_suffix (create_sequence k l) 0)
and create_rec arb = function
    | [] -> arb
    | h::t when Bytes.length h = 0 -> create_rec arb t
    | h::t -> create_rec (inserter h arb) t
and inserter elem = function
    | Racine(elems) -> Racine(insert_in_list elem 0 elems)
    | _ -> failwith "Feuille dans insert_rec"
and insert_in_list elem index = function
    | Feuille(char)::t when char=(Bytes.get elem index) && index=(Bytes.length elem - 1) -> Feuille(char)::t
    | Noeud(char, elems)::t when char=(Bytes.get elem index) && index < (Bytes.length elem - 1) -> Noeud(char, insert_in_list elem (index + 1) elems)::t
    | Racine(_)::t -> failwith "Missplaced root"
    | h::t -> h::(insert_in_list elem index t)
    | [] when index=(Bytes.length elem - 1) -> [Feuille((Bytes.get elem index))]
    | [] -> [Noeud((Bytes.get elem index), insert_in_list elem (index + 1) [])]
and get_suffix base = function
    | i when i = Bytes.length base -> [Bytes.of_string "X"]
    | i -> (completer (Bytes.sub base i (Bytes.length base - i)))::(get_suffix base (i + 1))
and completer b =
    let result = Bytes.make (Bytes.length b + 1) 'X' in
        for i = 0 to (Bytes.length b - 1) do
            Bytes.set result i (Bytes.get b i)
        done ;
        result
;;

let count_letter_suffixe letter k n l =
    let rec counter = function
        | Racine(elems) -> let noeuds, lettres = counter_list elems in
            (noeuds + 1), lettres
        | Feuille(char) when char=letter -> 1, 1
        | Feuille(_) -> 1, 0
        | Noeud(char, elems) when char=letter -> let noeuds, lettres = counter_list elems in
            noeuds + 1, lettres + 1
        | Noeud(_, elems) -> let noeuds, lettres = counter_list elems in
            noeuds + 1, lettres
    and counter_list = function
        | h::t -> let noeuds_h, lettres_h = counter h and noeuds_t, lettres_t = counter_list t in
            noeuds_h + noeuds_t, lettres_h + lettres_t
        | [] -> 0, 0
    in counter (create_arbre_suffixe k n l)
;;


let rec shortest_subsequence k l =
    fetch_shortest_subsequence (create_sequence k l) 1 (Bytes.make l 'C')
and fetch_shortest_subsequence sequence length default =
    let result = fetch_shortest_sub_from sequence default (Array.of_list (gen_sequences length)) in
        if result = default then
            fetch_shortest_subsequence sequence (length + 1) default
        else
            result
and fetch_shortest_sub_from sequence default source =
    let found = ref false and result = ref default in
        for i = 0 to (Array.length source - 1) do
            if not_sub source.(i) sequence then
                if !found then (
                    if lexico_compare source.(i) !result then
                        result := source.(i)
                ) else (
                    found := true ;
                    result := source.(i)
                )
        done ;
        !result
and not_sub sub sequence =
    let rec not_sub_rec = function
        | i when i + (Bytes.length sub) > (Bytes.length sequence) -> true
        | i when sub = Bytes.sub sequence i (Bytes.length sub) -> false
        | i -> not_sub_rec (i + 1)
    in not_sub_rec 0
;;
