(* Chivé Florian 2407 *)
(*
DM décision binaire

Copyright (C) 2021 Faholan <https://github.com/Faholan>
*)


(*Partie I - Définition*)
let u0 = 2407 ;;

(*Partie II - Génération aléatoire de formules logiques*)

(*Question 1*)

let u_cache = ref [|u0|] ;; (*Cache the values of un for access in O(1) instead of O(n)*)

let un_cache = ref u0 ;; (*Verify that the cache is valid*)

let un n =
  if u0 <> !un_cache then (
    un_cache := u0;
    u_cache := [|u0|]
  ) ; (*Cache validation*)
  if n < Array.length !u_cache then
    !u_cache.(n) (*Cache hit*)
  else (*Cache miss -> increase cache size*)
    let result = Array.make (n + 1) 0 in
      for i = 0 to Array.length (!u_cache) - 1 do
        result.(i) <- !u_cache.(i) (*Copy cache*)
      done ;
      for i = Array.length (!u_cache) to n do (*Fill new cache*)
        result.(i) <- 15091 * result.(i - 1) mod 64007
      done ;
      u_cache := result ;
      result.(n)
;;

(*Question 2*)

type formule =
  | TRUE
  | FALSE
  | Ou of formule * formule
  | Et of formule * formule
  | Neg of formule
  | Var of int
;;

let rec eval x = function
  | TRUE -> true
  | FALSE -> false
  | Ou(f, g) -> eval x f || eval x g
  | Et(f, g) -> eval x f && eval x g
  | Neg(f) -> not (eval x f)
  | Var(i) -> x.(i - 1)
;;

let rec f n m p = _f_rec n m p 1
and _f_rec n m p = function
  | i when i = m -> t n i p
  | i -> Ou(t n i p, _f_rec n m p (i + 1))
and t n i p = _t_rec n i p 1
and _t_rec n i p = function
  | j when j = n -> delta j ((i - 1) * n + j) p
  | j -> Et(delta j ((i - 1) * n + j) p, _t_rec n i p (j + 1))
and delta j k p = match un k with
  | v when 0 <= v && float_of_int v < 32003.5 *. p -> Neg(Var(j))
  | v when 32003.5 *. p <= float_of_int v && float_of_int v < 64007. *. p -> Var(j)
  | _ -> TRUE
;;

let v n =
  let result = Array.make n true in
    result.(0) <- false ;
    result.(1) <- false ;
    result
;;

(*Question 3*)

let simplify f =
  let rec _simplify = function (*Simplification - si ça a été simplifié*)
    | TRUE -> TRUE, false
    | FALSE -> FALSE, false
    | Ou(TRUE, _) -> TRUE, true
    | Ou(_, TRUE) -> TRUE, true
    | Ou(FALSE, f) -> let f', _ = _simplify f in
      f', true
    | Ou(f, FALSE) -> let f', _ = _simplify f in
      f', true
    | Ou(f1, f2) when f1 = f2 -> f1, true
    | Ou(f1, f2) -> let f1', c1 = _simplify f1 in
      let f2', c2 = _simplify f2 in
        Ou(f1', f2'), c1 || c2
    | Et(FALSE, _) -> FALSE, true
    | Et(_, FALSE) -> FALSE, true
    | Et(TRUE, f) -> let f', _ = _simplify f in
      f', true
    | Et(f, TRUE) -> let f', _ = _simplify f in
      f', true
    | Et(f1, f2) when f1 = f2 -> f1, true
    | Et(f1, f2) -> let f1', c1 = _simplify f1 in
      let f2', c2 = _simplify f2 in
        Et(f1', f2'), c1 || c2
    | Neg(TRUE) -> FALSE, true
    | Neg(FALSE) -> TRUE, true
    | Neg(Neg(f)) -> f, true
    | Neg(formula) -> let formula', c = _simplify formula in
      Neg(formula'), c
    | Var(n) -> Var(n), false
  in
    let f' = ref f and continue = ref true in
      while !continue do  (*Simplify while the formula can be simplified*)
        let f'', simplified = _simplify !f' in
          if simplified then
            f' := f''
          else
            continue := false
      done ;
      !f'
;;

let replace n v f =
  let rec _replace = function
    | TRUE -> TRUE
    | FALSE -> FALSE
    | Ou(f1, f2) -> Ou(_replace f1, _replace f2)
    | Et(f1, f2) -> Et(_replace f1, _replace f2)
    | Neg(f) -> Neg(_replace f)
    | Var(i) -> if i = n then v else Var(i)
  in simplify (_replace f)
;;

let rec len_forme_normale_disjonctive = function
  (*Forme Ou(f1, Ou(f2, Ou(f3, ...)))*)
  | Ou(f1, f2) -> len_forme_normale_disjonctive f1 + len_forme_normale_disjonctive f2
  | _ -> 1
;;

(*Question 4*)
let val_num n k =
  let result = Array.make n false in
    let rec _val_rec = function
      | 0 -> ()
      | i -> _increment_result (n - 1) ; _val_rec (i - 1)
    and _increment_result i = match result.(i) with
      | false -> result.(i) <- true
      | true -> result.(i) <- false; _increment_result (i - 1)
    in
      _val_rec k ;
      result
;;

(*Question 5*)
let first_val n = Array.make n false ;;

let next_val arr =
  let rec _increment_result i = match arr.(i) with
    | false -> arr.(i) <- true
    | true -> arr.(i) <- false; _increment_result (i - 1)
  in _increment_result (Array.length arr - 1)
;;  (*This is more efficient than using val_num*)

let anysat n k p =
  let formula = simplify (f n k p) and result = first_val n in
    let rec _anysat_rec () = match eval result formula with
      | true -> result
      | false -> next_val result; _anysat_rec ()
    in _anysat_rec ()
;;

(*Question 6*)
let is_final arr = (*Test if the valuation is the last n-valuation*)
  let rec _is_final i =
    if i = Array.length arr then
      true
    else
      match arr.(i) with
        | true -> _is_final (i + 1)
        | false -> false
  in _is_final 0
;;

let numsat n k p =
  let formula = simplify (f n k p) and curval = first_val n in
    let result = ref (if eval curval formula then 1 else 0) in
      while not (is_final curval) do
        next_val curval ;
        if eval curval formula then
          result := !result + 1
        done ;
      !result
;;


(*Partie III - Tables de hachage*)

(*Question 7*)
let hn p =
  let rec _hash_rec = function
    | [] -> 0
    | h::t -> ((un h) + _hash_rec t) mod p
in _hash_rec
;;

let new_table p = Array.make p [] ;;

let put key elem table =
  let h = hn (Array.length table) key in
    table.(h) <- (key, elem)::table.(h)
;;

let get key default table =
  let rec _get_rec = function
    | [] -> default
    | (k, e)::t -> if k = key then e else _get_rec t
  in
    let h = hn (Array.length table) key in
      _get_rec table.(h)
;;

let test_table p t =
  let table = new_table p in
    for k = 1 to t do
      put [un (2 * k); un (2 * k + 1)] k table
    done ;
    table
;;

let min_avg_max table = (*Get the minimum, average & maximum list length of a hash table*)
  let cur_min = ref (List.length table.(0)) and cur_sum = ref 0 and cur_max = ref 0 in
    for i = 0 to Array.length table - 1 do
      let len = List.length table.(i) in
        cur_min := min !cur_min len ;
        cur_sum := !cur_sum + len ;
        cur_max := max !cur_max len
    done ;
    !cur_min, !cur_sum / Array.length table, !cur_max
;;


(*Partie 4 - Diagramme de décision binaire*)
(*Question 9*)
type noeud =
  | Feuille of bool
  | Noeud of int * int * int * int  (*identifier - variable - left - right*)
;;

type decision = {mutable arr: noeud array; mutable length: int; table: (int list * noeud) list array} ;;

let empty_decision () = {arr=[|Feuille(false); Feuille(true)|]; length=2; table=new_table 10000} ;;

let find_node var id_left id_right abr =
  let extracted = get [var; id_left; id_right] (Feuille(false)) abr.table in
    if extracted = Feuille(false) then
      Noeud(abr.length, var, id_left, id_right), true (*N'existe pas déjà*)
    else
      extracted, false (*Existe déjà*)
;;

let get_id = function  (*Get the identifier of a node*)
  | Feuille(false) -> 0
  | Feuille(true) -> 1
  | Noeud(id, _, _, _) -> id
;;

let get_var = function  (*Get the variable associated with a given node*)
  | Feuille(_) -> -1
  | Noeud(_, var, _, _) -> var
;;

let insert_node var id_left id_right abr =
  if id_left = id_right then
    id_left  (*Ne pas insérer de noeuds triviaux*)
  else
    let new_node, to_insert = find_node var id_left id_right abr in
      if to_insert then ((*Création d'un nouveau noeud*)
        if abr.length = Array.length abr.arr then (
          let new_arr = Array.make (2 * abr.length) (Feuille(false)) in
            for i = 0 to abr.length - 1 do
              new_arr.(i) <- abr.arr.(i)
            done ;
            abr.arr <- new_arr
        );
        put [var; id_left; id_right] new_node abr.table ;
        abr.arr.(abr.length) <- new_node ;
        abr.length <- abr.length + 1
      ) ;
      get_id new_node
;;

let rec construct_decision formula =
  let result = empty_decision () in
    let _construct_rec = _construct_rec result 1 (simplify formula) in
      result
and _construct_rec abr i = function
  | FALSE -> 0 (*id du noeud*)
  | TRUE -> 1
  | f -> let id_left = _construct_rec abr (i + 1) (replace i FALSE f) in
    let id_right = _construct_rec abr (i + 1) (replace i TRUE f) in
      insert_node i id_left id_right abr
;;

let nb_noeud abr = abr.length ;;

let nb_noeud_formula formula = nb_noeud (construct_decision formula) ;;

(*Question 10*)
let find_root abr =  (*Find the root node of a sorted deision tree*)
  let result = ref 2 and cur_var = ref (get_var abr.arr.(2)) in
    for i = 2 to abr.length - 1 do
      if get_var abr.arr.(i) < !cur_var then
        cur_var := get_var abr.arr.(i) ;
        result := i
    done ;
    !result
;;

let anysat_decision n abr =  (*Solve anysat for a given decision tree*)
  let result = Array.make n false in
    let rec _anysat_rec i = match abr.arr.(i) with
      | Feuille(false) -> false
      | Feuille(true) -> true
      | Noeud(_, var, id_left, id_right) ->
        if _anysat_rec id_left then
          true
        else
          if _anysat_rec id_right then (
            result.(var - 1) <- true;
            true
          ) else
            false
    in ignore (_anysat_rec (find_root abr));
      result
;;

let anysat_decision_f n k p = anysat_decision n (construct_decision (f n k p)) ;;

let rec pow_2_fast = function
  | 0 -> 1
  | 1 -> 2
  | n -> (pow_2_fast (n / 2)) * (pow_2_fast ((n + 1) / 2))
;;

let numsat_decision n abr =  (*Solve numsat for a given decision tree*)
  let rec _allsat_rec var i = match abr.arr.(i) with
    | Feuille(false) -> 0
    | Feuille(true) -> pow_2_fast (n - var)
    | Noeud(_, var', id_left, id_right) -> (pow_2_fast (var' - var - 1)) * (_allsat_rec var' id_left + _allsat_rec var' id_right)
in let root = find_root abr in
  pow_2_fast (get_var abr.arr.(root) - 1) * _allsat_rec (get_var abr.arr.(root)) root
;;

let numsat_decision_f n k p = numsat_decision n (construct_decision (f n k p)) ;;

(*Question 12*)
let linear_coords i j n = (i - 1) * n + j ;; (*Get the identifier of xi,j*)

let gen_formula_row i n =
  (*Generate the condition for one line*)
  let rec _gen_formula_row = function
    | j when j = n -> _gen_neg j 1
    | j -> Ou(_gen_neg j 1, _gen_formula_row (j + 1))
    and _gen_neg j = function
    | k when k = j && k = n -> Var(linear_coords i k n)
    | k when k = n -> Neg(Var(linear_coords i k n))
    | k when k = j -> Et(Var(linear_coords i k n), _gen_neg j (k + 1))
    | k -> Et(Neg(Var(linear_coords i k n)), _gen_neg j (k + 1))
  in Ou(_gen_formula_row 1, _gen_neg 0 1)  (*1 ou 0 dame par ligne*)
;;

let gen_formula_rows n =
  let rec _gen_formula_rows = function
    | i when i = n -> gen_formula_row i n
    | i -> Et(gen_formula_row i n, _gen_formula_rows (i + 1))
  in _gen_formula_rows 1
;;

let gen_formula_column j n =
  (*Generate the condition for one column*)
  let rec _gen_formula_column = function
    | i when i = n -> _gen_neg i 1
    | i -> Ou(_gen_neg i 1, _gen_formula_column (i + 1))
  and _gen_neg i = function
    | k when k = i && k = n -> Var(linear_coords k j n)
    | k when k = n -> Neg(Var(linear_coords k j n))
    | k when k = i -> Et(Var(linear_coords k j n), _gen_neg i (k + 1))
    | k -> Et(Neg(Var(linear_coords k j n)), _gen_neg i (k + 1))
  in Ou(_gen_formula_column 1, _gen_neg 0 1) (*1 ou 0 dame par colonne*)
;;

let gen_formula_columns n =
  let rec _gen_formula_columns = function
    | j when j = n -> gen_formula_column j n
    | j -> Et(gen_formula_column j n, _gen_formula_columns (j + 1))
  in _gen_formula_columns 1
;;

let gen_formula_diagonal_sup i n =
  (*Generate the formula for one superior diagonal*)
  let rec _gen_formula_diagonal_sup = function
    | j when i + j = n + 1 -> _gen_neg j 1
    | j -> Ou(_gen_neg j 1, _gen_formula_diagonal_sup (j + 1))
  and _gen_neg j = function
    | k when k = j && i + k = n + 1 -> Var(linear_coords k (i + k - 1) n)
    | k when i + k = n + 1-> Neg(Var(linear_coords k (i + k - 1) n))
    | k when k = j -> Et(Var(linear_coords k (i + k - 1) n), _gen_neg j (k + 1))
    | k -> Et(Neg(Var(linear_coords k (i + k - 1) n)), _gen_neg j (k + 1))
  in Ou(_gen_formula_diagonal_sup 1, _gen_neg 0 1) (*1 ou 0 dame par diagonale supérieure*)
;;

let gen_formula_diagonal_inf i n =
  (*Generate the formula for one inferior diagonal*)
  let rec _gen_formula_diagonal_inf = function
    | j when i + j = n + 1 -> _gen_neg j 1
    | j -> Ou(_gen_neg j 1, _gen_formula_diagonal_inf (j + 1))
  and _gen_neg j = function
    | k when k = j && i + k = n + 1 -> Var(linear_coords (i + k - 1) k n)
    | k when i + k = n + 1 -> Neg(Var(linear_coords (i + k - 1) k n))
    | k when k = j -> Et(Var(linear_coords (i + k - 1) k n), _gen_neg j (k + 1))
    | k -> Et(Neg(Var(linear_coords (i + k - 1) k n)), _gen_neg j (k + 1))
  in Ou(_gen_formula_diagonal_inf 1, _gen_neg 0 1) (*1 ou 0 dame par diagonale inférieure*)
;;

let gen_formula_diagonal_rev_sup i n =
  (*Generate the formula for one reverse superior diagonal*)
  let rec _gen_formula_diagonal_rev_sup = function
    | j when i - j = 1 -> _gen_neg j 0
    | j -> Ou(_gen_neg j 0, _gen_formula_diagonal_rev_sup (j + 1))
  and _gen_neg j = function
    | k when k = j && i - k = 1 -> Var(i)
    | k when i - k = 1 -> Neg(Var(i))
    | k when k = j -> Et(Var(linear_coords (i - k) (k + 1) n), _gen_neg j (k + 1))
    | k -> Et(Neg(Var(linear_coords (i - k) (k + 1) n)), _gen_neg j (k + 1))
  in Ou(_gen_formula_diagonal_rev_sup 0, _gen_neg (-1) 0)
;;

let gen_formula_diagonal_rev_inf i n =
  (*Generate the formula for one reverse inferior diagonal*)
  let rec _gen_formula_diagonal_rev_inf = function
    | j when i + j = n -> _gen_neg j 0
    | j -> Ou(_gen_neg j 0, _gen_formula_diagonal_rev_inf (j + 1))
  and _gen_neg j = function
    | k when k = j && i + k = n -> Var(linear_coords (n - k) (i + k) n)
    | k when i + k = n -> Neg(Var(linear_coords (n - k) (i + k) n))
    | k when k = j -> Et(Var(linear_coords (n - k) (i + k) n), _gen_neg j (k + 1))
    | k -> Et(Neg(Var(linear_coords (n - k) (i + k) n)), _gen_neg j (k + 1))
  in Ou(_gen_formula_diagonal_rev_inf 0, _gen_neg (-1) 0)
;;

let gen_formula_diagonals n =  (*This is absolutely inefficient, but should be correct*)
  let rec _gen_formula_diagonals = function
    | i when i = n -> Et(gen_formula_diagonal_sup i n, Et(gen_formula_diagonal_inf i n, Et(gen_formula_diagonal_rev_sup i n, gen_formula_diagonal_rev_inf i n)))
    | i -> Et(gen_formula_diagonal_sup i n, Et(gen_formula_diagonal_inf i n, Et(gen_formula_diagonal_rev_sup i n, Et(gen_formula_diagonal_rev_inf i n, _gen_formula_diagonals (i + 1)))))
  in _gen_formula_diagonals 1
;;

let gen_exactly_n_dames n =
  let pos_max = n * n in
    let rec _gen_exactly num = function
      | i when i = pos_max && num = 0 -> Neg(Var(i))
      | i when i = pos_max -> Var(i)
      | i when i + num = pos_max + 1 -> Et(Var(i), _gen_exactly (num - 1) (i + 1))
      | i when num = 0 -> Et(Neg(Var(i)), _gen_exactly (num - 1) (i + 1))
      | i -> Ou(Et(Var(i), _gen_exactly (num - 1) (i + 1)), Et(Neg(Var(i)), _gen_exactly (num) (i + 1)))
    in _gen_exactly n 1
;;

let gen_formula_dames n =
  simplify (Et(gen_formula_rows n, Et(gen_formula_columns n, Et(gen_formula_diagonals n, gen_exactly_n_dames n))))
;;

let num_sol_dames n = numsat_decision (n * n) (construct_decision (gen_formula_dames n)) ;; (*This is inefficient as hell. Takes too much RAM & CPU bruh*)

(*Here is real code that really finds the number of solutions to this problem*)
let can_place n grid i j =
  let rec check_row_column = function
    | k when k = n -> check_diagonal1 (-min i j)
    | k when grid.(i).(k) || grid.(k).(j) -> false
    | k -> check_row_column (k + 1)
  and check_diagonal1 = function
    | k when k + i = n || k + j = n -> check_diagonal2 (-min i (n - j - 1))
    | k when grid.(k + i).(k + j) -> false
    | k -> check_diagonal1 (k + 1)
  and check_diagonal2 = function
    | k when k + i = n || j - k = -1 -> true
    | k when grid.(k + i).(j - k) -> false
    | k -> check_diagonal2 (k + 1)
  in check_row_column 0
;;

let numsat_dames n =
  let grid = Array.make_matrix n n false and history = Stack.create () and result = ref 0 in
    let rec place i j =
      if i = n then (
        result := !result + 1 ;
        backtrack ()
      )
      else
        if j = n then
          backtrack ()
        else
          if can_place n grid i j then (
            Stack.push (i, j) history ;
            grid.(i).(j) <- true ;
            place (i + 1) 0
          )
          else
            place i (j + 1)
    and backtrack () =
      if Stack.is_empty history then
        !result
      else (
        let i, j = Stack.pop history in
          grid.(i).(j) <- false ;
          place i (j + 1)
      )
    in place 0 0
;;

(*Réponses aux questions*)
let string_of_bool_arr arr =
  let result = ref ("[|" ^ (if arr.(0) then "1" else "0")) in
    for i = 1 to Array.length arr - 1 do
      if arr.(i) then
        result := !result ^ "; 1"
      else
        result := !result ^ "; 0"
      done ;
    !result ^ "|]"
;;

let string_of_int_triple (n1, n2, n3) =
  "(" ^ string_of_int n1 ^ ", " ^ string_of_int n2 ^ ", " ^ string_of_int n3 ^ ")"
;;

let print_answers () =
  print_string ("u0 = " ^ string_of_int u0 ^ "\n") ;
  print_string "Question 1\n" ;
  print_string ("u10 = " ^ string_of_int (un 10) ^ " ; u100 = " ^ string_of_int (un 100) ^ " ; u1000 = " ^ string_of_int (un 1000) ^ "\n") ;
  print_string "Question 2\n" ;
  print_string ("F5,1,1/2(Vn) = " ^ string_of_bool (eval (v 5) (f 5 1 0.5)) ^ " ; F10,2,1/3(Vn) = " ^ string_of_bool (eval (v 10) (f 10 2 (1./.3.))) ^ " ; F20,20,1/3(Vn) = " ^ string_of_bool (eval (v 20) (f 20 20 (1./.3.))) ^ "\n") ;
  print_string "Question 3\n" ;
  print_string ("len (replace 1 FALSE (f 5 1 1/2)) = " ^ string_of_int (len_forme_normale_disjonctive (replace 1 FALSE (f 5 1 (1./.2.)))) ^ " ; len (replace 1 FALSE (f 10 2 1/3)) = " ^ string_of_int (len_forme_normale_disjonctive (replace 1 FALSE (f 10 2 (1./.3.)))) ^ " ; len (replace 1 FALSE (f 20 20 1/3)) = " ^ string_of_int (len_forme_normale_disjonctive (replace 1 FALSE (f 20 20 (1./.3.)))) ^ "\n") ;
  print_string "Question 4\n" ;
  print_string ("val_num 5 (u1000 mod 32) = " ^ string_of_bool_arr (val_num 5 ((un 1000) mod 32)) ^ " ; val_num 8 (u2000 mod 256) = " ^ string_of_bool_arr (val_num 8 ((un 2000) mod 256)) ^ " ; val_num 12 (u3000 mod 4096) = " ^ string_of_bool_arr (val_num 12 ((un 3000) mod 4096)) ^ "\n") ;
  print_string "Question 5\n" ;
  print_string ("anysat 5 1 1/2 = " ^ string_of_bool_arr (anysat 5 1 (1./.2.)) ^ " ; anysat 10 2 1/3 = " ^ string_of_bool_arr (anysat 10 2 (1./.3.)) ^ " ; anysat 20 20 1/3 = " ^ string_of_bool_arr (anysat 20 20 (1./.3.)) ^ "\n") ;
  print_string "Question 6\n" ;
  print_string ("numsat 5 1 1/2 = " ^ string_of_int (numsat 5 1 (1./.2.)) ^ " ; numsat 10 2 1/3 = " ^ string_of_int (numsat 10 2 (1./.3.)) ^ " ; numsat 20 20 1/3 = " ^ string_of_int (numsat 20 20 (1./.3.)) ^ "\n") ;
  print_string "Question 7\n" ;
  print_string ("hn 100 [u0, u10, u20] = " ^ string_of_int (hn 100 [un 0 ; un 10 ; un 20]) ^ " ; hn 100 [u10, u20, u30] = " ^ string_of_int (hn 100 [un 10 ; un 20 ; un 30]) ^ " ; hn 100 [u0, u10, u20, u30] = " ^ string_of_int (hn 100 [un 0 ; un 10 ; un 20 ; un 30]) ^ "\n") ;
  print_string "Question 8\n" ;
  print_string ("min_avg_max 100 100 = " ^ string_of_int_triple (min_avg_max (test_table 100 100)) ^ " ; min_avg_max 100 1000 = " ^ string_of_int_triple (min_avg_max (test_table 100 1000)) ^ " ; min_avg_max 100 10000 = " ^ string_of_int_triple (min_avg_max (test_table 100 10000)) ^ "\n") ;
  print_string "Question 9\n" ;
  print_string ("nb_noeud_formula (f 25 10 9/10) = " ^ string_of_int (nb_noeud_formula (f 25 10 (9./.10.))) ^ " ; nb_noeud_formula (f 25 20 9/10) = " ^ string_of_int (nb_noeud_formula (f 25 20 (9./.10.))) ^ " ; nb_noeud_formula (f 25 30 9/10) = " ^ string_of_int (nb_noeud_formula (f 25 30 (9./.10.))) ^ "\n") ;
  print_string "Question 10\n" ;
  print_string ("anysat_f 25 10 9/10 = " ^ string_of_bool_arr (anysat_decision_f 25 10 (9./.10.)) ^ " ; anysat 25 20 9/10 = " ^ string_of_bool_arr (anysat_decision_f 25 20 (9./.10.)) ^ " ; anysat 25 30 9/10 = " ^ string_of_bool_arr (anysat_decision_f 25 30 (9./.10.)) ^ "\n") ;
  print_string "Question 11\n" ;
  print_string ("numsat_f 25 10 9/10 = " ^ string_of_int (numsat_decision_f 25 10 (9./.10.)) ^ " ; numsat 25 20 9/10 = " ^ string_of_int (numsat_decision_f 25 20 (9./.10.)) ^ " ; numsat 25 30 9/10 = " ^ string_of_int (numsat_decision_f 25 30 (9./.10.)) ^ "\n") ;
  print_string "Question 12\n" ;
  print_string ("numsat_dames 5 = " ^ string_of_int (numsat_dames 5) ^ " ; numsat_dames 6 = " ^ string_of_int (numsat_dames 6) ^ " ; numsat_dames 7 = " ^ string_of_int (numsat_dames 7) ^ " numsat_dames 8 = " ^ string_of_int (numsat_dames 8) ^ "\n") ;
;;
