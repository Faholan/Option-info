(*Question 1*)

let rec suivant nuplet =
    let final, retenue = _suivant_lexico nuplet in
      final, not retenue
and _suivant_lexico = function
  | [] -> [], true
  | [0] -> [1], false
  | [1] -> [0], true
  | h::t -> let base, retenue = _suivant_lexico t in
    if retenue && h = 1 then
      0::base, true
    else
      if retenue then
        1::base, false
      else
        h::base, false
;;

(*Question 2*)

let rec affiche_nuplet nuplet =
    print_char '(' ;
    affiche_rec nuplet ;
    print_newline ()
and affiche_rec = function
  | [] -> print_char ')'
  | [a] -> (
    print_int a;
    print_char ')'
  )
  | h::t -> (
    print_int h;
    print_char ',';
    print_char ' ';
    affiche_rec t
  )
;;

let rec display_all n =
  display_rec (create_base_list n)
and create_base_list = function
  | 0 -> []
  | n -> 0::(create_base_list (n - 1))
and display_rec nuplet =
  affiche_nuplet nuplet ;
  let nextuplet, valid = suivant nuplet in
    if valid then
      display_rec nextuplet
;;

(*Question 3*)

let rec rev lst = recrev [] lst
and recrev base = function
  | [] -> base
  | h::t -> recrev (h::base) t
;;

let rec ajout a = function
  | [] -> []
  | h::t -> (a::h)::(ajout a t)
;;

(*Question 4*)
let rec monte = function
  | 0 -> []
  | 1 -> [[0]; [1]]
  | n -> (ajout 0 (monte (n - 1))) @ (ajout 1 (descend (n - 1)))
and descend n = rev (monte n)
;;

(*Question 5*)
(*
On note la complexité de monte M(n), celle de descend D(n) en termes d'appels à monte et à descend.

On a :
T(n + 1) = T(n - 1) + D(n - 1) + 2
et D(n) = T(n)

Donc : T(n) = 2T(n-1) + 2

T(n) = O(2**n)


*)

(*
On peut améliorer la complexité simplement de cette manière
*)
let rec monte_mieux = function
  | 0 -> []
  | 1 -> [[0]; [1]]
  | n -> let monte_prec = monte_mieux (n - 1) in
    (ajout 0 monte_prec) @ (ajout 1 (rev monte_prec))
and descend_mieux n = rev (monte_mieux n)
;;
(*Dans ce cas la complexité est de la forme :
T(n) = T(n - 1) + 1

D'où T(n) = O(n)
*)

(*
LE premier indice de la représentation binaire de g(k) est 1.
De plus, le reste de g(k) est g(2^n - 1 - r)

Donc g(k) = 2^n + g(2^n - 1 - r)
*)


let rec enumeration n = enumere_rec 0 n 3
and enumere_rec lower upper = function
  | 0 -> intervalle lower upper
  | _ when lower > upper -> []
  | n -> (ajout lower (enumere_rec (lower + 1) upper (n - 1))) @ (enumere_rec (lower + 1) upper n)
and intervalle lower upper =
  if upper <= lower then
    []
  else
    ajout lower (intervalle (lower + 1) upper)
;;


(*
La première :
[0, 1, ..., p]
la dernière :
[n - p, ..., n - 1]
*)

(*Question 16*)

let rec comb_suivante = function
  | [] -> failwith "Empty list"
  | [x] -> [x + 1]
  | h::t -> comb_rec_suivante h t
and comb_rec_suivante cj = function
  | [] -> [cj + 1]
  | h::t when h > cj + 1 -> (cj + 1)::(h::t)
  | h::t -> cj::(comb_rec_suivante h t)
;;

(*Question 18*)

let rec comb_suivante_bool n = function
  | [] -> failwith "Empty list"
  | [x] -> [x + 1], x + 1 < n
  | h::t -> comb_rec_suivante_bool n h t
and comb_rec_suivante_bool n cj = function
  | [] -> [cj + 1], cj + 1 < n
  | h::t when h > cj + 1 -> (cj + 1)::(h::t), true
  | h::t -> let suivant, result = comb_rec_suivante_bool n h t in
    cj::suivant, result
;;

let rec all_combs n p =
  if p > n then
    []
  else
    combs_rec n (rev (create_first_comb p))
and create_first_comb = function
    | 0 -> []
    | p -> (p-1)::(create_first_comb (p - 1))
and combs_rec n comb = let suivant, result = comb_suivante_bool n comb in
    if result then
      comb::(combs_rec n suivant)
    else
        [comb]
;;
