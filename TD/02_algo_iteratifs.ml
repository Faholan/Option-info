(*Exercices sur les algorithmes itératifs*)

(*Exercice 1*)

let somme vect1 vect2 =
  let summed = Array.make (Array.length vect1) 0. in
    for i = 0 to (Array.length vect1 - 1) do
      summed.(i) <- vect1.(i) +. vect2.(i)
    done ;
    summed
;;

let prod_sca vect1 vect2 =
  let tot = ref 0. in
    for i = 0 to (Array.length vect1 - 1) do
      tot := !tot +. vect1.(i) *. vect2.(i)
    done ;
    !tot
;;


(*Exercice 2*)

let u_n u0 n =
  let final = Array.make (n + 1) 0 in
    final.(0) <- u0 ;
    for i = 0 to (n - 1) do
      for k = 0 to i do
        final.(i + 1) <- final.(i + 1) + final.(k) * final.(i - k)
      done ;
    done ;
    final
;;

(*Exercice 3*)

let printer str =
  for i = 0 to (String.length str - 1) do
    if str.[i] = ' ' then
      print_newline()
    else
      print_char str.[i]
  done
;;

(*Exercice 4*)

(*a*)

let moyenne tab =
  let tot = ref 0. in
    for i = 0 to (Array.length tab - 1) do
      tot := !tot +. tab.(i)
    done ;
    !tot /. (float_of_int (Array.length tab))
;;

(*b*)

let ecart_max tab =
  let current_max = ref tab.(0) and current_min = ref tab.(0) in
    for i = 1 to (Array.length tab - 1) do
      if tab.(i) < !current_min then
        current_min := tab.(i)
      ;
      if tab.(i) > !current_max then
        current_max := tab.(i)
    done ;
    !current_max -. !current_min
;;

(*c*)

let gain_max tab =
  let current_max = ref (tab.(1) -. tab.(0)) in
    for i = 1 to (Array.length tab - 2) do
      if tab.(i + 1) -. tab.(i) > !current_max then
        current_max := tab.(i + 1) -. tab.(i)
    done ;
    !current_max
;;

(*d*)

let duree_max tab =
  let current_streak = ref 0 and max_streak = ref 0 in
    for i = 1 to (Array.length tab - 1) do
      if tab.(i) >= tab.(i - 1) then
        current_streak := !current_streak + 1
      else
        max_streak := max !max_streak !current_streak
    done ;
    max !current_streak !max_streak
;;

(*Exercice 5*)

type fraction = {num : int ; den : int} ;;

(*Exercice 6*)
let rec pgcd a b =
  match min a b with
  | 0 -> max a b
  | _ -> pgcd (min a b) ((max a b) mod (min a b))
;;

let simple q =
  let pg = pgcd q.num q.den in
    {num = q.num / pg ; den = q.den / pg}
;;

(*b*)

let egal q1 q2 = simple q1 = simple q2 ;;

let inf q1 q2 = (float_of_int q1.num) /. (float_of_int q1.den) <= (float_of_int q2.num) /. (float_of_int q2.den) ;;

(*d*)

let rec _puissance integer n = match n with
  | 0 -> 1
  | _ -> integer * (_puissance integer (n - 1))
;;
let puissance q1 n = {num = _puissance q1.num n ; den = _puissance q2.num n} ;;

(*Exercice 6*)

(*a*)

(*Renvoie -1 pour -inf*)
let rec degre pol = match pol with
  | [||] -> -1
  | _ -> if pol.(Array.length pol - 1) <> 0 then
    Array.length pol - 1
  else
    degre (Array.sub pol 0 (Array.length pol - 1))
;;

(*b*)

let eqal pol1 pol2 =
  match degre pol1 with
  | -1 -> degre pol2 = -1
  | _ -> (Array.sub pol1 0 (degre pol1 + 1)) = (Array.sub pol2 0 (degre pol2 + 1))
;;

let poly_der pol =
  let final = Array.make (Array.length pol - 1) 0 in
    for i = 1 to (Array.length pol - 1) do
      final.(i - 1) <- i * pol.(i)
    done ;
    final
;;

(*d*)

let poly_add poly_1 poly_2 =
  let length = max (Array.length poly_1) (Array.length poly_2) in
    let final = Array.make length 0 in
      for i = 0 to (length - 1) do
        if i < Array.length poly_1 then
          final.(i) <- final.(i) + poly_1.(i) ;
        if i < Array.length poly_2 then
          final.(i) <- final.(i) + poly_2.(i)
      done ;
      final
;;

let poly_mult poly_1 poly_2 =
  let degre_1 = degre poly_1 and degre_2 = degre poly_2 in
    let final = Array.make (degre_1 + degre_2 + 1) 0 in
      for i = 0 to degre_1 do
        for j = 0 to degre_2 do
          final.(i + j) <- final.(i + j) + poly_1.(i) * poly_2.(j)
        done ;
      done ;
      final
;;
(*ntot*(ntot + 1) / 2 opérations environ*)

(*e*)

let evalue poly x =
  let final = ref 0 in
    for i = 0 to degre poly do
      final := !final + poly.(i) * (_puissance x i)
    done ;
    !final
;;

let rec evalue_2 poly x =
  let dpoly = degre poly in
    match dpoly with
      | -1 -> 0
      | _ -> poly.(0) + x * evalue_2 (Array.sub poly 1 dpoly) x
;;

let rec div_euclid poly_1 poly_2 =
  let p1 = Array.sub poly_1 0 (degre poly_1 + 1) and p2 = Array.sub poly_2 0 (degre poly_2 + 1) in
    if Array.length p1 < Array.length poly_2 then
      [|0|], p1
    else (
      let dom = p1.(Array.length p1 - 1) / p2.(Array.length p2 - 1) in
        for i = 0 to (Array.length p2 - 1) do
          let cursor = Array.length p1 - 1 - i in
            p1.(cursor) <- p1.(cursor) - dom * p2.(Array.length p2 - 1 - i)
        done ;
        let (quot, res) = div_euclid (Array.sub p1 0 (Array.length p1 - 1)) p2 in
          if quot <> [|0|] then
            (Array.append quot [|dom|]), res
          else
            [|dom|], res
    )
;;
