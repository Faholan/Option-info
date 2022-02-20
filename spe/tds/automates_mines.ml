(*TD des Mines sur les automates finis

Copyright (C) 2022  Faholan <https://github.com/Faholan>
*)

type automate = int * (int * int) array * bool array ;;

(*Partie I*)

(*Question 1
Le langage reconnu par l'automate A1 est celui des mots composé d'un nombre impair de caractère

Question 2
Le langage reconnu par l'automate A2 est celui des mots comportant un nombre impair de b

Question 3
Le langage reconnu par l'automate A3 est celui des mots comportant un nombre impair de b

Question 4
Le langage reconnu par l'automate A4 est celui des mots comportant un nombre impair de b
*)

(*Question 5*)
let auto_2: automate = (
  2,
  [|(0, 1); (1, 0)|],
  [|false; true|]
) ;;

(*Partie 2*)
(*Question 6*)

let rec _get_index current x = function
  | [] -> -1
  | h::_ when h = x -> current
  | _::t -> _get_index (current + 1) x t
;;

let numero n lst = Array.init n (
  fun x -> _get_index 0 x lst
) ;;

(*Question 7*)
let etats_accessibles ((n, delta, f): automate) =
  let accedes = Array.make n false and result = ref [] in
    let rec accessor i =
      if not accedes.(i) then (
        accedes.(i) <- true ;
        result := i::!result ;
        let f1, f2 = delta.(i) in
          accessor f1 ;
          accessor f2
      )
  in
    accessor 0 ;
    List.rev !result
;;
(*La complexité de etats_accessibles est en O(n) car la fonction accessor
traite les indexes uniquement à la première rencontre.
*)

let partie_accessible ((n, delta, f): automate): automate =
  let accessibles = etats_accessibles (n, delta, f) in
    let renum = numero n accessibles in
      let newn = Array.fold_left max 0 renum in
        let newdelta = Array.make newn (-1, -1) and newfinal = Array.make newn false in
          for i = 0 to n - 1 do
            if renum.(i) <> -1 then (
              let a, b = delta.(i) in
                newdelta.(renum.(i)) <- (renum.(a), renum.(b)) ;
              if f.(i) then
                newfinal.(renum.(i)) <- true
            )
          done ;
          (newn, newdelta, newfinal)
;;

(*Partie III*)
type morphisme = int array ;;

(*Question 9

A3 ==> A2
| q  | φ(q) |
---------------
| E | C
| F | C
| G | D
*)

(*Question 10

A4 ==> A2
| q  | φ(q) |
---------------
| H | C
| I | C
| J | D
| K | D
*)

(*Question 11

Si l'on avait un morphisme d'automates φ entre A1 et A2

Alors on aurait φ(C) = A et φ(D) = B d'après la propriété (4)

Cependant, φ(δ2(C, a)) = φ(C) = A alors que δ1(φ(C), a) = δ1(A, a) = B

En contradiction avec la propriété (3)
*)

(*
Question 12

Si'l existait un morphisme φ entre A5 et 12 alors :

D'après (4), φ(L) = C = φ(N), φ(M) = D

Mais : φ(δ5(N, b)) = φ(L) = C alors que δ2(φ(N), b) = δ2(C, b) = D

Ce qui est absurde d'après (3)
*)

(*Question 13

S'il existe un morphisme φ entre deux automates A et B.

Soit ω un mot accepté par A

On a alors : δB*(iB, ω) = δB*(φ(iA), ω) d'après (2)

= δB(δB(δB(...δB(φ(iA), ω1)...), ωn-1), ωn)

= δB(δB(δB(...φ(δA(iA, ω1))...), ωn-1), ωn) d'après (3)

= φ(δA(δA(δA(...δA(iA, ω1)...), ωn-1), ωn))

= φ(δA*(iA, ω)) € FB car δA*(iA, ω) € FA

Ainsi, tout mot reconnu par A est reconnu par B.

On peut ensuite remonter le résonnement grâce à la surjectivité de φ.
*)

(*Question 15

Soit φ: A -> B, φ': B -> C deux morphismes d'automates.

φ' o φ est bien surjective (1)
φ'(φ(iA)) = φ'(iB) = iC (2)

Soit q € QA, σ € {a, b}.

φ'(φ(δA(q, σ))) = φ'(δB(φ(q), σ)) = δC(φ'(φ(q)), σ) (3)

Soit q € QA

q € FA <=> φ(q) € FB <=> φ'(φ(q)) € FC (4)
*)

(*Question 16

Quand les 2 automates sont accessibles, soit q € QB

q = δB*(iB, ω) = δB*(φ(iA), ω) d'après (2)

q = φ(δA*(iA, ω)) par itération de (3) d'où (1)
*)

(*Question 17

LE principe de l'algorithme est de tenter de construire de proche en proche le morphisme
en s'appuyant sur la propriété (3), en initialisant grâce à (2), puis de vérifier (4)
*)

exception PasDeMorphisme ;;

let existe_morphisme ((n1, delta1, f1): automate) ((n2, delta2, f2): automate): bool * morphisme =
  if n1 < n2 then  (*Condition nécessaire de surjectivité*)
    false, [||]
  else
    let phi = Array.make n1 (-1) in
      let rec morphisme_builder ia ib =
        if phi.(ia) <> -1 then (
          if phi.(ia) <> ib then
            raise PasDeMorphisme
        ) else (
          phi.(ia) <- ib ;
          let a_a, a_b = delta1.(ia) and b_a, b_b = delta2.(ib) in
            morphisme_builder a_a b_a ;
            morphisme_builder a_b b_b
        )
      and verif_morphisme = function
        | i when i = n1 -> ()
        | i when (f1.(i) && not f2.(phi.(i))) || (f2.(phi.(i)) || not f1.(i)) -> raise PasDeMorphisme
        | i -> verif_morphisme (i + 1)
      in
        try
          morphisme_builder 0 0 ;
          verif_morphisme 0 ;
          true, phi
        with
          | PasDeMorphisme -> false, [||]
;;

(*Question 18

La partie accessible du produit A3 x A4 est :


->(E, H) ------------> (F, I) <-----------> (F, H)
    |         a         ^           a         ^
    |                   |                     |
    |                   |                     |
    |                   | b                   | b
    | b                 |                     |
    |                   |                     |
    |                   v         a           v
    |                (G, K) <------------> (G, J)
    |                                        ^
    -----------------------------------------|
*)

(*Question 19*)

let produit ((n1, delta1, f1): automate) ((n2, delta2, f2): automate): automate =
  let newdelta = Array.make (n1 * n2) (0, 0) and newfinal = Array.make (n1 * n2) false in
    for i = 0 to n1 - 1 do
      let a1, b1 = delta1.(i) in
        for j = 0 to n2 - 1 do
          let a2, b2 = delta2.(i) in
            newdelta.(i * n2 + j) <- (a1 * n2 + a2, b1 * n2 + b2) ;
            if f1.(i) && f2.(j) then
              newfinal.(i * n2 + j) <- true
        done ;
    done ;
    ((n1 * n2), newdelta, newfinal)
;;

(*
Question 20

Si les deux automates acceptent le même langage, et que (q, q') est un état
de l'automate produit, q final, alors il existe un mot ω tel que :

δA*(iA, ω) = q final

On a alors δB*(iB, ω) = q' par définition de l'automate final.

ω est accepté par l'automate A, donc par le B, donc q' est final.
*)

(*Question 21

Le morphisme φ: (q1, q2) -> q1 est toujours disponible.

Il est bien surjectif (car tous les états apparaissent si l'automate est accessible)
On a bien φ((iA, iB)) = iA

On a bien φ(δ((q, q'), σ)) = φ((δA(q, σ), δB(q', σ))) = δA(q, σ) = δA(φ((q, q')), σ)

Et : (q, q') final <=> q final d'après la question 20
*)
