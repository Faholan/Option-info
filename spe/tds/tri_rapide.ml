(*
Algorithmes de tri rapide

Copyright (C) 2021  Faholan <https://github.com/Faholan>
*)

(*Partie I*)
(*Question A*)
let echange i j tab =
  let elem = tab.(i) in
    tab.(i) <- tab.(j) ;
    tab.(j) <- elem
;;

(*Question B

Un algorithme de tri simple et l'algorithme de tri bulle.
En notant n la longueur du tableau, il consiste à parcourir le tableau en échangeant les éléments deux à deux si ils ne sont pas triés
Puis à parcourir les n-1 premiers éléments, en répétant jusqu'à devoir parcouri 1 élément, auquel cas le tableau est trié.
Sa complexité est en O(n²)
*)
let tri_bulle tab =
  for k = Array.length tab - 2 downto 0 do
    for i = 0 to k do
      if tab.(i) > tab.(i+1) then
        echange i (i+1) tab
    done
  done
;;

(*Question C

On prend comme pivot le premier élément du tableau.
L'algorithme consiste à parcourir le tableau, et, si un élément est plus petit que le pivot, on effectue ses deux opérations :
- On échange le pivot avec cet élément
- On échange le pivot avec l'élément situé juste après son ancienne position
On n'effectue qu'un unique parcours du tableau, donc la complexité temporelle est en O(n)
*)

let separation tab i1 i2 =
  let pos_pivot = ref i1 and pivot = ref tab.(i1) in
    for i = (i1 + 1) to i2 do
      if tab.(i) < !pivot then (
          echange i !pos_pivot tab ;
          pos_pivot := !pos_pivot + 1 ;
          echange i !pos_pivot tab
        )
    done ;
    !pos_pivot
;;

let tri_rapide tab =
  let rec tri_recursif i1 i2 =
    if i1 = i2 then
      ()
    else
      let pivot = separation tab i1 i2 in
        tri_recursif i1 pivot ;
        tri_recursif pivot i2
  in tri_recursif 0 (Array.length tab)
;;

(*Partie II*)
(*A
Si le tableau est trié dans l'ordre croissant, alors à chaque passage, on n'effectue aucun échange.
La position du pivot suite à un échange est celle du curseur de début :
A chaque itération, on effectue une comparaison de moins qu'à l'itération précédente, jusqu'à 1 comparaison.
On a donc :

v(n) = Somme(i=1, n, i) = n(n+1)/2

De la même manière, si le tableau est trié dans l'ordre décroissant, alors tous les éléments du tableau, à chaque itération, passent à gauche du tableau, mais leur ordre est respecté : la partie gauche est encore triée dans l'ordre décroissant.
La position du pivot est donc celle du curseur de fin.

De même, v(n) = n(n+1)/2
*)

(*B.1

Soit un tableau de longueur 2n. On coupe le tableau en deux moitiés de longueur n lors du tri rapide, puis l'on effectue le tri rapide ur chacune de ces moitiés.

On a déjà vu que la phase de séparation est de complexité O(n), donc il existe α tel que le nombre de comparaisons effecutées soit équivalent à αn

M(2n) doit donc majorer 2M(n) + αn, donc M(2n) = 2M(n) + αn car M(n) est un majorant raisonnable.
*)

(*B.2*)
(*
Si n est de la forme 2^k, alors on pose u(k) = M(2^k)

u(k) = 2*u(k-1) + α2^k

u(k) / (2^k) = u(k-1) / (2^(k-1)) + α

D'où, en sommant de 1 à k :

u(k) / 2^k = k*α

Soit : u(k) = 2^k * k * α

Donc :
M(n) = O(n * log2(n))
*)


(*Partie III*)
(*Question A.1*)

let mediane tab i j k =
  if tab.(i) < tab.(j) && tab.(j) < tab.(k) then
    j
  else
    if tab.(i) > tab.(j) && tab.(j) > tab.(k) then
      j
    else
      if tab.(i) > tab.(k) && tab.(k) > tab.(j) then
        k
      else
        if tab.(i) < tab.(k) && tab.(k) < tab.(j) then
          k
        else
          j
;;

(*Question A.2*)
let get_pseudo_mediane tab =
  let every = ref 1 in
    while 3 * !every <= Array.length tab do
      let cur_pos = ref 0 in
        while !cur_pos < Array.length tab do
          echange !cur_pos (mediane tab !cur_pos (!cur_pos + !every) (!cur_pos + 2 * !every)) tab ;
          cur_pos := !cur_pos + 3 * !every
        done ;
      every := 3 * !every
    done ;
    tab.(0)
;;

(*Question B*)
type ternaire = Feuille of int | Noeud of int * ternaire * ternaire * ternaire ;;

(*B.1*)
let mediane3 a b c =
  if a < b && b < c then
    b
  else
    if a > b && b > c then
      b
    else
      if b < a && a < c then
        a
      else
        if b > a && a > c then
          a
        else
          c
;;

(*B.2*)
let get_value = function
  | Feuille(x) -> x
  | Noeud(x, _, _, _) -> x
;;

let arbre_mediane a b c = Noeud(mediane3 (get_value a) (get_value b) (get_value c), a, b, c) ;;

(*B.3*)

let rec construire tab i j = match j - i with
  | 0 -> Feuille(tab.(i))
  (*(k-2)/3 = (k+1)/3 - 1*)
  | k -> arbre_mediane (construire tab i (i + (k-2)/3)) (construire tab (i + (k+1)/3) (j - (k+1)/3)) (construire tab (j - (k-2)/3) j)
;;

(*B.4*)
let pseudo_mediane tab = get_value (construire tab 0 (Array.length tab - 1)) ;;

(*Question C*)
(*C.1*)
(*
On a l'équation de complexité suivante :

C(3n) = 3 * C(n) + O(1) car la complexité d'arbre_mediane est constante

On a donc :
C(n) = O(n)
*)
