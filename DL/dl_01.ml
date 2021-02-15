(*
MIT License

Copyright (c) 2021 Faholan

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

(*
Conventions d'écriture adoptées

La convention d'écriture est une adaptation libre de flake8.
Les fonctions dont le nom est préfixé par _ sont destinées à un usage interne et ne font pas partie de l'API public.
*)

(*Partie 1 : Recherche unidimensionnelle*)

(*Question 1*)

let rec nombreZerosDroite cursor table =
  if cursor >= Array.length table then (*Le curseur n'appartient pas au tableau*)
    0
  else
    if table.(cursor) = 0 then
      1 + nombreZerosDroite (cursor + 1) table
    else
      0
;;

(*Question 2*)

let nombreZerosMaximum table =
  let current_max = ref 0 and current = ref 0 in
    for cursor = 0 to (Array.length table - 1) do
      if table.(cursor) = 0 then
        current := !current + 1
      else
        begin
          if !current > !current_max then
            current_max := !current
          ;
          current := 0
        end
    done ;
    max !current !current_max
;;

(*La complexité est bien en O(n) : on ne parcours qu'une seule fois la liste.*)

(*Partie 2 : De la 1D vers la 2D*)

let table_ref = [| (*Tableau de référence de la partie 2*)
  [|0 ; 0 ; 1 ; 1 ; 0 ; 0 ; 0 ; 1|] ;
  [|0 ; 0 ; 0 ; 0 ; 1 ; 0 ; 0 ; 1|] ;
  [|1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1|] ;
  [|0 ; 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1|] ;
  [|0 ; 0 ; 1 ; 1 ; 0 ; 0 ; 0 ; 1|] ;
  [|0 ; 0 ; 1 ; 0 ; 0 ; 0 ; 0 ; 1|] ;
  [|0 ; 1 ; 1 ; 1 ; 0 ; 1 ; 0 ; 1|] ;
  [|0 ; 0 ; 1 ; 1 ; 0 ; 0 ; 0 ; 1|]
|] ;;

(*Question 3*)

let rec _can_increase table right down up =
  (*Vérifie que tous les éléments d'une certaine colonne sont des zéros entre deux bornes*)
  if down = up then
    table.(up).(right) = 0
  else
    table.(up).(right) = 0 && _can_increase table right down (up + 1)
;;
(*Est-ce qute tous les éléments d'une colonne donnée sont des 0*)

let rectangleHautDroit table i j =
  if table.(i).(j) = 1 then
    0
  else
  begin
    let up = ref i and right = ref j and area_max = ref 1 and length = Array.length table.(0) in
      while !up > 0 && table.(!up - 1).(j) = 0 do (*O(n)*)
        up := !up - 1
      done ;
      (*Coordonée en i maximale*)
      area_max := i - !up + 1 ; (*Un rectangle ne contenant que des zéros*)
      while !up <= i do (*O(n)*)
        while !right < (length - 1) && _can_increase table (!right + 1) i !up do
          (*On étend vers la droite progressivement*)
          right := !right + 1 (*O(n)*)
        done ;
        area_max := max !area_max ((i - !up + 1) * (!right - j + 1)) ;
        up := !up + 1 ;
        (*Réduction d'1 de la hauteur considérée*)
      done ;
      !area_max
  end
;;

(*Question 4*)

let rec _naif table i j =
  (*Analyse du tableau avec le coin inférieur gauche balayant les lignes de gauche à droite, et de bas en haut*)
  if j = Array.length table then (*Fin d eligne atteinte*)
    if i = 0 then
      if table.(i).(j) = 0 then 1 else 0 (*Coin supérieur droit*)
    else
      max (rectangleHautDroit table i j) (_naif table (i - 1) 0) (*Ligne précédente*)
  else
    max (rectangleHautDroit table i j) (_naif table i (j + 1)) (*Case suivante*)
;;

let max_rectangles_naif table = _naif table (Array.length table) 0 ;;

(*
La fonction _can_increase est de complexité O(n)
La  fonction rectangleHautDroit est de complexité 0(n^3)
La fonction _naif est de complexité 0(n^5) (n itérations sur j par itération sur i)
*)

(*Question 5*)

let colonneZeros table =
  let final = Array.make_matrix (Array.length table) (Array.length table.(0)) 0 in
    for i = 0 to Array.length table - 1 do
      for j = 0 to Array.length table.(i) - 1 do
        if table.(i).(j) = 0 then
          if i = 0 then
            final.(i).(j) <- 1
          else
            final.(i).(j) <- 1 + final.(i - 1).(j)
      done ;
    done ;
    final
;;

(*Cet algorithme est de complexité O(n) (le tableau est parcouru une et une seule fois)*)

(*Partie 3 : Algorithme optimisé*)

(*Question 7*)

(*
On considère la suite K[i] des valeurs prises par j pour une valeur donnée de i.
La suite K[i] est une suite entière, strictement décroissante et minorée par 0 :
  minorée par 0 car j correspond à des indices d'éléments d'une liste
  décroissante car dans le cas où l'algorithme ne se termine pas, j prend la valeur L[j-1] <= j - 1 < j.
Cela garantit donc la terminaison de l'algorithme.

De plus, on a l'invariant de boucle suivant pour tout k appartenant à K[i] : histo[k] >= histo[i]
 En effet, dans le cas où la boucle poursuit son itération, j prend la valeur L[j - 1].
 Or L[j-1] est tel que pour tout k appartenant à [L[j-1], j - 1], histo[k] >= histo[j - 1]
 De plus, histo[j - 1] >= histo[j] d'après la condition de boucle.
 D'où, pour tout k appartenant à [L[j-1], j], histo[k] >= histo[j]
 avec histo[j] >= histo[i] par construction.

Il n'y a que deux conditions pour sortir de la boucle :
  j = 0, dans ce cas j est le plus petit entier possible
  histo[j-1] < histo[i], et dans ce cas on a bien que j est le plus petit entier respectant la condition.

Pour histo = [1, 2, 3, ..., n-1, n, n, n-1, ..., 2, 1] on a :
  Pour tout k appartenant à [1, n - 1], histo[k - 1] < histo[k]
  Sur la première moitié du tableau, on a L[i] = i, en une opération.
  Sur la deuxième moitié du tableau, on a L[i] = L[i-1] - 1 :
    L[i-1] correspond à l'élément de la première partie égal à histo[i - 1]
    et histo[L[i - 1] - 1] = histo[i] : une deuxième opération
    avec histo[L[i - 1] - 2] < histo[i] (si i != 2n) : une troisième opération

  Ainsi, l'algorithme s'effectue en n + 3n opérations :
  Il est bien de complexité O(n)
*)

(*Question 8*)

let calculeL table =
  (*Implémentaton bête et méchante de l'algorithme proposé.*)
  let final = Array.make (Array.length table) 0 in
    for i = 1 to (Array.length final - 1) do
      let j = ref i in
        while !j <> 0 && table.(!j - 1) >= table.(i) do
          j := final.(!j - 1)
        done ;
        final.(i) <- !j
    done ;
    final
;;

let calculeR table =
  let length = Array.length table in
    let final = Array.make length 0 in
      for i = (length - 1) downto 0 do
        let j = ref i in
          while !j <> length - 1 && table.(!j + 1) >= table.(i) do
            j := final.(!j + 1)
          done ;
          final.(i) <- !j
      done ;
      final
;;

(*La complexité est bien en O(n) grâce à l'implémentation proposée*)

(*Question 9*)

(*
Pour tout k appartenant à [L[i], R[i]], on a par définition histo[i] <= histo[k]
Ainsi, un rectangle de hauteur histo[i] sera inclus dans celui de hauteur histo[k].
En mettant cote à cote ces rectangles de hauteur histo[i], on obtient qu'un rectangle
allant de L[i] à R[i] de hauteur histo[i] est inclus dans l'histogramme (son bord supérieur se situe en tout point en dessous de celui de l'histogramme.)
*)

(*Question 10*)

(*
Soit un rectangle d'aire maximale inclus dans l'histogramme.
pour tout k appartenant à [l, r], histo[k] >= h. *
Il existe un i appartenant à [l, r] tel que histo[i] = h
En effet, dans le cas contraire, on aurait :
  pour tout k dans [L, r], histo[k] > h et donc un rectangle de hauteur h + 1 conviendrait, et son aire serait strictement supérieure à celle de notre rectangle.
De plus, on a nécessairement : **
  l = 0 ou histo[l - 1] < h = histo[i]
  r = n - 1 ou histo[r + 1] < h = histo[i]
  (Dans le cas contraire, on pourrait étendre le rectangle)
Et les propriétés * et ** impliquent donc :
l = L[i] et r = R[i] par définition de l et r.
Il existe donc bel et bien i appartenant à [l, r] tel que :
  - h = histo[i]
  - l = L[i]
  - r = R[i]
*)

(*Question 11*)

let plusGrandRectangleHistogramme histo =
  let list_r = calculeR histo and list_l = calculeL histo and current = ref 0 in
    for i = 0 to Array.length histo - 1 do
      let aire = histo.(i) * (list_r.(i) - list_l.(i) + 1) in
        if aire > !current then
          current := aire
    done ;
    !current
;;
(*Complexité : O(n) (parcours de liste : O(n) plus calculeL et calculeR tous deux O(n))*)

(*Question 12*)

let rectangleToutZero table =
  let zeros = colonneZeros table and current = ref 0 in
    for i = 0 to Array.length(table) - 1 do
      let aire = plusGrandRectangleHistogramme zeros.(i) in
        if aire > !current then
          current := aire
    done ;
    !current
;;

(*Question 13*)
(*Complexité : O(n²) (Parcours de liste : O(n) * plusGrandRectangleHistogramme : O(n))*)
