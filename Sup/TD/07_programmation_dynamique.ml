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
Exercice 1
  Carrés de 1
*)

(*
1

Si mi,j = 0, alors ti,j = 0
Sinon, ti,j= min(ti-1,j ; ti,j-1 ; ti-1,j-1) + 1
*)

let maker n1 n2 x =
  let final = Array.make n1 [||] in
    for i = 0 to n1 - 1 do
      final.(i) <- Array.make n2 x
    done ;
    final
;;

(*2*)
let constr_t mat =
  let n = Array.length mat and p = Array.length mat.(0) in
    let final = maker n p 0 in
      for j = 0 to p - 1 do
        final.(0).(j) <- mat.(0).(j)
        (*Première ligne : pas de récurrence, carrés de taille 0 ou 1*)
      done ;
      for i = 0 to n - 1 do
        final.(i).(0) <- mat.(i).(0)
        (*Première colonne : pas de récurrence, carrés de taille 0 ou 1*)
      done ;
      for i = 1 to n - 1 do
        for j = 1 to p - 1 do
          if mat.(i).(j) = 1 then
            final.(i).(j) <- 1 + min final.(i-1).(j) (min final.(i).(j-1) final.(i-1).(j-1))
        done
      done ;
    final
;;

(*
La complexité des deux premières boucles est en O(n) et O(p) respectivement.
La troisième est de complexité O(np)
La complexité de la fonction est donc en O(np) : elle est quadratique
*)

let maker_random n =
  let final = Array.make n [||] in
    for i = 0 to n - 1 do
      final.(i) <- Array.make n 0 ;
      for j = 0 to n - 1 do
        final.(i).(j) <- Random.int 2
      done
    done ;
    final
;;

let max_sqr n =
  let mat_t = constr_t (maker_random n) in
    let cur_max = ref 0 in
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          cur_max := max !cur_max mat_t.(i).(j)
        done
      done ;
      !cur_max
;;

(*
Exercice 2
  Chemin de poids minimal
*)

(*1
On considère la suite des déplacements d'une case à l'autre.
Cette suite est composée de n-1 V (vertical) et p-1 H (horizontal)
Le nombre de chemin possible correspond donc au nombre de combinaisons possibles de n-1 V dans n + p - 2 directions.
Le nombre de chemin vaut donc : (n + p - 2)!/(n-1)!(p-1)!
*)

let construire_tableaux tab =
  let n = Array.length tab and p = Array.length tab.(0) in
    let cout = maker n p 0 and choix = maker n p 0 in
      for i = n - 1 downto 0 do
        for j = p - 1 downto 0 do
          if i = n - 1 && j = p - 1 then
            cout.(i).(j) <- tab.(i).(j)
          else
            if i = n - 1 then (
              cout.(i).(j) <- cout.(i).(j+1) + tab.(i).(j) ;
              choix.(i).(j) <- 1
            )
            else
              if j = p - 1 then
                cout.(i).(j) <- cout.(i+1).(j) + tab.(i).(j)
              else
                if cout.(i+1).(j) >= cout.(i).(j+1) then (
                  cout.(i).(j) <- cout.(i).(j+1) + tab.(i).(j) ;
                  choix.(i).(j) <- 1
                )
                else
                  cout.(i).(j) <- cout.(i+1).(j) + tab.(i).(j)
        done
      done ;
      cout, choix
;;

(*3*)

let affiche_chemin tab =
  let _, choix = construire_tableaux tab in
    let rec aux i j =
      if i = Array.length tab || j = Array.length tab.(0) then
        []
      else
        if choix.(i).(j) = 0 then
          (i, j)::(aux (i+1) j)
        else
          (i, j)::(aux i (j+1))
    in aux 0 0
;;

let tableau = [|
	[|1; 4; 6; 8; 2|] ;
	[|2; 4; 0; 2; 4|] ;
	[|3; 5; 0; 8; 9|] ;
	[|8; 0; 7; 6; 0|] ;
	[|0; 9; 5; 9; 1|] ;
	[|2; 7; 5; 5; 2|]
|] ;;
