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
  Produit naïf de matrices
*)

let produit m1 m2 =
  let final = Array.make (Array.length m1) (Array.make (Array.length m2.(0)) 0) in
    for i = 0 to Array.length m1 - 1 do
      for j = 0 to Array.length m2.(i) - 1 do
        for k = 0 to Array.length m2 do
          final.(i).(j) <- final.(i).(j) + m1.(i).(k) * m2.(k).(j)
        done
      done
    done ;
    final
;;
(*Pour des matrices carrées, complexité en Θ(n^3)*)

(*
Exercice 2
  Méthode de Strassen
*)

(*
Question 1
  Expression de C2 et C3

On a :

F1 + F2 = A1(B2 - B4) + (A1 + A2)B4
F1 + F2 = A1*B2 - A1*B4 + A1*B4 + A2*B4
C2 = F1 + F2

C3 = F3 + F4

*)

(*
Question 2
  Fonction morceaux
*)

let morceaux m =
  let n = Array.length m in
    let a1, a2, a3, a4 = Array.make (n/2) (Array.make (n/2) 0), Array.make (n/2) (Array.make (n/2) 0), Array.make (n/2) (Array.make (n/2) 0), Array.make (n/2) (Array.make (n/2) 0) in
      for i = 0 to n/2 - 1 do
        for j = 0 to n/2 - 1 do
          a1.(i).(j) <- m.(i).(j) ;
          a2.(i).(j) <- m.(i).(n/2 + j) ;
          a3.(i).(j) <- m.(n/2 + i).(j) ;
          a4.(i).(j) <- m.(n/2 + i).(n/2 + j)
        done
      done ;
      a1, a2, a3, a4
;;

(*
Question 3
  Fonction recombine
*)

let recombine a1 a2 a3 a4 =
  let n = Array.length a1 in
    let m = Array.make (2*n) (Array.make (2*n) 0) in
      for i = 0 to n do
        for j = 0 to n do
          m.(i).(j) <- a1.(i).(j) ;
          m.(i).(n + j) <- a2.(i).(j) ;
          m.(n + i).(j) <- a3.(i).(j) ;
          m.(n + i).(n + j) <- a4.(i).(j)
        done
      done ;
      m
;;

(*
Question 4
*)

let somme m1 m2 =
  let n = Array.length m1 in
    let final = Array.make n (Array.make n 0) in
      for i = 0 to n do
        for j = 0 to n do
          final.(i).(j) <- m1.(i).(j) + m2.(i).(j)
        done
      done ;
      final
;;

let difference m1 m2 =
  let n = Array.length m1 in
    let final = Array.make n (Array.make n 0) in
      for i = 0 to n do
        for j = 0 to n do
          final.(i).(j) <- m1.(i).(j) - m2.(i).(j)
        done
      done ;
      final
;;

let rec prodst m1 m2 = match Array.length m1 with
  | 0 -> [||]
  | 1 -> [|m1.(0).(0) * m2.(0).(0)|]
  | n ->
    let a1, a2, a3, a4 = morceaux m1 in
      let b1, b2, b3, b4 = morceaux m2 in
        let f1 = prodst a1 (difference b2 b4) and f2 = prodst (somme a1 a2) b4 and f3 = prodst (somme a3 a4) b1 and f4 = prodst a4 (difference b3 b1) and f5 = prodst (somme a1 a4) (somme b1 b4) and f6 = prodst (difference a2 a4) (somme b3 b4) and f7 = prodst (difference a1 a3) (somme b1 b2) in
          let c1 = somme f6 (somme f5 (difference f4 f2)) and c2 = somme f1 f2 and c3 = somme f3 f4 and c4 = somme (difference f1 f3) (difference f5 f7) in
            recombine c1 c2 c3 c4
;;

(*
Question 5
  Adapter le programme à des matrices dont la taille n'est pas une puissance de 2
*)

(*
Il suffit, dans le cas où la taille des matrices n'est pas paire, de les compléter avec des zéros.
*)

let morceaux_2 m =
  let k = (Array.length m)/2 + 1 in
    let a1, a2, a3, a4 = Array.make k (Array.make k 0), Array.make k (Array.make k 0), Array.make k (Array.make k 0), Array.make k (Array.make k 0) in
      for i = 0 to k - 1 do
        for j = 0 to k - 1 do
          a1.(i).(j) <- m.(i).(j) ;
          if j <> k - 1 then
            a2.(i).(j) <- m.(i).(k + j) ;
          if i <> k - 1 then
            a3.(i).(j) <- m.(k + i).(j) ;
          if i <> k - 1 && j <> k - 1 then
            a4.(i).(j) <- m.(k + i).(k + j)
        done
      done ;
      a1, a2, a3, a4
;;

let recombine_2 a1 a2 a3 a4 =
  let k = Array.length a1 in
    let final = Array.make (2 * k - 1) (Array.make (2 * k - 1) 0) in
      for i = 0 to k - 1 do
        for j = 0 to k - 1 do
          final.(i).(j) <- a1.(i).(j) ;
          if j <> k - 1 then
            m.(i).(k + j) <- a2.(i).(j) ;
          if i <> k - 1 then
            m.(k + i).(j) <- a3.(i).(j) ;
          if i <> k - 1 && j <> k - 1 then
            m.(k + i).(k + j) <- a4.(i).(j)
        done
      done ;
      final
;;

let rec prodst_2 m1 m2 = match Array.length m1 with
  | 0 -> [||]
  | 1 -> [|m1.(0).(0) * m2.(0).(0)|]
  | n when n mod 2 = 0 ->
    let a1, a2, a3, a4 = morceaux m1 in
      let b1, b2, b3, b4 = morceaux m2 in
        let f1 = prodst_2 a1 (difference b2 b4) and f2 = prodst_2 (somme a1 a2) b4 and f3 = prodst_2 (somme a3 a4) b1 and f4 = prodst_2 a4 (difference b3 b1) and f5 = prodst_2 (somme a1 a4) (somme b1 b4) and f6 = prodst_2 (difference a2 a4) (somme b3 b4) and f7 = prodst_2 (difference a1 a3) (somme b1 b2) in
          let c1 = somme f6 (somme f5 (difference f4 f2)) and c2 = somme f1 f2 and c3 = somme f3 f4 and c4 = somme (difference f1 f3) (difference f5 f7) in
            recombine c1 c2 c3 c4
  | n ->
  let a1, a2, a3, a4 = morceaux_2 m1 in
    let b1, b2, b3, b4 = morceaux_2 m2 in
      let f1 = prodst_2 a1 (difference b2 b4) and f2 = prodst_2 (somme a1 a2) b4 and f3 = prodst_2 (somme a3 a4) b1 and f4 = prodst_2 a4 (difference b3 b1) and f5 = prodst_2 (somme a1 a4) (somme b1 b4) and f6 = prodst_2 (difference a2 a4) (somme b3 b4) and f7 = prodst_2 (difference a1 a3) (somme b1 b2) in
        let c1 = somme f6 (somme f5 (difference f4 f2)) and c2 = somme f1 f2 and c3 = somme f3 f4 and c4 = somme (difference f1 f3) (difference f5 f7) in
          recombine_2 c1 c2 c3 c4
;;


(*
Question 6
  Complexité de l'algorithme
*)

(*
a

On s'intéresse déjà à la complexité des fonctions morceaux, recombine, difference et somme.
Elles sont toutes composées de deux boucles imbriquées, et sont donc de complexité :

Θ(n²)

La récursion de prodst constitue quand à elle :

- 2 appels de morceaux, de longueur n

- 11 appels de somme, de longueur n/2

- 7 appels de difference, de longueur n/2

- 1 appel de recombine, de longueur n/2

- 7 appels de prodst, de longueur n/2

L'équation de récurrence s'écrit donc, pour n puissance de 2 :

T(1) = 1

T(n) = 2*Θ(n²) + 11*Θ((n/2)²) + 7*Θ((n/2²)) + Θ((n/2)²) + 7*T(n/2)
T(n) = 7T(n/2) + Θ(n²)
*)

(*
b

On reconnaît une complexité de la forme :

T(n) = αT(n/2) + Θ(n^β), pour α = 7 et β = 2

On a α > 2^β donc :

T(n) = Θ(n^(log2 α))

T(n) = Θ(n^(log2 7))
*)

(*
Question 7

On avait trouvé une complexité en Θ(n^3) pour l'algorithme naïf.

Comme log2 7 < 3, on a bien une amélioration de la complexité de l'algorithme.
*)
