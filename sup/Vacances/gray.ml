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

(*Question 1

L1 = [0; 1]
L2 = [00; 01; 11; 10]
L3 = [000; 001; 011; 010; 110; 111; 101; 100]
*)

(*Question 2

On veut montrer par récurrence sur n que la liste Ln contient tous les mots de longueur n

Initialisation : vrai pour n = 1

Hérédité : Soit n tel que Ln contiennent tous les mots de longueur n.
On veut montrer qu'alors Ln+1 contient tous les mots de longueur n+1

Pour tout mot de longueur n+1, il commence soit par 0 soit par 1.
On note la suite du mot m. m est un mot de longueur n.

Par hypothèse, m appartient à Ln. Or, si m appartient à Ln, alors 0m et 1m appartiennent à Ln+1 par construction.

La liste Ln+1 contient donc tous les mots de longueur n+1
*)

(*Question 3*)

let rec gray = function
  | 1 -> ["0"; "1"]
  | n -> let base = gray (n - 1) in
    let rec add_0 base cur = match base with
      | [] -> cur
      | h::t -> add_0 t (("0"^h)::cur)
    in let rec add_1 base rest cur = match base with
      | [] -> add_0 rest cur
      | h::t -> add_1 t (h::rest) (("1"^h)::cur)
    in add_1 base [] []
;;
