(*TD Mines-Pont 2009

Copyright (C) 2022  Faholan <https://github.com/Faholan>
*)

(*Question 1*)

(*La condition est que u soit de longueur paire.*)

(*Question 1

L1 est l'ensemble des mots u tels qu'il existe k, u2k != u2k+1
*)

(*Question 2
L1 === (..)*(ab|ba).*

Si u2k != u2k+1, alors u2k = a et u2k+1 = b ou u2k=b et u2k+1 = a
Cela correspond à ce que j'ai écrit.
*)

(*Question 3*)

(*Question 5                  --- |
                          b  |    |
-> 1 ----------------> 3 --->5f<--- a, b
  ^           |      a      ^
  | a, b      |             | a
  v           |             |
  2           -------> 4 ----
                    b
*)

(*Question 5*)

(*
φ(L2) = a*(ba|ε)b*

On peut avoir un ba si il y a un nombre impair de a.
*)

(*Question 6          ----|
               b     |    | b
-> 1f--------------> 2f<---
   --|         |     ^
   ^ |         |     | a
   | | a       |---> 3
   --
*)

(*Question 7

On peut prendre deux copies de l'automate, nomées P et I

L'état initial est l'état initial de I, et on modifie chaque liaison tq :
Chaque liaison partant de I mène à un noeud de P, et vice-versa.

Pour obtenir P(L) et I(L), on ne conserve que les états finaux de P ou de I
*)

(*Question 8

S'il existait une transition de q à q.

Il existe un mot validé par un chemin passant par q. On peut donc rajouter à ce mot un caractère en
utilisant l'arête q -> q, et on inverse la parité, d'où l'absurdité.
*)

(*Question 9

Les chemins allant de q0 à q sont de même parité, car il existe un chemin allant de q à un état final ;
Si il existait deux chemins allant de q0 à q de parité différente, on pourrait les employer pour valider 2 mots de parité différente.

*)
(*Question 10

Ils reconnaissent le même langage car s'il existait un chemin q' -> q -> q'', il existe un chemin q' -> r -> q'' portant les mêmes étiquettes.
*)

(*Question 11

Il faut appliquer la transformation S jusqu'à ce que tout état non final et non initial utile
possède
Et ensuite on échange les noeuds
*)



(*Question 16*)
type lettre = A | B ;;

let mot = [A; B; B; A; B] ;;

let rec phi = function
  | [] -> []
  | h::t -> phi2 h t
and phi2 e = function
  | [] -> [e]
  | h::t -> h::e::(phi t)
;;

(*Question 17*)
type graphe = {
  racine: int ;
  fils: int array ;
  freres: int array ;
} ;;

(*Question 18*)
let calculer_peres abr =
  let n = Array.length abr.fils in
    let result = Array.make n (-1) in
      for i = 0 to n - 1 do
        if abr.fils.(i) <> -1 then
          result.(abr.fils.(i)) <- i
      done ;
      for i = 0 to n - 1 do
        if abr.freres.(i) <> -1 then
          result.(abr.freres.(i)) <- result.(i)
      done ;
      result
;;
(*Question 19*)
(*La fonction calculer_peres est en O(n).*)

(*Question 20*)
let calculer_arites abr =
  let peres = calculer_peres abr in
    let n = Array.length peres in
      let result = Array.make n 0 in
        for i = 0 to n - 1 do
          if peres.(i) <> -1 then
            result.(peres.(i)) <- result.(peres.(i)) + 1
        done ;
        result
;;
(*Question 21*)
(*La fonction calculer_arites est en O(n)*)

(*Question 22*)
let rec inserer table nb d = inserer_rec table nb d 0 ; nb + 1
and inserer_rec table nb d i =
  if i = nb then
    table.(i) <- d
  else (
    if d > table.(i) then
      inserer_rec table nb d (i + 1)
    else (
      for j = nb downto (i + 1) do
        table.(j) <- table.(j - 1)
      done ;
      table.(i) <- d
    )
  )
;;

(*Question 23*)
(*La fonction insérer est en O(n) (on parcours dans un sens puis dans l'autre une fois)*)

(*Question 24*)
(*A3 :

9 1 6 7 1 3 7 9 9 3
*)

(*Question 25*)
let calcule_prufer abr =
  let peres = calculer_peres abr and arites = calculer_arites abr in
    let n = Array.length peres in
      let result = Array.make n 0 and feuilles = Array.make n 0 and nb = ref 0 in
        for i = 0 to n - 1 do
          if arites.(i) = 0 then
            nb := inserer feuilles !nb i
        done ;
        for j = 0 to n - 1 do
          nb := !nb - 1 ;
          result.(j) <- feuilles.(!nb) ;
          arites.(peres.(j)) <- arites.(peres.(j)) - 1 ;
          if arites.(peres.(j)) = 0 then
            nb := inserer feuilles !nb peres.(j)
        done ;
        result
;;

(*Question 26*)
(*La complexité de calcule_prufer est en O(n)*)

(*Question 27

La fonction calcule_arites_prufer utilise le fait qu'un noeud apparaît dans le codage
autant de fois que son arité.
*)

let calcule_arites_par_prufer prufer =
  let n = (Array.length prufer) + 1 in
    let arites = Array.make n 0 in
      for i = 0 to n - 2 do
        arites.(prufer.(i)) <- arites.(prufer.(i)) + 1
      done ;
      arites
;;

(*On effectue la construction à l'envers*)
