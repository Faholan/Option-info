(*Graphes

Copyright (C) 2021  Faholan <https://github.com/Faholan>
*)

(*
En général, on utilise les graphes pour représenter des relations entre des éléments, en particulier des réseaux.

I) Graphes non orientés

Définiton : G = (V, E) est un graphe non orienté lorsque V est un ensemble fini d'éléments appelés sommets (Vertices)
et E € {{x, y} | x, y € V et x != y}

Les éléments de E sont appelés arêtes (Edges)

On dit que Card(V) est l'ordre du graphe G
*)

(*
Définition :
On dit que x et y sont voisins ou adjacents lorsque {x, y} € E
On appelle degré d'un sommet le nombre de voisins de ce sommet

On appelle chaîne (parfois chemin) tout produit cartésien de sommets (s1, ..., sn)
tel que si et si+1 soient adjacents
On dit que la chaîne est de longueur n-1
Si de plus s1 = sn, on parle de cycle.

On définit la distance entre deux sommets x et y par :

d(x, y) = 0 si x = y
          minimum des longueurs des chaînes entre x et y s'il en existe
          + infinity sinon

Définition :
On dit qu'un graphe non orienté G = (V, E) est connexe lorsqu'il existe une chaîne reliant tout couple de sommets

Définition : on dit que G' = (V', E') est le graphe induit
de G = (V, E) par les sommets S lorsque :
V' = S et S C V

E' = {{x, y} €E | x, y € S}

Définition : On dit que S C V est une composante connexe de G = (V, E)
lorsque :
Le graphe induit de G par S est connexe
S est maximal au sens de l'inclusion

Définiton : parcourir un graphe en profondeur consiste à partir d'un sommet (choisi ou non) pour
aller visiter un de ses voisins non encore visité, en réitérant ce procédé avec le voisin.

Une fois que le sommet d'arrivée n'a plus de voisin non visité, on reprend au dernier sommet qui
en a encore, tant que c'est possible.

Un parcours (en profondeur) donne donc une composante connexe du graphe.
Pour les déterminer toutes, il suffit de recommencer à partir d'un sommet non visité jusqu'à épuisement.

Propriété :
L'ensemble des composantes connexes d'un graphe G = (V, E)
forme une partition de l'ensemble des sommets

Définition : effectuer un parcours en largeur d'un graphe consiste à partir
d'un sommet et à visiter les sommets par ordre distance au sommet initial et dans la même composante
connexe

Définiton :
On dit que G = (V, E, P) est un graphe non orienté pondéré lorsque :
(V, E) est un graphe non orienté, et P est une application de E dans R
*)

(*On considère le tableau des listes d'adjacence*)
let ordre = Array.length ;;

let liste_voisins g noeud = g.(noeud) ;;

let pile_vide () = ref [] ;;

let est_pile_vide p = !p = [] ;;

let depile p =
  let e = List.hd !p in
    p := List.tl !p ;
    e
;;

let empile p e =
  p := e::!p
;;

let empile_liste p =
  let rec empilator = function
    | [] -> ()
    | h::t -> empile p h; empilator t
  in empilator
;;


type 'a file = {mutable l1: 'a list; mutable l2: 'a list} ;;

let file_vide () = {l1=[]; l2=[]} ;;

let est_file_vide f = f.l1 = [] && f.l2 = [] ;;

let enfile f e = f.l1 <- e::f.l1 ;;

let enfile_liste f =
  let rec enfilator = function
    | [] -> ()
    | h::t -> enfile f h; enfilator t
  in enfilator
;;

let rec defile f = match f.l1, f.l2 with
  | [], [] -> failwith "defile: File vide"
  | _, [] -> f.l2 <- List.rev f.l1 ;
    f.l1 <- [];
    defile f
  | _, h::t -> f.l2 <- t;
    h
;;


let parcours_prof g i =
  let a_traiter = pile_vide () and vus = Array.make (ordre g) false in
    let rec traiter_prochain_noeud () =
      if est_pile_vide a_traiter then []
      else (
        let noeud = depile a_traiter in
          if not vus.(noeud) then (
            vus.(noeud) <- true ;
            empile_liste a_traiter (liste_voisins g noeud) ;
            noeud::(traiter_prochain_noeud ())
          )
          else
            traiter_prochain_noeud ()
      )
    in
      empile a_traiter i ;
      traiter_prochain_noeud ()
;;


let parcours_prof g i =
  let a_traiter = file_vide () and vus = Array.make (ordre g) false in
    let rec traiter_prochain_noeud () =
      if est_file_vide a_traiter then []
      else (
        let noeud = defile a_traiter in
          if not vus.(noeud) then (
            vus.(noeud) <- true ;
            enfile_liste a_traiter (liste_voisins g noeud) ;
            noeud::(traiter_prochain_noeud ())
          )
          else
            traiter_prochain_noeud ()
      )
    in
      enfile a_traiter i ;
      traiter_prochain_noeud ()
;;
