(*
Sujet concours centrale-Supélec 2000

Copyright (C) 2021  Faholan <https://github.com/Faholan>
*)

type noeud = {
  mutable etiquette: int ;
  mutable gauche: arbre_binaire ;
  mutable droit: arbre_binaire
}
and arbre_binaire =
  | ArbreVide
  | Pointeur of noeud
;;

let est_vide = function
  | ArbreVide -> true
  | _ -> false
;;

let noeud = function
  | ArbreVide -> failwith "Arbre vide"
  | Pointeur(x) -> x
;;

let echange_gauche_droit a =
  if est_vide a then
    a
  else
    let temp = (noeud a).gauche in
      (noeud a).gauche <- (noeud a).droit ;
      (noeud a).droit <- temp ;
      a
;;

(*I.B.1.a*)

let rec rotation_gauche = function
  | ArbreVide -> ArbreVide
  | pointeur -> match (noeud pointeur).droit with
    | ArbreVide -> pointeur
    | droit -> Pointeur({
      etiquette = (noeud droit).etiquette ;
      gauche = Pointeur({
        etiquette = (noeud pointeur).etiquette ;
        gauche = (noeud pointeur).gauche ;
        droit = (noeud droit).gauche
      }) ;
      droit = (noeud droit).droit
    })
;;

(*I.B.1.b
Si l'arbre est un arbre de recherche, alors on a :
- i1 < i2
- tout élément de Ag1 est inférieur à i1
- tout élément de Ag2 est inférieur à i2 mais supérieur à i1
- Tout élément de Ad2 est supérieur à Ad2

Ainsi, l'arbre obtenu par rotation respecte bien les conditions d'arbre binaire.
*)

(*I.B.2*)

let rec rotation_droite = function
  | ArbreVide -> ArbreVide
  | pointeur -> match (noeud pointeur).gauche with
    | ArbreVide -> pointeur
    | gauche -> Pointeur({
      etiquette = (noeud gauche).etiquette ;
      gauche = (noeud gauche).gauche ;
      droit = Pointeur({
        etiquette = (noeud pointeur).etiquette ;
        gauche = (noeud gauche).droit ;
        droit = (noeud pointeur).droit
      })
    })
;;

(*I.B.3.a*)
let rotation_gauche_droite = function
  | ArbreVide -> ArbreVide
  | pointeur -> rotation_droite (Pointeur({
    etiquette = (noeud pointeur).etiquette ;
    gauche = rotation_gauche (noeud pointeur).gauche ;
    droit = (noeud pointeur).droit
  }))
;;

(*I.B.3.b*)

(*
Etape 1 :
      4
  2       6
1   3   5   7
Rotation gauche :
      4
    3   6
  2    5  7
1

Rotation droite :

    3
  2     4
1         6
        5   7
*)

(*I.B.3.c*)
let rotation_droite_gauche = function
  | ArbreVide -> ArbreVide
  | pointeur -> rotation_gauche (Pointeur({
    etiquette = (noeud pointeur).etiquette ;
    gauche = (noeud pointeur).gauche ;
    droit = rotation_droite (noeud pointeur).droit
  }))
;;

(*I.C.1*)
type couleur =
  | Blanc
  | Noir
and noeud = {
  mutable etiquette: int ;
  mutable couleur: couleur ;
  mutable gauche: arbrebinaire ;
  mutable droit: arbrebinaire ;
  mutable pere: arbrebinaire ;
}
and arbrebinaire =
  | ArbreVide
  | Pointeur of noeud
;;

(*
        N
      N
    N
  N
N
*)

(*I.C.3.a*)
(*Si l'arbre initial est vide, il faut colorier le nouveau noeud en noir.*)

(*I.C.3.b*)
(*
Si le père de Robert après insertion initiale est noir,
*)
