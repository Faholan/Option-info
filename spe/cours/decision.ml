(*Problèmes de décision

Copyright (C) 2022  Faholan <https://github.com/Faholan>
*)

(*
Stable (sous-graphe : aucune arête)
Données : Un graphe non orienté et un entier k
Question : Exite-t-il dans le graphe un stable à au moins k sommets ?
-> Problème de décision

Stable_max
Données : Un graphe non orienté
Question : Quelle est la taille maximale d'un stable du graphe ?
-> Problème d'optimisation associé à Stable

Même chose pour Clique

Clique est NP-complet

Soit P1 € NP,

Puisque Clique est NP-complet, il est possible de résoudre P1 à l'aide de Clique suite à une transformation polynomiale.

Soit une instance de Clique : (G, k)

Considérons G_ le complémentaire de G, ie G_ = (V, V x V\E) où G = (V, E)

Alors :
Clique(G, k) = Vrai <=> il existe une clique dans G de taille au moins k
<=> Il existe un stable dans G_ de taille au moins k <=> Stable(G_, k) = vrai

Donc Stable résoud Clique quitte à effectuer une transformation polynomiale des données

=> Stable est NP-Complet

Théorème :
k-COLOR est NP-Complet

Où k-Color :
  Données : Un graphe non orienté
  Problème : LE graphe est-il coloriable avec au plus k couleurs


Exercice :
  k-Sudoku
  Données : une grille de taille k² x k²
  Problème : cette grille admet-elle au moins une solution valide

1) k-Sudoku est-il NP ?
2) Comment faire résoudre un k-Sudoku à n-color avec n à déterminer

1)
Pour vérifier qu'une grille est valide, il suffit de vérifier que les valeurs sur chaque ligne et
  chaque colonne sont distinctes 2 à 2 et entre 1 et k, pareil pour chaque carré. -> temps polynomial

2) Puis après on souffre et on relie pour les couleurs

Arbres couvrants de poids minimal

Rq : Il n'y a pas unicité

PRIM :
- On part d'un sommet
- On essaie d'étandre l'arbre enraciné en ce sommet jusqu'à couvrir tout l'arbre :
  - On prend à chaque itération une arête de poids minimal parmis celles incidentes aux sommets couverts par l'arbre
  - On s'interdit de prendre des arêtes reliant 2 sommets déjà connectés par l'arbre

KRUSKAL :
  - On sélectionne de proche en proche les arêtes de poids minimal à condition qu'elle ne relie pas 2 sommets déjà reliés dans la même composante connexe.

Procédé d'UNION-FIND :
UNION : on rattache deux composantes connexes à la racine. On rattache l'arbre le moins haut au plus haut

Pour améliorer l'efficacité, on pratique la compression de chemin :
Lors de la recherche de représentant avec FIND, on rattache directement les sommets parcourus à leur représentant

*)
