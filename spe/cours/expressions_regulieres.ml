(*Expressions régulières

Objectif :

Etant donné une expression régulière, déterminer un automate fini acceptant le langage qu'elle définit

On affaiblit le problème dans un premier temps pour s'intéresser à un ensemble de langages plus restreint.

Définition :

Soit L C ∑* un langage.

L est local lorsque :

∃ P C ∑, ∃ S C ∑, ∃ N C ∑²,  L \ {ε} = (P∑* ∩ ∑*S) \ ( ∑*N∑* )

Dès lors :  P est l'ensemble des préfixes de L
            S est l'ensemble des suffixes de L
            F = ∑² \ N est l'ensemble des facteurs (de longueur 2) de L.

Un tel langage est donc entièrement décrit par la donnée de P, S, F et de la présence ou non de ε dans le langage.

Exemple : L(ab*|c) = (P∑* ∩ ∑*S) \ ( ∑*N∑* )

où :  P = {a, c}
      S = {a, b, c}
      F = {ab, bb}
      Et ε ∉ L

Définition : On dit qu'une expression régulière est locale lorsque les lettre qui la composent apparaissent
au plus une fois.contents

Ex : ab*|c est locale, mais ab*|b ne l'est pas

Définition :
Un automate fini est local lorsque toutes les transitions portant sur une même lettre aboutissent dans le même état.

Propriétés :
- Un langage local peut être décrit par une expression régulière locale
- Il existe un automate local reconaissant un langage local pour tout langage local

Problème : Etant donné une expression régulière locale, comment construire un automate fini local
qui accepte le langage qu'elle décrit ?

Exemple : L(ab*|c)

On commence par déterminer P, S, F et si ε € L

Le langage local admet pour préfixes P = {a, c}, suffixes {a, b, c},
pour facteurs F = {ab, bb} et il ne contient pas ε.!
On construit dès lors l'automate local de la façon suivante :

1) On définit l'état initial: ε

2) On construit un état e pour chaque lettre e de ∑

3) On ajoute une transition (ε, e, e) pour tout e € P

4) On ajoute une transition (x, y, y) pour xy € F

5) S est l'ensemble des états finaux si ε ∉ S, sion c'est S U {ε}.

Remarque : tout automate local est déterministe.

Comment procéder avec un langage régulier ?

1) On linéarise l'expression régulière :

L = L((a|b* )(a|b)*|c)

L' = L((1|2* )(3|4)*|5)

Avec l'association : 1 -> a, 2 -> b, 3 -> a, 5 -> b, 5 -> c

Cette expression régulière est locale.

On a : P = {1, 2, 3, 4, 5}, S = {1, 2, 3, 4, 5}

F = {13, 14, 22, 23, 24, 33, 34, 43, 44}, ε € L'

On fait l'automate, puis on délinéarise

-> Altorithme de Berry-Sethi, automate de Glushkov
*)
