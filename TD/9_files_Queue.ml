(* 4T *)

(* Exercices sur les files *)
open Queue
open Random

let file_de_liste t = 
	let f = Queue.create() in
	let rec aux f = function
		[] -> ()
		|t::q -> Queue.add t f; 
			aux f q;
	in
	aux f t;
	f;;

let rec liste_de_file f =
	if Queue.is_empty f then (
		[];
	) else (
		let t = Queue.take f
		and q = liste_de_file f in
		t::q
	);;
let f = file_de_liste [1; 2; 3; 4];;
liste_de_file f;;



(* 1 *)
let rec dernier f = 
	let x = Queue.take f in
	if Queue.is_empty f then (
		x;
	) else (
		dernier f;
	);;
let f = file_de_liste [1; 2; 3; 4];;
dernier f;;

(* 2 *)
let une_deux f = 
	let f1 = Queue.create()
	and f2 = Queue.create() in
	let rec aux _f1 _f2 = 
		if not (Queue.is_empty f) then (
			Queue.add (Queue.take f) _f1;
			aux _f2 _f1;
		) in
	aux f1 f2;
	(f1, f2);;
let f = file_de_liste [1; 2; 3; 4; 5];;
let f1, f2 = une_deux f;;
liste_de_file f1;;
liste_de_file f2;;

(* 3 *)
let appliquer file f =
	let file_ = Queue.create() in
	let rec aux file_ =
		if not (Queue.is_empty file) then (
			Queue.add (f (Queue.take file)) file_;
			aux file_;
		) in
	aux file_;
	file_;;

let fonction x = x + 2;;
let f = file_de_liste [1; 2; 3; 4; 5];;
let f = appliquer f fonction;;
liste_de_file f;;

(* 4 *)
let file_de_liste t = 
	let f = Queue.create() in
	let rec aux f = function
		[] -> ()
		|t::q -> Queue.add t f; 
			aux f q;
	in
	aux f t;
	f;;

(* 5 *)
let liste_sans_modif f =
	let liste = liste_de_file f in
	let rec aux = function
		[] -> ()
		| t::q -> Queue.add t f; 
			aux q; in
	aux liste;
	liste;;
let f = file_de_liste [1; 2; 3; 4; 5];;
liste_sans_modif f;;

(* 6 - Problème de Josephus *)
let josephus n k =
	let pile = Queue.create() in
	for i=1 to n do
		Queue.add i pile;
	done;
	let rec aux i =
		if i = k then (
			let x = Queue.take pile in
			if Queue.is_empty pile then (
				x;
			) else (
				print_int x;
				aux 1;
			)
		) else (
			Queue.add (Queue.take pile) pile;
			aux (i+1);
		) in
	aux 1;;
josephus 6 4;;

(* Permutation aléatoire d'une liste *)
(* 1 *)
let rec extrait i n = function
	[] -> failwith "Empty error i >= n"
	| t::q when i=0 -> (t, q)
	| t::q -> let x, y = extrait (i-1) n q in
		(x, t::y);;
extrait 3 5 [1; 2; 3; 4; 5];;

let rec choix but n = function
	[] -> but
	|source -> let (t, q) = extrait (Random.int n) n source in
		(choix (t::but) (n-1) q);;
choix [7; 8; 9] 6 [1; 2; 3; 4; 5; 6];;


let shuffle liste = 
	choix [] (List.length liste) liste;;
shuffle [1; 2; 3; 4; 5; 6; 7; 8; 9];;

(* 2 *)
let permutation n =
	let rec liste n = 
		if n = 0 then (
			[]
		) else (
			n::(liste (n-1))
		) in
	shuffle (liste (n-1));;
permutation 10;;


(* Mariages stables *)

(* 1 -
Prenons :
 - f1, f2 de F
 - g1, g2 de G
tq 
	f1 aime g1
	g1 aime f2
	f2 aime g2
	g2 aime f1

alors les couples 
{(f1, g1), (f2, g2)} et {(f1, g2), (f2, g1)}
sont aussi stable l'un que l'autre,
la solution n'est donc pas unique
*)

(* 2 *)
(* H et F int array array *)
let permutationListe n =
	let tab = Array.make n 0 in
	let rec aux i = function
		[] -> ()
		| t::q -> tab.(i) <- t;
			aux (i+1) q in
	aux 0 (permutation n);
	tab;;
permutationListe 10;;


let randomMatrix n = 
	let matrix = Array.make_matrix n n 0 in
	for i=0 to n-1 do
		matrix.(i) <- permutationListe n;
	done;
	matrix;;
randomMatrix 10;;

let creerRang f =
	let n = Array.length f in
	let rang = Array.make_matrix n n 0 in
	for j=0 to n-1 do
		for ordre=0 to n-1 do
			rang.(j).(f.(j).(ordre)) <- (ordre+1);
		done;
	done;
	rang;;
let f = randomMatrix 3;;
f;;
creerRang f;;

let creerCelib n =
	let pile = Queue.create() in
	let rec aux i =
		if i=0 then (
			Queue.add 0 pile;
		) else (
			aux (i-1);
			Queue.add i pile;
		) in
	aux (n-1);
	pile;;
liste_de_file (creerCelib 10);;

let gale_shapley h f =
	let n = Array.length h in
	let celib = creerCelib n
	and suiv = Array.make n 0
	and mari = Array.make n (-1)
	and rang = creerRang f in
	while not (Queue.is_empty celib) do
		let i = Queue.take celib in
		let j = h.(i).(suiv.(i)) in
		suiv.(i) <- suiv.(i) + 1;
		if mari.(j) = (-1) then (
			(* la fille est célib fonce !!! *)
			mari.(j) <- i;
		) else (
			let k = mari.(j) in
			if rang.(j).(k) > rang.(j).(i) then (
				(* la fille préfère i à k, i va la charo *)
				mari.(j) <- i;
				Queue.add k celib;
			) else (
				(* la fille reste avec k son gars sur *)
				Queue.add i celib;
			)
		)
	done;
	mari;;
(*
let h = randomMatrix 3;;
let f = randomMatrix 3;;
h;;	
f;;
*)

let h = [|
	[|1; 0; 2|];
	[|2; 1; 0|];
	[|0; 1; 2|];
|];;
let f = [|
	[|2; 1; 0|]; (* elle sera mariée à 2 *)
	[|0; 1; 2|]; (* elle sera mariée à 0 *)
	[|1; 0; 2|]; (* elle sera mariée à 1 *)
|];;
gale_shapley h f;;

(* 4 - 
creerRang et creerCelib sont 2 boucles sur n imbriqués
La création des tableaux intermédiaires est O(n^2)

Dans le pire des cas chaque homme fait une finçaille à chaque femme
Il y a n homme et n femmes la complexité pour les mariages est O(n^2) 

l'agorithme est donc de complexité O(n^2)
*)

(* 5 *)
let n = 10;;
let h = randomMatrix n;;
let f = randomMatrix n;;
gale_shapley h f;;