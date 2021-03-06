(*
MIT License

Copyright (c) 2021 tttienthinh
*)



let rec del occur = function 
    [] -> []
    | t::q -> if t=occur then (del occur q) else (t::(del occur q));;

del 2 [1; 2; 3; 2; 2; 4; 5];;

let rec redondances = function
    [] -> []
    | t::q -> t::(redondances (del t q));;
redondances [1; 2; 1; 3; 2; 3];;

let rec aplatir = function
    [] -> []
    | t::q -> t@(aplatir q);;

aplatir [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]];;

let rec renverse = function
    [] -> []
    | t::q -> (renverse q)@[t];;
renverse [1; 2; 3; 4; 5];;

let f x = print_int(x);;

let rec iterer f = function
    [] -> ()
    | t::q -> f(t); (iterer f q);;
    
iterer f [0; 1; 2; 3];;
print_newline();;

let f x y = x+y;; 

let rec fold_l f a = function 
    [] -> a
    | t::q -> fold_l f (f a t) q;;
    
fold_l f 0 [1; 2; 3; 4];;

let p x = x=0;;

let rec find p = function
    [] -> failwith "pas dans la liste"
    | t::q -> if (p t) then t else find p q;;
    
find p [1; 2; 3];;

let p x = x<0;;

let rec filter p = function
    [] -> []
    | t::q when (p t) -> t::(filter p q)
    | t::q -> filter p q;;
    
filter p [-1; -2; 1; 2; -3; 5; -4];;

let rec assoc a = function
    [] -> failwith "Pas de a"
    | t::q when (t=a) -> List.hd q
    | t::q -> assoc a q;;

assoc 3 [1; 2; 3; 4; 5; 6];;

let rec split = function
    [] -> [], []
    | (a,b)::q -> let (a_, b_) = split q in
        a::a_, b::b_;;
split [(1, 2); (3, 4); (5, 6)];;

let ord x y = x<y;;

let rec merge ord l1 l2 = match (l1, l2) with 
    [], l_ -> l_
    | l_, [] -> l_
    | t1::q1, t2::q2 -> if (ord t1 t2) 
        then t1::(merge ord q1 l2)
        else t2::(merge ord l1 q2);;
        
merge ord [1; 3; 4; 6] [2; 5; 9];;

let rec union a b = match a with
    [] -> b
    | t::q when (List.mem t b) -> (union q b)
    | t::q -> t::(union q b);;
union [1; 3; 4] [3; 4; 5];;

let rec intersection a b = match a with
    [] -> []
    | t::q when (List.mem t b)-> t::(intersection q b)
    | t::q -> intersection q b;;
    
intersection [1; 3; 4] [3; 4; 5];;

let rec diff a b = match a with 
    [] -> []
    | t::q when (List.mem t b)-> diff q b
    | t::q -> t::(diff q b);;
diff [1; 3; 4] [3; 4; 5];;

let rec inclus a b = match a with
    [] -> true
    | t::q -> (List.mem t b) && (inclus q b);;

inclus [1; 2; 3] [3; 2; 4];;

let egal a b = (inclus a b) && (inclus b a);;

inclus [1; 2; 3] [3; 2; 1];;

let rec bin n = 
    let q = n / 2 and r = n mod 2 in
    if q = 0 then [r]
    else (bin q) @ [r];;
bin 16;;    

let dec l =
    let rec dec_ = function
        [] -> 0
        | t::q -> t + (2 * (dec_ q)); in
    dec_ (List.rev l);;

dec [1; 0; 0; 0; 0];;

let rec decomp n p =
    let i = ref p and l = ref [] in
    while !i <= n do
        if (n mod !i) = 0 then (
            l := [!i]@(decomp (n / !i) !i);
            i := n;
        );
        i := !i+1;
    done;
    !l;;
    
decomp 18 2;;

let isprime n = (List.length (decomp n 2)) = 1;;

isprime 17;;

let rec prime n = 
    if n <= 2 then [2]
    else 
        if isprime n then (prime (n-1))@[n]
        else (prime (n-1));;
prime 1_000;;

let keys key = match key with
    "M" -> 1000
    |"D" -> 500
    |"C" -> 100
    |"L" -> 50
    |"X" -> 10
    |"V" -> 5
    |"I" -> 1
    | _ -> failwith "Caratère non reconnu";;
keys "M";;

let roman str = 
    let n = String.length str and r = ref 0 in
    for i=0 to n-1 do
        let a = (String.sub str i 1) in
        if i=n-1 then r := !r + (keys a)
        else
            let b = (String.sub str (i+1) 1) in
            if keys a < keys b then r := !r - (keys a)
            else r := !r + (keys a);
    done;
    !r;;
roman "MDCVL";;

let anagrammes mot =
    let rec ana_aux a b =
        let n=String.length b in
        if n=1 then (print_string (a^b^" ");)
        else (
            for i=0 to n-1 do
                ana_aux 
                    (a^(String.sub b i 1))
                    ((String.sub b 0 i)^(String.sub b (i+1) (n-i-1)))
            done
        );
    in ana_aux "" mot; 
    print_newline();;
anagrammes "test";;

let rec coef_b n p =
    if n<0 || p<0 || p>n then 0
    else
        if p=0 || p=n then 1
        else (coef_b (n-1) p) + (coef_b (n-1) (p-1));;
coef_b 8 4;;
