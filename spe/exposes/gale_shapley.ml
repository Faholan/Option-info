(*Algorithme de Gale Shapley des mariages stables

Copyright (C) 2022  Faholan <https://github.com/Faholan>
*)

type humain = {
  nom: string ;
  preferences: string list ;
} ;;

let rec prefere preferences h1 h2 = match preferences with
  (*Check if h1 is preferred over h2*)
  | h::t when h = h1 -> true
  | h::t when h = h2 -> false
  | _::t -> prefere t h1 h2
  | [] -> false
;;

let find_opt_ind func arr =
  (*stdlib function

  find first element satisfying the predictate
  *)
  let rec _find_rec = function
    | i when i = Array.length arr -> None
    | i when func arr.(i) -> Some i
    | i -> _find_rec (i + 1)
  in _find_rec 0
;;

let rec get_preferences_index reference = function
  (*Convert a list of names to a list of indexes*)
  | [] -> []
  | h::t -> match find_opt_ind (fun pers -> pers.nom = h) reference with
    | None -> get_preferences_index reference t
    | Some x -> x::(get_preferences_index reference t)
;;

let gale_shapley hommes femmes =
  let status = Array.init (Array.length hommes) (fun i -> get_preferences_index femmes hommes.(i).preferences)
  (*Convert name list to index list; current status of men*)
  and preferences_femmes = Array.init (Array.length femmes) (fun i -> get_preferences_index hommes femmes.(i).preferences)
  (*Convert name list to inde list*)
  and engaged_homme = Array.make (Array.length hommes) (-1)
  (*Current status of men*)
  and engaged_femme = Array.make (Array.length femmes) (-1)
  (*Current status of women*)
  and to_match = Queue.create () in
    (*Men to process*)
    for i = 0 to Array.length hommes - 1 do
      if hommes.(i).preferences <> [] then
        Queue.add i to_match
      done ;
    let rec engage () = if Queue.is_empty to_match then
        let rec process_engagement_homme = function
          (*Present the results as names.*)
          | i when i = Array.length hommes -> process_engagement_femme (Array.length femmes - 1)
          | i when engaged_homme.(i) = -1 -> (Some hommes.(i).nom, None)::(process_engagement_homme (i + 1))
          | i -> (Some hommes.(i).nom, Some femmes.(engaged_homme.(i)).nom)::(process_engagement_homme (i + 1))
        and process_engagement_femme = function
          | i when i = Array.length femmes -> []
          | i when engaged_femme.(i) = -1 -> (None, Some femmes.(i).nom)::(process_engagement_femme (i + 1))
          | i -> process_engagement_femme (i + 1)
        in process_engagement_homme 0
      else (
        let i = Queue.pop to_match in
          let j = List.hd status.(i) in
            status.(i) <- List.tl status.(i) ;
            (*i proposes to j*)
            if engaged_femme.(j) = -1 then (
              (*j isn't married*)
              engaged_femme.(j) <- i ;
              engaged_homme.(i) <- j ;
            ) else (
              (*j is married*)
              if prefere preferences_femmes.(j) i engaged_femme.(j) then
                (*j divorces*)
                engaged_homme.(engaged_femme.(j)) <- -1 ;
                if status.(engaged_femme.(j)) <> [] then
                  (*The abandoned man should move on and find a new woman*)
                  Queue.add engaged_femme.(j) to_match ;
                engaged_femme.(j) <- i ;
                engaged_homme.(i) <- j ;
            ) ;
            engage ()
      )
    in engage ()
;;

let hommes = [|
  {
    nom = "Roméo" ;
    preferences = [
      "Robert";
      "Cléopâtre";
      "Cruella";
      "Edith Piaf";
      "Raiponce";
      "Hélène";
      "Circée";
      "Bowsette";
      "Zelda";
      "Shéhérazade";
    ] ;
  } ;
  {
    nom = "Figaro" ;
    preferences = [
      "Hélène";
      "Circée";
      "Zelda";
      "Cruella";
      "Edith Piaf";
      "Robert";
      "Bowsette";
      "Cléopâtre";
      "Shéhérazade";
      "Raiponce";
    ] ;
  } ;
  {
    nom = "Jésus 2 - le retour" ;
    preferences = [
      "Edith Piaf";
      "Hélène";
      "Robert";
      "Circée";
      "Bowsette";
      "Shéhérazade";
      "Cléopâtre";
      "Cruella";
      "Zelda";
      "Raiponce";
    ] ;
  } ;
  {
    nom = "Barbe bleue" ;
    preferences = [
      "Robert";
      "Shéhérazade";
      "Cléopâtre";
      "Circée";
      "Hélène";
      "Raiponce";
      "Cruella";
      "Edith Piaf";
      "Zelda";
      "Bowsette";
    ] ;
  } ;
  {
    nom = "Garry Topper" ;
    preferences = [
      "Hélène";
      "Cruella";
      "Circée";
      "Edith Piaf";
      "Robert";
      "Shéhérazade";
      "Cléopâtre";
      "Zelda";
      "Raiponce";
      "Bowsette";
    ] ;
  } ;
  {
    nom = "Genghis Khan" ;
    preferences = [
      "Zelda";
      "Cruella";
      "Cléopâtre";
      "Shéhérazade";
      "Robert";
      "Bowsette";
      "Edith Piaf";
      "Hélène";
      "Circée";
      "Raiponce";
    ] ;
  } ;
  {
    nom = "Caligula" ;
    preferences = [
      "Robert";
      "Cruella";
      "Cléopâtre";
      "Raiponce";
      "Edith Piaf";
      "Circée";
      "Zelda";
      "Shéhérazade";
      "Bowsette";
      "Hélène";
    ] ;
  } ;
  {
    nom = "Staline" ;
    preferences = [
      "Robert";
      "Raiponce";
      "Cruella";
      "Shéhérazade";
      "Zelda";
      "Hélène";
      "Cléopâtre";
      "Edith Piaf";
      "Circée";
      "Bowsette";
    ] ;
  } ;
  {
    nom = "Midas" ;
    preferences = [
      "Circée";
      "Robert";
      "Hélène";
      "Shéhérazade";
      "Bowsette";
      "Cruella";
      "Edith Piaf";
      "Cléopâtre";
      "Raiponce";
      "Zelda";
    ] ;
  } ;
  {
    nom = "Cules Jésar" ;
    preferences = [
      "Robert";
      "Shéhérazade";
      "Cléopâtre";
      "Bowsette";
      "Edith Piaf";
      "Hélène";
      "Raiponce";
      "Circée";
      "Zelda";
      "Cruella";
    ] ;
  }
|] ;;

let femmes = [|
  {
    nom = "Cléopâtre" ;
    preferences = [
      "Jésus 2 - le retour";
      "Staline";
      "Roméo";
      "Midas";
      "Cules Jésar";
      "Genghis Khan";
      "Caligula";
      "Figaro";
      "Garry Topper";
      "Barbe bleue";
    ] ;
  } ;
  {
    nom = "Hélène" ;
    preferences = [
      "Genghis Khan";
      "Cules Jésar";
      "Midas";
      "Caligula";
      "Figaro";
      "Barbe bleue";
      "Roméo";
      "Garry Topper";
      "Staline";
      "Jésus 2 - le retour";
    ] ;
  } ;
  {
    nom = "Robert" ;
    preferences = [
      "Jésus 2 - le retour";
      "Roméo";
      "Genghis Khan";
      "Cules Jésar";
      "Garry Topper";
      "Caligula";
      "Figaro";
      "Staline";
      "Midas";
      "Barbe bleue";
    ] ;
  } ;
  {
    nom = "Circée" ;
    preferences = [
      "Caligula";
      "Cules Jésar";
      "Staline";
      "Barbe bleue";
      "Figaro";
      "Jésus 2 - le retour";
      "Roméo";
      "Garry Topper";
      "Genghis Khan";
      "Midas";
    ] ;
  } ;
  {
    nom = "Edith Piaf" ;
    preferences = [
      "Staline";
      "Figaro";
      "Roméo";
      "Genghis Khan";
      "Barbe bleue";
      "Garry Topper";
      "Cules Jésar";
      "Midas";
      "Caligula";
      "Jésus 2 - le retour";
    ] ;
  } ;
  {
    nom = "Zelda" ;
    preferences = [
      "Jésus 2 - le retour";
      "Garry Topper";
      "Genghis Khan";
      "Staline";
      "Midas";
      "Roméo";
      "Cules Jésar";
      "Figaro";
      "Caligula";
      "Barbe bleue";
    ] ;
  } ;
  {
    nom = "Bowsette" ;
    preferences = [
      "Garry Topper";
      "Jésus 2 - le retour";
      "Staline";
      "Caligula";
      "Figaro";
      "Roméo";
      "Cules Jésar";
      "Barbe bleue";
      "Genghis Khan";
      "Midas";
    ] ;
  } ;
  {
    nom = "Shéhérazade" ;
    preferences = [
      "Staline";
      "Cules Jésar";
      "Figaro";
      "Barbe bleue";
      "Caligula";
      "Roméo";
      "Genghis Khan";
      "Garry Topper";
      "Jésus 2 - le retour";
      "Midas";
    ] ;
  } ;
  {
    nom = "Cruella" ;
    preferences = [
      "Figaro";
      "Midas";
      "Garry Topper";
      "Cules Jésar";
      "Roméo";
      "Jésus 2 - le retour";
      "Caligula";
      "Genghis Khan";
      "Barbe bleue";
      "Staline";
    ] ;
  } ;
  {
    nom = "Raiponce" ;
    preferences = [
      "Jésus 2 - le retour";
      "Figaro";
      "Genghis Khan";
      "Barbe bleue";
      "Roméo";
      "Midas";
      "Staline";
      "Garry Topper";
      "Cules Jésar";
      "Caligula";
    ] ;
  }
|] ;;
