open Bsp;;
open K_parmis_n;;

module Variables = 
  struct
    type t = int
    let compare x y =
      if x > y then 1
      else if x = y then 0
      else -1
  end
;;

(*utilisation du foncteur Sat_solver.Make*)
module Sat = Sat_solver.Make(Variables);;

type bsp_of_var = V of int | L' of label * color * bsp_of_var * bsp_of_var;;

type bsp_of_fnc = FEUILLE | B of Sat.literal list list * bsp_of_fnc * bsp_of_fnc;;


let player_move
      (bsp : bsp)
      (bsp_of_var : bsp_of_var) =
  (
    let rec aux bsp' bsp_of_var' =
      (
        match (bsp',bsp_of_var') with
        |L(lab1,left,right),L'(lab2,_,left_var,right_var)->
          (aux left left_var) @ (aux right right_var)
        |R(x), V(y) when x = Some(red) -> [[(true,y)]]
        |R(x), V(y) when x = Some(blue) -> [[(false,y)]]
        |R(x), V(y) when x = None -> []
        |_-> failwith "arg error"
      )
    in aux bsp bsp_of_var
  )
;;

let neg_solution
      (bsp : bsp)
      (bsp_of_var : bsp_of_var) =
  (
    print_endline "LOG : neg_solution \n";
    let rec aux bsp' bsp_of_var' =
      (
        print_endline "LOG : neg_solution_aux \n";
        match (bsp',bsp_of_var') with
        |L(lab1,left,right),L'(lab2,_,left_var,right_var)->
          (aux left left_var) @ (aux right right_var)
        |R(x), V(y) when x = Some(red) -> [(false,y)]
        |R(x), V(y) when x = Some(blue) -> [(true,y)]
        |_-> failwith "arg error"
      )
    in aux bsp bsp_of_var
  )
;;

let rec magenta_exp l =
  let taille = (List.length l) in
  let k = (taille / 2) + 1 in (*l est toujours de taille pair sinon
                                il est stupide d'appeler magenta_exp*)
  List.rev_append
    (k_parmis_n k false l)
    (k_parmis_n k true l)
;;

let blue_or_red_exp red l =
  let taille = (List.length l) in

  if (taille mod 2) = 1 then 
    let k = (taille + 1) / 2 in
    k_parmis_n k red l
  else
    let k = (taille / 2) + 1 in
    List.rev_append
      (k_parmis_n k red l)
      (k_parmis_n (taille / 2) red l)
;;
  
let blue_exp l =
    blue_or_red_exp false l
;;
  
let red_exp l =
    blue_or_red_exp true l
;;
let get_list_of_var bsp = 

  let rec aux
            is_hauteur_pair'
            from_left
            bsp' =

    match bsp' with
    |L'(_,_,left,right) ->
      let f = aux (not is_hauteur_pair') from_left in
      
      if (not is_hauteur_pair') then
        List.rev_append
          (f left)
          (f right)
        
      else if from_left then
        f right
      else
        f left
     
    |V(x) -> [x] in
  
  match bsp with
  |L'(_,_,left,right) ->
    let l1 = aux false true left and
        l2 = aux false false right
    in List.rev_append l1 l2
     
  |V(x) -> [x]
;; (* peut surement se factoriser avec color_of_line à voir *)


let from_bsp_to_bsp_var bsp =

  let rec aux bsp' i =
    
    match bsp' with
    |L(lab,left,right) ->
      let (l' ,i') = aux left i in
      let (r',j') = aux right i' in
      let color = (color_of_line (L(lab,left,right))) in
        
      (L'(lab,color,l',r'),j')

    |R(x) -> (V(i),(i+1)) in

  let bsp_var,i  = aux bsp 0
                 
  in bsp_var
;;

let from_list_to_fnc
      l color =
  
  if color = blue then
    blue_exp l
  else if color = red then
    red_exp l     
  else
    magenta_exp l
;;

let from_bsp_var_to_fnc
      bsp_var =

  let rec aux bsp_var' =
    
    match bsp_var' with
    |L'(label,color,l,r) ->
          
      let list_var = get_list_of_var
                       (L'(label,color,l,r)) in
      
      let fnc = from_list_to_fnc
                  list_var color in
      
      B(fnc,(aux l),(aux r))
      
    |V(x) -> FEUILLE

  in aux bsp_var
;;

let is_unique bsp bsp_fnc bsp_of_var =
  
  let rec aux bsp' bsp_fnc' =
    
    match (bsp',bsp_fnc') with   
    |L(lab,left1,right1) , B(list,left2,right2) ->
      let l1 = (aux left1 left2) in
      let l2 = (aux right1 right2) in

      if lab.colored then
        List.rev_append list (List.rev_append l1 l2)
      else
        List.rev_append l1 l2
     
    |_,FEUILLE -> []
    |_ -> failwith "bsp and bsp_of_fnc doesnt match" in

  let fnc =
    List.rev_append
      (aux bsp bsp_fnc)
      [(neg_solution bsp bsp_of_var)] in

  ((Sat.solve fnc) = None)
;;

let rec switch_false coord bsp =
  match bsp with
  |L(lab,l,r) when coord = lab.coord ->
    L({coord = lab.coord;
       colored = false
      },l,r)
   
  |L(lab,l,r) ->
    let f = switch_false coord in
    L(lab,(f l),(f r))
    
  |R(x) -> R(x)
;;

let minimale bsp bsp_fnc bsp_of_var =

  let rec aux bsp_aux bsp_changement =

    match bsp_aux with
    |L(lab,l,r) ->
      
      let bsp' = switch_false
                   lab.coord
                   bsp_changement in
      
      if (is_unique bsp' bsp_fnc bsp_of_var) then
        L(
            {
              coord = lab.coord;
              colored = false
            },(aux l bsp'),(aux r bsp')
          )
      
      else 
        L(lab,(aux l bsp_changement),(aux r bsp_changement))
    |R(x) -> R(x)
  in aux bsp bsp
;;

let rec random_bsp_unique
      (hauteur : int)
      (size_x : int)
      (size_y : int) : bsp  =

    let bsp = random_bsp_naive
                hauteur size_x size_y in
 
    let bsp_var = (from_bsp_to_bsp_var bsp) in

    let bsp_fnc = from_bsp_var_to_fnc bsp_var in
    
    if not (is_unique bsp bsp_fnc bsp_var) then
      random_bsp_unique hauteur size_x size_y
    else
      minimale bsp bsp_fnc bsp_var
;;


let solution
      (bsp_solution : bsp)
      (bsp_game : bsp)
      (bsp_fnc : bsp_of_fnc)
      (bsp_of_var : bsp_of_var) =
  
  let rec aux bsp' bsp_fnc' =
    
    match (bsp',bsp_fnc') with   
    |L(lab,left1,right1) , B(list,left2,right2) ->
      let l1 = (aux left1 left2) in
      let l2 = (aux right1 right2) in

      if lab.colored then
        List.rev_append list (List.rev_append l1 l2)
      else
        List.rev_append l1 l2
     
    |_,FEUILLE -> []
    |_ -> failwith "bsp and bsp_of_fnc doesnt match" in

  let fnc =
    List.rev_append
      (aux bsp_solution bsp_fnc)
      (player_move bsp_game bsp_of_var) in

  Sat.solve fnc
;;



let rec get_value
      (var : int)
      (list : (bool * int) list) =
  match list with
  |[] -> failwith "argument error"
  |(bool,x)::q when x=var -> bool
  |p::q -> get_value var q
;;

let rec paint_solution
          bsp_var sol_list =
  match bsp_var with
  |L'(lab,_,l,r) -> L(lab,
                      paint_solution l sol_list,
                      paint_solution r sol_list)
  |V(x) ->
    if (get_value x sol_list) then
      R(Some(red))
    else
      R(Some(blue))
;;

let get_solution
      (bsp_solution : bsp)
      (bsp_game : bsp) =
  let bsp_var = (from_bsp_to_bsp_var bsp_solution) in

  let bsp_fnc = from_bsp_var_to_fnc bsp_var in

  let list_sol = solution
                   bsp_solution bsp_game
                   bsp_fnc bsp_var in

  match list_sol with
  |None -> bsp_game
  |Some(list) -> paint_solution bsp_var list
;;
