type label = {coord : int ; colored : bool};;

type bsp = R of color option | L of label * bsp * bsp;;

type line = {pos_1 : int * int ; pos_2 : int * int ; color : color};;

type rectangles = {pos : int * int * int * int ; color : color};;


let string_of_color color =
  match color with
  |x when x = blue -> "blue"
  |x when x = red -> "red"
  |_ -> "magenta"
;;

let rec print_bsp bsp str =
  match bsp with
  |R(x) when x = None -> print_endline (str^"|NONE");
  |R(x) when x = Some (blue) -> print_endline (str^"|BLUE");
  |R(x) when x = Some (red) -> print_endline (str^"|RED");
  |R(x) -> print_endline "| COLOR";
  |L(label,left,right) ->   
    (
      print_endline (str^"|LEFT \n");
      print_bsp left (str^"    ");
      print_endline (str^"|RIGHT \n");
      print_bsp right (str^"    ")   
    )
;;

let random_number
      ((sup_x,sup_y,inf_x,inf_y) : int * int * int * int)
      (is_hauteur_pair : bool) : int option =
  
  
  if (is_hauteur_pair && sup_x - inf_x > 40) then (* 10 / 90 %*)
    let x_1 = int_of_float
                (0.8 *. (float_of_int (sup_x - inf_x))) in
    let x_2 = int_of_float
                (0.1 *. (float_of_int (sup_x - inf_x))) in
    Some((Random.int x_1) + inf_x + x_2)
  else if (not is_hauteur_pair && sup_y - inf_y > 40) then 
    let y_1 = int_of_float
                (0.8 *. (float_of_int (sup_y - inf_y))) in
    let y_2 = int_of_float
                (0.1 *. (float_of_int (sup_y - inf_y))) in
    Some((Random.int y_1) + inf_y + y_2)
  else
    None
;;

let new_coord
      (new_val : int)
      ((sup_x,sup_y,inf_x,inf_y) : int * int * int * int)
      (is_left_node  : bool)
      (is_hauteur_pair  : bool) =

  if is_hauteur_pair then
    if is_left_node then
      (new_val,sup_y,inf_x,inf_y)
    else
      (sup_x,sup_y,new_val,inf_y)
  else
    if is_left_node then
      (sup_x,new_val,inf_x,inf_y)
    else
      (sup_x,sup_y,inf_x,new_val)
;;

let get_feuille () =
  if (Random.bool()) then
    R(Some(blue))
  else
    R(Some(red))
;;

let rec random_bsp_aux
          (hauteur : int)
          (bornes : int * int * int * int )
          (is_hauteur_pair  : bool) : bsp =
  
  if hauteur = 0 then
    get_feuille ()
  else
    let coord = random_number
                    bornes
                    (is_hauteur_pair) in
    
    match coord with
    |None -> get_feuille()
    |Some(new_val) -> 
      L(
          {
            coord   = new_val;
            colored = true;
          },

          (random_bsp_aux
             (hauteur - 1)
             (new_coord
                new_val bornes
                true is_hauteur_pair)
             (not is_hauteur_pair)),

          (random_bsp_aux
             (hauteur - 1)
             (new_coord
                new_val bornes
                false is_hauteur_pair)
             (not is_hauteur_pair) )
        )
;;

let random_bsp_naive
      (hauteur : int)
      (size_x : int)
      (size_y : int) : bsp =

  Random.self_init();
  random_bsp_aux
    hauteur
    (size_x,size_y,0,0) 
    (true)
;;



(*Les fonctions suivantes pourront être mis 
dans un autre module une fois les structures de données
bien organisé*)

let value_of_color color =
  match color with
  |None -> (0,0)
  |Some(x) when x = red  -> (0,1)
  |Some(x) when x = blue -> (1,0)
  |Some(x) -> (0,0)
;;

let get_color (b,r) =
  if b > r then
    blue
  else if b = r then
    magenta
  else
    red
;;

let color_of_line bsp = 

  let rec aux
            bsp'
            is_hauteur_pair'
            from_left =
    match bsp' with
    |L(_,left,right) ->
      if (not is_hauteur_pair') then
        let (b1,r1) = aux left  (not is_hauteur_pair') from_left and
            (b2,r2) = aux right (not is_hauteur_pair') from_left
        in (b1+b2, r1+r2)
      else if from_left then
        aux right (not is_hauteur_pair') from_left
      else
        aux left (not is_hauteur_pair') from_left
     
    |R(x) -> value_of_color x

  in
  
  let color_val =
    match bsp with
    |L(_,left,right) ->
      let (b1,r1) = aux left  false true and
          (b2,r2) = aux right false false
      in (b1+b2, r1+r2)
    |R(x) -> value_of_color x

  in get_color color_val
;;

let get_color_for_line label color =
  if (not label.colored) then black
  else
    match color with
    | x when x = blue -> blue
    | x when x = red -> red
    | x when x = magenta -> rgb 169 69 211
    |_ -> black
;; 

let create_line
      (is_hauteur_pair : bool)
      ((last_x_sup,last_y_sup, last_x_inf,last_y_inf) : int * int * int * int)
      (new_val : int)
      (label : label)
      (bsp : bsp) =
  
  if is_hauteur_pair then
    {pos_1 = (new_val,last_y_inf) ; pos_2 = (new_val,last_y_sup);
     color = get_color_for_line label (color_of_line bsp)}
  else
    {pos_1 = (last_x_inf,new_val) ; pos_2 = (last_x_sup,new_val);
     color = get_color_for_line label (color_of_line bsp)}
;;



let lines_from_bsp  (main_bsp : bsp) =

  let rec lines_from_bsp_aux
            (bsp' : bsp)
            (bornes : int * int * int * int)
            (is_hauteur_pair : bool) =

    match bsp' with
    |L(label, left, right) ->
      
      let new_val = label.coord in
      [(create_line
          is_hauteur_pair
          bornes
          new_val
          label
          (L(label, left, right))
      )]
      @
        (lines_from_bsp_aux
           right
           (new_coord
              new_val bornes
              false is_hauteur_pair)
           (not is_hauteur_pair))

      @ (lines_from_bsp_aux
           left
           (new_coord
              new_val bornes
              true is_hauteur_pair)
           (not is_hauteur_pair))
      
    | _ -> []

  in lines_from_bsp_aux
       main_bsp
       (size_x(),size_y(),0,0)
       true  
;;

let get_color_for_rectangle x =
  match x with
  |None -> white
  |Some(x) when x = red -> rgb 250 80 80
  |Some(x) when x = blue -> rgb 31 188 244
  |_ -> black
;;

let rectangles_from_bsp  (main_bsp : bsp) =

  let rec rectangles_from_bsp_aux
            (bsp' : bsp)
            (bornes : int * int * int * int)
            (is_hauteur_pair : bool) =

    match bsp' with
    |L(label, left, right) ->

      let new_val = label.coord in
      (rectangles_from_bsp_aux
         right
         (new_coord
            new_val bornes
            false is_hauteur_pair)
         (not is_hauteur_pair))

      @ (rectangles_from_bsp_aux
           left
           (new_coord
              new_val bornes
              true is_hauteur_pair)
           (not is_hauteur_pair))
      
    | R(x) -> [ { pos = bornes; color = get_color_for_rectangle x } ]

  in rectangles_from_bsp_aux
       main_bsp
       (size_x(),size_y(),0,0)
       true  
;;

let fill_colored_rect
      (sup_x,sup_y,inf_x,inf_y)
      color =
  set_color color;
  fill_rect (inf_x + 2) (inf_y + 2) (sup_x - inf_x - 4 ) (sup_y - inf_y - 4 )
;;

let draw_colored_line
      (x1,y1) (x2,y2) color =
  set_color color;
  moveto x1 y1;
  lineto x2 y2
;;
  
let affiche_bsp bsp bsp_solution =
  
  let list_rect = rectangles_from_bsp bsp in
  List.iter    
    (fun x -> fill_colored_rect x.pos x.color)
    list_rect;
  
  let list = lines_from_bsp bsp_solution in
  List.iter    
    (fun x -> draw_colored_line
                x.pos_1 x.pos_2 x.color)
    list
;;

let get_next_val (color : color option) =
  match color with
  |None -> Some(blue)
  |Some(x) when x = blue -> Some(red)
  |Some(x) when x = red -> None
  |_ -> None
;;

let click (x,y) bsp = 

  let rec aux
            bsp'
            is_hauteur_pair
            is_here = 

    match bsp' with 
    |R(x) when is_here -> R(get_next_val x)
    |R(x) -> R(x)
	|L(lab,left,right) -> 
      let new_val = lab.coord in
      if (is_here) then
        if (is_hauteur_pair && x > new_val) ||
             (not is_hauteur_pair && y > new_val) then        
          L (
              lab,
              aux left (not is_hauteur_pair) false,
              aux right (not is_hauteur_pair) true
            )
        else 
          L (
              lab,
              aux left (not is_hauteur_pair) true,
              aux right (not is_hauteur_pair) false
            )
      else
        L(lab,left,right)
        

  in aux bsp true true
;;

let rec init_feuille bsp =
  match bsp with
  |R(x) -> R(None)
  |L(lab,left,right) ->
    L (
        lab,
        (init_feuille left),
        (init_feuille right)
      )
;;

let print_color col =
  match col with
  |x when x=blue -> print_endline "blue"
  |x when x=red -> print_endline "red"
  |_ -> print_endline "magenta"
;;

let pr_info col1 col2 bsp1 bsp2 =
  if (col1 = col2) then
    ()
  else
    (print_endline "\n -------------";
     print_color col1;
     print_endline " |||| ";
     print_color col2;
     print_bsp bsp1 "";
    )
;;
          
    

let is_gagnant bsp bsp_solution =
  
  let rec aux
            bsp'
            bsp_solution' = 

    match bsp',bsp_solution' with
    |R(x),_ when x = None -> false
    |R(x),_ -> true

    |L(label1,l1,r1),L(label2,l2,r2) ->
      if not label2.colored then 
        aux l1 l2 && aux r1 r2
      else
        let c1 = color_of_line (L(label1,l1,r1)) in
        let c2 = color_of_line (L(label2,l2,r2)) in 
        (
          (*pr_info c1 c2 (L(label1,l1,r1)) (L(label2,l2,r2));*)
          (c1 = c2) && aux l1 l2 && aux r1 r2
        )
    |_,_ -> failwith "bsp and bsp_solution doesnt match"

  in
  (print_endline "BEGIN ::::";
  aux bsp bsp_solution)
;;
