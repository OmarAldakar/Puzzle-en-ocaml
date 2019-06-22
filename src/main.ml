open Graphics;;

open Sat_solver;;
open Bsp;;
open Bsp_unique;;

exception End;;

type player = Nil | Jouer of int;;
type buttons = Vide | Button;;
type board = buttons array;;

let rec main_loop
          (bsp_jeu : bsp)
          (bsp_solution : bsp): unit =

  (
    affiche_bsp
      bsp_jeu
      bsp_solution;
    if is_gagnant bsp_jeu bsp_solution then
      ()
    else 
      (
        let e = wait_next_event [Button_down;Key_pressed] in
        if (e.button) then
          let x = e.mouse_x and
              y = e.mouse_y in
          
          main_loop
            (click (x,y) bsp_jeu)
            bsp_solution
        else
          match e.key with
          |'h' ->
            main_loop
              (get_solution bsp_solution bsp_jeu)
              bsp_solution
          | _-> main_loop bsp_jeu bsp_solution
      )
  )
;;

let get_function unique =
  if unique <> 1 then
    random_bsp_unique
  else
    random_bsp_naive
;;

(* boucle utilisé en menu*)
let go_to_play (unique : int) (h : int) =
  
  open_graph " 1300x675";
  let f = get_function unique in
  
  let bsp_solution =
    f h (size_x()) (size_y()) in
  let bsp_jeu =
    init_feuille bsp_solution in 

  try
    main_loop bsp_jeu bsp_solution
  with
    End -> ();
;;
