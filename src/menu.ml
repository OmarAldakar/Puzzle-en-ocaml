open Graphics;;
open Sat_solver;;
open Bsp;;
open Bsp_unique;;
open Main;;

exception End;;

type player = Nil | Jouer of int;;
type buttons = Vide | Button;;
type board = buttons array;;

let create_menu () : board =
  Array.make 4 Vide;;

let draw_empty_board () : unit =
  for i=0 to 4 do
    begin
    set_color black;
    draw_rect 400 (i*100+75) 400 100;
    end
  done
;;

let draw_nom () : unit =
  moveto 450 550;
  draw_string "PLAY THE GAME";
  moveto 450 450;
  draw_string "SETTING THE HAUTEUR";
  moveto 450 350;
  draw_string "GAME RULES";
  moveto 450 250;
  draw_string "VERSION : Beta 1.1";
  moveto 450 150;
  draw_string "EXIT"
;;

let draw_maker () : unit =
  moveto 1100 120;
  draw_string "Omar ALDAKAR";
  moveto 1100 100;
  draw_string "Yuchen BAI"
;;


let draw_hauteur_board () : unit =
  for i=0 to 3 do
    begin
    set_color red;
    draw_rect 870 (i*60+275) 180 60;
    end
  done
;;

let draw_hauteur_number () : unit =
  moveto 890 300;
  draw_string "Hauteur 2";
  moveto 890 360;
  draw_string "Hauteur 3";
  moveto 890 420;
  draw_string "Hauteur 5";
  moveto 890 480;
  draw_string "Hauteur 7";

;;

let affiche_game_rules () : unit= 
  set_color red;
  moveto 430 290;
  draw_string "Click the case for setting the color or change color";
  moveto 430 320;
  draw_string "Press 'h' for get the solution";
;;

let affiche_version () : unit= 
  set_color red;
  moveto 450 220;
  draw_string "Bonne annee"
;;

let affiche_boutton_retourne () : int =
        set_color black;
        draw_circle 1200 575 28;
        set_color white;
        fill_circle 1200 575 30;
        set_color black;
        moveto 1188 570;
        draw_string "MENU";

        let rec aux () =
        (
        let e = wait_next_event [Button_down] in
        let pos_x = e.mouse_x in
        let pos_y = e.mouse_y in
        if(pos_x >=1170) && (pos_x<=1230) && (pos_y >=535) && (pos_y <= 615) then
                100
        else aux ()
        )
        in aux ()
;;

let rec get_hauteur (): int = 
       draw_hauteur_board ();
       draw_hauteur_number ();
       let aux () =      
       (
       let e = wait_next_event [Button_down] in
       let pos_x = e.mouse_x in
       let pos_y = e.mouse_y in
       if (pos_x >= 890) && (pos_x <= 1070) then
         begin
           if (pos_y >= 275) && (pos_y) <= 335 then 2 
           else if (pos_y >= 335) && (pos_y <= 395) then 3
           else if (pos_y >= 395) && (pos_y <= 455) then 5
           else if (pos_y >= 455) && (pos_y <= 515) then 7
           else  get_hauteur ()
         end
       else if (pos_x >=400) && (pos_x <= 801) then
         begin
                 if(pos_y>=475) &&(pos_y <= 575) then 5
                 else if (pos_y >=275)&&(pos_y <=375) then (affiche_game_rules (); get_hauteur() )
                 else if (pos_y >=175)&&(pos_y <=275) then (get_hauteur () )
                 else if (pos_y >=75)&&(pos_y <= 175) then (print_endline "Bonne annee !!"; exit 0)
                 else 
                         get_hauteur ()
         end
       
       else
         get_hauteur ()
     ) in aux () 
;;

let draw_board_unique () =
        set_color black;
        draw_rect 340 500 60 50;
        draw_rect 340 450 60 50;

        moveto 350 530;
        draw_string "naive";
        moveto 350 480;
        draw_string "unique"

;;


let unique_ou_pas (): int = 

        draw_board_unique ();
        let rec aux () = 
        let e = wait_next_event [Button_down] in
        let pos_x = e.mouse_x in
        let pos_y = e.mouse_y in

        if (pos_x >=340) && (pos_x <= 400) then
               begin
               if(pos_y >=500) && (pos_y <= 550)then 1 (*bsp naive*)
               else if (pos_y>=450) && (pos_y <= 500) then 2 (*bsp unique*)
               else aux ()
               end
        else if (pos_y >= 75) && (pos_y <= 175) && (pos_x >= 400) && (pos_x <= 801) then (exit 0)
        else if (pos_y >= 275) && (pos_y <= 375) && (pos_x >= 400) && (pos_x <= 801) then ( affiche_game_rules ();aux ())
        else aux ()

        in aux ()
;;

let rec next_action (b : board) (p : player) : int =
  draw_empty_board ();
  draw_nom ();
  draw_maker ();
  
  match p with
  | Nil -> 0
  | Jouer x ->
     (
       let e = wait_next_event [Button_down] in
       let pos_x = e.mouse_x in
       let pos_y = e.mouse_y in
       if (pos_x >= 400) && (pos_x <= 801) then
         begin
           if (pos_y >= 475) && (pos_y) <= 575 then 10 (* go to play the game*)
           else if (pos_y >= 375) && (pos_y <= 475) then 9 (* setting the hauteur*)
           else if (pos_y >= 275) && (pos_y <= 375) then 8 (* affiche game rules*)
           else if (pos_y >= 175) && (pos_y <= 275) then 7
           else if (pos_y >= 75) && (pos_y <= 175) then 6 (* exit the game*)
           else  0
         end
       
       else 
         next_action b p
     );;

let main() =
  open_graph (" 1300x675");
  let b = create_menu () in
  let p = Jouer 1 in
  draw_empty_board ();

  let rec aux () = 
  let action = next_action b p in
  (
  if action = 10 then (
          go_to_play (unique_ou_pas ()) 3 ;
          if affiche_boutton_retourne () = 100 then 
                 begin 
                 clear_graph (); 
                 aux () 
                 end 
           )
  else if action = 9 then (go_to_play (unique_ou_pas ()) (get_hauteur ()) ; 
           if affiche_boutton_retourne () = 100 then 
                 begin 
                 clear_graph (); 
                 aux () 
                 end 
           )
  else if action = 8 then (affiche_game_rules (); aux () )
  else if action = 7 then (affiche_version (); aux ())
  else (print_endline "Bonne année !!!" ; exit 0)
  )

  in try aux () with
     |Graphic_failure x -> ()
     |End -> ()
;;

let _ = main ()
;;  
