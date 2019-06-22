let add_to_all elem list red =
  let rec aux l1 l2 =
    match l1 with
      [] -> l2
     |p::q  ->
       if red then 
         aux q (((true,elem)::p)::l2)
       else
         aux q (((false,elem)::p)::l2)
  in aux list []
;;


let forall f list red : 'a list list =
  let rec aux l1 l2  =
    match l1 with
      [] -> l2
     |p::q ->
       aux q
         (List.rev_append
            (add_to_all p (f q) red)
            l2)
  in aux list []
;;

let rec to_list_of_list l red =
  match l with
  |[] -> []
  |p::q ->
    if red then
      [(true,p)]::(to_list_of_list q red)
    else
      [(false,p)]::(to_list_of_list q red)
;;


let rec k_parmis_n k red l =
  if ((List.length l) < k) || k < 0 then
    []
  else
    match l with
    |[] -> []
    |p::q ->
      if k = 1 then
        to_list_of_list (p::q) red
      else
        let f = k_parmis_n (k-1) red in
        forall f (p::q) red
;;
