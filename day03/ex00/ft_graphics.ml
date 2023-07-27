(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_graphics.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/25 14:36:12 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/26 14:06:30 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree


let draw_square x y size =
	if size < 1 then
		()
	else
	begin
		Graphics.moveto (x - (size / 2)) (y - (size / 2));
    Graphics.lineto (x - (size / 2)) (y + (size / 2));
    Graphics.lineto (x + (size / 2)) (y + (size / 2));
    Graphics.lineto (x + (size / 2)) (y - (size / 2));
    Graphics.lineto (x - (size / 2)) (y - (size / 2));
	end

let draw_string x y str = 
  Graphics.moveto x y;
  Graphics.draw_string str

let coord_left (x,y) = 
  ((x + 20),(y))

let draw_tree_node tree = 
  let start_x = 50 in
  let start_y = 400 in
  match tree with
  | Nil -> ()
  | Node (value, left, right) ->
    draw_square (start_x) start_y 50;
    let (x,y) = coord_left (start_x, start_y) in
    Graphics.moveto (start_x + 25) start_y;
    Graphics.lineto (start_x + 50 ) (start_y + 50);
    draw_square (start_x + 25 + 50)(start_y + 50) 50
    
let main () =
	Graphics.open_graph " 1500x800" ;
	draw_square 200 400 50 ;
  draw_string 200 400 "Titi";

  let node = Node(42, Nil, Nil) in
  draw_tree_node node;
	ignore ( Graphics.read_key () ) ;
	Graphics.close_graph ()

let () = ignore(main ())
