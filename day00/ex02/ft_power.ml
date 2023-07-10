(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_power.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/05 19:39:15 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/05 21:10:51 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_power x y = match (x,y) with
  | (x,y) when x < 0 || y < 0 -> -1
  | (0,0) -> -1
  | (_,0) -> 1
  | (_,_) -> x * ft_power x (y - 1)

let main () = 
  print_int (ft_power 2 4); print_char '\n';
  print_int (ft_power 0 4); print_char '\n';
  print_int (ft_power 4 0); print_char '\n';
  print_int (ft_power (-5) 4); print_char '\n';
  print_int (ft_power 2 (-4)); print_char '\n'
  
let () = main ()