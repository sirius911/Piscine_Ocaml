(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_countdown.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/06/30 08:35:49 by clorin            #+#    #+#             *)
(*   Updated: 2023/06/30 08:48:50 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_countdown x =
  if x <= 0 then
    begin
      print_int 0; print_char '\n'
    end
  else
    begin
      print_int x; print_char '\n';
      ft_countdown (x - 1)
    end
    
let main () = 
  ft_countdown 0;
  ft_countdown 5

let () = main ()