(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   leibniz_pi.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/19 11:21:42 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/19 12:41:05 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_sum f lo_idx up_idx = 
  if up_idx < lo_idx then
    nan
  else
    let rec sum_ idx acc = 
      if idx >up_idx then
        acc
      else
        sum_ (idx + 1) (acc +. f (idx))
      in
      sum_ lo_idx 0.0

(* let leibniz i = 
  ((-1.0 ** float_of_int i) /. (2. *. float_of_int i +. 1.)) *)

let leibniz_pi delta =
    let pi = 4. *. atan 1. in
    let calc_pi it = 
      ft_sum (fun i->((-1.0 ** float_of_int i) /. (2. *. float_of_int i +. 1.))) 0 it *. 4. in
    if delta < 0. then -1
    else
      let rec calc_delta it =
        let diff = 
          if pi >  (calc_pi it) then
            pi -. (calc_pi it)
          else
            (calc_pi it) -. pi
        in
        if diff <= delta then
          it
        else
          calc_delta (it + 1)
      in
    calc_delta 1
      
    
let main () =
  print_int((leibniz_pi (-1.)));
  print_char '\n';
  print_int((leibniz_pi 0.0001 ));
  print_char '\n'

let () = main ()