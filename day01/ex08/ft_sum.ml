(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_sum.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/19 10:51:50 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/19 12:45:09 by clorin           ###   ########.fr       *)
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

let leibniz i = 
  (-1.0 ** float_of_int i) /. (2. *. float_of_int i +. 1.)

let main () =
  print_float(ft_sum (fun i -> float_of_int(i * i)) 1 10);
  print_char '\n';
  print_float((ft_sum (leibniz) 0 1000000) *. 4.);
  print_char '\n'
  
let () = main ()
