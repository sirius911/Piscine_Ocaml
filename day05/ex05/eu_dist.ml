(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   eu_dist.ml                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/05 14:11:18 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/05 14:48:27 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let eu_dist (a : float array) (b : float array) : float = 
  let i = ref 0 in
  let sum = ref 0.0 in
  while !i < Array.length a do
    begin
      sum.contents <- !sum +. ((a.(!i) -. b.(!i)) *. ((a.(!i) -. b.(!i)))) ;
      incr i
    end
  done;
  sqrt !sum

let () = 
  let pointA = [|0.0; 0.0; 0.0; 0.0|] in
  let pointB = [|2.0; 2.0; 2.0; 3.0|] in

  Printf.printf "euclidien Dist = %f\n" (eu_dist pointA pointB)