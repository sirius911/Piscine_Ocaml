(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sum.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/04 19:16:56 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/04 19:17:16 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let sum (a : float) (b : float) : float =
  a +. b

let () =
  Printf.printf "%f\n" (sum 20. 21.)