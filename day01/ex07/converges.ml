(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   converges.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/19 10:51:53 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/19 10:52:02 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec converges f x n =  
  if n < 0 then false
  else if (f x) = x then true
  else
    converges f (f x) (n-1)

let main () =
  print_endline(string_of_bool(converges (( * )2) 2 5));
  print_endline(string_of_bool(converges (fun x -> x / 2) 2 3));
  print_endline(string_of_bool(converges (fun x -> x / 2) 2 3));
  print_endline(string_of_bool(converges (fun x -> x * x) 1 10))

let () = main ()