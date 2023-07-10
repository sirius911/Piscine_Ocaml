(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_string_all.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/05 22:51:22 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/06 08:22:49 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_string_all predicat str =

  let rec test_predicat i =
    if not (predicat (String.get str i)) then
      false
    else if i = 0 then
      true
    else
      test_predicat (i - 1)
    in
    if String.length str = 0 then
      false
    else
      test_predicat (String.length str - 1)

let is_digit c = c >= '0' && c <= '9';;

let main () =
  print_endline(string_of_bool (ft_string_all is_digit "012345678"));
  print_endline(string_of_bool (ft_string_all is_digit "0123d45678"));
  print_endline(string_of_bool (ft_string_all is_digit ""))
  
let () = main ()
