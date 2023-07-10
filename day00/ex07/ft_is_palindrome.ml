(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_is_palindrome.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/07 08:33:47 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/07 09:14:41 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_is_palindrome str = 
  let len = (String.length str) - 1 in

  let rec pos i =
    if i = len  then
        if String.get str i <> String.get str (len - i) then
          false
        else
          true
    else
        if String.get str i <> String.get str (len - i) then
          false
        else
          pos (i + 1)
  in
  if str = "" then 
    true
  else
    pos 0 
  
let main () =
  print_endline (string_of_bool (ft_is_palindrome "radar"));
  print_endline  (string_of_bool (ft_is_palindrome "radars"));
  print_endline  (string_of_bool (ft_is_palindrome ""))
let () = main ()