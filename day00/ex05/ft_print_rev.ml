(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_rev.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/05 22:21:25 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/05 22:48:26 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_rev str =
  let rec print i = 
    if i < 0 then
      print_char '\n'
    else
      begin
        print_char (String.get str i);
        print (i - 1)
      end
  in
  print ((String.length str) - 1)

let main () =
    ft_print_rev "Hello world !";
    ft_print_rev ""

let () = main ()