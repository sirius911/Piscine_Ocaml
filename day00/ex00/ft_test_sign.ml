(* # **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    ft_test_sign.ml                                    :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: clorin <clorin@student.42.fr>              +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2023/06/30 08:16:20 by clorin            #+#    #+#              #
#    Updated: 2023/06/30 08:16:27 by clorin           ###   ########.fr        #
#                                                                              #
# **************************************************************************** # *)

let ft_test_sign x =
  if x >= 0 
  then print_endline "positive"
  else print_endline "negative"

let print_positive x = 
  print_string "Test with [";
  print_int x;
  print_string "]: ";
  ft_test_sign x

let main () =
  print_positive 42;
  print_positive 0;
  print_positive (-42)

let () = main ()