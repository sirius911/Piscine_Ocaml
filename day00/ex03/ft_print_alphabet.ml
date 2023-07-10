(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_alphabet.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/05 21:14:26 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/05 21:41:32 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_alphabet () =
    let rec alphabet c = match c with
      | 'z' -> print_endline "z"
      | _ -> print_char c ; alphabet (char_of_int ((int_of_char c) + 1))
    in
    alphabet 'a'
  
let main () =
  ft_print_alphabet ()

let () = main ()