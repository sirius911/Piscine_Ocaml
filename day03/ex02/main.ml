(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/08/30 21:26:34 by clorin            #+#    #+#             *)
(*   Updated: 2023/08/30 21:39:03 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let main () = 
  let fonctions1 = [(fun s -> Cipher.xor s 18);(fun s -> Cipher.caesar s 42)] in
  let fonctions2 = [(fun s -> Cipher.xor s 18);(fun s -> Uncipher.uncaesar s 42)] in
  let sentence = "Hello world" in 
  print_endline (Cipher.rot42 "A$bcdefghijklmnopqrstuvwxyza");
  print_endline(Cipher.caesar "abcdef" 42);
  print_endline(Uncipher.uncaesar "qrstuv" 42);
  print_endline(Cipher.rot42 "AbCdEf");
  print_endline(Uncipher.unrot42 "QrStUv");
  print_endline(Cipher.xor "ABCDEF" 42);
  print_endline(Cipher.xor "khinol" 42);
  print_string sentence;
  print_string(" -- ft_scrypt() --> ");
  let code = Cipher.ft_crypt sentence fonctions1 in
  print_string code;
  print_string(" -- ft_unscrypt() --> ");
  print_string(Uncipher.ft_uncrypt code fonctions2);
  print_endline "\n-----------"
let () = main ()