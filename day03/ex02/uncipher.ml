(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   uncipher.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/08/30 21:23:02 by clorin            #+#    #+#             *)
(*   Updated: 2023/08/30 21:30:19 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let uncaesar (str : string) (n : int) : string = 
  Cipher.caesar str (-n)

let unrot42 str = 
  Cipher.caesar str (-42)

let rec ft_uncrypt (str: string) (f: (string -> string) list) : string =
  match List.rev f with  (* Inverse the list of functions *)
  | head::queue -> ft_uncrypt (head str) (List.rev queue)  (* Apply functions in reversed order *)
  | _ -> str
  