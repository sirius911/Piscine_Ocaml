(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   micronap.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/04 16:07:38 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/04 17:35:56 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* compil with
   ocamlfind ocamlopt -package unix -linkpkg micronap.ml
*)
let do_sleep sec =
  let my_sleep () = Unix.sleep 1 in
    for i = 0 to (sec - 1) do
        my_sleep ()
    done

let () = 
    if Array.length (Sys.argv) = 2 then
        begin
            let sec =
                try int_of_string(Array.get Sys.argv 1) with
                    | _         -> exit 1
            in
            do_sleep sec
        end