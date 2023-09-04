(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_ref.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/04 17:40:52 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/04 17:52:56 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a ft_ref = {mutable contents : 'a}

let return (v : 'a): 'a ft_ref = 
  {contents = v}

let get (v : 'a ft_ref): 'a = 
  v.contents

let set (v : 'a ft_ref) (value : 'a) : unit =
  v.contents <- value
 
let bind (v : 'a ft_ref) (f : ('a -> 'b ft_ref)) : 'b ft_ref = 
  f (get v)


let () =
    let a = return 420 in
    Printf.printf "%d\n" (get a);
    set a 42;
    Printf.printf "%d\n" (get a);
    let b = bind a (fun x -> return "abcdef") in
    Printf.printf "%s\n" (get b);
    set b "ghijkl";
    Printf.printf "%s\n" (get b);
    let c = bind a (fun x -> return (x * 2)) in
    Printf.printf "%d\n" (get c);
    set c 42;
    Printf.printf "%d\n" (get c);
