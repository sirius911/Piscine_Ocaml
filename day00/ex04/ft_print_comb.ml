(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/05 21:42:53 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/05 22:18:49 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_comb () =
  let n = 0 in
	let _100 x = x / 100 in         (* hundred of x *)
	let _10 x = (x mod 100) / 10 in (* ten of x *)
	let _0 x = x mod 10 in          (* unit of x *)
  
	let before hundred ten numeral =
		if hundred > ten && hundred > numeral
			then false
		else if ten > numeral || ten < hundred
			then false
		else true
	in

  let print comb = 
    if (comb <> 12) then
      print_string ", " ;
    if (comb < 100) then
        print_int 0 ;
    print_int comb
  in
  
	let rec loop comb =
		if comb < 1000 then
			begin
				if ((_100 comb) <> (_10 comb) && (_100 comb) <> (_0 comb) && (_10 comb) <> (_0 comb))
					&& ( before (_100 comb) (_10 comb) (_0 comb) )
				then
					print comb;
				loop (comb + 1)
			end
	in
	loop n ;
	print_string "\n"

let main () =
	ft_print_comb ()

let () = main ()