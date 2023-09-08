(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/07 22:14:21 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/07 22:39:40 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () = 
    print_endline "\nTest avec les dalek";
    let dalek = new Dalek.dalek in
    let army = new Army.army in
        army#add dalek;
        army#add (new Dalek.dalek);
        army#add (new Dalek.dalek); 
        army#add (new Dalek.dalek);
        army#add (new Dalek.dalek);
        army#add (new Dalek.dalek);
        army#add (new Dalek.dalek);
        print_string "add 7 dalek, size of army = ";
        let list_army = army#get_army in
        print_int (List.length list_army);
        print_char '\n';
        print_string "delete 8 dalek, size of army = ";
        army#delete;
        army#delete;
        army#delete;
        army#delete;
        army#delete;
        army#delete;
        army#delete;
        army#delete;
        let list_army = army#get_army in
        print_int (List.length list_army);
        print_char '\n';

    print_endline "\nTest avec les people";
    let army1 = new Army.army in
        army1#add (new People.people "alex");
        army1#add (new People.people "truc");
        army1#add (new People.people "bidule");
        army1#add (new People.people "toto");
        army1#add (new People.people "titi");
        army1#add (new People.people "tata");
        army1#add (new People.people "tru");
        print_string "add 7 people, size of army = ";
        let list_army = army1#get_army in
        print_int (List.length list_army);
        print_char '\n';
        army1#delete;
        army1#delete;
        army1#delete;
        army1#delete;
        army1#delete;
        army1#delete;
        army1#delete;
        army1#delete;
        print_string "delete 8 dalek, size of army = ";
        let list_army = army1#get_army in
        print_int (List.length list_army);
        print_char '\n';

    print_endline "\nTest avec les docteur";
    let army2 = new Army.army in
        army2#add (new Doctor.doctor "Doctor" 1000 (new People.people "alex"));
        print_string "add 1 docteur, size of army = ";
        let list_army = army2#get_army in
        print_int (List.length list_army);
        print_char '\n';
        army2#delete;
        print_string "delete 1 doceur, size of army = ";
        let list_army = army2#get_army in
        print_int (List.length list_army)