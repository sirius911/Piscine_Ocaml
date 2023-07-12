
let ft_rot_n n str = 
    let fonction c =
        let is_alpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') in
        
        let rotate c = 
            let tmp = char_of_int(int_of_char(c) + 1) in 
            if tmp = '[' then 
                'A'
            else if tmp = '{' then
                'a'
            else 
            tmp
        in
        if is_alpha c then
            rotate c
        else
            c
    in
    
    let rec loop str i =
        if i = 0 then
            str
        else
            String.map fonction (loop str (i - 1))
    in
    loop str n



let main () = 
    print_endline (ft_rot_n 1 "bcdefghijklmnopqrstuvwxyza");
    print_endline(ft_rot_n 13 "nopqrstuvwxyzabcdefghijklm");
    print_endline(ft_rot_n 42 "0123456789");
    print_endline(ft_rot_n 2 "OI2EAS67B9");
    print_endline(ft_rot_n 0 "Damned !");
    print_endline(ft_rot_n 42 "" );
    print_endline(ft_rot_n 1  "NBzlk qnbjr !")

let () = main ()
