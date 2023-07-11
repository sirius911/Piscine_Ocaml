
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
            String.map fonction str
        else
          begin
            str = (String.map fonction str);
            loop str (i - 1)
          end
    in
    loop str n



let main () = 
    print_endline (ft_rot_n 10 "abcdefg")

let () = main ()
