let rec fibonacci n = match n with
  | x when x < 0 -> -1
  | 0 -> 0
  | 1 -> 1
  | _ -> fibonacci (n - 2) + fibonacci (n - 1)

let main () =
  print_int(fibonacci (-42));
  print_char '\n';
  print_int(fibonacci 1);
  print_char '\n';
  print_int(fibonacci 3);
  print_char '\n';
  print_int(fibonacci 6);
  print_char '\n';
  print_int(fibonacci 10);
  print_char '\n'
 
let () = main ()