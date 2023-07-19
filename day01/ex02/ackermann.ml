let rec ackermann m n = match (m,n) with
  | (m,n) when m < 0 || n < 0 -> -1
  | (0,_) -> n + 1
  | (m,n) when m > 0 && n = 0 -> ackermann (m - 1) 1
  | (_,_) -> ackermann (m - 1) (ackermann m (n - 1))

let main () = 
  print_int (ackermann (-1) 7);
  print_char '\n';
  print_int (ackermann 0 0);
  print_char '\n';
  print_int (ackermann 2 3);
  print_char '\n';
  print_int (ackermann 4 1);
  print_char '\n'

let () = main ()