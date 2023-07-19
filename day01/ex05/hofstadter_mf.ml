let rec hfs_f n = match n with
  | x when x < 0 -> -1
  | 0 -> 1
  | _ -> n - hfs_m (hfs_f (n - 1))

and hfs_m n = match n with
  | x when x < 0 -> -1
  | 0 -> 0
  | _ -> n - hfs_f(hfs_m(n-1))

let main () = 
  print_int(hfs_m 0);
  print_char '\n';

  print_int(hfs_f 0);
  print_char '\n';

  print_int(hfs_m 4);
  print_char '\n';

  print_int(hfs_f 4);
  print_char '\n';

  print_int(hfs_f (-1));
  print_char '\n'

let () = main ()