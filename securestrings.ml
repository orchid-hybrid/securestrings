let string_char ch =
 ch >= 0x30 && ch <= 0x7A

let rec accumulate c str =
 let ch = input_byte c in
  if string_char ch
    then accumulate c (ch :: str)
    else if (List.length str > 3)
          then print_endline (List.fold_left (fun acc ch -> String.make 1 (char_of_int ch) ^ acc) "" str)
          else ();;

let rec strings c =
 let ch = input_byte c in
  if string_char ch
    then let () = accumulate c [ch] in strings c
    else strings c;;

let () =
 try (strings (try open_in_bin (Array.get Sys.argv 1) with
                Invalid_argument _ -> stdin)) with
   | Sys_error e -> print_endline e
   | End_of_file -> ()

(*
 * $ ./securestrings ~/Downloads/stringme
 * S303
 * S303
 * $ ./securestrings ~/Downloads/strings-bfd-badptr  
 * hello
 * world
 * shstrtab
 * text
 * data
 *)
