
(** [readdir_stripped handle].
    Requires: [handle <> NULL]. 
    Returns: A unix directory that is not `.` or `..`*)
let rec readdir_stripped handle = 
  let entry = Unix.readdir handle in 
  if entry <> "." && entry <> ".." 
    then entry 
  else readdir_stripped handle

let argv_debug _ =
  for i = 0 to Array.length Sys.argv - 1 do
    Printf.printf "[%i] %s\n" i Sys.argv.(i)
  done


let print_head lst =
  match lst with
  | [] -> print_endline "The list is empty."
  | head :: _ -> print_endline head 


let is_invisible file =
  if file.[0] = '.' then true
  else false