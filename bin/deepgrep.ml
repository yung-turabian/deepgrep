
let usage_msg = "dg [-verbose] <pattern> <directory>"
let verbose = ref false
let pattern = ref ""
let directory = ref ""

let speclist =
  [("-verbose", Arg.Set verbose, "Output debug information");
  ]

type filesystem = File of string | Directory of filesystem



(** Returns: [open_and_read file] prints contents of the file*)
let read_whole_chan chan filename =
  let buf = Buffer.create 4096 and line_no = 0 in
  let rec loop (line_no) = 
    let line = input_line chan and line_no = line_no + 1 in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n';
      if ExtString.strstr line !pattern
      then Printf.printf "\x1B[1;31m%s\x1B[0m\n\x1B[33mL%d\x1B[0m) %s\n" filename line_no line;
      loop(line_no)
  in
    try loop(line_no) with
      End_of_file -> Buffer.contents buf

let read_whole_file filename =
  let chan = open_in filename in
  try
    read_whole_chan chan filename
  with
    e -> close_in_noerr chan; raise e




let rec grab_files path =
  let open Sys in
  let handle = Unix.opendir path in
  let rec loop () =
   try
      let entry = ExtIO.readdir_stripped handle in
      let full_path = Filename.concat path entry in
         if is_directory full_path then
            grab_files full_path
         else if file_exists full_path && not (ExtIO.is_invisible (Filename.basename full_path) ) then begin 
            (*If entry is found then print name and line no*)
            ignore (read_whole_file full_path);
         end;
            loop ()
   with End_of_file -> ()
  in
  try
   loop();
   Unix.closedir handle
  with e ->
   Unix.closedir handle;
   raise e

and list_directory path =
   try grab_files path
   with e -> Printf.eprintf "Err: %s\n" (Printexc.to_string e)


let anon_fun arg =
 match !pattern, !directory with
 | "", _ -> pattern := arg
 | _, "" -> directory := arg
 | _ -> failwith "Too many arguments provided."

let () = 
    Arg.parse speclist anon_fun usage_msg;

    if Array.length Sys.argv = 1 then (
      Printf.eprintf "%s\n" usage_msg;
      exit 1
    );
    if !directory = "" then directory := "."; 
    
    list_directory !directory
