

(** Brief: Check for substring within a string. *)
let strstr s1 s2 =
  let re = Str.regexp_string s2 in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false