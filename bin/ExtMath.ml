
(** [fact n] is [n!].
    Requires: [n >= 0]. *)
    let rec fact n = if n = 0 then 1 else n * fact (n - 1)