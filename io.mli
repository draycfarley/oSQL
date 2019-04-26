(** [open_file] contructs a database from a file  *)
val open_file : string -> Database.t

(** [save_file] saves all information from a database
    to a file  *)
val save_file : string -> Database.t -> unit

(** [get_val_from_string] gets the value associated with a string *)
val get_val_from_string : string -> Database.value

(** [get_col_type_from_string] gets the column type associated with a string *)
val get_col_type_from_string : string -> Database.column_type