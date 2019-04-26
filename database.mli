(** [table_name] is the name of a table  *)
type table_name=string

(** [column_name] is the name a of a column  *)
type column_name=string

(** [value_types] are the concrete types of a value  *)
type value_types=Int of int | Str of string

(** [value] is the entry for a column in a record  *)
type value = Value of value_types | None

(** [record] contains the values for multiple columns defined in a table  *)
type record = {
  id:int;
  values:(column_name * value) list
}

(** [predicate] is a function that is used to check a condition on a record  *)
type predicate = record -> bool

(** [join_type] is the type of a join.  *)
type join_type = INNER | LEFT | RIGHT | FULL

(** [column_type] is the type of value a column accepts.  *)
type column_type = Int | Str | None

(** [table] is a group of columns and its records. *)
type table = {
  name:table_name;
  columns: (column_name * column_type) list;
  records: record list
}
(** [t] is  a database  *)
type t = table list

(** Raised when an invalid table is used in an operation *)
exception NoSuchTable of table_name

(** Raised when an invalid column is used in an operation *)
exception NoSuchColumn of column_name

(** Raised when an invalid value is used in an operation *)
exception InvalidEntry of value

(** Raised when an invalid where condition is output  *)
exception InvalidWhere

(** [TypeMismatch] is raised when a value does not match its column type.  *)
exception TypeMismatch of (value * column_type)

(* [new db] is a an empty database *)
val new_db : t

(** [select column_names records] returns the values in the columns with the given [column_names]
    in [records].  *)
val select : column_name list -> record list -> ((column_name*value) list) list

(** [where where_list db table_name] gets all the records from table with name
    [table_name] in database [db] that satisfy boolean conditons in [where_list]. 
    raises InvalidWhere if [where_list] doesn't represent valid boolean conditions
*)
val where: string list -> t->table_name -> record list

(* [col_type] is the column_type of column [column_name] *)
val col_type : (column_name * column_type) list -> column_name -> column_type 

(** [type_mismatch] raises an error is the type of any value in [c_v_list] is of a different 
    type than [column_name]'s [column_type]. *Raises exception TypeMismatch (value, column_type) *)
val type_mismatch : (column_name * column_type) list -> (column_name * value) list  -> unit

(** [insert record table] adds [record] to [table].  *)
val insert : t -> (column_name*value) list -> table_name -> t

(** [order_by records column_namesbool] orders [records] by columns with
    [column_names]. [bool] determines whether it is ascending or descending.  *)
val order_by : record list -> column_name list -> bool -> record list

(** [group_by records column_name list] is a list of lists of records, each 
    of which have the same values in the specified columns*)
val group_by : record list -> column_name list -> record list list 
(** [update table records new_vals] changes the [records] in [table]
    to have columns with values as specified in [new_vals]  *)
val update : t-> table_name-> record list -> (string * value) list -> t

(** [create_table table_name column_names] creates a table with name [table_name]
    and columns [column_names].  *)
val create_table : t -> table_name -> (column_name * column_type) list -> t

(** [drop_table t table_name] removes table with name [table_name] from
    database [t].  *)
val drop_table : t -> table_name -> t


(** [rename_table] is [db] with table [old_name] changed to [new_name] *)
val rename_table : t -> table_name -> string -> table list -> t


(** [replace_col] is a list of columns(strings) with column [old_val] replaced with [new_val] *)
val replace_col : 'a ->
  'b ->
  (column_name * column_type) list ->
  column_name ->
  (column_name * column_type) list ->
  (column_name * column_type) list -> (column_name * column_type) list

(** [replace_values] is the list of values on a record with values of column [old_name] changed
     to [new_val]. For [new_val] = an empty list, this is a deletion *)
val replace_values : (column_name * value) list -> column_name -> (column_name * value) list -> (column_name * value) list -> (column_name * value) list 

(** [replace_recs] is the list of records with all associations to column [old_name] being 
    changed to [new_val]--or deleted if new_val = [] *)
val replace_recs : t -> table -> record list -> column_name -> (column_name * value) list -> record list -> record list

(** [rename_drop_col] is [db] with column [old_name] in table [table_name]renamed to [new_val] or dropped *)
val rename_drop_col : t ->
  table_name ->
  column_name -> (column_name * column_type) list -> (string * value) list -> t

(** [drop_row] is [db] with row [id] in table [table_name] removed. After removal the row numbers 
    compact; row ids larger than [id] are decremented  *)
val drop_row: t -> table_name -> int -> t 

(* ADD Functions *)
(** [lengthen_row] is [records] with [column_name * value] appended to each record's [values] *)
val lengthen_row : column_name -> record list -> record list -> record list

(** [add_col] is the [db] with column of [column_name] and data_type added to table of [table_name] *)
val add_col : t -> table_name -> column_name -> column_type -> t

val to_rec : (int* ((column_name * value) list)) -> record 
val to_table : table_name -> (column_name * column_type) list -> record list -> table
val to_db : table list -> t

val inner_join: t->table_name->table_name->column_name->column_name-> record list
(** [not in records] removes records which have a value for a column
    desired to not be included. *)
val not_in : record list -> (column_name * value) list -> record list

(** [drop_row_of] removes a record from a table in a database  *)
val drop_row_of : t -> table_name -> string list -> t

(** [get_val_from_record] is a value stored in a record.  *)
val get_val_from_record : (column_name * value) list -> column_name -> value

(** [get_table_by_name] is a table object that has a certain name.  *)
val get_table_by_name: t -> table_name -> table

(*HELPER: [change_col_type] is the column_list with the the type of column [column_name] modified to [data_type]*)
val change_col_type: (column_name*column_type) list -> column_name -> column_type -> (column_name * column_type) list -> (column_name * column_type) list

(*[modify_column] is the db with [column_name]'s type modifies to [data_type]
  If the column's type and  [data_type] are the same the values will stay, otherwise the column's values become [None].
  RAISES [NoSuchColumn of column_name] if the column isn't in the table *)
val modify_column: t -> table_name -> column_name -> column_type -> t

(*HELPER: [nullify_values] is the list of records with values in column [column_name] turned to None *)
val nullify_values: record list -> column_name -> record list -> record list