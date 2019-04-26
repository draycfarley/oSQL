open Database
open Parser
open Io

(** [Invalid_predicate] is raised when an invalid function
    is input as a where condition.  *)
exception Invalid_predicate

(** [InvalidAggregate] is raised when an invalid aggregate function is
    input.  *)
exception InvalidAggregate

(** [get_id where_list] removes id from
    [where_list]  *)
let get_id where_list= match where_list with 
  |col::op::id::[]->int_of_string id


(** [get_cols cols_and_types acc] gets column names from string list
    of columns and types [cols_and_types]  *)
let rec get_cols_and_types cols_and_types acc = match cols_and_types with
  |h::t-> (match String.split_on_char  ' ' h  with
      |col::typ::[]-> get_cols_and_types t ((col, get_col_type_from_string typ)::acc))
  |[]->acc

(** [get_cols_and_vals pairs acc] constructs column name and value pairs
    from [pairs]  *)
let rec get_cols_and_vals pairs acc :(string*value) list  = match pairs with
  |h::t-> (
      match String.split_on_char '=' h with
      |col::value::[]-> get_cols_and_vals t ((col, get_val_from_string value)::acc) )
  |[]->acc

(** [get_cols_and_types_sep cols_and_types acc] is the column-type list
    from string list [cols_and_types]. *)
let rec get_cols_and_types_sep cols_and_types acc = match cols_and_types with
  |col::typ::t-> get_cols_and_types_sep t ((col, get_col_type_from_string typ)::acc)
  |[]->acc

(** [print_value value] prints a value [value] to
    console.  *)
let print_value value= match value with
  |Value v-> (match v with
      |Int i-> print_int i; print_endline "";
      |Str s-> print_endline s)
  |None-> print_endline "Null"


(** [construct_notin_cols_and_vals column values acc] is the column-value list
    of column and each value in [values]  *)
let rec construct_notin_cols_and_vals column values acc =
  match values with
  |h::t-> construct_notin_cols_and_vals column t ((column, get_val_from_string h)::acc)
  |[]->acc

(**[print_select_helper cols_and_vals] prints to console all the columns and 
   corresponding values in [cols_and_vals]  *)
let rec print_select_helper cols_and_vals = match cols_and_vals with
  |(column,value)::t-> ANSITerminal.(print_string [cyan] column); print_endline "";
    print_value value;  print_select_helper t
  |[]->print_endline ""

(** [print_colval_list lst] outputs column-value list
    [lst] to console.  *)
let rec print_colval_list lst = match
    lst with
|h::t-> print_select_helper h; print_colval_list t
|[]->()

(** [print_groups group columns] outputs the values for each column
    in [columns] in each group [group] to console. *)
let rec print_groups group columns=match group with
  |h::t-> print_colval_list (select columns h); print_groups t columns
  |[]->()

(** [print_select_results res] prints to console result
    of select query [res];  *)
let rec print_select_results res= match res with
  |h::t-> print_select_helper h; print_select_results t
  |[]->print_endline ""

(** [standard_output] prints to console a standard message
    before every command *)
let standard_output () =
  print_endline "Please enter a command.\n";
  print_string  "> "

(** [construct_cv_list columns values] constructs a list
    of column and value pairs from [columns] and [values] *)
let construct_cv_list columns values =
  let rec const_helper cols vals acc = 
    match cols, vals with
    |(ch::ct),(vh::vt)-> const_helper ct vt ((ch,vh)::acc)
    |[],[]->acc
  in const_helper columns values []

(**  [select_without_order db columns table_name where_list] is the
     column-values of columns [columns] of records that satisfy the boolean
     conditions in [where_list] *)
let select_without_order db columns table_name where_list =
  let records = where where_list db table_name in
  select columns records  

(** [select_with_order db columns table_name where_function where_list] is the
     column-values of columns [columns] of records that satisfy the boolean 
     conditions in [where_list] ordered by columns in [column_names]*)
let select_with_order db columns table_name where_list (h::column_names)=
  let records = where where_list db table_name in
  let is_asc = (match h with
      |"ASC" -> true
      |"DESC" -> false
      |_ -> failwith "Invalid command") in
  let sorted_records = order_by records column_names is_asc in
  select columns sorted_records

(** [get_alias_pairs pairs acc] is the pair of a column name and
    its alias in [pairs]. *)
let rec get_alias_pairs pairs acc =
  match pairs with
  |c::"AS"::alias::t-> get_alias_pairs t ((c,alias)::acc)
  |[]->acc

(** [rename_res res alias_pairs acc] is the list of column value lists
    [res] with columns renamed based on aliases in [alias_pairs] *)
let rec rename_res res alias_pairs acc = 

  let rec rename_helper (c,v) alias_pairs =
    match alias_pairs with
    |(col,a)::t-> if c=col then (a,v) else rename_helper (c,v) t 
    |[]-> (c,v) 
  in
  let rec rename_lst lst alias_pairs acc =
    match lst with
    |h::t->rename_lst t alias_pairs ((rename_helper h alias_pairs)::acc)
    |[]->acc in
  match res with
  |h::t-> rename_res t alias_pairs ((rename_lst h alias_pairs [] ):: acc)
  |[]-> acc

(** [get_real_name_from_alias_pairs alias_pairs acc] is the list
    of actual column names in column name alias list [alias_pairs] *)
let rec get_real_name_from_alias_pairs alias_pairs acc =
  match alias_pairs with
  |(col, al)::t-> get_real_name_from_alias_pairs t (col::acc)
  |[]->acc

(** [counter res] is the number of items in list [res]  *)
let counter (res: (Database.column_name * Database.value) list) = 
  let rec counter_helper (res: (Database.column_name * Database.value) list) acc = match res with
    |h::t -> counter_helper t (1+acc)
    |[]-> acc
  in 
  counter_helper res 0

(** [sum res] is the sum of all values in column-value pairs [res].
    A value adds 0 f it is [None] *)
let sum res = 
  let rec sum_helper res acc  =
    match res with
    |(col, value)::t-> begin
        match value with
        |Value v-> (match v with
            |Int i-> sum_helper t (i+acc)
            |Str s-> raise (InvalidEntry (Value v)))
        |None-> sum_helper t acc
      end
    |[]-> acc
  in 
  sum_helper res 0

(** [average res] is the average of of all values in column-value pairs [res] *)
let average res =
  (sum res)/(counter res)

(** [get_cols_and_vals_from_groups lst [col_name]] is a list of the column-value pair lists
    with columns matching name [col_name] from a group [lst].  *)
let get_cols_and_vals_from_groups lst [col_name] = 
  List.map (fun x -> (List.map (fun r -> col_name, (Database.get_val_from_record r.values col_name)) x)) lst

(** [group_aggregate groups fn] applies aggregate function [fn] to groups [groups]. *)
let group_aggregate (groups: ((Database.column_name * Database.value) list) list) (fn: ((Database.column_name * Database.value) list) -> 'a)= 
  List.map fn groups

(** [groupings records [col_name]] gets the values associated with column [col_name]
    for records in [records].  *)
let groupings (records: (Database.record list) list) [col_name] = 
  List.map (fun x -> (Database.get_val_from_record ((List.hd x).values) col_name)) records

(** [print_group group] prints a group of records [group] to terminal.  *)
let print_group group = match group with
  |Value v-> (match v with
      |Int i-> ANSITerminal.(print_string [cyan] (string_of_int i));
      |Str s-> ANSITerminal.(print_string [cyan] s))
  |None-> print_endline "Null"

(** [group_printer res] prints the results of aggregate function applied to a group
    for all group, result pairs in [res] to terminal.  *)
let rec group_printer res = 
  match res with
  |[] -> print_endline "";
  |(group,value)::t -> print_group group; print_endline ""; print_int value; print_endline "\n";
    group_printer t

(** [main_helper db command] performs command [command] on
    database [db] *)
let rec main_helper db command= 
  try
    (match parse command with

     |[("SELECT", name::"AS"::alias::columns); ("FROM", [table_name])]->
       let alias_pairs = get_alias_pairs (name::"AS"::alias::columns) [] in
       let actual_columns =get_real_name_from_alias_pairs  alias_pairs [] in
       let res = rename_res (select_without_order db actual_columns table_name ["true"]) alias_pairs [] in
       print_select_results res;
       standard_output ();
       main_helper db (read_line ())

     |[("SELECT",name::"AS"::alias::columns); ("FROM", [table_name]); ("WHERE", where_list)]->
       let alias_pairs = get_alias_pairs (name::"AS"::alias::columns) [] in
       let actual_columns =get_real_name_from_alias_pairs  alias_pairs [] in
       let res = rename_res (select_without_order db actual_columns table_name where_list) alias_pairs [] in
       print_select_results res;
       standard_output ();
       main_helper db (read_line ())

     (* TODO: make line shorter *)
     |[("SELECT",name::"AS"::alias::columns); ("FROM", [table_name]); ("WHERE", where_list); ("ORDER BY", h::column_names) ]->
       let alias_pairs = get_alias_pairs (name::"AS"::alias::columns) [] in
       let actual_columns =get_real_name_from_alias_pairs  alias_pairs [] in
       let res = rename_res (select_with_order db actual_columns table_name where_list (h::column_names)) alias_pairs [] in 
       print_select_results res;
       standard_output ();
       main_helper db (read_line ())

     |[("SELECT",name::"AS"::alias::columns); ("FROM", [table_name]); ("ORDER BY", h::column_names)]->
       let alias_pairs = get_alias_pairs (name::"AS"::alias::columns) [] in
       let actual_columns =get_real_name_from_alias_pairs  alias_pairs [] in
       let res = rename_res (select_with_order db actual_columns table_name ["true"] (h::column_names)) alias_pairs [] in 
       print_select_results res;
       standard_output ();
       main_helper db (read_line ())

     |[("SELECT",columns); ("FROM", [table_name]); ("GROUP BY", column_names)]->

       let records = where ["true"] db table_name in
       let groups = group_by records column_names in
       print_groups groups columns;
       standard_output ();
       main_helper db (read_line ())

     |[("SELECT",columns); ("FROM", [t1]); ("INNER JOIN", [t2]); ("ON", [c1; c2]) ]->

       let res = (inner_join db (t1) (t2) (c1) (c2)) in
       print_select_results (List.map (fun x-> x.values) res);
       standard_output ();
       main_helper db (read_line ())

     |[("SELECT",columns); ("FROM", [table_name]); ("WHERE", where_list); ("GROUP BY", column_names)]->
       let records = where where_list db table_name in
       let groups = group_by records column_names in
       print_groups groups columns;
       standard_output ();
       main_helper db (read_line ())

     |[("SELECT",columns); ("FROM", [table_name]); ("WHERE", where_list)]->
       let res = select_without_order db columns table_name where_list in
       print_select_results res;
       standard_output ();
       main_helper db (read_line ())

     |[("SELECT",columns); ("FROM", [table_name])]->
       let res = select_without_order db columns table_name ["true"] in
       print_select_results res;
       standard_output ();
       main_helper db (read_line ())

     |[("SELECT",columns); ("FROM", [table_name]); ("WHERE", where_list); ("ORDER BY", h::column_names) ]->
       let res = select_with_order db columns table_name where_list (h::column_names) in 
       print_select_results res;
       standard_output ();
       main_helper db (read_line ())

     |[("SELECT",columns); ("FROM", [table_name]); ("ORDER BY", h::column_names)]->
       let res = select_with_order db columns table_name ["true"] (h::column_names) in 
       print_select_results res;
       standard_output ();
       main_helper db (read_line ())

     |[("SELECT",[]); (funct, column); ("FROM",[table_name])] ->
       (match funct with
        | "COUNT" -> let h::res = select_without_order db column table_name ["true"] in
          let count = counter h in
          print_int count;
          print_endline "";
          standard_output ();
          main_helper db (read_line ())

        | "SUM" -> let h::res = select_without_order db column table_name ["true"] in
          let summation = sum h in
          print_int summation;
          print_endline "";
          standard_output ();
          main_helper db (read_line ())

        | "AVG" -> let h::res = select_without_order db column table_name ["true"] in
          let avg = average h in
          print_int avg;
          print_endline "";
          standard_output ();
          main_helper db (read_line ())

        | s -> raise InvalidAggregate

        |_-> raise InvalidAggregate)

     |[("SELECT",[]); (funct,column); ("FROM",[table_name]); ("WHERE",where_list)] -> 
       (match funct with
        | "COUNT" -> let h::res = select_without_order db column table_name where_list in
          let count = counter h in
          print_int count;
          print_endline "";
          standard_output ();
          main_helper db (read_line ())

        | "SUM" -> let h::res = select_without_order db column table_name where_list in
          let summation = sum h in
          print_int summation;
          print_endline "";
          standard_output ();
          main_helper db (read_line ())

        | "AVG" -> let h::res = select_without_order db column table_name where_list in
          let avg = average h in
          print_int avg;
          print_endline "";
          standard_output ();
          main_helper db (read_line ())

        | s -> raise InvalidAggregate)

     |[("SELECT", group_col); (funct, column); ("FROM",[table_name]); ("GROUP BY", grp_col)] ->
       if group_col <> grp_col then raise (Invalid_argument "Unequal columns") else
         let records = where ["true"] db table_name in
         let groups = group_by records grp_col in
         let cols_and_vals = get_cols_and_vals_from_groups groups column in 
         let groupings = groupings groups group_col in 
         (match funct with
          | "COUNT" -> let counts = group_aggregate cols_and_vals counter in 
            let res = List.map2 (fun x y -> (x,y)) groupings counts in 
            group_printer res;
            standard_output ();
            main_helper db (read_line ())

          | "SUM" -> let sums = group_aggregate cols_and_vals sum in 
            let res = List.map2 (fun x y -> (x,y)) groupings sums in 
            group_printer res;
            standard_output ();
            main_helper db (read_line ())

          | "AVG" -> let avgs = group_aggregate cols_and_vals average in 
            let res = List.map2 (fun x y -> (x,y)) groupings avgs in 
            group_printer res;
            standard_output ();
            main_helper db (read_line ())

          | s -> raise InvalidAggregate)

     |[("SELECT", group_col); (funct, column); ("FROM",[table_name]); ("WHERE",where_list); ("GROUP BY", grp_col)] ->
       if group_col <> grp_col then failwith "unimplemented" else
         let records = where where_list db table_name in
         let groups = group_by records grp_col in
         let cols_and_vals = get_cols_and_vals_from_groups groups column in 
         let groupings = groupings groups group_col in 
         (match funct with
          | "COUNT" -> let counts = group_aggregate cols_and_vals counter in 
            let res = List.map2 (fun x y -> (x,y)) groupings counts in 
            group_printer res;
            standard_output ();
            main_helper db (read_line ())

          | "SUM" -> let sums = group_aggregate cols_and_vals sum in 
            let res = List.map2 (fun x y -> (x,y)) groupings sums in 
            group_printer res;
            standard_output ();
            main_helper db (read_line ())

          | "AVG" -> let avgs = group_aggregate cols_and_vals average in 
            let res = List.map2 (fun x y -> (x,y)) groupings avgs in 
            group_printer res;
            standard_output ();
            main_helper db (read_line ())

          | s -> raise InvalidAggregate)

     |[("SELECT", columns); ("FROM",[table_name]); ("WHERE",column::[]); ("NOT IN", values)]->
       let excl_cols_and_vals = construct_notin_cols_and_vals column values [] in
       let records = (get_table_by_name db table_name ).records in
       let recs = not_in records excl_cols_and_vals in
       let res= select columns recs in
       print_select_results res;
       standard_output ();
       main_helper db (read_line ())


     |[("LOAD", [file_name])]->
       let loaded_db = open_file file_name in
       standard_output ();
       main_helper loaded_db (read_line())

     |[("SAVE", [file_name])]->
       save_file file_name db;
       standard_output ();
       main_helper db (read_line())

     | [("INSERT INTO", (table_name::columns));("VALUES",values)]->
       let real_values= List.map get_val_from_string values in
       let cols_and_vals= construct_cv_list columns real_values in
       let new_db=insert db cols_and_vals table_name in
       standard_output ();
       main_helper new_db (read_line ())

     | [("CREATE TABLE",table_name::cols_and_types)]->
       let columns_and_types = get_cols_and_types cols_and_types [] in
       let new_db = create_table db table_name columns_and_types in
       standard_output ();
       main_helper new_db (read_line ())

     | [("DROP TABLE", [table_name])] -> 
       let new_db = drop_table db table_name in
       standard_output ();
       main_helper new_db (read_line ())

     | [("UPDATE", [table_name]); (set, pairs); (where_key, where_list)]  ->
       let cols_and_vals= get_cols_and_vals pairs [] in
       let records = where where_list db table_name in
       let new_db = update db table_name records cols_and_vals in
       standard_output ();
       main_helper new_db (read_line ())

     | [("UPDATE", [table_name]); (set, pairs)]  ->
       let cols_and_vals= get_cols_and_vals pairs [] in
       let records = where ["true"] db table_name in
       let new_db = update db table_name records cols_and_vals in
       standard_output ();
       main_helper new_db (read_line ())

     |[("DELETE FROM", table_name::t); (where, where_list)]->
       let new_db = drop_row_of db table_name where_list  in
       standard_output ();
       main_helper new_db (read_line ())

     |[("ALTER TABLE",[table_name]); ("ADD", cols_and_types)]->
       let columns_and_types = get_cols_and_types_sep cols_and_types [] in
       let new_db = List.fold_left (fun x (col,typ)-> add_col x table_name col typ) db columns_and_types in 
       standard_output ();
       main_helper new_db (read_line ())

     |[("ALTER TABLE", [table_name]); ("RENAME TO", [new_name])]->
       let new_db = rename_table db table_name new_name  [] in
       standard_output ();
       main_helper new_db (read_line ())

     |[("ALTER TABLE", [table_name]); ("DROP", [column])]->
       let new_db = rename_drop_col db table_name column [] []  in
       standard_output ();
       main_helper new_db (read_line ())

     |[("ALTER TABLE", [table_name]); ("MODIFY",col::typ::[])]->
       let new_db = modify_column db table_name col (get_col_type_from_string typ)   in 
       standard_output ();
       main_helper new_db (read_line ())


     |_->ANSITerminal.(
         print_endline "Please enter a valid command!\n";
       );
       print_string  "> ";
       main_helper db (read_line ()))
  with 
  |NoSuchTable table-> ANSITerminal.(
      print_endline "Please enter a valid table!\n";
    );
    print_string  "> ";
    main_helper db (read_line ())
  |NoSuchColumn col-> ANSITerminal.(
      print_endline "Please enter a valid column!\n";
    );
    print_string  "> ";
    main_helper db (read_line ())
  |TypeMismatch tuple -> ANSITerminal.(
      print_endline "Type Mismatch: value-type and column-type don't match!\n";
    );
    print_string "> ";
    main_helper db (read_line ())
  |InvalidEntry e -> ANSITerminal.(
      print_endline "Error invalid entry!\n";
    );
    print_string  "> ";
    main_helper db (read_line ())
  |Sys_error t -> ANSITerminal.(
      print_endline "Error invalid file name!\n";
    );
    print_string  "> ";
    main_helper db (read_line ())
  |InvalidAggregate -> ANSITerminal.(
      print_endline "Error invalid aggregate function!\n";
    );
    print_string  "> ";
    main_helper db (read_line ())
  |InvalidWhere -> ANSITerminal.(
      print_endline "Error invalid where condition!\n";
    );
    print_string  "> ";
    main_helper db (read_line ())

(** [main] initializes a database and terminal for user input.  *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\n Welcome to our SQL database.\n");
  print_endline "Please enter a command.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | command -> main_helper new_db command


let () = main ()