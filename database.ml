(**[column_name] is the name of a column  *)
type column_name = string

(** [column_type] is the type of value a column accepts.  *)
type column_type = Int | Str | None

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

(** [table_name] is the name of a table  *)
type table_name =string

(** [table] is a group of columns and its records. *)
type table={
  name:table_name;
  columns: (column_name * column_type) list;
  records: record list
}

(** [bool_ops] is the type of a boolean operator *)
type bool_ops = AND | OR | NAND | NOR


type where_tree = 
  |Nil
  |Leaf of (record -> bool)
  |NLeaf of (record -> bool)
  |Node of bool_ops * where_tree * where_tree

(** [InvalidColumnType] is raised when an ivalid column type is inserted  *)
exception InvalidColumnType of column_type

(** [TypeMismatch] is raised when a value does not match its column type.  *)
exception TypeMismatch of (value * column_type)

(** [NoSuchTable] is raised when an invalid table is used in an operation *)
exception NoSuchTable of table_name

(** [NoSuchColumn] is raised when an invalid column is used in an operation *)
exception NoSuchColumn of column_name

(** [InvalidEntry] is raised when an invalid value is used in an operation *)
exception InvalidEntry of value

(** [InvalidWhere] is raised when an invalid where condition is output  *)
exception InvalidWhere

(** [t] is a database  *)
type t = table list

(** [new_db] creates a new database  *)
let new_db = []

(** (get_table_by_name db table_name) gets the table record with name [table_name]
    from database [db].   *)
let rec get_table_by_name (db:t) table_name = 
  match db with
  |{name; columns; records}::t-> 
    if table_name= name then {name; columns; records} else 
      get_table_by_name t table_name
  |[]->raise (NoSuchTable table_name)

(** [get_val s] is the value represented by string [s]  *)
let get_val s = 
  if s = "Null" then None else
    let ascii=Char.code (String.get s 0 ) in
    if ascii>=65 && ascii<=90 || ascii>=97 && ascii<=122 then
      Value (Str s)
    else Value (Int (int_of_string s))

(** [comparison_op r col op value] returns a function
    that takes in a a record and performs an expression using
    column [col], operator [op], and value [val] that returnsa bool. *)
let comparison_op (r:record) col op value = 
  let vals = r.values in 
  let rec op_helper cols_and_vals op' = 
    match cols_and_vals with
    | [] -> false
    | (c,v)::t -> if c = col then op' v value
      else op_helper t op' in 
  op_helper vals op

(** [predicate_constructor lst] determines the appropriate operator
    from [lst] to create a predicate. *)
let predicate_constructor (lst:string list) = 
  match lst with
  | [] -> raise InvalidWhere
  | h::[] -> (match String.uppercase_ascii h with
      | "TRUE" -> (fun x -> true)
      | "FALSE" -> (fun x -> false)
      | s -> raise InvalidWhere)

  | h::m::t::[] -> (match m with
      | "<" -> (fun x -> comparison_op x h (<) (get_val t))
      | ">" -> (fun x -> comparison_op x h (>) (get_val t))
      | "=" -> (fun x -> comparison_op x h (=) (get_val t))
      | "<>" -> (fun x -> comparison_op x h (<>) (get_val t))
      | ">=" -> (fun x -> comparison_op x h (>=) (get_val t))
      | "<=" ->  (fun x -> comparison_op x h (<=) (get_val t))
      | q -> raise InvalidWhere)
  | _ -> (fun w ->false)

(** [negate_root tree] negates the boolean tree [tree]  *)
let negate_root tree = 
  match tree with
  | Nil -> Nil
  | Leaf(fn) -> NLeaf fn
  | Node(op, ltr, rtr) -> (match op with
      | AND -> Node(NAND, ltr, rtr)
      | OR -> Node(NOR, ltr, rtr)
      | NAND -> Node(AND, ltr, rtr)
      | NOR -> Node(OR, ltr, rtr))
  | NLeaf fn -> Leaf fn


(** [and_or lst] is a predicate created from [lst] *)
let make_pred_tree (lst:string list) = 
  let rec helper lftOP lst= 
    match lst with
    | [] -> Nil
    | h::[] -> (if h = "AND" || h = "OR" then raise InvalidWhere
                else Leaf (predicate_constructor (h::[])))
    | h::m::t::[] -> (if (List.mem "AND" (h::m::t::[]) || List.mem "OR" (h::m::t::[])) then
                        helper (h::lftOP) (m::t::[])
                      else
                        Leaf (predicate_constructor (h::m::t::[])))
    | h::t -> match h with 
      | "AND" -> Node(AND, Leaf (predicate_constructor (List.rev lftOP)), helper [] t)
      | "OR" ->  Node(OR, Leaf (predicate_constructor (List.rev lftOP)), helper [] t)
      | "NOT" -> negate_root(helper lftOP t)
      | s -> helper (s::lftOP) t
  in 
  if (List.hd lst = "AND" || List.hd lst = "OR") then raise InvalidWhere
  else helper [] lst

(** [where p db table_name] gets all the records from table with name
    [table_name] in database [db] that satisfy predicate [p].  
    let where (p:(record->bool)) (db:t) (table_name:table_name) :record list =
    let table= get_table_by_name db table_name in 
    List.filter p table.records *)

let where (where_list: string list) (db:t) (table_name:table_name) : record list = 
  let pred_tree = make_pred_tree where_list in 
  let table = get_table_by_name db table_name in 
  let records = table.records in 
  let rec where_helper r = function
    | Nil -> true
    | Leaf fn -> fn r
    | NLeaf fn -> not(fn r)
    | Node(op, ltr, rtr) -> (match op with
        | AND -> (where_helper r ltr) && (where_helper r rtr)
        | OR -> (where_helper r ltr) || (where_helper r rtr)
        | NAND -> not((where_helper r ltr) && (where_helper r rtr))
        | NOR -> not((where_helper r ltr)||(where_helper r rtr))
      )
  in
  List.filter (fun r -> where_helper r pred_tree) records

(** [get_val_from_record values column] gets the value in [values] that has
    column name [column]. *)
let rec get_val_from_record (values : (column_name * value) list ) (column : column_name)=match values with
  |(columnName, value)::t->if columnName=column then value else get_val_from_record t column
  |[]->raise (NoSuchColumn column)


(** [order_by records columns asc] sorts the records [records] by
    columns with names [columns]. [asc] determines if they are in ascending or
    descending order. *)
let order_by records columns  asc : record list = 
  let rec comp (cols:(column_name ) list) (a:record) (b:record) : int = 
    (match cols with
     |[]-> 0
     |name::t -> 
       let (a_val, b_val) = ((get_val_from_record a.values name),(get_val_from_record b.values name)) in
       (match (a_val, b_val) with
        |(Value v1, Value v2)-> (match v1,v2 with
            |(Str(astr),Str(bstr)) -> if ((String.compare astr bstr) = 0) then (comp t a b) else (String.compare astr bstr) 
            |(Int(ai), Int(bi)) -> if (ai - bi = 0) then (comp t a b) else (ai-bi) )
        |(None,None)->raise (InvalidEntry a_val)
        |_ -> raise (InvalidEntry a_val))) in
  let compare_pos = comp columns in
  let compare_neg a b = -(compare_pos a b) in
  if asc then (List.stable_sort compare_pos records) else (List.stable_sort compare_neg records)

(** [select_helper column records acc] gets the value associated with each
    column in [columns] for each record in [records]. *)
let rec select_helper column records acc =match records with
  |{id; values}::t-> select_helper column t ((column,(get_val_from_record values column))::acc)
  |[]->acc
let group_by (recs:record list) (cols:(column_name) list): (record list) list =
  let col = List.hd cols in
  let accumulate (acc:(record list) list) (r:record) : (record list) list =
    let rec find_match (lst:(record list list)) (r:record) : (record list) option =
      (match lst with
       |[] -> None
       |h::t->
         (match h with
          |x::xs->(if ((get_val_from_record x.values col)=(get_val_from_record r.values col)) then Some(h) else find_match t r)
          |[]->failwith "bad list")) in
    match find_match acc r with
    |None -> [r]::acc
    |Some(x) -> List.map (fun y -> if (x=y) then r::x else y) acc in
  List.fold_left accumulate [] recs 

(** [select columns records] returns the values associated with all columns in
    [columns]. *)
let select (columns:column_name list) (records: record list) : ((column_name*value) list) list = 
  let rec column_select columns records acc=
    match columns with
    |name::t-> column_select t records ( (select_helper name records []) :: acc)
    |[]->acc
  in 
  column_select columns records []

let pred_and (p1:predicate) (p2:predicate) : predicate = 
  fun (r:record) -> ((p1 r) && (p2 r))
let pred_or (p1:predicate) (p2:predicate) : predicate = 
  fun (r:record) -> ((p1 r) || (p2 r))

(** [update_table_in_db db {name; columns; records} acc] replaces or inserts 
    [{name; columns; records}] in database [db].  *)
let rec update_table_in_db db ({name; columns; records}:table) acc = match db with
  |{name=tbl_name; columns=tbl_columns; records=tbl_records}::t-> (
      if name=tbl_name then acc@[{name; columns; records}]@t
      else update_table_in_db t {name; columns; records} ({name=tbl_name; columns=tbl_columns; records=tbl_records}::acc) )
  |[]-> db

(** [make_values_helper tcol cols_and_vals] returns a pair of [tcol]
    and the corresponding value in column and value list [cols_and_vals]. [tcol] is paired
    with [None] if no column name matches.   *)
let rec make_values_helper tcol cols_and_vals=
  match cols_and_vals with
  |(col,value)::t-> if col=tcol then (tcol,value)
    else make_values_helper tcol t
  |[]->(tcol,None)

(** [make_values_for_insert tb_cols cols_and_vals acc] creates column and
    value pairs for all columns in [tcols] based on what the same column name in
    [cols_and vals] is paired with. *)
let rec make_values_for_insert (tb_cols:(column_name * column_type) list) (cols_and_vals:(column_name*value) list) acc=
  match tb_cols with
  |(name, typ)::t-> make_values_for_insert t cols_and_vals ((make_values_helper name cols_and_vals::acc))
  |[]->acc

(* Helper: [col_type] is the column_type of column [column_name] *)
let rec col_type (columns:(column_name * column_type) list ) column_name = 
  match columns with 
  |[] -> raise (NoSuchColumn column_name)
  |(name, typ )::t -> if name = column_name then typ else col_type t column_name 

(*Helper [type_mismatch] raises an error is the type of any value in [c_v_list] is of a different 
  type than [column_name]'s [column_type]. **Raises exception TypeMismatch (value, column_type) *)
let rec type_mismatch columns c_v_list =
  match c_v_list with
  |[]-> ()
  |(name,value)::t->
    let typ = col_type columns name in
    match value with
    |Value (Int i) -> if (typ = Int) then type_mismatch columns t else raise (TypeMismatch (value,typ))
    |Value (Str s) -> if not (typ = Str) then type_mismatch columns t else raise (TypeMismatch (value,typ))
    |None -> type_mismatch columns t

(** [insert db cols_and_vals table_name] inserts a records with the columns
    and values in [cols_and_vals] into a table with [table_name] in database [db].*)
let insert db (cols_and_vals:(column_name*value) list) table_name =
  let table = get_table_by_name db table_name in
  match table with
  |{name; columns; records}-> let revRecords= List.rev records in
    let id=
      match revRecords with
      |{id; values}::t-> id+1
      |[]->0 in
    let new_record = {id=id; values=(List.rev (make_values_for_insert columns cols_and_vals []))} in
    type_mismatch columns cols_and_vals;
    let new_table= 
      {name=name; columns=columns; records=records@[new_record]}
    in
    update_table_in_db db new_table []

(** [create_table db table_name columns] creates a table in database [db]
    with columns [columns] and name [table_name].  *)
let create_table (db:t) table_name columns : t = 
  let rec create_table_r (db:t) table_name columns (acc:t) : t =
    match db with
    |{name=tbl_name; columns= tbl_columns; records=records}::t-> 
      if tbl_name=table_name then acc@t
      else create_table_r t table_name columns ({name=tbl_name; columns= tbl_columns; records=records}::acc)
    |[]->{name=table_name; columns= columns; records=[]}::acc in
  create_table_r db table_name columns (new_db)

(**[drop_table db table_name] removes the table with name [table_name] from
   database [db].   *)
let drop_table db (table_name:table_name) : t=
  let rec drop_helper tbl_lst acc  = begin 
    match tbl_lst with
    |{name; columns; records}::tail -> if name= table_name then acc@tail else drop_helper tail ({name; columns; records}::acc)
    |[]->acc 
  end
  in
  drop_helper db []

(** [update_pair (col,value) new_cols_and_vals] returns a column and value
    pair that has [col] and the corresponding value in column and value list
    [new_cols_and_vals]. Returns [(col,val)] if there is no match.  *)
let rec update_pair (col,value) new_cols_and_vals=
  match new_cols_and_vals with 
  |(ncol,nvalue)::t->
    if col=ncol then (col, nvalue) else update_pair (col,value) t
  |[]->(col,value)

(**[update_cols_and_vals old_cols_and_vals new_cols_and_vals acc] replaces the
   value in the column value pairs [old_cols_and_vals] with the value corresponding
   to the same column name in [new_cols_and_vals] if a match exists, maintains
   old pair if not.  *)
let rec update_cols_and_vals old_cols_and_vals new_cols_and_vals acc =
  match old_cols_and_vals with
  |(col,value)::t-> 
    update_cols_and_vals t new_cols_and_vals ((update_pair (col,value) new_cols_and_vals)::acc)
  |[]->acc

(**[update_cmd_helper table records cols_and_vals acc] returns [table] with
   each record in [records] having its columns and values updated to match
   the column value pairs in [cols_and_vals]. *)
let rec update_cmd_helper table records cols_and_vals acc =
  match records with 
  |{id=id; values=vals}::t-> 
    update_cmd_helper table t cols_and_vals (({id=id; values=(update_cols_and_vals vals cols_and_vals [])})::acc)
  |[]->{table with records=acc}

(** [update db table_name records cols_and_vals] updates the records [records]
    with the columnd and value pairs in [cols_and_vals] in table with name
    [table_name] in database [db].  *)
let update db table_name records cols_and_vals : t =
  let table= get_table_by_name db table_name in
  update_table_in_db db (update_cmd_helper table records (cols_and_vals) []) []

(* [rename_table] is [db] with table [old_name] changed to [new_name] *)
let rec rename_table (db:table list) old_name new_name new_db: t = 
  match db with 
  |[] -> new_db
  |tab::t -> if tab.name = old_name 
    then rename_table t old_name new_name new_db@[{name=new_name;columns=tab.columns;records=tab.records}]
    else rename_table t old_name new_name new_db@[tab]

(* [replace_col] is a list of columns with column [old_val] replaced with [one_val_list] *)
let rec replace_col db table columns old_val one_val_list acc: (column_name*column_type) list =
  match columns with
  |[] -> acc
  |(name, typ)::t -> if name = old_val 
    then replace_col db table t old_val one_val_list acc@one_val_list
    else replace_col db table t old_val one_val_list acc@[(name, typ)]

(* HELPER: [replace_values] is the list of values on a record with values of column [old_name] 
    changed to [new_val]. For [new_val] = an empty list, this is a deletion *)
let rec replace_values (values:('a * 'b) list) old_name new_val acc : ('a * 'b) list = 
  match values with
  |[] -> acc 
  |(col_name, v)::t -> 
    let nv = begin match new_val with
      |[] -> []
      |[(c,_)]-> [(c,v)]
      |_ -> raise (Invalid_argument "new_val") end in
    if col_name = old_name then replace_values t old_name new_val acc@nv
    else replace_values t old_name new_val acc@[(col_name, v)]

(* HELPER: [replace_recs] is the list of records with all association to 
   [old_name] being changed to [new_val]--or deleted if new_val = [].
   Examples:
   Delete records with values linked with col "john":  replace_recs db table table.records "john" [] []
   Rename record values linked to col "john" to "joan": replace_recs db table table.records "john" [("joan",any_random_value)] [] *)
let rec replace_recs db table records old_name new_val acc: record list = 
  match records with
  |[] -> acc
  |h::t -> replace_recs db table t old_name new_val acc@[{id=h.id; values=replace_values h.values old_name new_val []}]

(* [rename_drop_col] is [db] with column [old_name] in table [table_name]renamed to [new_val] or dropped 
    Rename col "john"->"daisy" expl: rename_drop_col db "cats" "john" ["daisy"] [("daisy",_)]
    Drop col "john" expl: rename_drop_col db "cats" "john" [] []  *)
let rec rename_drop_col db table_name old_name new_name new_val: t =
  let table = get_table_by_name db table_name in
  let recs = replace_recs db table table.records old_name new_val [] in
  let cols = replace_col db table table.columns old_name new_name [] in
  let new_db = drop_table db table_name in 
  new_db @ [{name=table.name;columns=cols;records=recs}]

(* [drop_row] is [db] with row [id] in table [table_name] removed.
   After removal the row numbers compact; row ids larger than [id] are decremented  *)
let rec drop_row db table_name id: t = 
  let rec row_exists id records = 
    begin match records with 
      |[] -> false 
      |h::t -> if h.id = id then true else row_exists id t 
    end in 
  let rec recs id records acc = 
    begin match records with
      |[] -> acc
      |h::t ->if h.id = id then recs id t acc
      (* compact the table by decrementing the proper row numbers to "fill" the vacancy of the deleted row *)
        else if h.id > id then recs id t acc@[{id=h.id-1;values=h.values}]
        else recs id t acc@[h] 
    end in
  let table = get_table_by_name db table_name in
  if not (row_exists id table.records) then db else
    let new_db = drop_table db table_name in
    new_db @ [{name=table.name;columns=table.columns;records=recs id table.records []}]

(*HELPER: [apply_predicate] is the list of records that do not make [predicate] false*)
let rec apply_predicate records predicate acc: record list=
  match records with
  |[] -> acc
  |h::t ->
    (* true = include record; false = don't include record *)
    if not (predicate h) then apply_predicate t predicate acc@[h]
    else apply_predicate t predicate acc

(* [drop_row_of] is [db] with table [table_name] updated to have records dropped if they
   make the [predicate] false*)
let drop_row_of db table_name where_list : t =
  let table = get_table_by_name db table_name in
  let recs = where ("NOT"::where_list) db table_name in
  let new_db = drop_table db table_name in
  new_db @ [{name=table.name;columns=table.columns;records=recs}]


(*HELPER: [nullify_values] is the list of records with values in column [column_name] turned to None *)
let rec nullify_values records column_name acc : record list  =
  let rec vals values acc :(string * value) list=
    match values with
    |[] -> acc 
    |(c, v)::t -> 
      if c = column_name then vals t acc @ [(c, None)] else vals t acc @ [(c,v)] in
  match records with
  |[] -> acc
  |h::t -> nullify_values t column_name acc@[{id=h.id;values=vals h.values []}]

(*HELPER: [change_col_type] is the column_list with the the type of column [column_name] modified to [data_type]*)
let rec change_col_type columns column_name data_type acc: (column_name * column_type) list =
  match columns with
  |[] -> acc
  |(name, typ)::t -> 
    if name = column_name then change_col_type t column_name data_type acc@[(name,data_type)]
    else change_col_type t column_name data_type acc@[(name,typ)]

(*[modify_column] is the db with [column_name]'s type modifies to [data_type]
  If the column's type and  [data_type] are the same the values will stay, otherwise the column's values become [None].
  RAISES [NoSuchColumn of column_name] if the column isn't in the table *)
let modify_column db table_name column_name data_type: t=
  let table = get_table_by_name db table_name in
  if (col_type table.columns column_name = data_type) then db else 
    let cols = change_col_type table.columns column_name data_type [] in
    let recs = nullify_values table.records column_name [] in 
    let new_db = drop_table db table_name in
    new_db @ [{name=table.name;columns=cols;records=recs}]

(* ADD Functions *)
(* [lengthen_row] is [records] with [column_name * value] appended to each record's [values] *)
let rec lengthen_row column_name records acc: record list = 
  match records with
  |[] -> acc
  |h::t -> lengthen_row column_name t acc@[{id=h.id; values= h.values @ [(column_name, None)]}]

(* [add_col] is the [db] with column of [column_name] and data_type added to table of [table_name] *)
let add_col db table_name column_name data_type: t =
  let table = get_table_by_name db table_name in
  let recs = lengthen_row column_name table.records [] in
  let cols = table.columns @ [(column_name, data_type)] in
  let new_db = drop_table db table.name in
  new_db @ [{name=table.name;columns=cols;records=recs}]

(** [to_rec x] constructs a record from int, coolumn-value pair list
    [x].  *)
let to_rec x : record = 
  {id = fst x; values = snd x}
let to_table name cols recs : table = 
  {name=name; columns=cols; records = recs}
let to_db (tabs:table list) : t = 
  tabs


(** [not in records excl_cols_and_vals] removes records from [records]
    that have a column-value pair that matches a cloumn-value pair in
    [excl_cols_and_vals]  *)
let not_in (records:record list) (excl_cols_and_vals: (column_name * value) list) = 
  let rec check_col_and_val (col,value) ex_cols_and_vals=
    match ex_cols_and_vals with
    |(c,v)::t-> if c = col then v = value
      else check_col_and_val (col,value) t
    |[]->false
  in
  let rec check_cols_and_vals cols_and_vals =
    match cols_and_vals with
    |h::t-> if check_col_and_val h excl_cols_and_vals then false
      else check_cols_and_vals t
    |[]->true
  in
  let check_record record =
    check_cols_and_vals record.values 
  in 
  List.filter check_record records

(** [remove_column r c] removes column-value pair in [r] with column name [c].  *)
let remove_column (r: (column_name*value) list) (c: column_name) : ((column_name*value) list) = 
  let f acc x = if ((fst x) != c) then (x::acc) else acc in
  List.fold_left (f) [] r

(** [inner_join db table_1 table_2 c1 c2] is the inner join from database [db]
    with tables [table_1] and [table_2] and columns [c1] and [c2].  *)
let inner_join (db: t) (table_1:table_name) (table_2:table_name) (c1:column_name) (c2:column_name): record list = 
  let (t1,t2) = ((get_table_by_name db table_1),(get_table_by_name db table_2)) in
  let find_matches (r: record): record list = 
    (let accumulate (acc: record list) (r2:record) : record list = 
       (let v2 = (get_val_from_record r2.values c2) in 
        let v1 = (get_val_from_record r.values c1) in
        match v2 with
        |None -> acc
        |Value _ -> if(v1 = v2) then {id=r.id; values = r.values@(remove_column r2.values c2)}::acc 
          else acc) in 
     List.fold_left accumulate [] t2.records) in
  let f (acc: record list) (r: record) : record list =
    (let matches = find_matches r in
     matches@acc) in 
  List.fold_left f [] t1.records