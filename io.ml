open Printf
open Database
open Printf

(* Reading from disk *)

(** [get_col_type_from_string s] returns the column type associated
    with string [s]  *)
let get_col_type_from_string s= match s with
  |"Int"-> Int
  |"Str"-> Str
  |"None"->None
  |_->raise (Invalid_argument s)

(** [get_string_from_col_type typ] is the string
    of the name of given column type [typ]. *)
let get_string_from_col_type typ=
  match typ with
  |Int-> "Int"
  |Str-> "Str"
  |None->"None"


(** [get_val_from_string val_s] constructs a value from
    string [val_s]  *)
let get_val_from_string val_s:value=
  if val_s = "Null" then None else
    let ascii=Char.code (String.get val_s 0 ) in
    if ascii>=65 && ascii<=90 || ascii>=97 && ascii<=122 then
      Value (Str val_s)
    else Value (Int (int_of_string val_s))

(** [recordLineParse words db lastTable acc] is the database 
    having a record constructed from [words] in table with name
    [lastTable]. *)
let rec recordLineParse words db lastTable acc=
  match words with
  |col::value::t->recordLineParse t db lastTable ((col,get_val_from_string value)::acc)
  |[]->db:=insert !db acc !lastTable; !db

(** [tableLineParse name words db acc] is the database [db] with the table
    with name [name] and columns and types from [words] added.  *)
let rec tableLineParse name words db acc=
  match words with
  |col::ctype::t-> tableLineParse name t db ((col, get_col_type_from_string ctype)::acc)
  |[]->db:=create_table !db name acc; !db

(** [line_parser_helper db in_channel lastTable] is the database
    constructed from the strings from [in_channel]  *)
let rec line_parser_helper db in_channel lastTable=
  let line= input_line in_channel in
  match String.split_on_char ' ' line with
  |"table"::name::t->lastTable:=name; 
    db:=(tableLineParse name t db []);
    line_parser_helper db in_channel lastTable
  |"record"::t->db:=(recordLineParse t db lastTable []);
    line_parser_helper db in_channel lastTable
  |[]-> db


(** [line_parser db in_channel lastTable] is the database constructed from
    a series of lines, starting with [in_channel] *)
let rec line_parser db in_channel lastTable =
  try 
    begin
      line_parser (line_parser_helper db (in_channel) lastTable ) in_channel lastTable
    end
  with
  |End_of_file-> close_in in_channel |> fun()-> db

(** [open_file file_name] is the database stored in file with
    name [file_name]. *)
let open_file file_name =
  let db = ref new_db in 
  let in_chan = open_in file_name in
  line_parser db in_chan (ref "");
  !db

(* Writing to disk *)

(** [string_of_value value] is the string associated with value [value] *)
let string_of_value value= match value with
  |Value v-> (match v with
      |Int i-> string_of_int i;
      |Str s-> s)
  |None-> "Null"

(** [get_table_string columns acc] is the column names and types of
    [columns] as a string. *)
let rec get_table_string columns acc =
  match columns with
  |(name,ctype)::t-> get_table_string t acc^" "^name^" "^(get_string_from_col_type ctype)
  |[]-> acc

(** [get_record_string cols_and_vals acc] is a string of columns and values from
    [cols_and_vals]  *)
let rec get_record_string cols_and_vals acc=
  match cols_and_vals with
  |(c,v)::t-> get_record_string t (acc^" "^c^" "^(string_of_value v))
  |[]->acc

(** [save_records oc records] writes records [records] to output channel [oc]  *)
let rec save_records oc records =
  match records with
  |h::t-> let record_string = get_record_string h.values "record" in
    fprintf oc "%s\n" record_string; save_records oc t
  |[]->()

(** [save_file_helper oc db] writes each table in [db] to a
    output channel [oc]  *)
let rec save_file_helper oc db =
  match db with
  |h::t-> let table_string =  get_table_string h.columns ("table "^h.name) in
    fprintf oc "%s\n" table_string; save_records oc h.records;
    save_file_helper oc t
  |[]->close_out oc

(**[save_file file_name db] stores database [db] in file with name
   [file_name]  *)
let save_file file_name db =
  let out_put_chan = open_out file_name in
  save_file_helper out_put_chan db


