(** keywords - special types of tokens that start off distinct clauses or commands
    tokens - keywords, boolean operators, comparison operators, table names
    column names, values*)

(** [parse query] returns a list of tuples where the first entry of each
    tuple is a keyword that starts a clause and the second entry is a list
    of the rest of the tokens from the keyword up to the next keyword or end
    of the list (whichever comes first). If the query doesn't meet syntax
    requirements (is improperly formed) then [parse] returns an empty list. NOTE:
    in most cases the number of whitespaces between tokens or parentheses doesn't
    matter but create table expects at least one space between the column name 
    and data type and there should be no spaces between a column/table/value and
    a trailing comma. NOTE: matching on keywords is case insensitive but parse
    stores each keyword as converted to uppercase in the result and there should 
    be at least one space between a keyword and the rest of the clause. 
    Example outputs of all currently supported commands:
    - "select col1, col2, ..., coln from table_name where column_name [op] value"
      -> [("SELECT",["col1"; "col2"; ...; "coln"]); ("FROM",["table_name"]); 
      ("WHERE",["column_name"; "op"; "value"])]
    - "create table table_name(col1 dtype1, col2 dtype2, ..., coln dtypen)"
      -> [(CREATE TABLE, ["table_name"; "col1 dtype1"; "col2 dtype2"; ... ; "coln dtypen"])]
    - "drop table table_name"
      -> [("DROP TABLE", ["table_name"])]
    - "delete from table_name where column_name [op] value"
      -> [("DELETE FROM",["table_name"]); ("WHERE",["column_name"; "op"; "value"])]
    - "update table_name set col1 = val1, col2 = val2, ..., coln = valn where column_name [op] value"
      -> [("UPDATE",["table_name"]); ("SET",["col1=val1"; "col2=val2"; ... ; "coln=valn"]);
        ("WHERE",["column_name"; "op"; "value"])]
    - "insert into table_name(col1, col2, ..., coln) values(v1, v2, ..., vn)"
      -> [("INSERT INTO",["table_name"; "col1"; "col2"; ... "coln"]); 
          ("VALUES", ["v1"; "v2"; ...; "vn"])]
    - "alter table table_name rename to new_table_name"
      -> [("ALTER TABLE",["table_name"]); ("RENAME TO",["new_table_name"])]
    - "alter table table_name add col_name dtype"
      -> [("ALTER TABLE",["table_name"]); ("ADD", ["col_name"; "dtype"])]
    - "alter table table_name drop col_name"
      -> [("ALTER TABLE",["table_name"]); ("DROP", ["col_name"])]
    - "alter table table_name modify col_name dtype"
      -> [("ALTER TABLE", ["table_name"]); ("MODIFY",["col_name"; "dtype"])]
*)
val parse: string -> (string * string list) list