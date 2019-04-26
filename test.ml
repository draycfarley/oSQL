open OUnit2
open Database

let test_select 
    (name:string)
    (cols:column_name list) 
    (recs:record list) 
    (expected_output:((column_name*value) list) list) = 
  name >:: (fun _ -> 
      assert_equal (Database.select cols recs) expected_output)

let test_where 
    (name:string)
    (pred:(record -> bool)) 
    (db:Database.t)
    (table:table_name) 
    (expected_output:record list) = 
  name >:: (fun _ -> 
      assert_equal (Database.where pred db table) expected_output)

let test_insert 
    (name:string)
    (db:Database.t) 
    (recs:(column_name * value) list)
    (tname:table_name) 
    (expected_output:Database.t) = 
  name >:: (fun _ -> 
      assert_equal (Database.insert db recs tname) expected_output)

let test_order_by 
    (name:string)
    (recs:record list)
    (cols: column_name list) 
    (asc:bool)
    (expected_output:(record list)) = 
  name >:: (fun _ -> 
      assert_equal (Database.order_by recs cols asc) expected_output)

let test_update
    (name:string)
    (db:Database.t)    
    (tname:table_name) 
    (recs:record list)
    (new_vals: (column_name*value) list) 
    (expected_output:Database.t) = 
  name >:: (fun _ -> 
      assert_equal (Database.update db tname recs new_vals) expected_output)


let test_create_table
    (name:string)
    (db:Database.t)    
    (tname:table_name) 
    (cols:column_name list)
    (expected_output:Database.t) = 
  name >:: (fun _ -> 
      assert_equal (Database.create_table db tname cols) expected_output)

let test_drop_table
    (name:string)
    (db:Database.t)    
    (tname:table_name) 
    (expected_output:Database.t) = 
  name >:: (fun _ -> 
      assert_equal (Database.drop_table db tname) expected_output)

let db = new_db
let r1 = [("Name",Value(Str "Archie"));
	("Grade", Value(Int 90))]
let r2 =  [("Name",Value(Str "Kleinberg" ));
	("Grade",Value(Int 99))]
let r3 =  [("Name",Value(Str "Matt" ));
	("Grade",Value(Int 89))]
let r4 = [("Name",Value(Str "Christopher" ));
	("Grade",Value(Int 89))]

let s1 = ("Name",Value(Str "Archie"))
let s2 =  ("Name",Value(Str "Kleinberg" ))
let s3 =  ("Name",Value(Str "Matt" ))
let rec1 = Database.to_rec (0,r1)
let recc2 = Database.to_rec (1,r1)
let recc3 = Database.to_rec (2,r1)

let rec2 = Database.to_rec (1,r2)
let rec3 = Database.to_rec (2,r3)
let rec4 = Database.to_rec (3,r4)
let t0 = to_table "GRADES" ["Name"; "Grade"] []
let t1 = to_table "GRADES" ["Name"; "Grade"] [rec1]
let t2 = to_table "GRADES" ["Name"; "Grade"] [rec1; rec2;]
let t3 = to_table "GRADES" ["Name"; "Grade"] [rec1; rec2; rec3]
let t4 = to_table "GRADES" ["Name"; "Grade"] [rec1;recc2;recc3]

let db0 = to_db [t0]
let db1 = to_db [t1]
let db2 = to_db [t2]
let db3 = to_db [t3]
let db4 = to_db [t4]

let a_students = [
	[("Name", Value(Str "Archie"))];	
	[("Name", Value(Str "Kleinberg" ))]
]
let a_recs = [
  rec1, rec2
]
let pred (x:Database.record) : bool =
  match snd (List.hd x.values) with 
  |Value (Str (x)) -> (x="Archie") 
  | _ -> false
let db_tests = [
  test_create_table "create_table test 1" db "GRADES" ["Name"; "Grade"] db0;

  test_insert "insert test 1 empty" db0 r1 "GRADES" db1;
  test_insert "insert test 2 " db1 r2 "GRADES" db2;

  test_select "select test 1" ["Name"] [rec1;rec2;rec3] [[s3;s2;s1]]; 

  test_where "where test 1"  pred db3 "GRADES" [rec1];

  test_order_by "order by test" [rec1;rec2;rec3;rec4] ["Grade";"Name"] true [rec4;rec3;rec1;rec2];

  test_drop_table "drop test" db3 "GRADES" new_db;
]

let suite =
  "test suite for A2"  >::: List.flatten [
    db_tests;
  ]

let _ = run_test_tt_main suite