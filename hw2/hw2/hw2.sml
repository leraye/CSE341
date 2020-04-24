(* CSE 341, HW2 Provided Code *)

(* main datatype definition we will use throughout the assignment *)
datatype json =
         Num of real (* real is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list

(* some examples of values of type json *)
val json_pi    = Num 3.14159
val json_hello = String "hello"
val json_false = False
val json_array = Array [Num 1.0, String "world", Null]
val json_obj   = Object [("foo", json_pi), ("bar", json_array), ("ok", True)]

(* some provided one-liners that use the standard library and/or some features
   we have not learned yet. (Only) the challenge problem will need more
   standard-library functions. *)

(* dedup : string list -> string list -- it removes duplicates *)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs

(* strcmp : string * string -> order compares strings alphabetically
   where datatype order = LESS | EQUAL | GREATER *)
val strcmp = String.compare                                        
                        
(* convert an int to a real *)
val int_to_real = Real.fromInt

(* absolute value of a real *)
val real_abs = Real.abs

(* convert a real to a string *)
val real_to_string = Real.toString

(* return true if a real is negative : real -> bool *)
val real_is_negative = Real.signBit

(* We now load 3 files with police data represented as values of type json.
   Each file binds one variable: small_incident_reports (10 reports), 
   medium_incident_reports (100 reports), and large_incident_reports 
   (1000 reports) respectively.

   However, the large file is commented out for now because it will take 
   about 15 seconds to load, which is too long while you are debugging
   earlier problems.  In string format, we have ~10000 records -- if you
   do the challenge problem, you will be able to read in all 10000 quickly --
   it's the "trick" of giving you large SML values that is slow.
*)

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important -- it ends the previous binding *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;

use "parsed_small_police.sml";
(*use "parsed_medium_police.sml";*)

(* uncomment when you are ready to do the problems needing the large report*)
(*use "parsed_large_police.sml"; 

val large_incident_reports_list =
    case large_incident_reports of
        Array js => js
      | _ => raise (Fail "expected large_incident_reports to be an array");*)

(* Now make SML print more again so that we can see what we're working with. *)
; Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 1-8 HERE ****)

fun make_silly_json (i) =
    let fun make_list n =
	    case n of
		0 => []
	      | _ => Object([("n", Num(int_to_real(n))), ("b", True)]) :: make_list(n - 1)
    in
	Array(make_list(i))
    end;

val p1 = make_silly_json(10);

fun assoc (k, xs) =
    case xs of
	[] => NONE
      | (k1, v1)::t => if k = k1 then SOME v1 else assoc (k, t);

val L2 = [("Pearson", 1857), ("Fisher", 1890), ("Neyman", 1894), ("Lebesgue", 1875)];
val k2 = "Le Cam";
val p2 = assoc(k2, L2);

fun dot (j, f) =
    case j of
	Object(lst) => assoc(f, lst)
      | _ => NONE;

val j1 = String "Pearl";
val j2 = Object([("Fisher", Num(int_to_real(72))), ("Pearson", Num(int_to_real(79))), ("Neyman", Num(int_to_real(87))), ("Conway", Num(int_to_real(82)))]);
val p3 = (dot(j1, "Conway"), dot(j2, "Conway"))

fun one_fields (j) =
    case j of
	Object(lst) => (
	 let fun aux (L, acc) =
		 case L of
		     [] => acc
		   | (f, _)::t => aux(t, f::acc)
	 in
	     aux(lst, [])
	 end
     )
      | _ => [];

val p4 = one_fields(j2);

fun no_repeats (lst) =
    List.length(lst) = List.length(dedup(lst))

val s1 = ["Anna", "Sue", "Bob", "Emma", "Anna"];
val p5 = no_repeats(s1);

fun two_fields (j) =
    case j of
	Object(lst) => (
	 let fun aux (L, acc) =
		 case L of
		     [] => acc
		   | (_, v)::t => aux(t, v::acc)
	 in
	     aux(lst, [])
	 end
     )
      | _ => [];

		   
fun recursive_no_field_repeats (j) =
    case j of
	Object(lst) => no_repeats(one_fields(j)) andalso recursive_no_field_repeats(Array(two_fields(j)))
      | Array(jlst) => (
	  case jlst of
	      [] => true
	    | h::t => recursive_no_field_repeats(h) andalso recursive_no_field_repeats(Array(t))
      )
      | _ => true;
	

fun count_occurrences (slst, excep) =
    let fun aux (lst, acc, curr, cnt) =
	    case lst of
		[] => (curr, cnt) :: acc
	      | h::t => (
		  case strcmp(h, curr) of
		      LESS => raise excep
		    | EQUAL => aux(t, acc, curr, cnt + 1)
		    | GREATER => aux(t, (curr, cnt)::acc, h, 1)
	      )
    in
	case slst of
	    [] => []
	  | h::t => aux(t, [], h, 1)
    end;
	
val L = ["Fisher", "Neyman", "Pearson", "Pearson", "Williams"];
exception NotSortedInAscendingOrder;
val res = count_occurrences(L, NotSortedInAscendingOrder);

fun string_values_for_field (k, jlst) =
    case jlst of
	[] => []
      | h::t => (
	  case dot(h, k) of
	      SOME(String(s)) => s::string_values_for_field(k, t)
	   | _ => string_values_for_field(k, t)
      )
val j8 = [j2, Object([("Pearson", String "Karl"), ("Fisher", String "Ronald"), ("Conway", String "John"), ("Neyman", String "Jerzy"), ("Pearson", String "Egon")])];
val res = string_values_for_field("Pearson", j8);


(* histogram and historgram_for_field are provided, but they use your 
   count_occurrences and string_values_for_field, so uncomment them 
   after doing earlier problems *)

(* histogram_for_field takes a field name f and a list of objects js and 
   returns counts for how often a string is the contents of f in js. *)

exception SortIsBroken

fun histogram (xs : string list) : (string * int) list =
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2

    val sorted_xs = ListMergeSort.sort compare_strings xs
    val counts = count_occurrences (sorted_xs,SortIsBroken)

    fun compare_counts ((s1 : string, n1 : int), (s2 : string, n2 : int)) : bool =
      n1 < n2 orelse (n1 = n2 andalso s1 < s2)
  in
    ListMergeSort.sort compare_counts counts
  end

fun histogram_for_field (f,js) =
  histogram (string_values_for_field (f, js))

(**** PUT PROBLEMS 9-11 HERE ****)
fun filter_field_value (k, v, jlst) =
    case jlst of
	[] => []
      | h::t => (
	  case dot(h, k) of
	      SOME(String(s)) => if s = v then h::filter_field_value(k, v, t) else filter_field_value(k, v, t)
	    | _ => filter_field_value(k, v, t)
      );
(*	       
val large_event_clearance_description_histogram = histogram_for_field("event_clearance_description", large_incident_reports_list);

val large_hundred_block_location_histogram = histogram_for_field("hundred_block_location", large_incident_reports_list);

;Control.Print.printDepth := 3;
Control.Print.printLength := 3;

(**** PUT PROBLEMS 12-15 HERE ****)
val forty_third_and_the_ave_reports = filter_field_value("hundred_block_location", "43XX BLOCK OF UNIVERSITY WAY NE", large_incident_reports_list);

val forty_third_and_the_ave_event_clearance_description_histogram = histogram_for_field("event_clearance_description", forty_third_and_the_ave_reports);

val nineteenth_and_forty_fifth_reports = filter_field_value("hundred_block_location", "45XX BLOCK OF 19TH AVE NE", large_incident_reports_list);

val nineteenth_and_forty_fifth_event_clearance_description_histogram = histogram_for_field("event_clearance_description", nineteenth_and_forty_fifth_reports);

;Control.Print.printDepth := 20;
Control.Print.printLength := 20;
*)

(**** PUT PROBLEMS 16-19 HERE ****)
fun concat_with (separator, str_list) =
    let fun aux(str_list, acc) =
	    case str_list of
		[] => acc
	      | h::t => aux(t, acc ^ separator ^ h)
    in
	case str_list of
	    [] => ""
	  | h::t => aux(t, h)
    end;

val test16 = concat_with (" ", ["Karl", "Egon", "Ronald", "Judea", "Jerzy", "John"]);

fun quote_string (s) =
    "\"" ^ s ^ "\"";

fun real_to_string_for_json (r) =
    if real_is_negative(r) then "-" ^ real_to_string(real_abs(r)) else real_to_string(r);

val test18 = real_to_string_for_json (3.17);


fun json_to_string (js) =
    let fun object_to_strings (arr) =
	    case arr of
		[] => []
	      | (k, v)::t => (quote_string(k) ^ " : " ^ json_to_string(v)) :: object_to_strings(t)
	fun array_to_strings (arr) =
	    case arr of
		[] => []
	      | h::t => json_to_string(h) :: array_to_strings(t)
    in
	case js of
	    Num r => real_to_string_for_json (r)
	  | String s => quote_string(s)
	  | False => "false"
	  | True => "true"
	  | Null => "null"
	  | Object lst => "{" ^ concat_with(", ", object_to_strings(lst)) ^ "}"
	  | Array lst => "[" ^ concat_with(", ", array_to_strings(lst)) ^ "]"
    end;

val x = Object [("School Name", String "Booth School of Business"), ("Faculty", Array [Object [("Title", String "Professor"), ("Name", String "Dacheng"), ("Age", Num 37.5), ("Gender", String "Male")]])];
val y = json_to_string(json_obj);


(* For CHALLENGE PROBLEMS, see hw2challenge.sml *)

