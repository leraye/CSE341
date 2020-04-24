(* CSE341, HW3 Provided Code *)

exception NoAnswer

datatype pattern = WildcardP
                 | VariableP of string
                 | UnitP
                 | ConstantP of int
                 | ConstructorP of string * pattern
                 | TupleP of pattern list

datatype valu = Constant of int
              | Unit
              | Constructor of string * valu
              | Tuple of valu list

fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            WildcardP         => f1 ()
          | VariableP x       => f2 x
          | ConstructorP(_, p) => r p
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | _                 => 0
    end

(*1*)
fun only_lowercase xs = List.filter (fn s => Char.isLower(String.sub(s, 0))) xs;

val string_list = ["Dan Grossman", "Linda Shapiro", "sushi"];
val res = only_lowercase string_list;

(*2*)
fun longest_string1 xs = List.foldl (fn (x, acc) => if String.size x > String.size acc then x else acc) "" xs;

val string_list = ["Dan Grossman", "Linda Shapiro", "Paul Allen", "Lazar Kaganovich"];
val res = longest_string1 string_list;

(*3*)
fun longest_string2 xs = List.foldl (fn (x, acc) => if String.size x >= String.size acc then x else acc) "" xs;

val string_list = ["Dan Grossman", "Vyacheslav M. Molotov", "Paul Allen", "Lazar Kaganovich", "Nikita S. Khrushchev"];
val res = longest_string2 string_list;

(*4*)
val longest_string_helper = fn f => fn xs => List.foldl (fn (x, acc) => if f(String.size x, String.size acc) then x else acc) "" xs;

val longest_string3 = longest_string_helper (fn (x, y) => x > y);
val longest_string4 = longest_string_helper (fn (x, y) => x >= y);

(*5*)
val longest_lowercase = (longest_string_helper (fn (x, y) => x > y)) o (List.filter (fn s => Char.isLower(String.sub(s, 0))));

val string_list = ["Dan Grossman", "Vyacheslav M. Molotov", "Paul Allen", "Lazar Kaganovich", "Nikita S. Khrushchev", "j. Stalin"];
val res = longest_lowercase string_list;

(*6*)
val caps_no_X_string = String.implode o List.map Char.toUpper o List.filter (fn c => c <> #"x" andalso c <> #"X") o String.explode;

val s = "aBxXXxDdx";
val res = caps_no_X_string s;

(*7*)
fun first_answer f xs =
    case xs of
	x::xs' => (case f x of
		      SOME v => v
		    | _ => first_answer f xs')
      | _ => raise NoAnswer;

(*8*)
fun all_answers f xs =
    let val res = List.map f xs
	fun aux (xs, acc) =
	    case xs of
		[] => acc
	      | SOME v :: xs' => aux(xs', SOME (v @ valOf acc))
	      | _ => NONE
    in
	aux(res, SOME [])
    end;

fun range i j = if i > j then [] else i :: range (i + 1) j;
val countup = range 1;

val res = all_answers (fn x => SOME (countup x)) [4, 2, 1, 8];

val count_wildcards = g (fn _ => 1) (fn s => 0);

val xs = TupleP [WildcardP, UnitP, ConstructorP("nouse", WildcardP), TupleP [WildcardP, VariableP "var"]];
val res = count_wildcards xs;

val count_wild_and_variable_lengths = g (fn _ => 1) (fn s => String.size s);

val res = count_wild_and_variable_lengths xs;

fun count_a_var (s, p) = g (fn _ => 0) (fn x => if x = s then 1 else 0) p;

val xs = TupleP [WildcardP, UnitP, ConstructorP("var", WildcardP), TupleP [WildcardP, VariableP "var"]];
val res = count_a_var("var", xs);

fun check_pat p =
    let fun all_var_strings p =
	    case p of
		VariableP s => [s]
	      | ConstructorP(_, q) => all_var_strings q
	      | TupleP ps => List.foldl (fn (q, x) => (all_var_strings q) @ x) [] ps
	      | _ => []
	fun check_repeats xs =
	    case xs of
		x :: xs' => if List.exists (fn e => e = x) xs' then false else check_repeats xs'
	      | _ => true
	val var_strs = all_var_strings p
    in
	check_repeats var_strs
    end;

val xs = TupleP [WildcardP, UnitP, ConstructorP("nouse", VariableP "y"), TupleP [WildcardP, VariableP "var"], VariableP "v", TupleP [ConstantP 25, VariableP "x"]];
val res = check_pat xs;

fun match (v, p) =
    case (v, p) of
	(v, VariableP s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Constant i, ConstantP j) => if i = j then SOME [] else NONE
      | (Constructor(s, u), ConstructorP(r, q)) => if s = r then match(u, q) else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps then all_answers match (ListPair.zip(vs, ps)) else NONE
      | (_, WildCardP) => SOME [];
	

fun first_match (v, ps) = SOME (first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE;

val vs = Tuple [Constant 42, Constructor("var1", Constant 2020), Tuple [Constructor("x", Constant 3), Constructor("y", Constant 4)]];
val ps = [ConstantP 34, ConstructorP("var1", ConstantP 2019), TupleP [ConstructorP("x", ConstantP 3), ConstructorP("z", ConstantP 4)]];
val res = first_match (Constructor("var1", Constant 2019), ps);
		

	
(**** for the challenge problem only ****)

datatype typ = AnythingT
             | UnitT
             | IntT
             | TupleT of typ list
             | DatatypeT of string

(**** you can put all your code here ****)	
