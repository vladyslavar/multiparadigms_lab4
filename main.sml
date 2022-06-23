exception NoAnswer

(*1*)
fun only_capitals sl : string list =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) sl
(*2*)
fun longest_string1 sl =
    List.foldl (fn (s1, s2) => if (String.size(s1) > String.size(s2)) then s1 else s2) "" sl
(*3*)
fun longest_string2 sl =
    List.foldr (fn (s1, s2) => if (String.size(s1) > String.size(s2)) then s1 else s2) "" sl
(*4*)
fun longest_string_helper f =
    if f (2, 1)
    then longest_string1
    else longest_string2
val longest_string3 = longest_string_helper (fn (a, b) => a > b)
val longest_string4 = longest_string_helper (fn (a, b) => a < b)
(*5*)
val longest_capitalized = longest_string1 o only_capitals
(*6*)
val rev_string = implode o rev o explode
(*7*)
fun first_answer f list =
    case list of
        [] => raise NoAnswer
      | hd::tl =>
        case f hd of
            NONE => first_answer f tl
          | SOME ans => ans
(*8*)
fun all_answers f list =
    case list of
        [] => SOME []
      | hd::tl =>
        case f hd of
            NONE => NONE
          | SOME list1  =>
            case all_answers f tl of
                NONE => NONE
              | SOME list2 => SOME (list1@list2)
(*9(example)*)
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end
(*9.1*)
fun count_wildcards patt = g (fn x => 1) (fn y => 0) patt
(*9.2*)
fun count_wild_and_variable_lengths patt = g (fn x => 1) (fn x => String.size(x)) patt
(*9.3*)
fun count_some_var (str, patt) = g (fn x => 0) (fn y => if str = y then 1 else 0) patt
(*10*)
fun check_pat patt =
    let
    fun get_string_list patt =
        case patt of
            Wildcard => []
          | Variable x => [x]
          | TupleP ps => List.foldl (fn (p, i) => (get_string_list p) @ i) [] ps
          | ConstructorP(_,p) => get_string_list p
          | _ => []
    fun check string_list =
        case string_list of
            [] => true
         |  hd::tl =>
            if List.exists (fn x => x = hd) tl
            then false
            else true andalso check tl
    in
    (check o get_string_list) patt
    end
(*11*)
fun match (v, p) =
    case (v, p) of
        (_, Wildcard) => SOME []
      | (Unit, UnitP) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | (Const x, ConstP y) => if x = y then SOME [] else NONE
      | (Tuple vl, TupleP pl) => if length vl = length pl
                                 then all_answers match (ListPair.zip (vl, pl))
                                 else NONE
      | (Constructor (strv, valu), ConstructorP (strp, patt)) => if String.size(strv) = String.size(strp) then match (valu, patt) else NONE
      | (_, _) => NONE
fun first_match variable patternList =
    SOME (first_answer (fn pattern => match (variable, pattern)) patternList)
    handle NoAnswer => NONE
