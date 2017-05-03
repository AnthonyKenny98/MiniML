open Printf
open Str
module Ev =  Evaluation

(* TESTING TYPES AND FUNCTIONS *)

type test = 
{ label : string;
  content : bool Lazy.t;
  time : int;
  fail_msg : string} ;;

type status = 
| Passed
| Failed of string
| Raised_exn of string
| Timed_out of int

exception Timeout ;;

let sigalrm_handler =
   Sys.Signal_handle (fun _ -> raise Timeout) ;;

let timeout (time : int) (content : bool lazy_t) : 'a =
   let old_behavior =
     Sys.signal Sys.sigalrm sigalrm_handler in
   let reset_sigalrm () =
     ignore (Unix.alarm 0);
Sys.set_signal Sys.sigalrm old_behavior in ignore (Unix.alarm time) ;
let res = Lazy.force content in reset_sigalrm (); res ;;

let run_test ({label; content; fail_msg; time}: test)
              (continue : string -> status -> unit) : unit = 

    try
		if timeout time content
		then continue label Passed
		else continue label (Failed fail_msg)
    with
    | Timeout -> continue label (Timed_out time)
    | Ev.EvalError msg ->continue label (Raised_exn msg)
    | exn     -> continue label
                    (Raised_exn (Printexc.to_string exn))

;;



let test ?(fail_msg="somehow") ?(time=5) label content =
{ label = label;
content = content;
fail_msg = fail_msg;
time = time } ;;

(* PRESENTATION TYPES, OBJECTS AND FUNCTIONS *)

type color = 
| Defaultc
| Black
| Green 
| Red 
| Cyan
| Yellow
| Purple

let get_color (c : color) : string = 
	match c with 
	| Defaultc -> "m"
	| Black	  -> "30m"
	| Green   -> "32m"
	| Red     -> "31m"
	| Cyan 	  -> "36m"
	| Yellow  -> "33m"
	| Purple  -> "35m"
;;

type font = 
| Default
| Bold

let get_font (f : font) : string = 
	match f with 
	| Default -> "0:"
	| Bold -> "1:"

(* TERMINAL OUTPUT *)

class terminal_output ?(color=Defaultc) ?(font=Default) () =

object(this)
	
	method color = color
	method font = font 
	method output_width = 140

	method format color font : string = 
		 sprintf "\027[%s%s" (get_font font) (get_color color)
	
	method set_format : string = this#format this#color this#font
	method reset_format : string = this#format Defaultc Default

	method output : string = this#set_format ^ this#reset_format

	method print = printf "%s" this#output;

end

(* TABLE ROW *)
class table_row ?(color=Defaultc) ?(font=Default) (row : string list) () =

object(this)
	
	inherit terminal_output ~color:color ~font:font ()

	method column_width = this#output_width / List.length row

	method columns = 
		let rec aux lst = 
			match lst with 
			| [] -> "\n"
			| h :: t -> let x = String.length h in (* FIXTHISFUCKING THING *)

				"| " ^ h ^ (String.make (this#column_width - x) ' ') ^ (aux t)
		in aux row

	method! output = this#set_format ^ "|" ^ this#columns ^ this#reset_format

end

(* PRESENT *)

let present (label : string) (status : status) = 
	match status with 
	| Passed -> 
		let x = new table_row ~color:Green [label; "Passed"; ""] () in x#print;
	| Failed message -> 
		let x = new table_row ~color:Red [label; "Failed"; message] () in x#print;
	| Raised_exn message ->
		let x = new table_row ~color:Cyan [label; "Raised"; message] () in x#print;
	| Timed_out seconds ->
		let x = new table_row ~color:Yellow [label; "Timed_out"; 
											sprintf "Timed out in %d" seconds] () in x#print;
;;

(* TABLE *)


class table (title : string) (rows : test list) = 
object(this)
	
	inherit terminal_output ~color:Defaultc ~font:Bold ()
	method name = title
	method header = 
		let buffer = (this#output_width - (String.length title))/2 in
			this#set_format ^ (String.make buffer ' ') ^ this#name ^ "\n"

	method content = rows 

	method! output = this#set_format ^ (String.make this#output_width '=' ) ^ this#reset_format ^ "\n"

	method draw = 
		printf "%s" this#header;
		this#print;
		List.iter (fun test -> run_test test present) this#content;
		this#print;
		printf "\n\n\n"

end

let report (tests : (string * test list) list) : unit =
	let report_on_testblock testblock = let testblockname, testlist = testblock in 
		let x = new table testblockname testlist in x#draw in
		(* List.iter (fun test -> run_test test present) testlist in *)
   	List.iter (report_on_testblock) tests ;;

