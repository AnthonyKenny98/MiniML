open Tester 
open Printf
open Expr
open Evaluation


let testr (compare : 'a -> 'a -> bool) (f : 'a) (label : string) (output : 'a) = 
	test (label) (lazy (compare f output)) 

let exp_to_abstract_string_tests : test list = 
	let f = exp_to_abstract_string in
	[

		test "Var(x)" (lazy ((f (Var("x"))) = "Var(x)"));

		test "Var(hello)" (lazy ((f (Var("hello"))) = "Var(hello)"));
		
		test "Num(1)" (lazy ((f (Num(1))) = "Num(1)"));

		test "Num(13021998)" (lazy ((f (Num(13021998))) = "Num(13021998)"));


		test "Num(13)" (lazy (f (Num(13)) = "Num(13)"));
		
		test "Bool(true)" (lazy (f (Bool(true)) = "Bool(true)"));

		test "Bool(false)" (lazy ((f (Bool(false))) = "Bool(false)"));
		
		
		test "Unop(Negate, Num(1))" (lazy ((f (Unop(Negate, Num(1)))) = ("Unop(Negate, Num(1))")));
		
		test "Unop(Negate, Var(Anthony))" (lazy ((f (Unop(Negate, Var("Anthony")))) = ("Unop(Negate, Var(Anthony))")));
		
		test "Unop(Negate, Bool(true))" (lazy ((f (Unop(Negate, Bool(true)))) = ("Unop(Negate, Bool(true))")));

		test "Binop(Plus, Num(4), Num(3))" (lazy ((f (Binop(Plus, Num(4), Num(3)))) = ("Binop(Plus, Num(4), Num(3))")));
		
		test "Binop(Minus, Var(variable), Num(150))" 
				(lazy ((f (Binop(Minus, Var("variable"), Num(150)))) = ("Binop(Minus, Var(variable), Num(150))")));

		test "Binop(Times, Var(x), Var(y))" (lazy ((f (Binop(Times, Var("x"), Var("y")))) = ("Binop(Times, Var(x), Var(y))")));


		test "Binop(Equals, Var(x), Num(3))" 
				(lazy ((f (Binop(Equals, Var("x"), Num(3)))) = ("Binop(Equals, Var(x), Num(3))")));

		test "Conditional test "
				(lazy ((f (Conditional(Binop (Equals, Var("x"), Num(10)), Num(10), Raise))) = ("Conditional(Binop(Equals, Var(x), Num(10)), Num(10), Raise)"))); 

		test "Fun(x, Binop(Plus, Var(x), Num(2)))" (lazy ((f (Fun ("x", Binop(Plus, Var("x"), Num(2))))) = ("Fun(x, Binop(Plus, Var(x), Num(2)))"))); 

		test "Let(x, Num(4), Binop(Plus, Var(x), Num(2)))"
			(lazy ((f (Let ("x", Num(4), Binop(Plus, Var("x"), Num(2))))) = ("Let(x, Num(4), Binop(Plus, Var(x), Num(2)))"))); 

		test "Letrec test"
			(lazy ((f (Letrec ("x", Binop(Plus, Var("x"), Num(1)), Binop(Plus, Var("x"), Num(2))))) = ("Letrec(x, Binop(Plus, Var(x), Num(1)), Binop(Plus, Var(x), Num(2)))"))); 

		test "Raise" (lazy ((f Raise) = ("Raise")));

		test "Unassigned" (lazy ((f Unassigned) = ("Unassigned")));

		test "App(Fun(x, Var(x)), Num(1))"
			(lazy ((f (App(Fun("x", Var("x")), Num(1)))) = ("App(Fun(x, Var(x)), Num(1))"))); 
	] ;;

(* let msg lst = List.fold_left (fun a b -> a ^ sprintf "%d " b) "" lst;;   *)

let free_vars_tests : test list = 
	let f = free_vars in
	[
		
		test "Num(2)" (lazy (same_vars (f (Num(2))) (vars_of_list [])));

		test "Var(x)" (lazy (same_vars (f (Var("x"))) (vars_of_list ["x"])));
		test "Var(helloanthony)" (lazy (same_vars (f (Var("helloanthony"))) (vars_of_list ["helloanthony"])));

		test "Unop test" (lazy (same_vars (f (Unop (Negate, Num(3)))) (vars_of_list [])));
		test "Unop test 2" (lazy (same_vars (f (Unop (Negate, Var("x")))) (vars_of_list ["x"])));

		test "Binop test" (lazy (same_vars (f (Binop (Plus, Unop(Negate, Var("x")), Var("y")))) 
					(vars_of_list ["x"; "y"])));
		
		test "Conditional test" (lazy (same_vars (f (Conditional (Binop(Equals, Var("x"), Num(3)), Var("x"), Num(3)))) 
					(vars_of_list ["x"])));

	 	test "Conditional not test" (lazy (not (same_vars (f (Conditional (Binop(Equals, Var("x"), Num(3)), Var("x"), Num(3)))) (vars_of_list []))));

		test "App" (lazy (same_vars (f (App (Var("x"), Num(100)))) (vars_of_list ["x"])));

		test "Fun test" (lazy (same_vars (f (Fun("x", Binop(Plus, Var("x"), Var("y"))))) (vars_of_list ["y"])));

		test "Let test" (lazy (same_vars (f (Let("x", Binop(Plus, Num(3), Num(5)), Binop(Minus, Var("x"), Num(2))))) (vars_of_list [])));

		test "Let test" (lazy (same_vars (f (Let("x", Binop(Plus, Var("x"), Num(5)), Binop(Minus, Var("x"), Num(2))))) (vars_of_list ["x"])));

		test "Let test" (lazy (same_vars (f (Let("x", Binop(Plus, Var("y"), Num(5)), Binop(Minus, Var("x"), Num(2))))) (vars_of_list ["y"])));

		test "Let test" (lazy (same_vars (f (Let("x", Binop(Plus, Var("x"), Var("y")), Binop(Minus, Var("x"), Num(2))))) (vars_of_list ["x";"y"])));

		test "Let test" (lazy (same_vars (f (Let("x", Binop(Plus, Var("x"), Var("y")), Binop(Minus, Var("x"), Var("z"))))) (vars_of_list ["x";"y";"z"])));

		test "Let set" (lazy (same_vars (f (Let("x", Binop(Plus, Var("a"), Var("y")), Binop(Minus, Var("x"), Var("z"))))) (vars_of_list ["a";"y";"z"])));

		test "Let set" (lazy (same_vars (f (Let("x", Binop(Plus, Var("a"), Var("y")), Binop(Minus, Var("x"), Var("z"))))) (vars_of_list ["a";"y";"z"])));

		test "Letrec test" (lazy (same_vars (f (Letrec("x", Binop(Plus, Var("x"), Var("y")), Binop(Minus, Var("x"), Num(2))))) (vars_of_list ["y"])));

		test "Letrec test" (lazy (same_vars(f (Letrec("x", Binop(Plus, Var("x"), Var("y")), Binop(Minus, Var("x"), Var("z"))))) (vars_of_list ["y";"z"])));
		
		test "Letrec test" (lazy (same_vars (f (Letrec("x", Binop(Plus, Var("a"), Var("y")), Binop(Minus, Var("x"), Var("z"))))) (vars_of_list ["a";"y";"z"])));

		test "complicated Let test" (lazy (same_vars (f (Let("v", Fun("y", Binop(Times, Var("y"), Var("y"))), Conditional(Bool(true), App(Var("v"), Num(3)), Raise)))) (vars_of_list [])));


		test "complicated Let test" (lazy (same_vars (f (Let("v", Fun("y", Binop(Times, Var("y"), Var("y"))), Conditional(Bool(true), App(Var("v"), Binop(Plus, Num(3), Var("b"))), Var("a")))))
				(vars_of_list ["a"; "b"])));

		test "Let rec test" (lazy (same_vars (f (Letrec("x", Binop(Plus,Var("x"), Num(1)), Var("x"))))
				(vars_of_list [])));

		test "Let test with fun" (lazy (same_vars (f (Let("x", Var("y"), Fun("y", Var("x"))))) (vars_of_list ["y"])));

		test "Raise" (lazy (same_vars (f Raise) (vars_of_list [])));


	] ;;


let subst_tests : test list = 
	let f = subst "x" (Var("y")) in
	let input s = "subst " ^ s in
	let test_sub = testr (fun a b -> (exp_to_abstract_string a) = (exp_to_abstract_string b)) in
	[
		test_sub (f (Num(2))) (input "Num") ((Num(2)));

		test_sub (f (Var("x"))) (input "Var") (Var("y"));

		test_sub (f (Var("helloanthony"))) (input "Var ") (Var("helloanthony"));

		test_sub (f (Unop (Negate, Num(3)))) (input "Unop") (Unop (Negate, Num(3)));

		test_sub (f (Unop (Negate, Var("x")))) (input "Unop") (Unop (Negate, Var("y")));

		test_sub (f (Binop (Plus, Unop(Negate, Var("x")), Var("y")))) (input "Binop")
				(Binop (Plus, Unop(Negate, Var("y")), Var("y")));

		test_sub(f (Conditional (Binop(Equals, Var("x"), Num(3)), Var("x"), Num(3)))) 
					(input "Binop") 
					(Conditional (Binop(Equals, Var("y"), Num(3)), Var("y"), Num(3)));

		test_sub (f (App (Var("x"), Num(100)))) 
					(input "Binop") (App (Var("y"), Num(100)));

		test_sub (f (Fun("x", Binop(Plus, Var("x"), Var("y")))))
					(input "Binop") (Fun("x", Binop(Plus, Var("x"), Var("y"))));

		test_sub (f (Fun("z", Binop(Plus, Var("x"), Num(2)))))
					(input "Binop") (Fun ("z", Binop(Plus, Var("y"), Num(2))));

		test_sub (subst "x" (Binop(Plus, Var("y"), Num(1))) (Fun("y", Binop(Plus, Var("y"), Var("x")))))
					(input "Binop")
					(Fun("1", Binop(Plus, Var("1"), Binop(Plus, Var("y"), Num(1)))));

		test_sub (f (Let("x", Num(5), Binop(Plus, Var("x"), Num(100))))) (input "Let") (Let("x", Num(5), Binop(Plus, Var("x"), Num(100))));

		test_sub (f (Let("z", Binop(Minus, Var("x"), Num(5)), Unop(Negate, Var("z"))))) (input "Let")
					(Let("z", Binop(Minus, Var("y"), Num(5)), Unop(Negate, Var("z"))));

		test_sub (subst "x" (Binop(Plus, Var("y"), Num(5))) (Let("y", Binop(Times, Var("x"), Num(2)), Unop(Negate, Var("y")))))
					(input "Big Binop with Let")
					(Let("0", Binop(Times, Binop(Plus, Var("y"), Num(5)), Num(2)), Unop(Negate, Var("0"))));

		test_sub (f Raise) (input "Raise") (Raise);
		test_sub (f Unassigned) (input "Unassigned") (Unassigned)

		(* Need to write test for LetRec *)

	] ;;


let eval_tests (eval : expr -> Env.env -> Env.value) : test list = 
	let env = Env.create () in
	let f e = eval e env in
	[

		test "Num" (lazy ((f (Num(2))) = Val(Num(2))));

		test "Unbound Var [SHOULD RAISE EXN]" (lazy ((f (Var("hello"))) = Val(Raise))); 

		test "biggerNum" (lazy (f (Num(13021998)) = Val(Num(13021998))));

		test "Bool(true)" (lazy ((f (Bool(true))) = Val(Bool(true))));
		test "Bool(false)" (lazy (f (Bool(false)) = Val(Bool(false))));

		test "Unop "(lazy (f (Unop(Negate, Num(1))) = Val(Unop(Negate, Num(1)))));

		test "Binop [SHOULD RAISE EXN]" (lazy (f (Binop (Plus, Unop(Negate, Var("x")), Var("y"))) =
					Val(Binop (Plus, Unop(Negate, Var("x")), Var("y")))));

		test "Binop" (lazy (f (Binop (Plus, Num(10), Num(20))) = Val(Num (30))));

		test "Binop Minus" (lazy (f (Binop (Minus, Num(10), Num(20))) = Val(Num (-10))));

		test "Binop Times" (lazy (f (Binop (Times, Num(10), Num(20))) = Val(Num (200))));

		test "Binop Equals" (lazy (f (Binop (Equals, Num(10), Num(20))) =  Val(Bool (false))));

		test "Binop Equals" (lazy (f (Binop (Equals, Num(15), Num(15))) = Val(Bool (true))));

		test "Binop LessThan" (lazy (f (Binop (LessThan, Num(-15), Num(15))) = Val(Bool (true))));

	 	test "Binop Plus complicated [SHOULD RAISE EXN]" (lazy (f (Binop (Plus, Num(10), Binop (Plus, Num(20), Var("x")))) = Val(Binop (Plus, Num(30), Var("x")))));

		test "Conditional" (lazy (f (Conditional(Binop(Equals, Num(1), Num(1)), Num(10), Num(20))) = Val(Num(10))));

		test "Fun  - eval_s and eval_d" (lazy (f (Fun("x", Binop(Plus, Var("x"), Num(11)))) = Val(Fun("x", Binop(Plus, Var("x"), Num(11))))));
		test "Fun  - eval_l" (lazy (f (Fun("x", Binop(Plus, Var("x"), Num(11)))) = Closure(Fun("x", Binop(Plus, Var("x"), Num(11))), Env.create())));

		test "Let" (lazy (f (Let("x", Num(1), Binop(Plus, Var("x"), Num(2)))) = Val(Num(3))));

	] ;;

let env_tests : test list = 
	let env = Env.create() in 
	let env = Env.extend env "a" (ref (Env.Val(Num(1)))) in

	[
		test "test1" (lazy ((Env.lookup env "a") = Val(Num(1))));

		let env = Env.extend env "b" (ref (Env.Val(Num(2)))) in 
		test "test2" (lazy ((Env.lookup env "b") = Val(Num(2))))
	] ;;


(* let testtests =
  [ test "should fail" (lazy (3 > 4));
    test "should pass" (lazy (4 > 3));
    test "should time out"(lazy (let rec f x = f x in f 1)) ;
  	test "should raise exception" (lazy ((List.nth [0;1] 3) = 3))
] ;; 
 *)
let tests : ((string * test list) list) =
[
   ("exp_to_abstract_string_tests", exp_to_abstract_string_tests);
   ("free_vars_tests", free_vars_tests);
   ("subst_test", subst_tests);
   ("eval_s_tests", eval_tests(eval_s));
   ("env_tests", env_tests);
   ("eval_d_tests", eval_tests(eval_d));
   ("eval_l_tests", eval_tests(eval_l))

 ];;


let _ = printf "succeded\n" in
let _ = report tests
in ();;







