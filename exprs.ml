

let number1 : expr = Num 1

let unop1 : expr = Unop (Negate, Num 1)

let raise : expr = Raise

let binop1 : expr = Binop (Plus, Num 1, Num 2)

let cond1 : expr = Conditional ((Binop (Equals, Var "x", Num 1)), Num 2, Num 3)

let fun1 : expr = Let("f", Fun("x", Var("x")), App(App(Var("f"), Var("f")), Num(3)))

let fun2 : expr = Fun("x", Binop(Plus, Var("x"), Var("y")))

let var1 : expr = Var "x"