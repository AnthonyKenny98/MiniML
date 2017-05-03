# Miniml

[![N|Solid](https://cldup.com/dTxpPi9lDf.thumb.png)](https://nodesource.com/products/nsolid)

Miniml is a small subset of an Ocaml like language. It is Turing complete and has no type inferencing or user defined types.

This writeup will go through all the functions I wrote for Miniml and explain the design and style decisions I made, highlight strengths and weaknesses of my implementations, justifying the tradeoff decisions I made. 

This writeup is structured in the following sections:

  - expr.ml
  - evaluation.ml 
        Substitution model, dynamically scoped, and lexically scoped

## expr.ml

expr.ml defines the expression types that are converted from the strings parsed from the users terminal, evaluated as values (see evaluation.ml), converted again to expressions, and then printed as strings to the terminal.

The expr type is an abstractraction of algebraic expressions common to humans (number, negate, add, subtract, times, equal to, less than), truth expressions (booleans), as well Let expressions, functions and applications.

The functions implemented from the expr.ml distribution code are:
    - exp_to_abstract_string
    - exp_to_string
    - free_vars
    - subst

The functions I added to expr.ml are:
    - is_free_var
    - inc
    - new_varname
    - bin_str
    - binop_to_symbol
    
### exp_to_abstract_string and exp_to_string

##### Design
These two functions follow the same pattern matching scheme. 
While I would have liked to abstract out the commonalitites in some way, I could not find any way of doing so in a way that didnt overcomplicate the solution. Because the commonalities are almost exclusively in the pattern matching, and the strings each expr type returns must be manually written, the two functions look largely the same.
My best attempt to optimise the design of these two functions were in the case of a Binop being passed in. For this case I wrote a function bin_str that returns the string representation of the Binop (for exp_to_abstract_string) and binop_to_symbol that returns the arithmetic symbol for the corresponding binop operator. 
I felt this was a better design than in the main pattern matching of exp_to_abstract_string and exp_to_string including a different match for each Binop. This way, each function does not need to match the input exp against four Binops before moving on.
In truth, since the two functions each use seperate binop matching functions (due to the same difficulty of abstracting the pattern matching between the two) I could have included each of these secondary pattern matches within the body of exp_to_abstract_string and exp_to_string, rather than defined them as seperate functions. The decision to do it this way was a style decision.

##### Style
As I mentioned above my decision to define bin_str and binop_to_symbol as seperate functions were mostly for reasons of readability and simplification of the two main functions. 

Another style decision I made was to keep all the -> operators of the pattern matching vertically aligned. I believe this helps to read the structure of the function and the result of each pattern match.

It was for this reason also that I put the "Condition" pattern match at the bottom of each function. Because it takes more characters, it is impossible to keep this pattern match on one 80 character line. By putting this in an arguably more logical spot in the pattern match (perhaps under Binop), it disturbs the readability of the entire function with its two lines. For this reason I kept it at the bottom of both functions.

The final style decision I made in these functions was to avoid using sprintf wherever I could, and using the string concat ^ operator instead, as I believe it makes the expression more readable and obvious. However, this is unavoidable in some instances.

### free_vars

##### Design
I used a similar pattern matching structure to this function.

I only included the expr types which could possibly have free variables, with all others going to SS.empty. 

I wrote a function within free_vars called filtervars, which had the purpose of returning all the free vars in an expression that were not a particular variable. This was used in finding the free vars of Fun, Let and Letrec. Originally I hadn't defined this function and just used the body of it (SS.filter (fun x -> not (x = y)) (free_vars exp)) in each pattern match. Upon realising that this was a commonality in these matches, I thought it good design (and style) to abstract it out. Since it would not be used elsewhere in the code, and called free_vars itself, I thought it best to define it within rec free_vars, rather than as an independant function.

I decided to rely mainly on SS.union to handle the recursive walking of the expr tree. I thought this better design than building lists and then using SS.of_list to convert it to a set of SS.t, although i woul argue marginally better stylistically.

Finally, wherever I could, I looked for commonalities in the result of each pattern match, for example with Binop and App, where I was able to condense that into a single result.

I am fairly happy with my design of this function.

##### Style
As I mentioned above, I think there was a trade off between using lists and SS.union throughout this function, but the benefit in design was greater than that of style.

I continued to keep the -> operators vertically aligned where I could, putting the Conditional expr down the bottom where it could not disrupt the readability of the program.

I also think that the definition of filtervars helped with the style of the program.

### subst

##### Design
The first design decision I made in subst was to define a partly applied function "sub = subst var_name repl" within subst, due the the large number of times I will recursively call subst with the same two initial arguments.

The pattern matching with each result being the defined rule for the substitution model is fairly similar to other pattern matches in expr.ml. Again, i try to eliminate repetition whereever possible. However, because this evaluates to an expr it makes it hard to abstract out/ group pattern matches together. 
This is particularly demonstrated in the pattern matches of Fun, Let and Letrec. Each follow a similarly structured condition, with Let and Letrec having no differences except that one returns a Let expr and the other a Letrec expr. Because of this, I couldn't find a way to abstract this condition out for these three matches. I do not believe that it is beyond the capabilities to abstract out the types, however after much experimentation and research I could not find a way to do it, much to my disappointment.

I defined a function is_free_var above this function that relies on free_vars and is used to determine if a variable can be substituted. Doing this allowed for cleaner and easier checking in subst.

I also defined inc and new_varname for the purpose of generating new variables for functions and Let statements as per the substitution model. 

##### Style
Again, I structured the pattern match so that I could keep the -> operators vertically aligned as much as possible.
I also indented the conditions for Fun, Let and Letrec in the most readable way I could. Unfotunatley my inability to abstract out these three matches meant that the style for these three isn't as nice as I would like it, but I think I did the best I could without being able to abstract these.


## evaluation.ml
evaluation.ml is made up of two main parts, the Env module, and the eval functions. I will begin by addressing my implementations in the Env Module, and finish by describing the eval functions.

### module Env

#### Env.close 
A fairly simple function that I don't think requires much explanation. It simply returns a closure with the given exp and env.

#### Env.lookup 
Given an env is simply a list, I thought it appropriate to pattern match with a list. I considered using List.iter, but with the recursive call of lookup, the if condition, the breaking up of list element into val and valref, and finally the "Unbound variable" case, I thought that it would be simpler and more elegant to use a simple pattern match.

Initially I matched it like so:
    | h :: t -> let val, valref = h in...
but realised that it was better style and design to match as 
    | (val, valref) :: t -> ...
    
I used the EvalError to return a user friendly notification of failure, as surely if a variable cannot be found in the env then it is unbound.

#### Env.extend 
Another simple function, I just appended the new var * val ref to the existing env

#### Env.value _to_string and Env.env_to_string
I was able to rely on my implementation of exp_to_string in value_to_string, only needing to add the strings to represent it was a value rather than an expression. 

The requirement to include the environment posed some challenge, but I think my implementation is fairly simple and easy to understand. If it needs to print the env, it will create a string representing the environment, else just a string "env", which it then appends to the Closure.
Knowing that I would need to print an env as a string in both functions I defined a function that uses List.iter to build this string that I use in both functions. It is a little awkward having to call it with the value_to_string function as an argument, but I couldn't think of any neater way to do it. I think this is fairly well designed and the readability of it is good, aside from needing to jump between three functions to understand what is happening.


#### Env.value_to_expr
I also defined a function that takes a value and returns an equivalent expression to help with the evaluation. I believed it was necessary due to the number of times I needed to switch between an expression and a value during evaluation, not to mention merely the fact that the progression of typing in Miniml from user input to output is string -> expr -> value -> expr -> string. It is a fairly simple pattern match with no irrelevant parts.

### Evaluation

#### eval_x

##### General Idea

I defined the function eval_x in order to abstract out some of the similarities between the other three eval functions (_s, _d, and _l).

The theory behind me defining this funciton was:
In each model of evaluation, only Function, Let, Letrec and App evaluation differ significantly. All other basic expressions evaluate using a very simple rule. 

For example, the rule for evaluating a binop is:

Binop (b, e1, e2) -> Binop(b, (eval e1), (eval e2)) 

where eval is the evaluation model we are using. I realised that if this is the only difference in many of these evaluations, then I in each eval model, I can call eval_x on the expression, passing in the same eval function so that when eval_x evaluates a sub expression, it does so using the correct model.

##### Design

Similarly to my other pattern matches, I tried to group expressions with similar results together, like Num, Unassigned, Raise and Bool all evaluating to a value of themselves.

A lot of the heavy lifting in this was done by earlier functions I had written.

There are a few overlaps for functions like eval_s. This is intentional.

For example, eval_s matches with Var first, and lastly sends anything remaining to eval_x, which also evaluates a Var, but it does so doing lookup (thus not using substitution semantics).

However, I was careful with this. By defining eval_x as non recursive, it always sends sub expressions back to the main eval function (eval_s, eval_d, eval_l) to be handled correctly.
As such, in the case of eval_s, a Var will never be evaluated by eval_l.

It also carrys an optional argument "subst" with a default argument of false, so that when eval_s calls eval_x with subst = true, it will return the value of an expression at the end of the pattern match, rather than calling the eval function on it like in eval_d and eval_l

###### Style
Again, I tried to keep this as readable as possible by grouping functions together and vertically aligning the -> operator where possible. It was difficult to do so due to this function handling the bulk of the pattern matching, but i think with the correct indenting and the clear function names it is relatively readable.


#### eval_s

Because of my implementation of eval_x, eval_s remains quite simple. It defines eval_s' as a partially applied function of eval_s, and then matches with Var, Let, Letrec and App using the substitution model's definitions for evaluation. Like I mentioned above, because eval_x is non recursive and instead calls eval_s, these above expr types will never be evaluated by eval_x in the case of eval_s.
"subst" (defined in expr.ml) helps with much of this function.
The partial application definition eval_s' i think improves both design and style, making it easier to read as well as not repeating the same arguments over and over again. B

#### eval_d

Unlike eval_s, eval_d does not define Var, Let or Letrec, meaning that they are evaluated by eval_x. It does however define an evaluation for App and Fun, which follows the rules set out in the dynamic scoping definition.

#### eval_l

Similarly, eval_l mostly relies on the implementation of eval_x. It does define App and Fun differently to eval_d though, following the semantics of lexical scoping. Everything else is passed through to eval_x. I had some trouble with evaluating functions in lets, but I managed to do it by only slightly changing the code from eval_d. Not only do I evaluate in a Closure, but I still evaluate in an updated environment to prevent getting an unbound variable error (within the body of the function) just as in eval_d, except I update the env from the closure, rather than the env from the function.


## General Notes

##### Style
All throughout my implementation I made the decision to keep with variables in my pattern matching as short and concise as possible, due to the often long length of each result and how many times I then referred to the variable in the result. For instance, instead of exp1 I would use e1. This is a minor note but I thought I would explain my thinking here.

