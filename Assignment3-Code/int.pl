/*
A skeleton for Assignment 3 on PROP HT2018 at DSV/SU.
Peter Idestam-Almquist, 2018-12-10.
*/

/* Loads the tokenizer. */
:- [tokenizer].

/*
To run your program you should call run/2 as in the following example:
?- run('program1.txt','myparsetree1.txt').
*/

run(InputFile,OutputFile):-
	tokenize(InputFile,Program),
	parse(ParseTree,Program,[]),
	/***  evaluate(ParseTree,[],VariablesOut), ***/
	output_result(OutputFile,ParseTree,VariablesOut).

output_result(OutputFile,ParseTree,Variables):-
	open(OutputFile,write,OutputStream),
	write(OutputStream,'PARSE TREE:'),
	nl(OutputStream),
	writeln_term(OutputStream,0,ParseTree),
	nl(OutputStream),
	write(OutputStream,'EVALUATION:'),
	nl(OutputStream),
	write_list(OutputStream,Variables),
	close(OutputStream).

writeln_term(Stream,Tabs,int(X)):-
	write_tabs(Stream,Tabs),
	writeln(Stream,int(X)).
writeln_term(Stream,Tabs,ident(X)):-
	write_tabs(Stream,Tabs),
	writeln(Stream,ident(X)).
writeln_term(Stream,Tabs,Term):-
	functor(Term,_Functor,0), !,
	write_tabs(Stream,Tabs),
	writeln(Stream,Term).
writeln_term(Stream,Tabs1,Term):-
	functor(Term,Functor,Arity),
	write_tabs(Stream,Tabs1),
	writeln(Stream,Functor),
	Tabs2 is Tabs1 + 1,
	writeln_args(Stream,Tabs2,Term,1,Arity).

writeln_args(Stream,Tabs,Term,N,N):-
	arg(N,Term,Arg),
	writeln_term(Stream,Tabs,Arg).
writeln_args(Stream,Tabs,Term,N1,M):-
	arg(N1,Term,Arg),
	writeln_term(Stream,Tabs,Arg),
	N2 is N1 + 1,
	writeln_args(Stream,Tabs,Term,N2,M).

write_tabs(_,0).
write_tabs(Stream,Num1):-
	write(Stream,'\t'),
	Num2 is Num1 - 1,
	write_tabs(Stream,Num2).

writeln(Stream,Term):-
	write(Stream,Term),
	nl(Stream).

write_list(_Stream,[]).
write_list(Stream,[Ident = Value|Vars]):-
	write(Stream,Ident),
	write(Stream,' = '),
	format(Stream,'~1f',Value),
	nl(Stream),
	write_list(Stream,Vars).

/*
parse(-ParseTree)-->
	A grammar defining our programming language,
	and returning a parse tree.
*/

/*
assign = id , ‘=’ , expression , ‘;’ ;
expression = term , [ ( ‘+’ | ‘-’ ) , expression ] ;
term = factor , [ ( ‘*’ | ‘/’) , term] ;
factor = int | ‘(’ , expression , ‘)’ ;

sentence --> noun_phrase, verb_phrase.
noun_phrase --> determiner, noun.
verb_phrase --> verb, noun_phrase.


determiner --> [a].
determiner --> [the].
noun --> [mouse].
noun --> [cat].
verb --> [scares].
verb --> [hates].
*/
/* WRITE YOUR CODE FOR THE PARSER HERE */
parse(ParseTree) --> assignment(ParseTree).	
assignment(assignment(Ident, AssignOp, Expr, Semicolon)) -->
	ident(Ident), assignOp(AssignOp), expression(Expr), semicolon(Semicolon).
expression(expression(Term, AddOp, Expr)) -->
	term(Term), addOp(AddOp), expression(Expr).
expression(expression(Term, SubOp, Expr)) -->
	term(Term), subOp(SubOp), expression(Expr).
expression(expression(Term)) -->
	term(Term).
term(term(Factor, MultOp, Term)) -->
	factor(Factor), multOp(MultOp), term(Term).
term(term(Factor, DivOp, Term)) -->
	factor(Factor), divOp(DivOp), term(Term).
term(term(Factor)) -->
	factor(Factor).
factor(factor(Int)) -->
	int(Int).
factor(factor(LeftParen, Expr, RightParen)) -->
	leftParen(LeftParen), expression(Expr), rightParen(RightParen).

ident(ident(X))-->[X],{atom(X)}.
int(int(X))-->[X],{integer(X)}.
assignOp(assign_op)-->['='].
addOp(add_op)-->[+].
subOp(sub_op)-->[-].
multOp(mult_op)-->[*].
divOp(div_op)-->[/].
semicolon(semi_colon)-->[';'].
leftParen(left_paren)-->['('].
rightParen(right_paren)-->[')'].


/*
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	Evaluates a parse-tree and returns the state of the program
	after evaluation as a list of variables and their values in
	the form [var = value, ...].
*/

/* WRITE YOUR CODE FOR THE EVALUATOR HERE */

	

/*member(X,[X|Xs]).
member(X,[Y|Ys]):- member(X,Ys).*/



