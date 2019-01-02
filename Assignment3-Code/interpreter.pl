/***
A skeleton for Assignment 3 on PROP HT2018 at DSV/SU.
Peter Idestam-Almquist, 2018-12-10.
***/

/* Loads the tokenizer. */
:- [tokenizer].

/***
To run your program you should call run/2 as in the following example:
?- run('program1.txt','myparsetree1.txt').
***/

run(InputFile,OutputFile):-
	tokenize(InputFile,Program),
	parse(ParseTree,Program,[]),
	/*evaluate(ParseTree,[],VariablesOut),*/ 
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
	
/***
parse(-ParseTree)-->
	A grammar defining our programming language,
	and returning a parse tree.
***/

/* WRITE YOUR CODE FOR THE PARSER HERE */
parse(ParseTree)-->block(ParseTree).
block(block(Leftcurly,Stmts,Rightcurly))-->
	leftcurly(Leftcurly),stmts(Stmts),rightcurly(Rightcurly).
stmts(statements)-->
	[].
stmts(statements(Assign,Stmts))-->
	assign(Assign),stmts(Stmts).
assign(assignment(Ident,Assignop,Expr,Semicolon))-->
	ident(Ident),assignop(Assignop),expr(Expr),semicolon(Semicolon).
expr(expression(Term))-->
	term(Term).
expr(expression(Term,Addop,Expr))-->
	term(Term),addop(Addop), expr(Expr).
expr(expression(Term,Subop,Expr))-->
	term(Term),subop(Subop), expr(Expr).
term(term(Factor))-->
	factor(Factor).
term(term(Factor,Multop,Term))-->
	factor(Factor),multop(Multop),term(Term).
term(term(Factor,Divop,Term))-->
	factor(Factor),divop(Divop),term(Term).
factor(factor(Int))-->
	int(Int).
factor(factor(Ident))-->
	ident(Ident).
factor(factor(Leftparen,Expr,Rightparen))-->
	leftparen(Leftparen),expr(Expr),rightparen(Rightparen).


ident(ident(X))-->[X],{atom(X)}.
int(int(X))-->[X],{integer(X)}.
addop(add_op)-->[+].
subop(sub_op)-->[-].
multop(mult_op)-->[*].
divop(div_op)-->[/].
leftcurly(left_curly)-->['{'].
rightcurly(right_curly)-->['}'].
leftparen(left_paren)-->['('].
rightparen(right_paren)-->[')'].
assignop(assign_op)-->['='].
semicolon(semicolon)-->[';'].

	
/***
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	Evaluates a parse-tree and returns the state of the program
	after evaluation as a list of variables and their values in 
	the form [var = value, ...].
***/

/* WRITE YOUR CODE FOR THE EVALUATOR HERE */
