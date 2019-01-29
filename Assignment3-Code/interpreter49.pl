/*** 
Grupp 49
Daniel Andersson - daan3440@student.su.se
Erik Lavfors - erla5605@student.su.se
***/

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
	evaluate(ParseTree,[],VariablesOut),
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

/*evaluate(ParseTree,VariablesIn,VariablesOut):-
	evaluate(ParseTree, VariablesIn, VariablesOut).*/
evaluate(block(_Leftcurly, Statements, _Rightcurly), VariablesIn, VariablesOut):-
	evaluate(Statements, VariablesIn, StatementsOut),
	VariablesOut = StatementsOut.
evaluate(statements, VariablesIn, VariablesOut):- VariablesOut = VariablesIn. 
evaluate(statements(Assignment, Statements), VariablesIn, VariablesOut):-
	evaluate(Assignment, VariablesIn, AssignOut),
	AssignOut = [Var, Val],
	\+ member1(Var=_X, VariablesIn),
	append1([Var = Val], VariablesIn,NVariablesIn),
	evaluate(Statements, NVariablesIn, StatementsOut),
	VariablesOut = StatementsOut.
evaluate(statements(Assignment, Statements), VariablesIn, VariablesOut):-
	evaluate(Assignment, VariablesIn, AssignOut),
	AssignOut = [Var, Val],
	member1(Var=_X, VariablesIn),
	remove(Var=_X, VariablesIn, Zs),
	append1([Var = Val], Zs,NVariablesIn),
	evaluate(Statements, NVariablesIn, StatementsOut),
	VariablesOut = StatementsOut.
evaluate(assignment(Ident,_Assignop,Expr,_Semicolon), VariablesIn, VariablesOut):-
	evaluate(Ident, VariablesIn, IdentOut),
	evaluate(Expr, VariablesIn, ExprOut),
	Value is ExprOut,
	VariablesOut = [IdentOut, Value].	
evaluate(expression(Term), VariablesIn, VariablesOut):-
	evaluate(Term, VariablesIn, TermOut),
	VariablesOut is TermOut.
evaluate(expression(Term, Addop, Expr), VariablesIn, VariablesOut):-
	evaluate(Term, VariablesIn, TermOut),
	Addop==add_op,
	evaluate(Expr, VariablesIn, TermOut, +,ExprOut),
	VariablesOut is ExprOut.
evaluate(expression(Term, Subop, Expr), VariablesIn, VariablesOut):-
	evaluate(Term, VariablesIn, TermOut),
	Subop==sub_op,
	evaluate(Expr, VariablesIn, TermOut, -, ExprOut),
	VariablesOut is ExprOut.
evaluate(term(Factor), VariablesIn, VariablesOut):-
	evaluate(Factor, VariablesIn, FactorOut),
	VariablesOut is FactorOut.
evaluate(term(Factor, Multop, Term), VariablesIn, VariablesOut):-
	Multop==mult_op,
	evaluate(Factor, VariablesIn, FactorOut),
	evaluate(Term, VariablesIn, FactorOut, *, TermOut),
	VariablesOut is TermOut.
evaluate(term(Factor, Divop, Term), VariablesIn, VariablesOut):-
	Divop==div_op,
	evaluate(Factor, VariablesIn, FactorOut),
	evaluate(Term, VariablesIn, FactorOut, /, TermOut),
	VariablesOut is TermOut.
evaluate(factor(Ident), VariablesIn, VariablesOut):-
	evaluate(Ident, VariablesIn, IdentOut),
	atom(IdentOut),
	member1(IdentOut=X, VariablesIn),
	VariablesOut = X.
evaluate(factor(Ident), VariablesIn, VariablesOut):-
	evaluate(Ident, VariablesIn, IdentOut),
	atom(IdentOut),
	\+ member1(IdentOut=_X, VariablesIn),
	VariablesOut = 0.
evaluate(factor(Int), VariablesIn, VariablesOut):-
	evaluate(Int, VariablesIn, IntOut),
	VariablesOut = IntOut.
evaluate(factor(_Leftparen, Expr, _Rightparen), VariablesIn, VariablesOut):-
	evaluate(Expr, VariablesIn, ExprOut),
	VariablesOut = (ExprOut).
	
evaluate(ident(X), [], X).
evaluate(ident(X), _Ys, X).
evaluate(int(X), [], X ).
evaluate(int(X), _Ys, X ).
	
evaluate(expression(Term), VariablesIn, Operand, Operator, VariablesOut):-
	Operator == -,
	evaluate(Term, VariablesIn, TermOut),
	VariablesOut is Operand - TermOut.
evaluate(expression(Term, Addop, Expr), VariablesIn, Operand, Operator, VariablesOut):-
	Addop==add_op,
	Operator == -,
	evaluate(Term, VariablesIn, TermOut),
	Temp is Operand - TermOut,
	evaluate(Expr, VariablesIn, Temp, +, ExprOut),
	VariablesOut is ExprOut.
evaluate(expression(Term, Subop, Expr), VariablesIn, Operand, Operator, VariablesOut):-
	Subop==sub_op,
	Operator == -,
	evaluate(Term, VariablesIn, TermOut),
	Temp is Operand - TermOut,
	evaluate(Expr, VariablesIn, Temp, -, ExprOut),
	VariablesOut is ExprOut.
evaluate(expression(Term), VariablesIn, Operand, Operator, VariablesOut):-
	Operator == +,
	evaluate(Term, VariablesIn, TermOut),
	VariablesOut is Operand + TermOut.
evaluate(expression(Term, Addop, Expr), VariablesIn, Operand, Operator, VariablesOut):-
	Addop==add_op,
	Operator == +,
	evaluate(Term, VariablesIn, TermOut),
	Temp is Operand + TermOut,
	evaluate(Expr, VariablesIn, Temp, +, ExprOut),
	VariablesOut is ExprOut.
evaluate(expression(Term, Subop, Expr), VariablesIn, Operand, Operator, VariablesOut):-
	Subop==sub_op,
	Operator == +,
	evaluate(Term, VariablesIn, TermOut),
	Temp is Operand + TermOut,
	evaluate(Expr, VariablesIn, Temp, -, ExprOut),
	VariablesOut is ExprOut.
	
evaluate(term(Factor), VariablesIn, Operand, Operator, VariablesOut):-
	Operator == *,
	evaluate(Factor, VariablesIn, FactorOut),
	VariablesOut is Operand * FactorOut.
evaluate(term(Factor, Multop, Term), VariablesIn, Operand, Operator, VariablesOut):-
	Multop==mult_op,
	Operator == *,
	evaluate(Factor, VariablesIn, FactorOut),
	Temp is Operand * FactorOut,
	evaluate(Term, VariablesIn, Temp, *, TermOut),
	VariablesOut is TermOut.
evaluate(term(Factor, Divop, Term), VariablesIn, Operand, Operator, VariablesOut):-
	Divop==div_op,
	Operator == *,
	evaluate(Factor, VariablesIn, FactorOut),
	Temp is Operand * FactorOut,
	evaluate(Term, VariablesIn, Temp, /, TermOut),
	VariablesOut is TermOut.
evaluate(term(Factor), VariablesIn, Operand, Operator, VariablesOut):-
	Operator == /,
	evaluate(Factor, VariablesIn, FactorOut),
	VariablesOut is Operand / FactorOut.
evaluate(term(Factor, Multop, Term), VariablesIn, Operand, Operator, VariablesOut):-
	Multop==mult_op,
	Operator == /,
	evaluate(Factor, VariablesIn, FactorOut),
	Temp is Operand / FactorOut,
	evaluate(Term, VariablesIn, Temp, *, TermOut),
	VariablesOut is TermOut.
evaluate(term(Factor, Divop, Term), VariablesIn, Operand, Operator, VariablesOut):-
	Divop==div_op,
	Operator == /,
	evaluate(Factor, VariablesIn, FactorOut),
	Temp is Operand / FactorOut,
	evaluate(Term, VariablesIn, Temp, /, TermOut),
	VariablesOut is TermOut.
	
member1(X,[X|_Xs]).
member1(X,[_Y|Ys]):- member1(X,Ys).
append1([],Ys,Ys).
append1([X|Xs],Ys,[X|Zs]):-append1(Xs,Ys,Zs).
remove(X,[X|Ys],Ys).
remove(X, [Y|Ys],[Y|Zs]):-
	remove(X, Ys,Zs).
