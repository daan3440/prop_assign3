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