sentence(sentence(NounPhrase,VerbPhrase)) -->
	noun_phrase(NounPhrase), 
	verb_phrase(VerbPhrase).

noun_phrase(noun_phrase(Determiner,Noun)) -->
	determiner(Determiner), 
	noun(Noun).
	
verb_phrase(verb_phrase(Verb,NounPhrase)) -->
	verb(Verb), 
	noun_phrase(NounPhrase).
	
determiner(determiner(a)) --> [a].

noun(noun(cat)) --> [cat].
noun(noun(mouse)) --> [mouse].

verb(verb(scares)) --> [scares].
verb(verb(hates)) --> [hates].

/* Task 3 */

translator(sentence(NounPhrase,VerbPhrase),Swedish3):- 
	translator(NounPhrase,Swedish1), 
	translator(VerbPhrase,Swedish2),
	append(Swedish1,Swedish2,Swedish3).
translator(noun_phrase(Determiner,Noun),Swedish3):-
	translator(Determiner,Swedish1),
	translator(Noun,Swedish2),
	append(Swedish1,Swedish2,Swedish3).
translator(verb_phrase(Verb,NounPhrase),Swedish3):-
	translator(Verb,Swedish1), 
	translator(NounPhrase,Swedish2), 
	append(Swedish1,Swedish2,Swedish3).
translator(determiner(a),[en]).
translator(noun(cat),[katt]).
translator(noun(mouse),[mus]).
translator(verb(scares),['skrämmer']).
translator(verb(hates),[hatar]).

append([],Xs,Xs).
append([X|Xs],Ys,[X|Zs]):-
	append(Xs,Ys,Zs).