List is from "DECsystem-10 PROLOG USER'S MANUAL" (1982).

#### Summary of Evaluable Predicates

- [ ] abolish(F,N)      Abolish the interpreted procedure named F arity N.
- [ ] abort             Abort execution of the current directive.
- [ ] ancestors(L)      The ancestor list of the current clause is L.
- [ ] arg(N,T,A)        The Nth argument of term T is A.
- [ ] assert(C)         Assert clause C.
- [ ] assert(C,R)       Assert clause C, reference R.
- [ ] asserta(C)        Assert C as first clause.
- [ ] asserta(C,R)      Assert C as first clause, reference R.
- [ ] assertz(C)        Assert C as last clause.
- [ ] assertz(C,R)      Assert C as last clause, reference R.
- [ ] atom(T)           Term T is an atom.
- [ ] atomic(T)         Term T is an atom or integer.
- [ ] bagof(X,P,B)      The bag of instances of X such that P is provable is B.
- [ ] break             Break at the next interpreted procedure call.
- [ ] 'C'(S1,T,S2)      (grammar rules) S1 is connected by the terminal T to S2.
- [ ] call(P)           Execute the interpreted procedure call P.
- [ ] clause(P,Q)       There is an interpreted clause, head P, body Q.
- [ ] clause(P,Q,R)     There is an interpreted clause, head P, body Q, ref R.
- [ ] close(F)          Close file F.
- [ ] compare(C,X,Y)    C is the result of comparing terms X and Y.
- [ ] compile(F)        Compile the procedures in text file F.
- [ ] consult(F)        Extend the interpreted program with clauses from file F.
- [ ] current_atom(A)   One of the currently defined atoms is A.
- [ ] current_functor(A,T) A current functor is named A, most general term T.
- [ ] current_predicate(A,P) A current predicate is named A, most general goal P.
- [ ] current_op(P,T,A) Atom A is an operator type T precedence P.
- [ ] debug             Switch on debugging.
- [ ] debugging         Output debugging status information.
- [ ] depth(D)          The current invocation depth is D.
- [ ] display(T)        Display term T on the terminal.
- [ ] erase(R)          Erase the clause or record, reference R.
- [ ] expand_term(T,X)  Term T is a shorthand which expands to term X.
- [ ] fail              Backtrack immediately.
- [ ] fileerrors        Enable reporting of file errors.
- [ ] functor(T,F,N)    The principal functor of term T has name F, arity N.
- [ ] gc                Enable garbage collection.
- [ ] gcguide(F,O,N)    Change garbage collection parameter F from O to N.
- [ ] get(C)            The next non-blank character input is C.
- [ ] get0(C)           The next character input is C.
- [ ] halt              Halt Prolog, exit to the monitor.
- [ ] incore(P)         Execute the compiled procedure call P.
- [ ] instance(R,T)     A most general instance of the record reference R is T.,
- [ ] integer(T)        Term T is an integer.
- [ ] Y is X            Y is the value of integer expression X.
- [ ] keysort(L,S)      The list L sorted by key yields S.
- [ ] leash(M)          Set leashing mode to M.
- [ ] length(L,N)       The length of list L is N.
- [ ] listing           List the current interpreted program.
- [ ] listing(P)        List the interpreted procedure(s) specified by P.
- [ ] log               Enable logging.
- [ ] maxdepth(D)       Limit invocation depth to D.
- [ ] name(A,L)         The name of atom or integer A is string L.
- [ ] nl                Output a new line.
- [ ] nodebug           Switch off debugging.
- [ ] nofileerrors      Disable reporting of file errors.
- [ ] nogc              Disable garbage collection.
- [ ] nolog             Disable logging.
- [ ] nonvar(T)         Term T is a non-variable.
- [ ] nospy P           Remove spy-points from the procedure(s) specified by P.
- [ ] numbervars(T,M,N) Number the variables in term T from M to N-1.
- [ ] op(P,T,A)         Make atom A an operator of type T precedence P.
- [ ] phrase(P,L)       List L can be parsed as a phrase of type P.
- [ ] plsys(T)          Allows certain interactions with the operating system.
- [ ] print(T)          Portray or else write the term T.
- [ ] prompt(A,B)       Change the prompt from A to B.
- [ ] put(C)            The next character output is C.
- [ ] read(T)           Read term T.
- [ ] reconsult(F)      Update the interpreted program with procedures from file F.
- [ ] recorda(K,T,R)    Make term T the first record under key K, reference R.
- [ ] recorded(K,T,R)   Term T is recorded under key K, reference R.
- [ ] recordz(K,T,R)    Make term T the last record under key K, reference R.
- [ ] reinitialise      Initialisation - looks for 'prolog.bin' or 'prolog.ini'.
- [ ] rename(F,G)       Rename file F to G.
- [ ] repeat            Succeed repeatedly.
- [ ] restore(S)        Restore the state saved in file S.
- [ ] retract(C)        Erase the first interpreted clause of form C.
- [ ] revive(F,N)       Revive the latest compiled procedure named F arity N.
- [ ] save(F)           Save the current state of Prolog in file F.
- [ ] save(F,R)         As save(F) but R is 0 first time, 1 after a 'restore'.
- [ ] see(F)            Make file F the current input stream.
- [ ] seeing(F)         The current input stream is named F.
- [ ] seen              Close the current input stream.
- [ ] setof(X,P,S)      The set of instances of X such that P is provable is S.
- [ ] skip(C)           Skip input characters until after character C.
- [ ] sort(L,S)         The list L sorted into order yields S.
- [ ] spy P             Set spy-points on the procedure(s) specified by P.
- [ ] statistics        Output various execution statistics.
- [ ] statistics(K,V)   The execution statistic key K has value V.
- [ ] subgoal_of(G)     An ancestor goal of the current clause is G.
- [ ] tab(N)            Output N spaces.
- [ ] tell(F)           Make file F the current output stream.
- [ ] telling(F)        The current output stream is named F.
- [ ] told              Close the current output stream.
- [ ] trace             Switch on debugging and start tracing immediately.
- [ ] trimcore          Reduce free stack space to a minimum.
- [ ] true              Succeed.
- [ ] ttyflush          Transmit all outstanding terminal output.
- [ ] ttyget(C)         The next non-blank character input from the terminal is C.
- [ ] ttyget0(C)        The next character input from the terminal is C.
- [ ] ttynl             Output a new line on the terminal.
- [ ] ttyput(C)         The next character output to the terminal is C.
- [ ] ttyskip(C)        Skip over terminal input until after character C.
- [ ] ttytab(N)         Output N spaces to the terminal.
- [ ] unknown(O,N)      Change action on unknown procedures from O to N.
- [ ] var(T)            Term T is a variable.
- [ ] version           Displays introductory and/or system identification messages.
- [ ] version(A)        Adds the atom A to the list of introductory messages.
- [ ] write(T)          Write the term T.
- [ ] writeq(T)         Write the term T, quoting names where necessary.
- [ ] 'LC'              The following Prolog text uses lower case.
- [ ] 'NOLC'            The following Prolog text uses upper case only.
- [ ] !                 Cut any choices taken in the current procedure.
- [ ] \\+ P              Goal P is not provable.
- [ ] X^P               There exists an X such that P is provable.
- [ ] X<Y               As integer values, X is less than Y.
- [ ] X=<Y              As integer values, X is less than or equal to Y.
- [ ] X>Y               As integer values, X is greater than Y.
- [ ] X>=Y              As integer values, X is greater than or equal to Y.
- [x] X=Y               Terms X and Y are equal (i.e. unified).
- [ ] T=..L             The functor and arguments of term T comprise the list L.
- [ ] X==Y              Terms X and Y are strictly identical.
- [ ] X\==Y             Terms X and Y are not strictly identical.
- [ ] X@<Y              Term X precedes term Y.
- [ ] X@=<Y             Term X precedes or is identical to term Y.
- [ ] X@>Y              Term X follows term Y.
- [ ] X@>=Y             Term X follows or is identical to term Y.
- [x] [F|R]             Perform the consult/reconsult(s) on the listed files..

#### Standard operators

```
[ ] :-op( 1200, xfx, [ :-, --> ]).
[x] :-op( 1200,  fx, [ :-, ?- ]).
[ ] :-op( 1150,  fx, [ mode, public ]).
[~] :-op( 1100, xfy, [ ; ]).
[ ] :-op( 1050, xfy, [ -> ]).
[x] :-op( 1000, xfy, [ ',' ]).
[ ] :-op(  900,  fy, [ \+, spy, nospy ]).
[~] :-op(  700, xfx, [ =, is, =.., ==, \==, @<, @>, @=<, @>=,
[ ]                                =:=, =\=, <, >, =<, >= ]).
[ ] :-op(  500, yfx, [ +, -, /\, \/ ]).
[ ] :-op(  500,  fx, [ +, - ]).
[ ] :-op(  400, yfx, [ *, /, <<, >> ]).
[ ] :-op(  300, xfx, [ mod ]).
[ ] :-op(  200, xfy, [ ^ ]).
```
