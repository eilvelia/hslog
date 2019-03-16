
burger2(f(X)) :- big_mac(X).

burger(X) :- big_mac(X).
burger(X) :- big_kahuna_burger(X).
burger(X) :- whopper(X).

big_mac(a).
big_mac(c).
big_kahuna_burger(b).
whopper(d).
