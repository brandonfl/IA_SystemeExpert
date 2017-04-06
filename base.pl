:- unknown(trace,fail).
:- dynamic(negatif/1).
%----------------------
%BASE DES FAITS ETABLIS
%----------------------
a.
c.
d.
g.
h.
k.

%----------------------
%BASE DES REGLES
%----------------------
Simpson :- animation, public_jeune.
Rich_et_Marty :- animation,  public_jeune, personnage_jeune.
Archer :- animation, negatif(public_jeune), negatif(personnage_jeune).
Dardevil :- negatif(animation), adaptation, super-heros.
Game_of_Thrones :- negatif(animation), adaptation, negatif(super-heros), fantastique.
Sherlock :- negatif(animation), adaptation, negatif(super-heros), negatif(fantastique).
The_Office :- negatif(animation), negatif(adaptation), americain, humoristique.
Sons_of_Anarchy :- negatif(animation), negatif(adaptation), americain, negatif(humoristique), personnage_criminels, bikers. 
Breaking_Bad :- negatif(animation), negatif(adaptation), americain, negatif(humoristique), personnage_criminels, negatif(bikers).
House :- negatif(animation), negatif(adaptation), americain, negatif(humoristique), negatif(personnage_criminels), medical.
Stranger_Things :- negatif(animation), negatif(adaptation), americain, negatif(humoristique), negatif(personnage_criminels), negatif(medical), science-fiction.


% MOTEUR D’INFERENCES
%-----------------------
si(C,A,_):- C,!,A.
si(_,_,S) :- S.

expertiser(L) :- si(effacer(L,X,[]),ecrire_succes(L,X),ecrire_echec(L)).

%ecrire_succes(L) :- print_conjonction(L,succes).
ecrire_echec(L)  :- print_conjonction(L,echec).

print_conjonction([T],Etat) :- ! , write('le fait ')
     , write(T)
     , si(Etat=succes, write(' est etabli'), write(' n''est pas etabli')), nl.
print_conjonction(L,Etat) :- write('la conjonction de faits ')
     , print_conjonction(L)
     , si(Etat=succes, write(' est etablie'), write(' n''est pas etablie')), nl.

print_conjonction([]).
print_conjonction([T])  :- !,write(T).
print_conjonction([T|Q]):- write(T),write(' et '),print_conjonction(Q).


%-----------4.02
rule(T,CorpsListe) :-
     clause(T,CorpsTerme),
     termeToListe(CorpsTerme,CorpsListe).

termeToListe(true,[]) :- !.

termeToListe(Terme,[Terme]) :- atom(Terme),!.

termeToListe(Terme,[T|Q]):-
    arg(1,Terme,T),
    arg(2,Terme,TT),
    termeToListe(TT,Q).

effacer([],[],_).
effacer([But|_],_,_) :- negative(But), !, fail.
effacer([But|AutresButs], [[But|TraceSousButs]|TraceAutreButs],Pq) :-
  rule(But,ListeSousButs),
  effacer(ListeSousButs,TraceSousButs, [But|Pq]), !,
  effacer(AutresButs,TraceAutreButs,Pq).
effacer([But|AutresButs], [[But]|TraceAutreButs],Pq) :- afficheP(Pq), nl,
     write('le fait '), write(But), write(' est-il etabli ? (o./n./p.):'), nl, read(R),
     si(R='o', (asserta(But),effacer(AutresButs,TraceAutreButs,Pq)) ,
                si(R='p', fail, (asserta(negative(But)),fail))).

afficheP([]).
afficheP([T|Q]) :-
  write('Je pose cette question pour établir '), write(T), affichePBis(Q).

affichePBis([]).
affichePBis([T|Q]) :-
  write(' puis '),
  write(T),
  affichePBis(Q).

ecrire_succes(L, Trace) :-
   print_conjonction(L,succes),
   afficher_trace(Trace).

afficher_trace(T) :-
  write('COMMENT : '), nl,
  % afficher la trace à partir de la première colonne
  afficher_trace(T,0).

afficher_trace([[]],_) :- !.
afficher_trace([[C]],Indent) :- atom(C),!,
  tab(Indent), write(C),write(' est un fait etabli'),nl.

afficher_trace([[C|SousButs]],Indent) :- atom(C), !,
  % C est un atome ; donc le premier parametre contient un unique arbre
  % et SousButs n'est pas vide ; ; donc C n'est pas un noeud terminal
  tab(Indent), write(C), write(' -->') , nl,
  NewIndent is Indent+5,
  afficher_trace(SousButs, NewIndent).

afficher_trace([T|Q], Indent) :-
  % T est une liste de listes donc le parametre represente une liste d'arbres
  % Q est une liste d'arbres
  afficher_trace([T], Indent),
  afficher_trace(Q, Indent).