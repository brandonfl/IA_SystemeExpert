:- unknown(trace,fail).
:- dynamic(negatif/1).

%----------------------
%BASE DES FAITS
%----------------------
serie(simpson).
serie(rich_et_morty).
serie(archer).
serie(daredevil).
serie(game_of_thrones).
serie(sherlock).
serie(the_office).
serie(sons_of_anarchy).
serie(breaking_bad).
serie(house).
serie(stranger_things).
serie(true_detective).
serie(doctor_who).
serie(black_mirror).
serie(broadchurch).
serie(the_thick_of_it).

%----------------------
%BASE DES INFERENCES
%----------------------

% ---- 3

theme_contreverse :- sexe.
theme_contreverse :- drogue.
theme_contreverse :- alcool.
theme_contreverse :- politique.
theme_contreverse :- racisme.

% ---- 2

violent :- armes.
violent :- sang.
violent :- combat.
violent :- cruautee.
politiquement-correct :- langage_correct, negatif(theme_contreverse).
medieval :- negatif(arme-a-feux),chevaliers,chateaux.
crime :- drogues.
crime :-  traffic.
crime :-  meurtre.
crime :-  fraude.
cime :- gang.
futuriste :- se_deroule_dans_le_futur.
futuriste :- robots.

% ---- 1
public_jeune :- non-violent, politiquement-correct.
super_heros :- costume, violence.
fantastique :- medieval, violence.
policier :- enquete, crime.
dystopie :- futuriste, pessimiste.
science_fiction :- voyage_dans_le_temps, espace, futuriste.
personnage_jeune :- enfants.
personnage_jeune :- collegiens.
personnage_jeune :- ecole.
medical :- docteurs, hopital,psychiatrie.
medical :- docters,hopital.
bikers :- gang,moto.



%----------------------
%BASE DE DONNEE
%----------------------
simpson :- animation, public_jeune.
rich_et_morty :- animation,  public_jeune, personnage_jeune.
archer :- animation, negatif(public_jeune), negatif(personnage_jeune).
daredevil :- negatif(animation), adaptation, super_heros.
game_of_thrones :- negatif(animation), adaptation, negatif(super_heros), fantastique.
sherlock :- negatif(animation), adaptation, negatif(super_heros), negatif(fantastique).
the_office :- negatif(animation), negatif(adaptation), americain, humoristique.
sons_of_anarchy :- negatif(animation), negatif(adaptation), americain, negatif(humoristique), personnage_criminels, bikers.
breaking_bad :- negatif(animation), negatif(adaptation), americain, negatif(humoristique), personnage_criminels, negatif(bikers).
house :- negatif(animation), negatif(adaptation), americain, negatif(humoristique), negatif(personnage_criminels), medical.
stranger_things :- negatif(animation), negatif(adaptation), americain, negatif(humoristique), negatif(personnage_criminels), negatif(medical), science_fiction.
true_detective :- negatif(animation), negatif(adaptation), americain, negatif(humoristique),negatif(personnage_criminel),negatif(medical),negatif(science_fiction).
doctor_who :- negatif(animation), negatif(adaptation), negatif(americain), science_fiction, negatif(dystopie).
black_mirror :- negatif(animation), negatif(adaptation), negatif(americain), science_fiction, dystopie.
broadchurch :- negatif(animation), negatif(adaptation), negatif(americain), negatif(science_fiction),policier.
the_thick_of_it :- negatif(animation), negatif(adaptation), negatif(americain), negatif(science_fiction), negatif(policier).


% MOTEUR D’INFERENCES
%-----------------------
si(C,A,_):- C,!,A.
si(_,_,S) :- S.

rdv-chez-le-medecin :-
  si((serie(Nom), effacer([Nom], Trace)),
      afficher_succes([Nom], Trace),
      write(’Aucune serie n’a pu être établi’)
).

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
egatif(animation), negatif(adaptation), americain, negatif(humoristique), negatif(personnage_criminels), negatif(medical), science_fiction.
true_detective :- negatif(animation), negatif(adaptation), americain, negatif(humoristique),negatif(personnage_criminel),negatif(medical),negatif(science_fiction).
doctor_who :- negatif(animation), negatif(adaptation), negatif(americain), science_fiction, negatif(dystopie).
black_mirror :- negatif(animation), negatif(adaptation), negatif(americain), science_fiction, dystopie.
broadchurch :- negatif(animation), negatif(adaptation), negatif(americain), negatif(science_fiction),policier.
the_thick_of_it :- negatif(animation), negatif(adaptation), negatif(americain), negatif(science_fiction), negatif(policier).


% MOTEUR D’INFERENCES
%-----------------------
si(C,A,_):- C,!,A.
si(_,_,S) :- S.

rdv-chez-le-medecin :-
  si((serie(Nom), effacer([Nom], Trace)),
      afficher_succes([Nom], Trace),
      write(’Aucune serie n’a pu être établi’)
).

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
