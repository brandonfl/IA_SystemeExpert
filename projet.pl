:- dynamic(non/1).

%----------------------
%BASE DES FAITS
%----------------------
serie(simpsons).
serie(rick_et_morty).
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
non_violent :- violent,!,fail.
non_themes_adultes :- themes_adultes,!,fail.

politiquement_correct :- langage_correct, non_themes_adultes.
public_jeune :- politiquement_correct, non_violent.
policier :- enquete, crime, action.
science_fiction :- futuriste, aventure.
science_fiction :- voyage_dans_le_temps.
science_fiction :- voyage_dans_l_espace.
science_fiction :- monstres.
science_fiction :- aliens.
themes_adultes :- sexe.
themes_adultes :- drogues.
themes_adultes :- alcool.
themes_adultes :- politique.
crime :- trafficants, drogues.
crime :- assassins.
crime :- gang.
crime :- violent.
crime :- coups_de_feu.
medieval :- chevaliers, epees.
medieval :- chevaliers, aventure.
action :- violent.
action :- coups_de_feu.
action :- combat.
bikers :- gang, moto.
medical :- hopital, docteur.
dystopique :- futuriste, ton_sombre.

%----------------------
%BASE DE DONNEE
%----------------------
simpsons :- animation, public_jeune.
rick_and_morty :- animation, personnage_jeune.
rick_and_morty :- animation, science_fiction.
archer :- animation.
daredevil :- adaptation, super_heros, drame, ton_sombre.
game_of_thrones :- adaptation, medieval, drame.
sherlock :- adaptation, policier, langage_correct, humour, drame.
the_office :- americain, humour.
sons_of_anarchy :- americain, crime, action, trafficants, bikers.
breaking_bad :- americain, crime, trafficants.
house :- americain, medical.
stranger_things :- americain,science_fiction, politiquement_correct, action.
true_detective :- americain, crime, themes_adultes, policier, ton_sombre.
doctor_who :- science_fiction, docteur, public_jeune, aventure.
black_mirror :-  drame, dystopique.
broadchurch :- policier, drame, ton_sombre, themes_adultes.
the_thick_of_it :-  humour.


% MOTEUR D’INFERENCES
%-----------------------
si(C,A,_):- C,!,A.
si(_,_,S) :- S.

trouveUneSerie :-
    si((serie(Nom),expertiser2([Nom])),
        ecrire_succes([Nom], _),
        write('Aucun diagnostique, n’a pu être établi')).

expertiser(L) :- si(effacer(L, Trace,[]), ecrire_succes(L, Trace), ecrire_echec(L)).
expertiser2(L) :- effacer(L, Trace,[]) , ! ,ecrire_succes(L, Trace).

effacer([],[],_).
effacer([But|_], _, _) :- non(But),!, fail.
effacer([But|AutresButs], [[But|TSousButs]|TAutresButs], Pourquoi) :-
    rule(But,SousButs),
    effacer(SousButs, TSousButs,[But|Pourquoi]), !,
    effacer(AutresButs, TAutresButs, Pourquoi).
effacer([But|AutresButs], [[But]|TAutresButs], Pourquoi) :-
    afficher_pourquoi(Pourquoi), nl,
    write('le fait '), write(But), write(' est-il etabli ? (o./n./p.):'), nl,
    read(Rep),
    si(Rep='o',
        (asserta(But), effacer(AutresButs,TAutresButs,Pourquoi)),
        si(Rep='n',
            (asserta(non(But)),fail),
                fail)).

ecrire_succes(L, Trace) :-
    print_conjonction(L,succes),
    afficher_trace(Trace).
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

%--
rule(T,CorpsListe) :-
    clause(T,CorpsTerme),
    termeToListe(CorpsTerme,CorpsListe).

termeToListe(true,[]) :- !.
termeToListe(Terme,[Terme]) :- atom(Terme),!.
termeToListe(Terme,[T|Q]):-
arg(1,Terme,T),
arg(2,Terme,TT),
termeToListe(TT,Q).

afficher_trace(T) :-
write('COMMENT : '), nl, afficher_trace(T,0).
%---------------------
afficher_trace([],_) :- !.
afficher_trace([[C]],Indent) :- atom(C),!,
tab(Indent), write(C),write(' est un fait etabli'),nl.
afficher_trace([[C|Q]],Indent) :- atom(C),!, tab(Indent), write(C), write(' ?'), nl, NewIndent is Indent + 5, afficher_trace(Q, NewIndent).
afficher_trace([X|Y], Indent) :- afficher_trace([X], Indent), afficher_trace(Y, Indent).

afficher_pourquoi([]). afficher_pourquoi([T|Q]) :-
write('Je pose cette question pour établir '), nl, write(T), nl,
afficher_pourquoiBis(Q).
%-----------
afficher_pourquoiBis([]).
afficher_pourquoiBis([T|Q]) :-
write('puis '),
write(T), nl,
afficher_pourquoiBis(Q).
