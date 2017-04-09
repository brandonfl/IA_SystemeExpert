%:- unknown(trace,fail).
:- dynamic(negatif/1).

%----------------------
%BASE DES FAITS
%----------------------
noeud(public_jeune).

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
crime :- gang.
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
simpson :- animation,public_jeune.
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

trouveUneSerie :-
    si((serie(Nom), effacer([Nom], Trace,_)),
        ecrire_succes([Nom], Trace),
        write('Aucun diagnostique, n’a pu être établi')).

expertiser(L) :- si(effacer(L, Trace,[]), ecrire_succes(L, Trace), ecrire_echec(L)).

effacer([]).
effacer([But|AutresButs]) :-
    rule(But,SousButs),
    effacer(SousButs),
    effacer(AutresButs).

effacer([],[]).
effacer([But|_], _) :- negatif(But), !, fail.
effacer([But|AutresButs], [[But|TSousButs]|TAutresButs]) :-
    rule(But,SousButs),
    effacer(SousButs,TSousButs), !,
    effacer(AutresButs,TAutresButs).
effacer([But|AutresButs], [[But]| TAutresButs]) :- write('le fait '), write(But),
    write(' est-il etabli ? (o./n./p.):'), nl, read(Rep),
    si(Rep='o',
        (asserta(But), effacer(AutresButs,TAutresButs)),
        si(Rep='n',
            (asserta(negatif(But)),fail),
                fail)).

effacer([],[],_).
effacer([But|_], _, _) :- negatif(But), !, fail.
effacer([But|AutresButs], [[But|TSousButs]|TAutresButs], Pourquoi) :-
    rule(But,SousButs),
    effacer(SousButs, TSousButs,[But|Pourquoi]), !,
    effacer(AutresButs, TAutresButs, Pourquoi).
effacer([But|AutresButs], [[But]|TAutresButs], Pourquoi) :-
    write('le fait '), write(But), write(' est-il etabli ? (o./n./p.):'), nl,
    afficher_pourquoi(Pourquoi), nl, read(Rep),
    si(Rep='o',
        (asserta(But), effacer(AutresButs,TAutresButs,Pourquoi)),
        si(Rep='n',
            (asserta(negatif(But)),fail),
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
