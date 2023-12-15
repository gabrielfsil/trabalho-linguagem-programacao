% Base de Conhecimento
doenca(gripe, [febre, dor_no_corpo, tosse, catarro, dor_de_cabeca]).
doenca(dengue, [febre, dor_no_corpo, dor_de_cabeca, indisposicao]).

doenca(resfriado, [nariz_escorrendo, espirros, garganta_iritada, febre]).
doenca(alergia, [coceira, olhos_aguados, espirros, garganta_iritada, nariz_escorrendo]).

doenca(covid, [febre, dor_no_corpo, dor_de_cabeca, falta_de_ar]).
doenca(infarto, [febre, dor_no_corpo, dor_no_peito, falta_de_ar]).

doenca(intoxicacao, [febre, nausea, dor_abdominal, pele_irritada]).
doenca(insolacao, [febre, nausea, dor_de_cabeca, pele_irritada]).

doenca(hipertensao, [tonteira, vertigem, dor_de_cabeca, indisposicao]).
doenca(ressaca, [nausea, vertigem, dor_de_cabeca, indisposicao]).

doenca(diabetes, [dor_no_corpo, gordura_abdominal, palpebra_amarela, feridas_abertas ]).
doenca(febre_amarela, [dor_no_corpo, gordura_abdominal, palpebra_amarela, manchas_na_pele ]).

doenca(aritmia, [dor_no_peito, gordura_abdominal, fadiga, vertigem ]).
doenca(cardiomegalia, [dor_no_peito, gordura_abdominal, fadiga, indisposicao ]).

doenca(colera, [dor_no_corpo, dor_de_cabeca, pele_irritada, manchas_na_pele ]).
doenca(catapora, [fadiga, indisposicao, pele_irritada, manchas_na_pele ]).

doenca(gota, [dor_no_pe, inchaco, pele_irritada, febre ]).
doenca(bronquite, [dor_no_peito, falta_de_ar, inchaco, fadiga ]).
doenca(asma, [dor_no_peito, falta_de_ar, inflamacao_nasal, inchaco ]).
doenca(desvio_de_septo, [dificuldade_de_respirar, inflamacao_nasal, inchaco ]).

% Predicados para manipulação de listas
pertence(X, [X|_]).
pertence(X, [_|T]) :- pertence(X, T).

% Predicados para interação com o usuário
perguntar(Sintoma) :-
    write('Você está sentindo '), write(Sintoma), write('? (s/n): '),
    read(Resposta),
    processar_resposta(Resposta, Sintoma).

processar_resposta(s, Sintoma) :- assertz(sintoma(Sintoma)).
processar_resposta(n, _).
processar_resposta(_, Sintoma) :- write('Resposta inválida. Responda com "s" para sim ou "n" para não.'), nl, perguntar(Sintoma).

% Predicado para diagnóstico
diagnostico(Doenca) :-
    doenca(Doenca, Sintomas),
    not(pertence(_, Sintomas)),
    write('Baseado nos sintomas informados, parece que você tem '), write(Doenca), nl.

% Predicado principal
iniciar_diagnostico :-
    retractall(sintoma(_)),
    write('Bem-vindo ao sistema de diagnóstico médico. Vamos começar.'), nl,
    sintomas(SINTOMAS),
    percorrer_sintomas(SINTOMAS).

% Predicado para percorrer os sintomas
percorrer_sintomas([]) :- 
    findall(Sintoma, sintoma(Sintoma), SintomasColetados),
    realizar_diagnostico(SintomasColetados).

percorrer_sintomas([Sintoma|Resto]) :-
    perguntar(Sintoma),
    percorrer_sintomas(Resto).

% Predicado para realizar o diagnóstico
realizar_diagnostico(SintomasColetados) :-
    findall(Sintoma, sintoma(Sintoma), SintomasColetados),
    diagnostico_final(SintomasColetados).

% Predicado para apresentar o diagnóstico final
diagnostico_final(SintomasColetados) :-
    doencas(DOENCAS),
    (percorrer_doencas(DOENCAS, SintomasColetados) -> true ; 
     write('Não consigo diagnosticar a doença. Consulte um médico.'), nl).

percorrer_doencas([], _) :- false.
percorrer_doencas([Doenca|_], SintomasColetados) :-
    diagnostico_individual(Doenca, SintomasColetados),
    !. % Corta a busca por mais diagnósticos se um foi encontrado

percorrer_doencas([_|Resto], SintomasColetados) :-
    percorrer_doencas(Resto, SintomasColetados).


diagnostico_individual(Doenca, SintomasColetados) :-
    doenca(Doenca, Sintomas),
    intersection(Sintomas, SintomasColetados, SintomasEmComum),
    length(SintomasEmComum, Compatibilidade),
    Compatibilidade >= 3,
    write('Baseado nos sintomas informados, parece que você tem '), write(Doenca), nl,
    write('Sintomas compatíveis: '), write(SintomasEmComum), nl, nl, nl.

% Lista de sintomas disponíveis
sintomas([febre, dor_no_corpo, tosse, catarro, dor_de_cabeca, indisposicao, nariz_escorrendo, espirros, garganta_iritada, coceira, olhos_aguados, falta_de_ar, dor_no_peito, nausea, dor_abdominal, pele_irritada, tonteira, vertigem, gordura_abdominal, palpebra_amarela, feridas_abertas, manchas_na_pele, fadiga, inflamacao_nasal, dor_no_pe, inchaco, dificuldade_de_respirar]).

% Lista de doenças disponíveis
doencas([gripe, resfriado, alergia, dengue, covid, infarto, intoxicacao, insolacao, hipertensao, ressaca, diabetes, febre_amarela, aritmia, cardiomegalia, colera, catapora, gota, bronquite, asma, desvio_de_septo]).

% Inicie o diagnóstico
:- iniciar_diagnostico.
