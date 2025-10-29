% lp24 - ist1114010 - projecto 
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- [puzzles]. % Ficheiro dado. A avaliação terá mais puzzles.
:- [codigoAuxiliar]. % Ficheiro dado. Não alterar.
% Atenção: nao deves copiar nunca os puzzles para o teu ficheiro de código
% Nao remover nem modificar as linhas anteriores. Obrigado.
% Segue-se o código
%%%%%%%%%%%%

%dentroTabuleiro

% Verifica se uma posição (L, C) está dentro dos limites do tabuleiro
dentroTabuleiro(Tabuleiro, (L, C)) :- 
    length(Tabuleiro, N_Linhas),  % Obtém o número de linhas do tabuleiro
    L > 0, L =< N_Linhas,         % Verifica se a linha L está dentro do intervalo válido
    nth1(L, Tabuleiro, Linha),    % Obtém a linha correspondente ao índice L
    length(Linha, N_Colunas),     % Obtém o número de colunas dessa linha
    C > 0, C =< N_Colunas.        % Verifica se a coluna C está dentro do intervalo válido

/*
  Predicado visualiza/1: Exibe uma lista de elementos, linha por linha.
  Certifica-se de que o argumento seja uma lista e delega a exibição para escreveLista/1.
*/
visualiza(Lista):-
    is_list(Lista),            % Verifica se Lista é realmente uma lista
    escreveLista(Lista).       % Chama escreveLista para exibir os elementos da lista

% Caso base: lista vazia, nada a ser exibido
escreveLista([]).

% Exibe o primeiro elemento da lista e continua com o restante
escreveLista([H|T]):-
    writeln(H),                % Escreve o elemento H seguido de uma nova linha
    escreveLista(T).           % Chama recursivamente escreveLista para o restante da lista

/*
  Predicado visualizaLinha/1: Exibe uma lista de elementos numerando cada linha.
*/
visualizaLinha(Lista):-
    is_list(Lista),            % Verifica se Lista é uma lista
    escreveLinhas(Lista,1).    % Chama escreveLinhas com a numeração inicial 1

% Caso base: lista vazia, nada a ser exibido
escreveLinhas([], _).

% Exibe o elemento com sua numeração e continua com o restante
escreveLinhas([H|T], N):-
    write(N), write(': '),     % Exibe o número da linha seguido de ': '
    writeln(H),                % Exibe o elemento H seguido de uma nova linha
    N1 is N + 1,               % Incrementa o contador de linhas
    escreveLinhas(T, N1).      % Chama recursivamente escreveLinhas para o restante da lista

/*
  Predicado insereObjecto/3: Insere um objeto em uma posição (L, C) do tabuleiro.
  Se a posição está dentro do tabuleiro e estiver vazia, insere o objeto.
*/
insereObjecto((L, C), Tabuleiro, Obj):-
    (dentroTabuleiro(Tabuleiro, (L, C)) -> % Verifica se a posição (L, C) está dentro do tabuleiro
        nth1(L, Tabuleiro, Linha),          % Obtém a linha L do tabuleiro
        nth1(C, Linha, Elem),               % Obtém o elemento na coluna C dessa linha
        (var(Elem) -> Elem = Obj ;          % Se o elemento estiver vazio, insere o objeto
            true)                           % Caso contrário, não faz nada
        ; true),
    !.                                      % Impede retrocesso

% Caso padrão para insereObjecto/3 (evita falhas quando a posição é inválida)
insereObjecto(_, _, _).

/*
  Predicado insereVariosObjectos/3: Insere vários objetos em várias coordenadas do tabuleiro.
  Certifica-se de que ambas as listas (coordenadas e objetos) sejam listas.
*/
insereVariosObjectos(ListaCoords, Tabuleiro, ListaObjs):-
    is_list(ListaCoords),                     % Verifica se ListaCoords é uma lista
    is_list(ListaObjs),                       % Verifica se ListaObjs é uma lista
    insereVariosObjectos_aux(ListaCoords, Tabuleiro, ListaObjs). % Chama o predicado auxiliar

% Caso base: listas vazias, não há nada a inserir
insereVariosObjectos_aux([], _, []).

% Insere o objeto H2 na coordenada H1 e continua com o restante
insereVariosObjectos_aux([H1|T1], Tabuleiro, [H2|T2]):-
    insereObjecto(H1, Tabuleiro, H2),            % Insere o objeto H2 na coordenada H1
    insereVariosObjectos_aux(T1, Tabuleiro, T2). % Chama recursivamente para o restante das listas

/*
  Predicado inserePontosVolta/2: Insere o símbolo 'p' nas oito posições adjacentes a uma coordenada (L, C).
*/
inserePontosVolta(Tabuleiro, (L, C)):-
    is_list(Tabuleiro),                             % Verifica se Tabuleiro é uma lista
    Vizinhos = [(-1, -1), (-1, 0), (-1, 1),         % Define as coordenadas relativas dos vizinhos
                (0, -1),          (0, 1),
                (1, -1), (1, 0), (1, 1)],
    inserePontosLista((L, C), Tabuleiro, Vizinhos). % Chama inserePontosLista para inserir os pontos

% Caso base: lista de vizinhos vazia, nada a inserir
inserePontosLista((_, _), _, []).

% Insere 'p' na posição vizinha e continua com o restante
inserePontosLista((L, C), Tabuleiro, [(Move_Linha, Move_Coluna)|T]):-
    DL is L + Move_Linha,                   % Calcula a nova linha
    DC is C + Move_Coluna,                  % Calcula a nova coluna
    insereObjecto((DL, DC), Tabuleiro, p),  % Insere 'p' na nova posição
    inserePontosLista((L, C), Tabuleiro, T).% Chama recursivamente para o restante dos vizinhos

/*
  Predicado inserePontos/2: Insere o símbolo 'p' em várias coordenadas fornecidas.
*/
inserePontos(_, []) :- !.                    % Caso base: lista vazia, nada a inserir

inserePontos(Tabuleiro, [Coord|T]):-
    insereObjecto(Coord, Tabuleiro, p),     % Insere 'p' na coordenada atual
    inserePontos(Tabuleiro, T).             % Chama recursivamente para o restante da lista

/*
  Predicado objectosEmCoordenadas/3: Obtém os objetos nas coordenadas especificadas do tabuleiro.
*/
objectosEmCoordenadas([], _, []).           % Caso base: lista vazia, nenhum objeto a obter

objectosEmCoordenadas([(L, C)|Coords], Tabuleiro, [Obj|RestoObjs]):-
    dentroTabuleiro(Tabuleiro, (L, C)),    % Verifica se a coordenada está dentro do tabuleiro
    nth1(L, Tabuleiro, Linha),             % Obtém a linha correspondente
    nth1(C, Linha, Obj),                   % Obtém o objeto na coluna correspondente
    objectosEmCoordenadas(Coords, Tabuleiro, RestoObjs). % Chama recursivamente para o restante

/*
  Predicado coordObjectos/5: Encontra as coordenadas de todas as ocorrências de um objeto no tabuleiro.
*/
coordObjectos(Objecto, Tabuleiro, ListaCoords, Res, N):-
    coordObjectosAux(Objecto, Tabuleiro, ListaCoords, ResNaoOrdenado), % Obtém coordenadas nao ordenadas
    sort(ResNaoOrdenado, Res),                % Ordena as coordenadas resultantes
    length(Res, N).                           % Calcula o número de ocorrências encontradas

% Caso base: listas vazias, nenhum resultado
coordObjectosAux(_, _, [], []):- !.

% Caso em que o elemento na coordenada corresponde ao objeto procurado
coordObjectosAux(Objecto, Tabuleiro, [(L, C)|ListaCoords], [(L, C)|Res]):-
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, Elem),
    (var(Objecto) -> var(Elem); Objecto == Elem), % Verifica se é uma variável ou corresponde ao objeto
    coordObjectosAux(Objecto, Tabuleiro, ListaCoords, Res), !.

% Caso em que o elemento na coordenada não corresponde ao objeto procurado
coordObjectosAux(Objecto, Tabuleiro, [_|ListaCoords], Res):-
    coordObjectosAux(Objecto, Tabuleiro, ListaCoords, Res).

/*
  Predicado coordenadasVars/2: Encontra todas as coordenadas do tabuleiro que contêm variáveis livres.
*/
coordenadasVars(Tabuleiro, ListaVars):-
    findall((L, C),
            (nth1(L, Tabuleiro, Linha),
             nth1(C, Linha, Elem),
             var(Elem)),                      % Filtra apenas as coordenadas com variáveis livres
            ListaCoordsVars),
    sort(ListaCoordsVars, ListaVars).         % Ordena a lista de coordenadas

/*
  Predicado fechaListaCoordenadas/2: Aplica regras para fechar uma lista de coordenadas no tabuleiro.
*/
fechaListaCoordenadas(Tabuleiro, ListaCoord):-
    coordObjectos(e, Tabuleiro, ListaCoord, _, N_Estrelas),
    coordObjectos(_, Tabuleiro, ListaCoord, ListaLivres, N_Livres),

    % H1: Se houver exatamente duas estrelas, insere pontos ao redor das posições livres
    (2 =:= N_Estrelas ->
        inserePontos(Tabuleiro, ListaLivres);
    
    % H2: Se houver uma estrela e uma posição livre, insere uma estrela e pontos ao redor dela
    (1 =:= N_Estrelas, 1 =:= N_Livres ->
            ListaLivres = [CoordLivre],
            insereObjecto(CoordLivre, Tabuleiro, e),
            inserePontosVolta(Tabuleiro, CoordLivre);

    % H3: Se houver duas posições livres e nenhuma estrela, insere duas estrelas e pontos ao redor
    (0 =:= N_Estrelas, 2 =:= N_Livres ->
        ListaLivres = [Coord1, Coord2],
        insereVariosObjectos(ListaLivres, Tabuleiro, e),
        inserePontosVolta(Tabuleiro, Coord1),
        inserePontosVolta(Tabuleiro, Coord2)
        ; true))).

/*
  Predicado fecha/2: Aplica a regra de fechamento para cada lista de coordenadas no tabuleiro.
*/
fecha(Tabuleiro, ListaListasCoords):-
    is_list(Tabuleiro),                      % Verifica se Tabuleiro é uma lista
    is_list(ListaListasCoords),              % Verifica se ListaListasCoords é uma lista
    fecha_aux(Tabuleiro, ListaListasCoords). % Chama o predicado auxiliar

% Caso base: lista vazia, nada a processar
fecha_aux(_, []):- !.

% Aplica fechaListaCoordenadas para cada lista de coordenadas
fecha_aux(Tabuleiro, [ListaCoords|T]):-
    is_list(ListaCoords),
    fechaListaCoordenadas(Tabuleiro, ListaCoords),
    fecha_aux(Tabuleiro, T),
    !.

/*
  Predicado encontraSequencia/4: Encontra uma subsequência de coordenadas que são variáveis livres.
*/
encontraSequencia(Tabuleiro, N, ListaCoords, Seq):-
    coordenadasVars(Tabuleiro, ListaVars),
    findall(Coords, (member(Coords, ListaCoords), member(Coords, ListaVars)), Seq),
    length(Seq, N),
    coordObjectos(e, Tabuleiro, ListaCoords, _, 0), %Verifica se não existem estrelas
    sublista(Seq, ListaCoords),                      %Verifica se Seq é uma sublista de ListaCoords
    !.

% Predicados auxiliares para encontrar subsequências
sublista(Sub, List) :-
    sublista_aux(Sub, List).

sublista_aux([], _).
sublista_aux([H|T], [H|T2]) :-
    sublista_aux(T, T2).
sublista_aux(Sub, [_|T]) :-
    sublista_aux(Sub, T).

/*
  Predicado aplicaPadraoI/2: Aplica um padrão de inserção de estrelas e pontos ao redor.
*/
aplicaPadraoI(Tabuleiro, [(L1, C1), (_, _), (L3, C3)]):-
    insereObjecto((L1, C1), Tabuleiro, e),          % Insere estrela na primeira posição
    insereObjecto((L3, C3), Tabuleiro, e),          % Insere estrela na terceira posição
    inserePontosVolta(Tabuleiro, (L1, C1)),         % Insere pontos ao redor da primeira posição
    inserePontosVolta(Tabuleiro, (L3, C3)).         % Insere pontos ao redor da terceira posição

/*
  Predicado aplicaPadroes/2: Aplica padrões em todas as listas de coordenadas fornecidas.
*/
% Caso base: se a lista de coordenadas está vazia, termina a recursão
aplicaPadroes(_, []) :- !.

% Regra 1: aplica um padrão do tipo 'I' se uma sequência de tamanho 3 for encontrada
aplicaPadroes(Tabuleiro, [ListaCoords | RestoListas]) :-
    encontraSequencia(Tabuleiro, 3, ListaCoords, Seq),   % Encontra uma sequência de tamanho 3 no tabuleiro
    aplicaPadraoI(Tabuleiro, Seq),                       % Aplica o padrão 'I' na sequência encontrada
    aplicaPadroes(Tabuleiro, RestoListas),               % Chamada recursiva para o restante das listas de coordenadas
    !.

% Regra 2: aplica um padrão do tipo 'T' se uma sequência de tamanho 4 for encontrada
aplicaPadroes(Tabuleiro, [ListaCoords | RestoListas]) :-
    encontraSequencia(Tabuleiro, 4, ListaCoords, Seq),  % Encontra uma sequência de tamanho 4 no tabuleiro
    aplicaPadraoT(Tabuleiro, Seq),                      % Aplica o padrão 'T' na sequência encontrada
    aplicaPadroes(Tabuleiro, RestoListas),              % Chamada recursiva para o restante das listas de coordenadas
    !.

% Caso geral: se nenhuma sequência de interesse for encontrada, continua com o restante das listas
aplicaPadroes(Tabuleiro, [_ | RestoListas]) :-
    aplicaPadroes(Tabuleiro, RestoListas), !.

/*
  Predicado resolve/2: Resolve o problema aplicando padrões e fechando listas de coordenadas até que não haja mais alterações.
*/
resolve(Estruturas, Tabuleiro) :-
    coordTodas(Estruturas, CoordTodas),               % Obtém todas as coordenadas das estruturas
    resolve_aux(CoordTodas, Tabuleiro, []).           % Chama o predicado auxiliar

resolve_aux(Estruturas, Tabuleiro, PreviousVars) :-
    aplicaPadroes(Tabuleiro, Estruturas),             % Aplica padrões ao tabuleiro
    fecha(Tabuleiro, Estruturas),                     % Fecha as listas de coordenadas
    coordenadasVars(Tabuleiro, CurrentVars),          % Obtém as coordenadas com variáveis livres
    (CurrentVars == PreviousVars; resolve_aux(Estruturas, Tabuleiro, CurrentVars)), !. % Continua até não haver alterações possíveis
