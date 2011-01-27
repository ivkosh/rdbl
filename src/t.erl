-module(t).
-author('ivan@koshkin.me').

-export([go/0, go/1, go/2, go1/2, go_repl/3, goyaws/1]).
-export([find_el/2, find_elems/2, rm_el/2, repl_el/3, repl_el/4, rm_brbr/1, count_commas/1]).
-export([clean_html_tree/1, addref_el/1, rmref_el/1, modify_score/3]).
-export([simplify_page/1, fetch_page/1, simplify_page/2]).
-export([full_url/2, url_context/1]).
-export([score_tree/1]).
-export([get_ref/1, get_parent_ref/1, get_score/1]).
-export([score_list/1]).

% хранит readability score для каждого элемента дерева
-record(score, {
		ref,
		readability=0,
		parent
	}).

-define(ROOT_REF, root).

-define(RE_NEGATIVE, "\\b(comment|meta|footer|footnote)\\b").
-define(RE_POSITIVE, "\\b(post|hentry|entry[-]?(content|text|body)?|article[-]?(content|text|body)?)\\b").

% считает количество запятых (,) в HtmlTree
count_commas({comment, _}) -> 0; 
count_commas({_,_,R})      -> count_commas(R);
count_commas({_,_,_,R})    -> count_commas(R);
count_commas([])           -> 0; 
count_commas([H|T])        -> count_commas(H) + count_commas(T);
count_commas(Leaf) when is_binary(Leaf) -> lists:foldl(fun(E, S) -> if E == $, -> S+1; true->S end end, 0, binary_to_list(Leaf)).

%
% Utils to walk and operate with HtmlTree from mochiweb_html:parse()
%

% HTML tag find: ищет в HtmlNode элемент по Key или по Ref и возвращает его
find_el(Ref, HtmlNode) when is_reference(Ref) -> find_el_byref(Ref, HtmlNode);
find_el(?ROOT_REF, HtmlNode)                  -> find_el_byref(?ROOT_REF, HtmlNode);
find_el(Key, HtmlNode) when is_binary(Key)    -> find_el_bykey(Key, HtmlNode, [], first).
% ищет все элементы и возвращает их списком 
find_elems(Key, HtmlNode) when is_binary(Key) -> find_el_bykey(Key, HtmlNode, [], multi).

%
% find_el_by_key(Key, HtmlTree, OutAcc, SearchType=first|multi)
%
% returns: 
% - first element found as a tuple if SearchType != multi 
% - list of all found elements (list of tuples) if SearchType == multi
find_el_bykey(_, HtmlNode, Out, multi) when is_binary(HtmlNode) -> Out; % leaf
find_el_bykey(_, HtmlNode, _, _)       when is_binary(HtmlNode) -> [];  % if first element search leaf is not an option
find_el_bykey(_, {comment, _}, _, _) -> [];                             % comments in mochiweb_html:parse are 2-element tuples, dropping them
find_el_bykey(Key, Elem, Out, SearchType) when is_tuple(Elem) ->        % Element found, adding to Out 
	case Elem of
		% element found (key in current Elem equals to Key)
		{Key, _, R} when SearchType == multi    -> [Elem | find_el_bykey(Key, R, Out, SearchType)];	
		{Key, _, _}                             ->  Elem; % return first element and stop processing	
		% if HtmlTree was modified with score tuple contains 4 elements
		{Key, _S, _, R} ->
			if 
				SearchType == multi -> [Elem | find_el_bykey(Key, R, Out, SearchType)];	
				true                ->  Elem
			end;
		% key in current Elem not equalt to Key, e.g. still not found, continue search in the rest part of tree
		{_, _, R}    -> find_el_bykey(Key, R, Out, SearchType); 
		{_, _, _, R} -> find_el_bykey(Key, R, Out, SearchType);
		_            -> [] % unsupported case
	end;
% Lists
find_el_bykey(_, [], Out, _) -> Out;
find_el_bykey(Key, [H|T], Out, SearchType) -> 
	E1 = find_el_bykey(Key, H, Out, SearchType),
	if 
		SearchType == multi ->
			E1 ++ find_el_bykey(Key, T, Out, SearchType); % walk rest part of listlist if it is not empty
		is_tuple(E1) -> % SearchType is not multi (e.g. first) AND E1 is tuple - we found it!
			E1;
		true -> % otherwise continue search
			find_el_bykey(Key, T, Out, SearchType)
	end.
			

%
% find_el_by_ref(ref, HtmlTree)
%
find_el_byref(_Ref, HtmlNode) when is_binary(HtmlNode) -> []; % leaf is not an option
find_el_byref(_, {comment, _}) -> [];                         % comment is not an option
% если ищем по ref, то тут во-первых всегда first, а во-вторых всегда модифицированное дерево, поэтому не паримся с доп проверками
find_el_byref(Ref, {Key, #score{ref=Ref}=S, A, R}) -> {Key, S, A, R};
find_el_byref(Ref, {_E, _Score, _A, R}) -> find_el_byref(Ref, R); % all other (not found) - just continue with the rest part of tree
% Lists
find_el_byref(_, []) -> [];
find_el_byref(Ref, [H|T]) -> 
	E1 = find_el_byref(Ref, H),
	if 
		is_tuple(E1) ->
			E1; % нашли, дальше не интересно
		true ->
			find_el_byref(Ref, T)
	end. % walk list if it is not empty

% HTML tag remover: rm_el(Key, HtmlNode) 
% удаляет из HtmlNode поддеревья вида Key и comment, возвращает очищенное HtmlNode
% example: rm_el(<<"script">>, HtmlTree) -> HtmlTreeWithoutScripts
%
% Если Key - это список, то значит надо удалить из дерева все тэги из списка:
rm_el([], HtmlTree) -> HtmlTree;
rm_el([KeyH|KeyT], HtmlTree) ->         % TODO: неэфективно - дерево пробегается столько раз, какова длина списка ключей.
	rm_el(KeyH, rm_el(KeyT, HtmlTree));	% переписать rm_el([..],_) чтобы он мог работать со списком Key и выкидывать все за один проход
% Key не список:
rm_el(_, NodeIn) when is_binary(NodeIn) -> NodeIn;
rm_el(_, {comment, _}) -> []; % dropping comments
rm_el(Key, {Key, _, _}) -> []; % Key found, dropping subtree
rm_el(Key, {Key, _, _, _}) -> []; % Key found, dropping subtree
rm_el(Key, {E, A, R}) -> {E, A, rm_el(Key, R)}; % continue to subtree
rm_el(Key, {E, S, A, R}) -> {E, S, A, rm_el(Key, R)}; % continue to subtree
rm_el(_, []) -> [];
rm_el(Key, [H|T]) -> [rm_el(Key, H) | rm_el(Key, T)]. % processing list recursively

% Если подряд идет два или больше <br>, заменяем на <p>
rm_brbr(NodeIn) when is_binary(NodeIn) -> NodeIn;
rm_brbr({comment, _})                  -> []; % dropping comments
rm_brbr({E,    A, R}) -> {E,    A, rm_brbr(R)}; % continue to subtree
rm_brbr({E, S, A, R}) -> {E, S, A, rm_brbr(R)}; 
rm_brbr([]) -> [];
rm_brbr([{<<"br">>,_,_},   {<<"br">>,_,_}   | T]) -> rm_brbr([{<<"p">>,[],[]} | T]); % Replacing <br><br> with <p>,
rm_brbr([{<<"p">>,_,_},    {<<"br">>,_,_}   | T]) -> rm_brbr([{<<"p">>,[],[]} | T]); % if more than two <br> in row, replacing all.
rm_brbr([{<<"br">>,_,_,_}, {<<"br">>,_,_,_} | T]) -> rm_brbr([{<<"p">>,[],[]} | T]); % Do the same for scored elements. 
rm_brbr([{<<"p">>,_,_,_},  {<<"br">>,_,_,_} | T]) -> rm_brbr([{<<"p">>,[],[]} | T]);
rm_brbr([H|T]) -> [rm_brbr(H) | rm_brbr(T)]. % processing list recursively

% HTML tag replacer:
% example: repl_el(<<"br">>, <<"p">>, HtmlTree) -> HtmlTreeWithBrReplacedToP
repl_el(Key, NewKey, Node) -> repl_el(Key, NewKey, [], Node). % by default no NewAttr
%
repl_el(_K, _NewKey, _NewAttr, NodeIn) when is_binary(NodeIn) -> NodeIn;
repl_el(_K, _NewKey, _NewAttr, {comment, _}) -> []; % dropping comments
repl_el(Key, NewKey, NewAttr, {Key, _A, Rest}) -> {NewKey, NewAttr, repl_el(Key, NewKey, NewAttr, Rest)}; % Key found changing and processing subtree
repl_el(Key, NewKey, NewAttr, {Key, S, _A, Rest}) -> {NewKey, S, NewAttr, repl_el(Key, NewKey, NewAttr, Rest)}; % Key found changing and processing subtree
repl_el(Key, NewKey, NewAttr, {E, A, R}) -> {E, A, repl_el(Key, NewKey, NewAttr, R)}; % continue to subtree
repl_el(Key, NewKey, NewAttr, {E, S, A, R}) -> {E, S, A, repl_el(Key, NewKey, NewAttr, R)}; % continue to subtree
repl_el(_, _, _, []) -> [];
repl_el(Key, NewKey, NewAttr, [H|T]) -> [repl_el(Key, NewKey, NewAttr, H) | repl_el(Key, NewKey, NewAttr, T)]. % processing list recursively

% HTML tag attrib with func:
% example: repl_el(<<"br">>, fun()->...end. HtmlTree) -> HtmlTree
% fun(AttrList) -> ModifiedAttrList
%
repl_el_attr_f(_K, _Func, NodeIn) when is_binary(NodeIn) -> NodeIn;
repl_el_attr_f(_K, _Func, {comment, _}) -> []; % dropping comments
repl_el_attr_f(Key, Func, {Key, Attr, Rest}) -> {Key, Func(Attr), repl_el_attr_f(Key, Func, Rest)}; % Key found changing and processing subtree
repl_el_attr_f(Key, Func, {Key, S, Attr, Rest}) -> {Key, S, Func(Attr), repl_el_attr_f(Key, Func, Rest)}; % Key found changing and processing subtree
repl_el_attr_f(Key, Func, {E, A, R}) -> {E, A, repl_el_attr_f(Key, Func, R)}; % continue to subtree
repl_el_attr_f(Key, Func, {E, S, A, R}) -> {E, S, A, repl_el_attr_f(Key, Func, R)}; % continue to subtree
repl_el_attr_f(_, _, []) -> [];
repl_el_attr_f(Key, Func, [H|T]) -> [repl_el_attr_f(Key, Func, H) | repl_el_attr_f(Key, Func, T)]. % processing list recursively

% modify readability score on scored tree
modify_score(_, Leaf, _) when is_binary(Leaf) -> Leaf;
modify_score(_, {comment, _}, _) -> []; 
modify_score(Ref, {Key, #score{ref=Ref, readability=Rdbl, parent=ParentRef}, A, R}, Score) -> {Key, #score{ref=Ref, readability=Rdbl+Score, parent=ParentRef}, A, R};
modify_score(Ref, {E, S, A, R}, Score) -> {E, S, A, modify_score(Ref, R, Score)}; 
modify_score(_, [], _) -> [];
modify_score(Ref, [H|T], Score) -> [modify_score(Ref, H, Score) | modify_score(Ref, T, Score)].

% addreftree
% добавляем в htmltree доп информацию - record score 
%
addref_el(Tree) -> addref_el(Tree, ?ROOT_REF). % the main parent ref is ?ROOT_REF
%
addref_el(R, _) when is_binary(R) -> R;
addref_el({comment, _}, _) -> []; % dropping comments
addref_el({E, A, Rest}, Parent) -> Ref=make_ref(), {E, #score{ref=Ref, parent=Parent}, A, addref_el(Rest, Ref)}; % setting current parent and giving my ref to child as parent
addref_el([], _) -> [];
addref_el([H|T], Parent) -> [addref_el(H, Parent) | addref_el(T, Parent)]. % processing list recursively

% rmref from tree
% TODO: проверять чтобы удалялись {ref_id, _} не только из головы Attr
rmref_el(NodeIn) when is_binary(NodeIn) -> NodeIn;
rmref_el({comment, _}) -> []; % dropping comments
rmref_el({E, _Score, A, Rest}) -> {E, A, rmref_el(Rest)};
rmref_el({E, A, Rest}) -> {E, A, rmref_el(Rest)};
rmref_el([]) -> [];
rmref_el([H|T]) -> [rmref_el(H) | rmref_el(T)]. % processing list recursively

%build_htmltree(HtmlPage) ->

% TODO: сохранять charset из <meta http-equiv="content-type" content="text/html; charset=utf-8" />
% и/или из httpc:request
clean_html_tree(Tree) -> % prepDocument in readability.js
	% TODO: add: find max <frame> in frameset and use it as document
	rm_brbr( % заменяем <br><br> на <p>
		rm_el([<<"style">>, <<"link">>, <<"script">>, <<"noscript">>,
			<<"form">>, <<"object">>, <<"iframe">>], Tree)
	).  % h1, h2?
	% add more clean-up calls if needed


get_title(Tree) ->
	{_, _, TitleStr} = find_el(<<"title">>, Tree),
	TitleStr.

% Returns html-page
fetch_page(Url) ->
	inets:start(), % TODO: handle errors & not start if already started
	ssl:start(),
	% TODO: cache page - save to ets by url
	case httpc:request(Url) of 
		{ok, {_, _, Body}} ->
			Body;
		{error, ErrVal} ->
			io_lib:format(<<"<html><head><title>Error</title></head><body>Cannot fetch ~s - ~p</body></html>">>, [Url, ErrVal])
	end.

% U is a binary!
to_abs_url({<<"src">>, U}, Ctx)  -> {<<"src">>,  list_to_binary(full_url(Ctx, binary_to_list(U)))};
to_abs_url({<<"href">>, U}, Ctx) -> {<<"href">>, list_to_binary(full_url(Ctx, binary_to_list(U)))};
to_abs_url(A, _) -> A.

simplify_page(Url) ->
	Body = fetch_page(Url), % FIXME: делать в отдельном процессе и слать сообщение по завершению
	Ctx = url_context(Url),
	try mochiweb_html:parse(Body) of % не сработает если в файле нет ни одного тэга html
		TreeOrig -> 
			TitleStr = get_title(TreeOrig),
			{_, _, TreeBody} = find_el(<<"body">>, TreeOrig), 
			ScoredTree = score_tree(
				addref_el(
					% превращаем относительные url в абсолютные
					repl_el_attr_f(<<"a">>,   fun(L) -> [ to_abs_url(El, Ctx) || El <- L ] end, 
					repl_el_attr_f(<<"img">>, fun(L) -> [ to_abs_url(El, Ctx) || El <- L ] end, 
					clean_html_tree(TreeBody)))
				)
			),
			OptimumRef = get_max_score_ref(ScoredTree),
			ContentBody = rmref_el(find_el(OptimumRef, ScoredTree)),
			TreeOut = {
				<<"html">>, [], [
					{
						<<"head">>, [], [{<<"title">>, [], TitleStr}]
					},
					{
						<<"body">>, [], 
							[ {<<"h1">>, [], TitleStr} ] ++ % можно и убрать
						[ContentBody]
					}
				]
			},
			mochiweb_html:to_html(TreeOut)
	catch 
		error:{badmatch,_} -> 
			% mochiweb_html:parse("<html><body>"++Body++"</body></html>") 
			% в этом случае дальше можно и не упрощать (и так уже чисто текст)
			% Возвращаем оригинальный вариант
			Body
	end. 

simplify_page(Url, FileName) ->
	Page = simplify_page(Url),
	{ok, F} = file:open(FileName, [binary, write]),
	file:write(F, Page),
	file:close(F).

%TODO:
%simplify_page(InFile, OutFile)

get_parent_ref(Node) ->
	{_, #score{parent=ParentRef}, _, _} = Node,
	ParentRef.

get_ref(Node) ->
	{_, #score{ref=Ref}, _, _} = Node,
	Ref.

get_score(Node) ->
	{_, #score{readability=Score}, _, _} = Node,
	Score.

score_by_class_or_id(Node) ->
	{_, _, Attrs, _} = Node,
	AttrVals = [ V || {K, V} <- Attrs, (K == <<"id">>) or (K == <<"class">>) ],
	if 
		AttrVals == [] -> 0; % no id or class (list is empty)
		true -> % e.g. we have id or class or both
			case lists:foldl(fun(El, Acc) -> Acc or (re:run(El, ?RE_NEGATIVE, [{capture, none}]) == match) end, false, AttrVals) of
				true -> -50;
				false ->
					case lists:foldl(fun(El, Acc) -> Acc or (re:run(El, ?RE_POSITIVE, [{capture, none}]) == match) end, false, AttrVals) of
						true -> 25;
						false -> 0
					end
			end
	end.

score_one_p(PNode, TreeFull) ->
	CommaScore = count_commas(PNode),
	ParentRef = get_parent_ref(PNode),
	% TODO: что если ParentRef == ?ROOT_REF?
	Parent = find_el(ParentRef, TreeFull),
	% TODO: что если нет Parent'a?
	CurParentScore = get_score(Parent),
	if 
		CurParentScore == 0 -> % e.g. parent was not scored yet
			ParentScoreDiff = score_by_class_or_id(Parent);
		true -> % if parent was already scored, no need to additionaly rescore it
			ParentScoreDiff = 0
	end,
	modify_score(ParentRef, TreeFull, ParentScoreDiff + CommaScore + 1). % +1 за сам <p>

score_tree(Tree) ->
	PList = find_elems(<<"p">>, Tree),
	lists:foldl(fun score_one_p/2, Tree, PList).


score_list(Tree) -> score_list(Tree, []).
score_list(HtmlNode, _Out) when is_binary(HtmlNode) -> []; %Out; % leaf ?TODO: or []?
score_list({comment, _}, _) -> [];                             % comments in mochiweb_html:parse are 2-element tuples, dropping them
score_list({_, #score{readability=Rdbl, ref=Ref}, _, R}, Out) -> [{Ref, Rdbl} | score_list(R, Out)];	
score_list({_, _, _, R}, Out) -> score_list(R, Out);	
score_list([], _Out) -> []; %Out; % or just []?
score_list([H|T], Out) -> score_list(H, Out) ++ score_list(T, Out). 

get_max_score_ref(Tree) ->
	{Ref, _MaxScore} = lists:foldl(
		fun({Ref, Score}, {Ref0, Score0}) -> 
				if 
					is_reference(Ref) -> 
						if 
							Score0<Score -> {Ref, Score}; 
							true->{Ref0, Score0} 
						end; 
					true->{Ref, Score} 
				end 
		end, {0, 0}, score_list(Tree)),
	Ref.

%%%% tests %%%%
go() -> go(<<"tr">>, "http://www.sainf.ru").

go(What) -> go(What, "http://www.sainf.ru").

go_repl(What, NewWhat, Where) ->
	inets:start(),
	{ok, {_, _, Body}} = httpc:request(Where),
	HtmlTree = mochiweb_html:parse(Body),
	Out = repl_el(What, NewWhat, HtmlTree),
	Out.

goyaws(Where) ->
	inets:start(),
	{ok, {_, _, Body}} = httpc:request(Where),
	%HtmlTree = yaws_html:h2e(binary_to_list(Body)),
	HtmlTree = yaws_html:h2e(Body),
	HtmlTree.

go(What, Where) ->
	inets:start(),
	{ok, {_, _, Body}} = httpc:request(Where),
	HtmlTree = mochiweb_html:parse(Body),
	Out = rm_el(What, HtmlTree),
	Out.

go1(What, Where) ->
	inets:start(),
	{ok, {_, _, Body}} = httpc:request(Where),
	HtmlTree = mochiweb_html:parse(Body),
	Out = find_el(What, HtmlTree),
	{Out, length(Out)}.

%% abs url inside the same server ej: /img/image.png    
full_url({Root, _Context}, ComponentUrl=[$/|_]) -> Root ++ ComponentUrl;

%% full url ej: http://other.com/img.png
full_url({_Root, _Context}, ComponentUrl="http://"++_)  -> ComponentUrl;
full_url({_Root, _Context}, ComponentUrl="https://"++_) -> ComponentUrl;
full_url({_Root, _Context}, ComponentUrl="ftp://"++_)   -> ComponentUrl;

% everything else is considerer a relative path.. obviously its wrong (../img) 
full_url({Root, Context}, ComponentUrl) ->
    Root ++ Context ++ "/" ++ ComponentUrl.

% returns the  domain, and current context path. 
% url_context("http://www.some.domain.com/content/index.html)
%      -> {"http://www.some.domain.com", "/content"}
url_context(URL) ->
    {Proto, _, Root, _Port, Path, _Query} = http_uri:parse(URL), 
    Ctx = string:sub_string(Path, 1, string:rstr(Path,"/")),
    {atom_to_list(Proto) ++ "://" ++ Root, Ctx}.

%% mochiweb_html:tokens (???)
%% mochiweb_html:to_html
%
% ?TODO: поменять местами в scored tuple S и A (более естественно) и убрать четвертый параметр Ref 
% отличить scored от unscored будет легко (хотя в этом случае оба вида будут 3х элементными tuple): 
% - если третий параметр list, то значит unscored, если tuple (record) то scored
