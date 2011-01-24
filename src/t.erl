-module(t).

-export([go/0, go/1, go/2, go1/2, find_el/2, rm_el/2, repl_el/3, repl_el/4, go_repl/3]).
-export([clean_html_tree/1, find_first_el/2, rm_list_el/2, addref_el/1, rmref_el/1]).
-export([simplify_page/1, fetch_page/1]).
-export([goyaws/1]).

%%% Utils to walk and operate with HtmlTree from mochiweb_html:parse()

% HTML tag find: 
% ищет в HtmlNode все элементы Key и возвращает их списком tuple {Key, Attr, SubTree}
find_el(Key, HtmlNode) -> find_el(Key, HtmlNode, []).
%
find_el(_Key, HtmlNode, Out) when is_binary(HtmlNode) -> Out; % Binary element (leaf)
find_el(_, {comment, _}, Out) -> Out; % Comments in mochiweb_html:parse are 2-element tuples 
% Element Tuples 
find_el(Key, {Key, A, R}, Out) -> [{Key, A, R} | find_el(Key, R, Out)];	% key found, adding to out
find_el(Key, {_E, _A, R}, Out) -> find_el(Key, R, Out); % all other (not found) - just continue with the rest part of tree
% Lists
find_el(_Key, [], Out) -> Out;
find_el(Key, [H|T], Out) -> find_el(Key, H, Out) ++ find_el(Key, T, Out). % walk list if it is not empty

% TODO: неэффективно - хоть нужен всего один элемент, все равно возвращается весь список. Переписать потом
find_first_el(Key, HtmlNode) ->
	L = find_el(Key, HtmlNode),
	case L of
		[{K, A, E}|_] -> 
			{K, A, E};
		[] ->
			{Key, [], []}
	end.

% HTML tag remover: rm_el(Key, HtmlNode) 
% удаляет из HtmlNode поддеревья вида Key и comment, возвращает очищенное HtmlNode
% example: rm_el(<<"script">>, HtmlTree) -> HtmlTreeWithoutScripts
rm_el(_, NodeIn) when is_binary(NodeIn) -> NodeIn;
rm_el(_, {comment, _}) -> []; % dropping comments
%rm_el(_, {comment, T}) -> {comment, T}; % dropping comments
rm_el(Key, {Key, _, _}) -> []; % Key found, dropping subtree
rm_el(Key, {E, A, R}) -> {E, A, rm_el(Key, R)}; % continue to subtree
rm_el(_, []) -> [];
rm_el(Key, [H|T]) -> [rm_el(Key, H) | rm_el(Key, T)]. % processing list recursively

% TODO: неэфективно - дерево пробегается столько раз, какова длина списка ключей.
% переписать rm_list_el чтобы он мог работать со списком Key и выкидывать все за один проход
rm_list_el([], HtmlTree) -> 
	HtmlTree;
rm_list_el([KeyH|KeyT], HtmlTree) ->
	rm_el(KeyH, rm_list_el(KeyT, HtmlTree)).	

% HTML tag replacer:
% example: repl_el(<<"br">>, <<"p">>, HtmlTree) -> HtmlTreeWithBrReplacedToP
repl_el(Key, NewKey, Node) -> repl_el(Key, NewKey, [], Node). % by default no NewAttr
%
repl_el(_K, _NewKey, _NewAttr, NodeIn) when is_binary(NodeIn) -> NodeIn;
repl_el(_K, _NewKey, _NewAttr, {comment, _}) -> []; % dropping comments
repl_el(Key, NewKey, NewAttr, {Key, _A, Rest}) -> {NewKey, NewAttr, repl_el(Key, NewKey, NewAttr, Rest)}; % Key found changing and processing subtree
repl_el(Key, NewKey, NewAttr, {E, A, R}) -> {E, A, repl_el(Key, NewKey, NewAttr, R)}; % continue to subtree
repl_el(_, _, _, []) -> [];
repl_el(Key, NewKey, NewAttr, [H|T]) -> [repl_el(Key, NewKey, NewAttr, H) | repl_el(Key, NewKey, NewAttr, T)]. % processing list recursively

% HTML tag attrib with func:
% example: repl_el(<<"br">>, fun()->...end. HtmlTree) -> HtmlTree
% fun(AttrList) -> ModifiedAttrList
%
repl_el_attr_f(_K, _Func, NodeIn) when is_binary(NodeIn) -> NodeIn;
repl_el_attr_f(_K, _Func, {comment, _}) -> []; % dropping comments
repl_el_attr_f(Key, Func, {Key, Attr, Rest}) -> {Key, Func(Attr), repl_el_attr_f(Key, Func, Rest)}; % Key found changing and processing subtree
repl_el_attr_f(Key, Func, {E, A, R}) -> {E, A, repl_el_attr_f(Key, Func, R)}; % continue to subtree
repl_el_attr_f(_, _, []) -> [];
repl_el_attr_f(Key, Func, [H|T]) -> [repl_el_attr_f(Key, Func, H) | repl_el_attr_f(Key, Func, T)]. % processing list recursively

% addreftree
addref_el(NodeIn) when is_binary(NodeIn) -> NodeIn;
addref_el({comment, _}) -> []; % dropping comments
%addref_el({E, A, Rest}) -> {E, [{ref_id,make_ref()}|A], addref_el(Rest)};
addref_el({E, A, Rest}) -> {E, [{ref_id,make_ref()}|A], addref_el(Rest)};
addref_el([]) -> [];
addref_el([H|T]) -> [addref_el(H) | addref_el(T)]. % processing list recursively

% rmref from tree
% TODO: проверять чтобы удалялись {ref_id, _} не только из головы Attr
rmref_el(NodeIn) when is_binary(NodeIn) -> NodeIn;
rmref_el({comment, _}) -> []; % dropping comments
rmref_el({E, [{ref_id,_}|AT], Rest}) -> {E, AT, rmref_el(Rest)};
rmref_el({E, A, Rest}) -> {E, A, rmref_el(Rest)};
rmref_el([]) -> [];
rmref_el([H|T]) -> [rmref_el(H) | rmref_el(T)]. % processing list recursively


clean_html_tree(Tree) -> % prepDocument in readability.js
	% TODO: add: find max <frame> in frameset and use it as document
	rm_list_el([<<"style">>, <<"link">>, <<"script">>, <<"noscript">>,
		<<"form">>, <<"object">>, <<"iframe">>, <<"img">>], Tree).  % h1, h2?
	% TODO: img жалко удалять лучше сделать относительные url абсолютными в img и a
	% add more clean-up calls if needed


get_title(Tree) ->
	{_, _, TitleStr} = find_first_el(<<"title">>, Tree),
	TitleStr.

% Returns html-page
fetch_page(Url) ->
	inets:start(), % TODO: handle errors & not start if already started
	% TODO: cache page
	case httpc:request(Url) of 
		{ok, {_, _, Body}} ->
			Body;
		{error, ErrVal} ->
			% TODO: проверить output на неправильном url, возможно не тот формат строки/binary
			io_lib:format(<<"<html><head><title>Error</title></head><body>Error: cannot fetch ~s ~p</body></html>">>, [Url, ErrVal])
	end.

simplify_page(Url) ->
	Body = fetch_page(Url), % TODO: делать в отдельном процессе и слать сообщение по завершению
	try mochiweb_html:parse(Body) of % не сработает если в файле нет ни одного тэга html
		TreeOrig -> 
			TitleStr = get_title(TreeOrig),
			{_, _, TreeBody} = find_first_el(<<"body">>, TreeOrig), 
			% ??? Every html has <body> or not? what if html is mailformed? 
			TreeBodyClean = clean_html_tree(TreeBody),
			TreeOut2 = {
				<<"html">>, [], [
					{
						<<"head">>, [], [{<<"title">>, [], TitleStr}]
					},
					{
						<<"body">>, [], 
							[ {<<"h1">>, [], TitleStr} ] ++ % можно и убрать
							TreeBodyClean 
					}
				]
			},
			mochiweb_html:to_html(TreeOut2)
	catch 
		error:{badmatch,_} -> 
			% mochiweb_html:parse("<html><body>"++Body++"</body></html>") 
			% в этом случае дальше можно и не упрощать (и так уже чисто текст)
			% Возвращаем оригинальный вариант
			Body
	end. 

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
	Out = find_el(What, HtmlTree, []),
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
    {atom_to_list(Proto)++"://" ++ Root, Ctx}.

%% mochiweb_html:tokens (???)
%% mochiweb_html:to_html
