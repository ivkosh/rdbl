%%
%% Erlang readability library inspired by readability.js
%%
%% @author Ivan Koshkin <ivan@koshkin.me>
%% @copyright 2011 by Ivan Koshkin
%%
-module(rdbl).
-author('Ivan Koshkin <ivan@koshkin.me>').
-vsn('0.3').

-export([simplify_url/1, simplify_url/2, simplify_file/2, simplify_page/1, simplify_page/3]).

-define(DEBUG, 1).

-ifdef(DEBUG).
-export([find_node/2, find_all_nodes/2, remove_node/2, replace_node/2, replace_node/3]).
-export([fetch_page/1]).
-export([brbr_to_p/1, count_commas/1, clean_html_tree/1]).
-export([init_scores/1, clean_scores/1, modify_score/3, get_score/1, get_ref/1, get_parent_ref/1, score_tree/1, score_list/1]).
-export([full_url/2, url_context/1]).
-endif.

%% @type score():
%% Keeps readability score and additional references for every HTML tree element
-record(score, {
		ref,
		readability=0,
		parent
	}).

-define(RE_NEGATIVE, "\\b(comment|meta|footer|footnote)\\b").
-define(RE_POSITIVE, "\\b(post|hentry|entry[-]?(content|text|body)?|article[-]?(content|text|body)?)\\b").
-define(USER_AGENT, "Safari/5.0.3").

% TODO: habrahabr.ru comments are entry-content-only and entry-content, fix this
% maybe add dependency score algorithm from page url?
% or maybe commas are not counted???

%%
%% @type scored_html_node() = {string(), score(), [html_attr()], [html_node() | string()]}
%%
%% See definitions of html_node() and html_attr() in mochiweb_html.erl

%% ===================================================================
%%
%% Main API functions
%%
%%

%% @spec simplify_url(string()) -> string()
%% @doc fetches url, simplifies its content and returns simplified page as a string()
%% example: simplify_url("http://news.yandex.ru/") -> SimplifiedPageText
simplify_url(Url0) ->
	Url = add_proto_if_none(Url0),
	{ContentType, Body} = fetch_page(Url), % TODO: делать в отдельном процессе и слать сообщение по завершению
	Ctx = url_context(Url),
	simplify_page(Body, Ctx, ContentType).

%% @spec simplify_url(string(), string()) -> ok
%% @doc fetches url, simplifies its content and saves to file
%% example: simplify_url("http://news.yandex.ru/", "out.html")
simplify_url(Url, FileName) ->
	Page = simplify_url(Url),
	{ok, F} = file:open(FileName, [binary, write]), % TODO: check if file open
	file:write(F, Page),
	file:close(F),
	ok.

%% @spec simplify_file(string(), string()) -> ok
%% @doc reads file from disk, simplifies its content and saves to file
%% example: simplify_url("index.html", "out.html")
simplify_file(FileNameIn, FileNameOut) ->
	{ok, Html} = file:read_file(FileNameIn),% TODO: check errors
	Page = simplify_page(Html),
	{ok, Fout} = file:open(FileNameOut, [binary, write]),
	file:write(Fout, Page),
	file:close(Fout),
	ok.

%% @spec simplify_page(string()) -> string()
%% @doc wrapper for simplify_page/2 (starts with empty url context)
simplify_page(Body) ->
	simplify_page(Body, {"", ""}, "text/html"). % empty context


%% @spec extract_content_type(html_node(), binary()) -> binary()
extract_content_type(Tree, DefaultContentType) ->
		L = lists:foldl( % find list of attrs with http-equiv=content-type (case sensitive)
			fun(El, Acc) -> 
				{_, Attrs, _} = El,
				case [ {A, V} || {A, V} <- Attrs, A == <<"http-equiv">>, string:to_lower(binary_to_list(V)) == "content-type" ] of
					[] -> 
						Acc;
					_ ->
						Attrs
				end
		end, [], find_all_nodes(<<"meta">>, Tree)),
		case L of
			[] -> % not found - return default content type (from httpc:request)
				C = DefaultContentType;
			_ -> % found
				{_, C} = lists:keyfind(<<"content">>, 1, L)
		end,
		C.

%% @spec simplify_page(string(), {string(), string()}, string()) -> string()
%% @doc main function
%% takes document contens (Body) and document context (Ctx, see url_context/1)
%% returns simplified page as a string()
simplify_page(Body, Ctx, DefaultContentType) ->
	try mochiweb_html:parse(Body) of % parse() will not work if Body contains no html tags
		TreeOrig -> 
			TitleStr = get_title(TreeOrig),
			% save charset from <meta http-equiv="content-type" content="text/html; charset=utf-8" /> or/and from httpc:request
			ContentType = extract_content_type(TreeOrig, list_to_binary(DefaultContentType)),
			% TODO: what if content type from meta or request was not text/html? support this
			case find_node(<<"body">>, TreeOrig) of
				{_, _, TB} -> TreeBody = TB;
				[]         -> TreeBody = [TreeOrig]; % actually need [], not _
				[H|T]      -> TreeBody = [H|T]
			end, 
			ScoredTree = score_tree(
				init_scores(
					% converting urls in <a> and <img> to absolute urls
					replace_node(
						[{<<"a">>,   <<"a">>}, {<<"img">>, <<"img">>}],   fun(L) -> [ to_abs_url(El, Ctx) || El <- L ] end, 
						% MAYBE TODO: remove all elements from <a> except href
						clean_html_tree({<<"div">>, [], TreeBody}) % converting body to div
					)
				)
			),
			OptimumRef = get_max_score_ref(ScoredTree),
			ContentBody = clean_scores(find_node(OptimumRef, ScoredTree)),
			% do something if Content body is started with <td> or <tr>
			case ContentBody of
				{<<"td">>, _, CellContent} -> Out = CellContent;
				{<<"tr">>, _, _}           -> Out = {<<"table">>, [], ContentBody};
				% ? what if it is a list of <td> or <tr>? FIXME
				_ -> Out = [ContentBody]
			end,
			case Out of % if content body starts from <h1> we assume page has its own title in body, keeping it
				[{<<"h1">>, _, _}|_] -> 
					Out2 = Out;
				[{<<"div">>, _, [{<<"h1">>, _, _}|_]}|_] -> 
					Out2 = Out;
				_ -> 
					Out2 = [{<<"h1">>, [], TitleStr} | Out] % adding page <title> in any other case as <h1>
			end,
			TreeOut = {
				<<"html">>, [], [
					{
						<<"head">>, [], 
							[
								{<<"title">>, [], TitleStr}, 
								{<<"meta">>, [{<<"http-equiv">>, <<"content-type">>}, {<<"content">>, ContentType}], []},
								{<<"meta">>, [{<<"name">>, <<"viewport">>}, {<<"content">>, <<"width=480">>}], []},
								{<<"style">>, [{<<"type">>, <<"text/css">>}], [<<"h1 { display: block; width: 100%; border-bottom: 1px solid #333; font-size: 1.2em; }">>]}
							]
					},
					{
						<<"body">>, [],
						Out2
					}
				]
			},
			mochiweb_html:to_html(TreeOut)
	catch 
		error:{badmatch,_} -> 
			% we can't simplify it, just return original text
			Body
			% MAYBE: mochiweb_html:parse("<html><body>"++Body++"</body></html>") 
	end. 

%% ===================================================================
%%
%% Utility functions to walk and operate with html_node() and scored_html_node()
%%
%%

%% @spec find_node(reference() | binary(), html_node() | scored_html_node()) -> html_node() | scored_html_node()
%% @doc returns first node from HTML tree with specific HTML tag. If tree is scored, search can be done by node reference
find_node(Ref, HtmlNode) when is_reference(Ref) -> find_node_byref(Ref, HtmlNode);
find_node(Key, HtmlNode) when is_binary(Key)    -> find_node_bykey(Key, HtmlNode, first).

%% @spec find_all_nodes(binary(), html_node() | scored_html_node()) -> [html_node() | scored_html_node()]
%% @doc the same as find_node(), but returns all nodes with specific tag as a list 
find_all_nodes(Key, HtmlNode) when is_binary(Key) -> find_node_bykey(Key, HtmlNode, multi).

%% @spec find_node_bykey(binary(), html_node() | scored_html_node(), first | multi) -> html_node() | scored_html_node() | [html_node() | scored_html_node()]
%% @doc helper function for find_node() and find_all_nodes()
%% returns: 
%% - first element found as html_node() | scored_html_node() if SearchType != multi 
%% - list of all found elements as [html_node() | scored_html_node()] if SearchType == multi
find_node_bykey(_, HtmlNode, _)       when is_binary(HtmlNode) -> [];  % don't searching for leafs
find_node_bykey(_, {comment, _}, _) -> [];    % comments in mochiweb_html:parse are 2-element tuples, dropping them
find_node_bykey(Key, Elem, SearchType) when is_tuple(Elem) ->        % Element found
	case Elem of
		% element found (key in current Elem equals to Key)
		{Key, _, R} when SearchType == multi    -> [Elem | find_node_bykey(Key, R, SearchType)]; % adding to Out
		{Key, _, _}                             ->  Elem; % return first element and stop processing	
		% if HtmlTree was modified with score tuple contains 4 elements
		{Key, _S, _, R} ->
			if 
				SearchType == multi -> [Elem | find_node_bykey(Key, R, SearchType)];	
				true                ->  Elem
			end;
		% key in current Elem not equalt to Key, e.g. still not found, continue search in the rest part of tree
		{_, _, R}    -> find_node_bykey(Key, R, SearchType); 
		{_, _, _, R} -> find_node_bykey(Key, R, SearchType);
		_            -> [] % unsupported case
	end;
% Lists of html_node() | scored_html_node()
find_node_bykey(_, [], _) -> [];
find_node_bykey(Key, [H|T], SearchType) -> 
	E1 = find_node_bykey(Key, H, SearchType),
	if 
		SearchType == multi ->
			E1 ++ find_node_bykey(Key, T, SearchType); % walk rest part of listlist if it is not empty
		is_tuple(E1) -> % SearchType is not multi (e.g. first) AND E1 is tuple - we found it!
			E1;
		true -> % otherwise continue search
			find_node_bykey(Key, T, SearchType)
	end.
			
%% @spec find_node_byref(reference(), scored_html_node()) -> scored_html_node()
%% @doc helper function for find_node()
find_node_byref(_Ref, HtmlNode) when is_binary(HtmlNode) -> []; % leaf is not an option
find_node_byref(_, {comment, _}) -> [];                         % comment is not an option
%tree is already modified, so we have 4-elem tuple in scored_html_node()
find_node_byref(Ref, {Key, #score{ref=Ref}=S, A, R}) -> {Key, S, A, R}; % found 
find_node_byref(Ref, {_E, _Score, _A, R}) -> find_node_byref(Ref, R); % all other (not found) - continue
% Lists
find_node_byref(_, []) -> [];
find_node_byref(Ref, [H|T]) -> % walk list if it is not empty 
	E1 = find_node_byref(Ref, H),
	if 
		is_tuple(E1) -> E1; % found it
		true         -> find_node_byref(Ref, T)
	end. 

%% @spec remove_node(binary() | [binary()], html_node() | scored_html_node()) -> html_node() | scored_html_node()
%% @doc HTML tag remover. Removes from node all subtrees with Key and returns cleaned html_node() | scored_html_node()
%% example: remove_node(<<"script">>, HtmlTree) -> HtmlTreeWithoutScripts
% if Key is a list - removing all list elements from tree
remove_node([], HtmlTree) -> HtmlTree;
% TODO: неэфективно - дерево пробегается столько раз, какова длина списка ключей.
% переписать remove_node([..],_) чтобы он мог работать со списком Key и выкидывать все за один проход
remove_node([KeyH|KeyT], HtmlTree) -> remove_node(KeyH, remove_node(KeyT, HtmlTree));	
% if Key is not a list:
remove_node(_, NodeIn) when is_binary(NodeIn) -> NodeIn;
remove_node(_, {comment, _}) -> []; % dropping comments
remove_node(Key, {Key, _, _}) -> []; % Key found, dropping subtree
remove_node(Key, {Key, _, _, _}) -> []; % Key found, dropping subtree
remove_node(Key, {E, A, R}) -> {E, A, remove_node(Key, R)}; % continue to subtree
remove_node(Key, {E, S, A, R}) -> {E, S, A, remove_node(Key, R)}; % continue to subtree
remove_node(_, []) -> [];
remove_node(Key, [H|T]) -> [remove_node(Key, H) | remove_node(Key, T)]. % processing list

%% @spec replace_node({binary(), binary()}|[{binary(), binary()}], fun( [html_attr()] ) -> [html_attr()], html_node() | scored_html_node()) -> html_node() | scored_html_node().
%% @doc HTML tag & attribute replacer.
%% First parameter is tuple of two binaries: {Key, NewKey} or list of such tuples to replace 
%% multiple keys at once in one run on html tree.
%% Func is used to transform list of tag attributes: fun(AttrList) -> ModifiedAttrList
%% if Func is omitted, F(L)->L end is used, e.g. list of attrs will be not modified at all.
%%
%% example: replace_node(<<"br">>, <<"p">>, HtmlTree) -> HtmlTreeWithBrReplacedToP
%% example: replace_node(<<"br">>, <<"br">>, fun(L)->TransformedL end, HtmlTree) -> HtmlTreeWithBrReplacedToP
replace_node(Ks, NodeIn) -> replace_node(Ks, fun(L)->L end, NodeIn).
%
replace_node(_Ks, _Func, NodeIn) when is_binary(NodeIn) -> NodeIn;
replace_node(_Ks, _Func, {comment, _}) -> []; % dropping comments
replace_node({Key, NewKey}, Func, {Key, Attr, Rest}) -> {NewKey, Func(Attr), replace_node({Key, NewKey}, Func, Rest)}; % Key found changing and processing subtree
replace_node({Key, NewKey}, Func, {Key, S, Attr, Rest}) -> {NewKey, S, Func(Attr), replace_node({Key, NewKey}, Func, Rest)}; % Key found changing and processing subtree
replace_node({Key, NewKey}, Func, {E, A, R}) -> {E, A, replace_node({Key, NewKey}, Func, R)}; % continue to subtree
replace_node({Key, NewKey}, Func, {E, S, A, R}) -> {E, S, A, replace_node({Key, NewKey}, Func, R)}; % continue to subtree
replace_node(KeyList=[_|_], Func, {Key, Attr, Rest}) -> % case when KeyList is list - changing multiple keys in one walk of tree
	case lists:keyfind(Key, 1, KeyList) of
		{Key, NewKey} -> {NewKey, Func(Attr), replace_node(KeyList, Func, Rest)}; % Key found changing and processing subtree
		false         -> {Key, Attr, replace_node(KeyList, Func, Rest)}
	end;
replace_node(KeyList=[_|_], Func, {Key, S, Attr, Rest}) ->
	case lists:keyfind(Key, 1, KeyList) of
		{Key, NewKey} -> {NewKey, S, Func(Attr), replace_node(KeyList, Func, Rest)}; % Key found changing and processing subtree
		false         -> {Key, S, Attr, replace_node(KeyList, Func, Rest)}
	end;
replace_node(_, _, []) -> [];
replace_node(Ks, Func, [H|T]) -> [replace_node(Ks, Func, H) | replace_node(Ks, Func, T)]. % processing list recursively

%% @spec brbr_to_p(html_node() | scored_html_node()) -> html_node() | scored_html_node()
%% @doc replaces more than 2 <br>s in row with <p>
brbr_to_p(NodeIn) when is_binary(NodeIn) -> NodeIn;
brbr_to_p({comment, _})                  -> []; % dropping comments
brbr_to_p({E,    A, R}) -> {E,    A, brbr_to_p(R)}; % continue to subtree
brbr_to_p({E, S, A, R}) -> {E, S, A, brbr_to_p(R)}; 
brbr_to_p([]) -> [];
brbr_to_p([{<<"br">>,_,_},   {<<"br">>,_,_}   | T]) -> brbr_to_p([{<<"p">>,[],[]} | T]); % Replacing <br><br> with <p>,
brbr_to_p([{<<"p">>,_,_},    {<<"br">>,_,_}   | T]) -> brbr_to_p([{<<"p">>,[],[]} | T]); % if more than two <br> in row, replacing all.
brbr_to_p([{<<"br">>,_,_,_}, {<<"br">>,_,_,_} | T]) -> brbr_to_p([{<<"p">>,[],[]} | T]); % Do the same for scored elements. 
brbr_to_p([{<<"p">>,_,_,_},  {<<"br">>,_,_,_} | T]) -> brbr_to_p([{<<"p">>,[],[]} | T]);
brbr_to_p([H|T]) -> [brbr_to_p(H) | brbr_to_p(T)]. % processing list recursively

%% @spec count_commas(html_node() | scored_html_node()) -> int()
%% @doc counts number of commas (,) in HTML tree
count_commas({comment, _}) -> 0; 
count_commas({_,_,R})      -> count_commas(R);
count_commas({_,_,_,R})    -> count_commas(R);
count_commas([])           -> 0; 
count_commas([H|T])        -> count_commas(H) + count_commas(T);
count_commas(Leaf) when is_binary(Leaf) -> lists:foldl(fun(E, S) -> if E == $, -> S+1; true->S end end, 0, binary_to_list(Leaf)).

%% ===================================================================
%%
%% Functions to operate scored_html_tree() - add, modify and calculate score
%%
%%

%% @spec init_scores(html_node()) -> scored_html_node()
%% @doc transforms html_node() to scored_html_node(). It now has 4 elements in tuple (not 3 as in mochiweb_html type), 
%% the second element in tuple is Score - record of #score, containing readability score, current element ref
%% and ref to parent of current element (see -record(score, ...) below)
init_scores(Tree) -> init_scores(Tree, make_ref()). % adding reference for topmost element too
%
init_scores(R, _) when is_binary(R) -> R;
init_scores({comment, _}, _) -> []; % dropping comments
init_scores({E, A, Rest}, Parent) -> Ref=make_ref(), {E, #score{ref=Ref, parent=Parent}, A, init_scores(Rest, Ref)}; % setting current parent and giving my ref to child as parent
init_scores([], _) -> [];
init_scores([H|T], Parent) -> [init_scores(H, Parent) | init_scores(T, Parent)]. % processing list recursively

%% @spec clean_scores(scored_html_node()) -> html_node()
%% @doc transforms scored tree to normal tree which can be processed by mochiweb_html functions
clean_scores(NodeIn) when is_binary(NodeIn) -> NodeIn;
clean_scores({comment, _}) -> []; 
clean_scores({E, _Score, A, Rest}) -> {E, A, clean_scores(Rest)};
%clean_scores({E, Score, A, Rest}) -> {E, [{<<"readability">>, Score#score.readability}|A], clean_scores(Rest)}; %DEBUG - save score to attrs
clean_scores({E, A, Rest}) -> {E, A, clean_scores(Rest)};
clean_scores([]) -> [];
clean_scores([H|T]) -> [clean_scores(H) | clean_scores(T)]. 

%% @spec modify_score(reference(), scored_html_node(), int()) -> scored_html_node()
%% @doc modify readability score for specific element on scored tree. Score is added to current node score 
%% e.g. int() is ScoreDiff, to subtract score for element pass negative int()
modify_score(_, Leaf, _) when is_binary(Leaf) -> Leaf;
modify_score(_, {comment, _}, _) -> []; 
modify_score(Ref, {Key, #score{ref=Ref, readability=Rdbl, parent=ParentRef}, A, R}, Score) -> {Key, #score{ref=Ref, readability=Rdbl+Score, parent=ParentRef}, A, R};
modify_score(Ref, {E, S, A, R}, Score) -> {E, S, A, modify_score(Ref, R, Score)}; 
modify_score(_, [], _) -> [];
modify_score(Ref, [H|T], Score) -> [modify_score(Ref, H, Score) | modify_score(Ref, T, Score)].


%% ===================================================================
%%
%% Internal and helper functions
%%
%%

%% @spec fetch_page(string()) -> string()
%% @doc fetches url and returns its content as a string()
fetch_page(Url) ->
	inets:start(), % TODO: handle errors & not start if already started
	ssl:start(),
	% TODO: cache page - save to ets by url
	{ok, RequestId} = httpc:request(get, {Url, [{"User-Agent", ?USER_AGENT}]}, [{autoredirect, true}, {relaxed, true}], [{sync, false}, {receiver, self()}]),
	receive
		{http, {RequestId, Result}} ->
			case Result of
				{_, Hdrs, Body} ->
					case lists:keyfind("content-type", 1, Hdrs) of
						{_, ContentType} ->
							ContentType;
						_ ->
							ContentType = "text/html"
					end,
					{ContentType, Body};
				{error, ErrVal} ->
					{
						"text/html",
						io_lib:format(<<"<html><head><title>Error</title></head><body>Cannot fetch ~s - ~p</body></html>">>, [Url, ErrVal])
					}
			end
	end.

%% @spec clean_html_tree(html_node() | scored_html_node()) -> html_node() | scored_html_node()
%% @doc cleans first-level unreadable junk from html tree
clean_html_tree(Tree) -> 
	% TODO: add: find max <frame> in frameset and use it as document
	brbr_to_p( 
		remove_node([<<"style">>, <<"link">>, <<"script">>, <<"noscript">>, <<"form">>, <<"object">>, <<"iframe">>], Tree)
	).  

%% @spec get_title(html_node() | scored_html_node()) -> binary()
get_title(Tree) ->
	case find_node(<<"title">>, Tree) of
		{_, _, TitleStr} -> TitleStr;
		[] -> "";
		_ -> ''
	end.

%% @spec to_abs_url({binary(), binary()}, string()) -> {binary(), binary()}
%% @doc helper function for simplify_page/2 - converts href & src in element attr to absolute path
to_abs_url({<<"src">>, U}, Ctx)  -> {<<"src">>,  list_to_binary(full_url(Ctx, binary_to_list(U)))};
to_abs_url({<<"href">>, U}, Ctx) -> {<<"href">>, list_to_binary(full_url(Ctx, binary_to_list(U)))};
to_abs_url(A, _) -> A.

%% @spec get_parent_ref(scored_html_node()) -> reference()
get_parent_ref(Node) ->
	{_, #score{parent=ParentRef}, _, _} = Node,
	ParentRef.

%% @spec get_ref(scored_html_node()) -> reference()
get_ref(Node) ->
	{_, #score{ref=Ref}, _, _} = Node,
	Ref.

%% @spec get_score(scored_html_node()) -> int()
get_score(Node) ->
	{_, #score{readability=Score}, _, _} = Node,
	Score.

%% @spec score_by_class_or_id(scored_html_node()) -> int()
%% @spec score_by_class_or_id([html_attr()]) -> int()
%% @doc calculates score for node element by its id or class name
score_by_class_or_id({_, _, Attrs, _})-> score_by_class_or_id(Attrs);
score_by_class_or_id([]) -> 0;
score_by_class_or_id(Attrs=[_|_]) ->
	AttrVals = [ V || {K, V} <- [Attrs], (K == <<"id">>) orelse (K == <<"class">>) ],
	if 
		AttrVals == [] -> 0; % no id or class (list is empty)
		true -> % e.g. we have id or class or both
			case lists:foldl(
					fun(El, Acc) -> 
							Acc orelse (re:run(El, ?RE_NEGATIVE, [{capture, none}]) == match) 
					end, false, AttrVals) of
				true -> -50;
				false ->
					case lists:foldl(
							fun(El, Acc) -> 
									Acc orelse (re:run(El, ?RE_POSITIVE, [{capture, none}]) == match) 
							end, false, AttrVals) of
						true -> 25;
						false -> 0
					end
			end
	end.

%% @spec score_tree(scored_html_node()) -> scored_html_node()
%% @doc score whole html tree depending on its contents
score_tree(Tree) -> % TODO: do score_tree in parallel (multiplie processes, map+reduce)
	Paragraphs = find_all_nodes(<<"p">>, Tree),
	%Map1 = [ {1, get_parent_ref(P)} || P <- Paragraphs ], % список вида {1, Parent} для каждого P (пары могут повторяться)
	UniqParents = lists:foldl( % building list of unique parent refs for all <p>'s
		fun(P, ParentRefList) -> 
			ParentRef = get_parent_ref(P),
			case lists:member(ParentRef, ParentRefList) of
				true  -> ParentRefList;
				false -> [ParentRef | ParentRefList]
			end
		end, [], Paragraphs),
	ScoreList = score_parallel(Tree, UniqParents, Paragraphs), % вернет список вида { Score, Element }, каждый Element встречается 1 раз
	lists:foldl( 
		fun({Score, P_ref}, TreeAcc) ->
				if 
					Score /= 0 -> 
						modify_score(P_ref, TreeAcc, Score);
					true -> % если в процессе reduce score стало равно 0, то не вносим измненения в дерево для этого элемента
						TreeAcc
				end
		end, Tree, ScoreList).
		

score_parallel(Tree, UniqParents, Paragraphs) -> 
	process_flag(trap_exit, true),
	S = self(),
	lists:foreach(fun(P_ref) -> spawn(fun() -> do_score(S, Tree, P_ref) end) end, UniqParents), 
	lists:foreach(fun(P) -> S ! { 1, get_parent_ref(P) } end, Paragraphs),
	Dict0 = dict:new(),
	Dict1 = gather(length(Paragraphs) + 2*length(UniqParents), Dict0), % *2 тк do_score запускает 2 процесса
	dict:fold(fun(K, ValList, L)-> [{lists:foldl(fun(E, Acc)-> E+Acc end, 0, ValList), K}| L] end, [], Dict1).

do_score(ParentPid, Tree, P_ref) -> 
	ParentElem = find_node(P_ref, Tree),
	spawn(fun()-> ParentPid ! { score_by_class_or_id(ParentElem), P_ref} end),
	spawn(fun()-> ParentPid ! { count_commas(ParentElem), P_ref} end). 

gather(0, Dict) -> Dict;
gather(N, Dict) ->
	receive
		{Score, P_ref} -> 
			case dict:is_key(P_ref, Dict) of
				true -> 
					Dict1 = dict:append(P_ref, Score, Dict),
					gather(N-1, Dict1);
				false ->
					Dict1 = dict:store(P_ref, [Score], Dict),
					gather(N-1, Dict1)
			end;
		{'EXIT', _, _Why} ->
			gather(N-1, Dict)
	end.

%% @spec get_max_score_ref(scored_html_node()) -> reference()
%% @doc finds ref to node with maximum readability score
get_max_score_ref(Tree) ->
	{Ref, _MaxScore} = lists:foldl(
		fun({Ref, Score}, {Ref0, Score0}) -> 
				if 
					is_reference(Ref0) -> % e.g. not first pass
						if 
							Score0<Score -> {Ref, Score}; 
							true->{Ref0, Score0} 
						end; 
					true->{Ref, Score} 
				end 
		end, {0, 0}, score_list(Tree)),
	Ref.

%% @spec score_list(scored_html_node()) -> [{reference(), int()}]
%% @doc builds list of pairs {Node_Ref, Node_Score} from html tree
%% helper function for get_max_score_ref()
score_list(HtmlNode) when is_binary(HtmlNode) -> []; % leaf
score_list({comment, _}) -> [];       
score_list({_, #score{readability=Rdbl, ref=Ref}, _, R}) -> [{Ref, Rdbl} | score_list(R)];	
score_list({_, _, _, R}) -> score_list(R);	
score_list([]) -> []; 
score_list([H|T]) -> score_list(H) ++ score_list(T). 

%% @spec url_context(string()) -> {string(), string()}
%% @doc returns the  domain, and current context path. 
%% example: url_context("http://www.some.domain.com/content/index.html) -> {"http://www.some.domain.com", "/content"}
url_context(URL) ->
    {Proto, _, Root, _Port, Path, _Query} = http_uri:parse(URL), 
    Ctx = string:sub_string(Path, 1, string:rstr(Path,"/")),
    {atom_to_list(Proto) ++ "://" ++ Root, Ctx}.

%% @spec full_url({string(), string()}, string()) -> string()
%% @doc example: full_url(url_context("http://www.somewhere.in/time/page.html"), "img/pic.gif" -> "http://www.somewhere.in/time/img/pic.gif"
full_url({Root, _Context}, ComponentUrl=[$/|_]) -> Root ++ ComponentUrl; % abs url inside the same server: /img/pig.gif    
full_url({_Root, _Context}, ComponentUrl="http://"++_)  -> ComponentUrl; % full url http://site.com/pic.gif
full_url({_Root, _Context}, ComponentUrl="https://"++_) -> ComponentUrl;
full_url({_Root, _Context}, ComponentUrl="ftp://"++_)   -> ComponentUrl;
full_url({Root, Context}, ComponentUrl) -> Root ++ Context ++ "/" ++ ComponentUrl. % everything else is a relative path

add_proto_if_none(U="http://"++_) -> U;
add_proto_if_none(U="https://"++_) -> U;
add_proto_if_none(U="ftp://"++_) -> U;
add_proto_if_none(U) -> "http://"++U.
