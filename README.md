rdbl.erl - Erlang readability library 
-------------------------------------

This is Erlang library to extract reasonable content and remove junk from html pages. 
Based on ideas from readability.js by arc90.

Installation
------------
cd src/ && make

Examples
--------

1> rdbl:simplify_url("http://www.somesite.at/internet/") -> simplified page text as string()

2> rdbl:simplify_url("http://www.somesite.at/internet/", "out.html") -> ok

3> rdbl:simplify_file("input.html", "out.html") -> ok

4> rdbl:simplify_page(HtmlPageText) -> PageTextSimplified

See other examples in rdbl.erl.

Dependencies
------------
Library uses [mochiweb](https://github.com/mochi/mochiweb) html library to parse HTML-content (included).
Only following files from mochiweb needed:
mochinum.erl
mochiutf8.erl
mochiweb_charref.erl
mochiweb_html.erl
