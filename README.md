rdbl.erl - Erlang readability library 
-------------------------------------
[Inspired by readability.js by arc90]

This is Erlang version of readability.js with my modifications.
Library uses mochiweb_html to parse HTML-content.

Installation
------------
cd src/
make

Examples
--------

rdbl:simplify_url("http://www.somesite.at/internet/") -> simplified page text as string()

rdbl:simplify_url("http://www.somesite.at/internet/", "out.html") -> ok

rdbl:simplify_file("input.html", "out.html") -> ok

rdbl:simplify_page(HtmlPageText) -> PageTextSimplified

See other examples in rdbl.erl.
