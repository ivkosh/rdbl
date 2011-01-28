rdbl.erl - Erlang readability library 
-------------------------------------

This is Erlang library to extract reasonable content and remove junk from html pages. 
Based on ideas from readability.js by arc90.
Library uses mochiweb_html to parse HTML-content.

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
