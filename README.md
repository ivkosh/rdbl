rdbl.erl - Erlang readability library 
=====================================

This is Erlang library to extract reasonable content and remove junk from html pages. 
Inspired by readability.js from arc90.

Source is primary hosted at [github.com/ivkosh](https://github.com/ivkosh/rdbl).
My [Readabilizer service](http://evl.me/) is using this library to extract content from web pages.

Licensing and author
--------------------

This library is distributed under the GNU General Public License version 3 and is also available 
under alternative licenses negotiated directly with rdbl author Ivan Koshkin <ivan@koshkin.me>. 

The GPL (version 3) is included in this source tree in the file COPYING.

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
Only following files from mochiweb are needed: mochinum.erl, mochiutf8.erl, mochiweb_charref.erl, mochiweb_html.erl.
