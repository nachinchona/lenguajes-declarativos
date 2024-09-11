:- consult(database).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).		
:- use_module(library(http/http_files)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_header)).

:- multifile http:status_page/3.
:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(files, '/f', []).
:- http_handler(files(.), serve_files, [prefix]).

%iniciar server en puerto 8000
iniciar_blog :-
    server(8000).

%detener server y limpiar base de datos
detener_blog :- http_stop_server(8000,[]), db_sync(gc).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

%handlers
:- http_handler(root(.), bienvenida, []).
:- http_handler(root(blog), blog, []).
:- http_handler('/publish', publish, []).
:- http_handler('/comment', comment, []).

listar_posts(Posts) :-
    %se listan como (Title, Body, Comentarios)
    findall((X,Y,Z),blog_post(X,Y,Z),Posts).

posts_to_html(HTML) :-
    listar_posts(Posts),
    reverse(Posts,Posts_Inverse), %para que salgan ordenados: publicados/comentados primeros
    posts_to_html_aux(Posts_Inverse, HTML).

posts_to_html_aux([],[]).

posts_to_html_aux([(Title,Body,Comments)|Rest],
		  [div(class(container),
		       [p(class(title_post), Title),
			p(class(body_post), Body),
			print_html(CommentsHTML),
			form([action='/comment', method='POST', autocomplete=off],
			     [input([type(hidden), name(title), value(Title)]),
			      input([type(hidden), name(body), value(Body)]),
			      input([class(input_field),
				     name(comment),
				     placeholder('Escribe tu comentario...'),
				     type(textarea), required, value('')]),
			      input([class(misubmit), type(submit), value('Comentar')])
			     ])
		       ])|HTML]) :-
    comments_to_html(Comments,CommentsHTML),
    posts_to_html_aux(Rest,HTML), !.

comments_to_html(Comments,HTML) :-
    comments_to_html_aux(Comments, HTML).

comments_to_html_aux([],[]).

comments_to_html_aux([(Comment)|Rest],
		     [div(class("comment_container"),[p(class("comment"), Comment)])
		      |HTML]) :-
    comments_to_html_aux(Rest,HTML).

blog(_Request) :-
    posts_to_html(HTML),
    reply_html_page(
	[title('Inicio - IN3240Blog'), \html_receive(css), link([rel('icon'), type('image/x-icon'), href('/f/favicon.ico')])],
	[\css('/f/estilo.css'),
	 section([id=top],
		 [\back_button, %cerrar sesion
		  a(href('#pub'), button([type(button)], 'Publicar!'))
		 ]),		 
	 div(class(todo),
	     [h1(b('Muro principal')),
	      print_html(HTML),
	      section([id=pub],
		      form([action='/publish', method='POST', autocomplete=off],
			   [div(class(titulo_box),
				[label([for(title)],'Titulo:'),
				 div(input(
					 [class(title_field),
					  name(title), required,
					  placeholder('Escribe el título de tu post...'),
					  type(textarea)]))
				]),
			    div(class(cuerpo_box),
				[label([for(body)],'Cuerpo:'),
				 div(textarea(
					 [class(body_field),
					  name(body), required,
					  placeholder('Escribe el cuerpo de tu post...'),
					  type(textarea)],['']))]),
			    input([class(misubmit), type(submit), value('Publicar')])
			   ])),
	      p(''),
	      p(''),	      
	      b('Página web estilo blog realizada en Prolog - Ignacio Alejandro Navarro Oliva - FAI-3240')
	     ]),
	 a(href('#top'), button([type(button)], 'Ver ultimos posts!'))
	]).

publish(Request) :-
    member(method(post), Request), !,
    http_parameters(Request,
		    [
			title(Title),
			body(Body)
		    ],
		    [
			attribute_declarations(param)			
		    ]),
    publicar(Title,Body),
    http_redirect(moved, root(blog), _).

publish(_Request) :-
    %redirigir si no es con metodo post
    http_redirect(moved, root(blog), _).

param(title, [length < 40]).
param(body,  [length < 480]).
param(comment,  [length < 200]).

comment(Request) :-
    member(method(post), Request), !,
    http_parameters(Request,
		    [
			title(Title),
			body(Body),
			comment(Comment)
		    ],
		    [
			attribute_declarations(param)			
		    ]),
    comentar(blog_post(Title,Body),Comment),
    http_redirect(moved, root(blog), _).

comment(_Request) :-
    %para redirigir de /comment a /blog si la request no es post
    http_redirect(moved, root(blog), _).

bienvenida(_Request) :-
    reply_html_page(
	[title('Bienvenido/a - IN3240Blog'), \html_receive(css), link([rel('icon'), type('image/x-icon'), href('/f/favicon.ico')])],
	[\css('/f/estilo.css'),
	 div(class(todo),
	     [
		 img(src('/f/fai.png')),
		 h1('Bienvenido/a a IN3240Blog!'),
		 p('Haz tu primer publicación!'),
		 p(i('Publica y comenta acerca de lo que quieras')),
		 a(href(blog), button([type=button], 'Entrar'))
	     ])
	]).

serve_files(Request) :-
    http_reply_from_files('assets', [], Request).

css(URL) -->
    html_post(css,
              link([ type('text/css'),
                     rel('stylesheet'),
                     href(URL)
                   ])).

back_button -->
    html(
	[
	    a(href('/'), button([type(button)], 'Atrás'))
	]).

http:status_page(URL, _Context, HTML) :-
    phrase(page([ title('Página no disponible - IN3240Blog') ],
		{|html(URL)||
		  <link rel="stylesheet" href="/f/estilo.css">
		  <h1 class=error_title>La dirección web no corresponde a ningún sitio del blog</h1>
		  <p class=error_body> Por favor, redirijase <a target='_self' href='/'>  aquí. </a> </p>									             |}
	       ),
	   HTML).

