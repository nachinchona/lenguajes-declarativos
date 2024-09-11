:- use_module(library(persistency)).

%hacer posts y comentarios persistentes
:- persistent blog_post(title:atom, body:atom, comments:list).

:- initialization(db_attach('database.journal', [])).

publicar(Title, Body) :-
    assert_blog_post(Title,Body,[]).

comentar(blog_post(A,B), Comment) :-
    blog_post(A,B,Comentarios),
    append(Comentarios, [Comment], Resultado),
    retract_blog_post(A,B,Comentarios),
    assert_blog_post(A,B,Resultado).

clear_blog :-
    retractall_blog_post(_,_,_).

static_blog :-
    %para cargar posteos si la base de datos esta vacia
    clear_blog,
    publicar('Mi primer posteo', 'Hola! Este es mi primer posteo.'),
    publicar('Pensando...', ':P'),
    comentar(blog_post('Mi primer posteo', 'Hola! Este es mi primer posteo.'), 'un comentariooossoos').

