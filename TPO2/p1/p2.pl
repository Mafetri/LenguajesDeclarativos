:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(persistency)).
:- http_handler(root(.), home_page, []).
:- http_handler(root(post), new_post, []).
:- http_handler(root(comment), new_comment, []).

:- persistent
    post(id:positive_integer, title:string, content:string),
    comment(post_id:positive_integer, comment:string).

:- db_attach('blog_db.pl', []).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

home_page(_Request) :-
    findall(post(Id, Title, Content), post(Id, Title, Content), Posts),
    reply_html_page(title('Blog'), \blog_page(Posts)).

new_post(Request) :-
    member(method(post), Request), !,
    http_parameters(Request, [
        title(Title, [string]),
        content(Content, [string])
    ]),
    (   max_post_id(MaxId) -> Id is MaxId + 1 ; Id = 1),
    assert_post(Id, Title, Content),
    format('Content-type: text/html~n~n'),
    format('<p>Post added. <a href="/">Back to home</a></p>').
new_post(_Request) :-
    reply_html_page(title('New Post'), \post_form).

new_comment(Request) :-
    member(method(post), Request), !,
    http_parameters(Request, [
        post_id(PostId, [integer]),
        comment(Comment, [string])
    ]),
    assert_comment(PostId, Comment),
    format('Content-type: text/html~n~n'),
    format('<p>Comment added. <a href="/">Back to home</a></p>').

:- dynamic max_post_id/1.
max_post_id(MaxId) :-
    aggregate(max(Id), Title^Content^post(Id, Title, Content), MaxId).

blog_page(Posts) -->
    html([
        h1('Taringa'),
        \posts_table(Posts),
        a(href('/post'), 'Add new post')
    ]).

posts_table([]) --> [].
posts_table([post(Id, Title, Content)|T]) -->
    html([
        h2(Title),
        p(Content),
        h3('Comments'),
        \comments(Id),
        form([action='/comment', method='POST'], [
            input([type=hidden, name=post_id, value=Id]),
            textarea([name=comment], ''),
            input([type=submit, value='Add Comment'])
        ]),
        \posts_table(T)
    ]).

comments(PostId) -->
    {
        findall(Comment, comment(PostId, Comment), Comments)
    },
    html(Comments).

post_form -->
    html([
        form([action='/post', method='POST'], [
            p([], [
                label([for=title], 'Title: '),
                input([name=title, type=text])
            ]),
            p([], [
                label([for=content], 'Content: '),
                textarea([name=content], '')
            ]),
            p([], input([type=submit, value='Post']))
        ])
    ]).

