% ========== Modules ==========
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).
:- use_module(library(persistency)).
:- use_module(library(http/http_path)).


% ========== Database ==========
:- persistent
    post(id:positive_integer, title:string, content:string),
    comment(post_id:positive_integer, comment:string).
:- db_attach('blog_db.pl', []).


% ========== Handlers ==========
:- http_handler(root(.), home_page, []).
:- http_handler(root(post), new_post, []).
:- http_handler(root(comment), new_comment, []).
:- http_handler(css('styles.css'),  http_reply_file('styles.css', []), []).
http:location(css, root('.'), []).


% ========== Initialization ==========
server(Port) :-
    http_server(http_dispatch, [port(Port)]).


% ========== Home Page ==========
home_page(_Request) :-
    findall(post(Id, Title, Content), post(Id, Title, Content), Posts),
    reply_html_page(
        [
            title('Blog'), link([rel=stylesheet, href='styles.css'])
        ],
        [
            h1([class="title"],'reddit.pl'),
            section([class=posts], [
                a(href('/post'), 'Add New Post'),
                \posts_table(Posts)
            ])
        ]
    ).


% ========== Posts Table ==========
posts_table([]) --> [].
posts_table([post(Id, Title, Content)|T]) -->
    html([
        div([class=post], [
            h2(Title),
            p(Content),
            div([class=comments], [
                h3('Comments'),
                div([
                    \comments(Id)
                ]),
                form([class="comment_form",action='/comment', method='POST'], [
                    input([type=hidden, name=post_id, value=Id]),
                    textarea([name=comment], ''),
                    input([type=submit, value='Add Comment'])
                ])
            ])
        ]),
        \posts_table(T)
    ]).

% Comments
comments(PostId) -->
    {
        findall(Comment, comment(PostId, Comment), Comments)
    },
    html([\comment_p(Comments)]).

comment_p([]) --> [].
comment_p([Comment|Rest]) -->
    html(p([class="comment"], ["➤ ", Comment])),
    comment_p(Rest).


% ========== New Post Handler ==========
new_post(Request) :-
    member(method(post), Request), !,
    http_parameters(Request, [
        title(Title, [string]),
        content(Content, [string])
    ]),
    (   max_post_id(MaxId) -> Id is MaxId + 1 ; Id = 1),
    assert_post(Id, Title, Content),
    reply_html_page(
        [title('Post Added'), link([rel=stylesheet, href='styles.css'])],
        [
            h1([class="title"],'reddit.pl'),
            section([class(notification)], [
                h2(['Post added! ', a([href('/')], 'Back to home')])
            ])
        ]).

new_post(_Request) :-
    reply_html_page(
        [title('Post Added'), link([rel=stylesheet, href='styles.css'])],
        [
            h1([class="title"],'reddit.pl'),
            section([class=notification], [
                form([class="post_form", action='/post', method='POST'], [
                    label([for=title], 'Title: '),
                    input([name=title, type=text]),
                    label([for=content], 'Content: '),
                    textarea([name=content, rows=10], ''),
                    input([type=submit, value='Post'])
                ])
            ])
        ]
    ).


:- dynamic max_post_id/1.
max_post_id(MaxId) :-
    aggregate(max(Id), Title^Content^post(Id, Title, Content), MaxId).


% ========== New Comment Handler ==========
new_comment(Request) :-
    member(method(post), Request), !,
    http_parameters(Request, [
        post_id(PostId, [integer]),
        comment(Comment, [string])
    ]),
    assert_comment(PostId, Comment),
    reply_html_page(
        [title('Post Added'), link([rel=stylesheet, href='styles.css'])],
        [
            h1([class="title"],'reddit.pl'),
            section([class(notification)], [
                h2(['Comment added! ', a([href('/')], 'Back to home')])
            ])
        ]).