% ========== Modules ==========
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).
:- use_module(library(persistency)).
:- use_module(library(http/http_path)).


% ========== Database ========== 
%  Creates the Database with the table: 
%  -> post(id, title, content)
%  -> comment(post_id, comment)
:- persistent
    post(id:positive_integer, title:string, content:string),
    comment(post_id:positive_integer, comment:string).
:- db_attach('blog_db.pl', []).


% ========== Handlers ========== 
%  Sets the handlers for the routes: 
%  -> http://localhost:PORT/ => home_page
%  -> http://localhost:PORT/post => new_post 
%  -> http://localhost:PORT/commet => new_comment
%  -> http://localhost:PORT/styles.css => returns the styles.css file
:- http_handler(root(.), home_page, []).
:- http_handler(root(post), new_post, []).
:- http_handler(root(comment), new_comment, []).
:- http_handler(css('styles.css'),  http_reply_file('styles.css', []), []).
http:location(css, root('.'), []).


% ========== Initialization ==========
% Set up the server in the Port given 
server(Port) :-
    http_server(http_dispatch, [port(Port)]).


% ========== Home Page ==========
% 1. Gets all the posts in the db
% 2. Creates and returns to the client a HTML with:
%     -> header: title Blog and a ref to the css file
%     -> body: a title of the page and a section with all the posts and comments
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
% Generates a HTML of the posts:
%   1. Creates a div for a post: Each div has the title and content of the post 
%   2. Creates a div inside 1 for comments:
%     2,1, Set the title of the comment section
%     2.2. Creates a div with all the comments given by the predicate comments
%     2.3. Creates the form for posting a new comment that will be handle by the route '/comment'
%   3. Loops until all the divs for each post in T are created
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

% ========== Comments ============
% 1. Finds all the comments with the PostId in the db 
% 2. Returns a HTML of p tag comments generated by comment_p
comments(PostId) -->
    {
        findall(Comment, comment(PostId, Comment), Comments)
    },
    html([\comment_p(Comments)]).

% ========== Comment_p ===========
% 1. Creates a p tag with the comment
% 2. Loops the Rest list of comments until there is no comment left  
comment_p([]) --> [].
comment_p([Comment|Rest]) -->
    html(p([class="comment"], ["➤ ", Comment])),
    comment_p(Rest).


% ========== New Post Form Handler ==========
% 1. Gets the parameters given by the form
% 2. Gets the max id of the posts plus 1 for the new post
% 3. Inserts in db the new post with the parameters of 1 and the id of 2 
% 4. Returns to the client a positive message
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

% ============ New Post Route Handler ============
% Returns to the client a HTML with a form to be filled
% to create a new post
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

% Returns the MaxId of the post table
:- dynamic max_post_id/1.
max_post_id(MaxId) :-
    aggregate(max(Id), Title^Content^post(Id, Title, Content), MaxId).


% ========== New Comment Handler ==========
% 1. Gets the parameters given by the form 
% 2. Inserts in the db the comment for that post 
% 3. Returns to the client a HTML with a positive message
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
