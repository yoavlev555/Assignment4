:- module('ex4',
        [author/2,
         genre/2,
         book/4
        ]).

/*
 * **********************************************
 * Printing result depth
 *
 * You can enlarge it, if needed.
 * **********************************************
 */
maximum_printing_depth(100).
:- current_prolog_flag(toplevel_print_options, A),
   (select(max_depth(_), A, B), ! ; A = B),
   maximum_printing_depth(MPD),
   set_prolog_flag(toplevel_print_options, [max_depth(MPD)|B]).



author(a, asimov).
author(h, herbert).
author(m, morris).
author(t, tolkien).

genre(s, science).
genre(l, literature).
genre(sf, science_fiction).
genre(f, fantasy).

book(inside_the_atom, a, s, s(s(s(s(s(zero)))))).
book(asimov_guide_to_shakespeare, a, l, s(s(s(s(zero))))).
book(i_robot, a, sf, s(s(s(zero)))).
book(dune, h, sf, s(s(s(s(s(zero)))))).
book(the_well_at_the_worlds_end, m, f, s(s(s(s(zero))))).
book(the_hobbit, t, f, s(s(s(zero)))).
book(the_lord_of_the_rings, t, f, s(s(s(s(s(s(zero))))))).

% You can add more facts
bigger_then(A, zero, A).
bigger_then(s(A), s(B), s(A)) :- bigger_then(A, B, A).


%list_bigger(A, [], A)
%list_bigger([T1 | S1], [T2 | S2], [T1 | S1]) :- list_bigger(S1, S2, S1)

%length_of_books_by_author(AuthorName, Lengths) :- author(AuthorID, AuthorName), findall(T, book(BookName, AuthorID, GenereID, T), Lengths).

% Signature: max_list(Lst, Max)/2
% Purpose: true if Max is the maximum church number in Lst, false if Lst is emoty.
max_list([], false).
max_list([T], T).
max_list([A, B], A) :- bigger_then(A, B, A), !.
max_list([_, B], B).
max_list([H | T], Max) :-
    max_list(T, MaxT),
    max_list([H, MaxT], Max).






% Signature: author_of_genre(GenreName, AuthorName)/2
% Purpose: true if an author by the name AuthorName has written a book belonging to the genre named GenreName.
author_of_genre(GenreName, AuthorName) :-
    author(AuthorID, AuthorName),
    genre(GenreID, GenreName),
    book(_, AuthorID, GenreID, _).





% Signature: longest_book(AuthorName, BookName)/2
% Purpose: true if the longest book that an author by the name AuthorName has written is titled BookName.
longest_book(AuthorName, BookName) :-
    author(AuthorID, AuthorName),
    findall((Title, Length), book(Title, AuthorID, _, Length), Pairs),
    findall(Length, member((_, Length), Pairs), Lengths),
    max_list(Lengths, Max),
    member((BookName, Max), Pairs).