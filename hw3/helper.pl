
%Convert into date datatype:
%it takes in the date in the format as a string type and convert the structure to be a date(D,M,Y) predicate.
is_date(InputString, Date) :-
    string(InputString),
    re_matchsub("^([0-9]{1,2})-([0-9]{1,2})-([0-9]{4})", InputString, MatchDict),
    atom_number(MatchDict.1, Month),
    atom_number(MatchDict.2, Day),
    atom_number(MatchDict.3, Year),
    valid_date(Day, Month, Year),
    Date = date(Year, Month, Day).

% Validate if the day, month, and year form a valid date.
valid_date(Day, Month, Year) :- 
    Month >= 1, Month =< 12,
    Day >= 1, Day =< 31, 
    Year > 0.



%Display Table
%use this print_tables at the end to print the table in tabular format
print_tables([]).
print_tables([Table | Rest]) :-
    print_table(Table),
    nl, 
    print_tables(Rest).
% Rule to print a single table
print_table([TableName, Headers, Rows]) :-
    nl,
    format('            ~w                ~n', [TableName]), % Print the table name
    format('--------------------------------~n'),
    print_headers(Headers), % Print the headers dynamically
    format('--------------------------------~n'),
    print_rows(Rows),
    format('--------------------------------~n').

% Helper predicate to print headers dynamically
print_headers(Headers) :-
    length(Headers, Length),
    ( Length > 1 -> 
        print_header_line(Headers)
    ; 
        Headers = [Header],
        format('~w~n', [Header])
    ).

% Helper predicate to print headers in a line
print_header_line([]):- nl.
print_header_line([H | T]) :-
    format('~w\t', [H]),
    print_header_line(T).

% Helper predicate to print rows, each row on a new line
print_rows([]).
print_rows([Row | Rest]) :-
    print_row(Row),
    print_rows(Rest).

% Print a single row
print_row([]) :-
    nl.
print_row([Item | Rest]) :-
    format('~w\t', [Item]),
    print_row(Rest).



% Helper predicate to split punctuation from words
separate_punctuation([], []).
separate_punctuation([H|T], Result) :-
    split_string_punctuation(H, SplitHead),
    separate_punctuation(T, SplitTail),
    append(SplitHead, SplitTail, Result).

% Helper predicate to split a single string
split_string_punctuation(String, [String]) :-
    \+ sub_string(String, _, 1, 0, ","),
    \+ sub_string(String, _, 1, 0, ".").

split_string_punctuation(String, Result) :-
    sub_string(String, Before, 1, After, ","),
    sub_string(String, 0, Before, _, BeforePart),
    sub_string(String, _, After, 0, AfterPart),
    split_string_punctuation(AfterPart, AfterSplit),
    append([[BeforePart, ","], AfterSplit], Result).

split_string_punctuation(String, Result) :-
    sub_string(String, Before, 1, After, "."),
    sub_string(String, 0, Before, _, BeforePart),
    sub_string(String, _, After, 0, AfterPart),
    split_string_punctuation(AfterPart, AfterSplit),
    append([[BeforePart, "."], AfterSplit], Result).


% Split the sentence into list
split_with_two_separators(String, Result) :-
    split_string(String, "\s", "\s", Parts1),
    separate_punctuation(Parts1,SplitList),
    exclude(=( ""), SplitList, Result).



    
%convert the list into string
convert_items_to_strings([], []).
convert_items_to_strings([Item|Rest], [StringItem|StringList]) :-
    %term_string(Item, StringItem), 
    atom_string(Item, StringItem), 
    convert_items_to_strings(Rest, StringList).

% Convert each string in a nested list to an atom
convert_to_atoms([], []).
convert_to_atoms([StrList|StrLists], [AtomList|AtomLists]) :-
    convert_list(StrList, AtomList), 
    convert_to_atoms(StrLists, AtomLists). 

%convert the list of stings to atom
convert_strings_to_atoms([], []).
convert_strings_to_atoms([String|RestStrings], [Atom|RestAtoms]) :-
    atom_string(Atom, String),
    convert_strings_to_atoms(RestStrings, RestAtoms).

% Helper predicate to convert each string in a list to an atom
convert_list([], []). 
convert_list([Str|Strs], [Atom|Atoms]) :-
    atom_string(Atom, Str),
    convert_list(Strs, Atoms).

% Read from file
read_file(Stream,[]) :- 
    at_end_of_stream(Stream),!.

read_file(Stream,[[Line,Converted_Atom_List]|L]) :-
   %\+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    string_codes(String_Line,Line),
	split_with_two_separators(String_Line,String_List),
    convert_items_to_strings(String_List,Converted_String_List),
    convert_strings_to_atoms(Converted_String_List,Converted_Atom_List),
    read_file(Stream,L). 

