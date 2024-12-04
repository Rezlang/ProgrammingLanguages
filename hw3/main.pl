:- [facts].
:- [helper].


%replace "fail" for your solution. It is simply a placeholder to avoid binding for the starter code.
nlp_parse(LineSplit,Query):- fail.
evaluate_logical(Query,FilteredTable):- fail.

% Parse individual commands and evaluate
parse_and_evaluate(_,[], []).
parse_and_evaluate(part1,[[_,LineSplit]|T], [Query|ResultTail]):- 
                nlp_parse(LineSplit,Query),
                write(Query),nl,
                parse_and_evaluate(part1,T,ResultTail).
                
parse_and_evaluate(part2,[[Line,LineSplit]|T], [Result|ResultTail]):- 
                write(Line),nl,
                nlp_parse(LineSplit,Query),
                evaluate_logical(Query,FilteredTable),
                %write("\t"),write(FilteredTable),nl,
                print_tables(FilteredTable),
                parse_and_evaluate(part2,T,ResultTail).
% Main 
main :-
    current_prolog_flag(argv, [DataFile, PrintOption|_]),
    open(DataFile, read, Stream),
    read_file(Stream,Lines), %Lines contain individual line within the file split by spaces and special character like (,) and (.) . 
    close(Stream),
	parse_and_evaluate(PrintOption,Lines,_).