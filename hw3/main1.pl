:- [facts].
:- [helper].


% DCG Rules for the grammar
:- dynamic nlp_parse/2.

% Start rule
nlp_parse(LineSplit, Query) :-
    % write('Parsing LineSplit: '), writeln(LineSplit),
    % write('Generating into Query: '), writeln(Query),
    % open('test.log', write, Stream),
    % set_prolog_IO(Stream, user_input, user_error).
    % trace,
    phrase(command(Query), LineSplit).
    % notrace.
    % close('test.log')
    % write('Generated Query: '), writeln(Query).
% DCG rules

% <command>::= Get <table column info> <command operation>.
command([command, TableColumnInfo, CommandOperation]) -->
    get, table_column_info(TableColumnInfo), command_operation(CommandOperation), ['.'].

% Get keyword
get --> ['Get'].

% <table column info>::= <table column detail> { and|, <table column info> }.
table_column_info([TableColumnDetail | Rest]) -->
    table_column_detail(TableColumnDetail), table_column_info_rest(Rest).
table_column_info([TableColumnDetail]) -->
    table_column_detail(TableColumnDetail).

table_column_info_rest([TableColumnDetail | Rest]) -->
    and_or_comma, table_column_detail(TableColumnDetail), table_column_info_rest(Rest).
table_column_info_rest([TableColumnDetail]) -->
    and_or_comma, table_column_detail(TableColumnDetail).
table_column_info_rest([]) --> [].

period --> ['.'].

% <table column detail>::= all from <table> | <columns> from <table>
table_column_detail([all, TableName]) -->
    [all, from], table(TableName).
table_column_detail([Columns, TableName]) -->
    columns(Columns), [from], table(TableName).

% <columns>::= <col> { and|, <columns> }
columns([Column | Rest]) -->
    column(Column), columns_rest(Rest).
columns([Column]) -->
    column(Column).

columns_rest([Column | Rest]) -->
    and_or_comma, column(Column), columns_rest(Rest).
columns_rest([Column]) -->
    and_or_comma, column(Column).
columns_rest([]) --> [].

% <col>::= <atom>
column(ColumnName) -->
    [ColumnName], { is_column(ColumnName) }.

% <table>::= <atom>
table(TableName) -->
    [TableName], { is_table(TableName) }.

% <command operation>::= <join operation> <match operation> <where operation>
command_operation(CommandOperation) -->
    join_operation(CommandOperation).
command_operation(CommandOperation) -->
    match_operation(CommandOperation).
command_operation(CommandOperation) -->
    where_operation(CommandOperation).
command_operation([]) --> [].

% <join operation> ::= linking <table> by their <col>
join_operation([join, TableName, ColumnName]) -->
    [linking], table(TableName), [by, their], column(ColumnName).

join_operation([join, TableName, ColumnName]) -->
    [connecting], table(TableName), [by, their], column(ColumnName).

% <match operation> ::= such that <match condition>
match_operation(MatchOperation) -->
    [such, that], match_condition(MatchOperation).

% <match condition>::= its values are either <values>
match_condition([matches, Values]) -->
    [its, values, are, either], values(Values).

% <match condition>::= <col> matches values within the <col> in <table> <where operation>
match_condition([matches, ColumnName1, SubQuery]) -->
    column(ColumnName1), [matches, values, within, the], column(ColumnName2), [in], table(TableName), where_operation(WhereOperation),
    { SubQuery = [command, [[ColumnName2, TableName]], WhereOperation] }.

match_condition([matches, ColumnName1, SubQuery]) -->
    column(ColumnName1), [matches, values, within, the], column(ColumnName2), [in], table(TableName),
    { SubQuery = [command, [[ColumnName2, TableName]], []] }.

% <values>::= <val> { or|, <values> }
values([Value | Rest]) -->
    value(Value), values_rest(Rest).
values([Value]) -->
    value(Value).

values_rest([Value | Rest]) -->
    or_comma, value(Value), values_rest(Rest).
values_rest([Value]) -->
    or_comma, value(Value).
values_rest([]) --> [].

% <value>::= <atom>
value(Value) -->
    [Value].

% <where operation>::= where <or condition> { and <or condition> }
where_operation([where, WhereCondition]) -->
    [where], where_condition(WhereCondition).

% <or condition>::= <condition> | either <condition> or <condition>
where_condition(Condition) -->
    condition(Condition).
where_condition([or, Condition1, Condition2]) -->
    [either], condition(Condition1), [or], condition(Condition2).
where_condition([and, Condition1, RestConditions]) -->
    condition(Condition1), [and], where_condition(RestConditions).

% For multiple 'and's, ensure right associativity
where_condition([and, Condition1, Condition2]) -->
    condition(Condition1), [and], condition(Condition2).

% <condition>::= <col> <equality> <val>
condition([condition, ColumnName, Equality, Value]) -->
    column(ColumnName), equality(Equality), value(Value).

% <equality>::= is less than | is greater than | equals
equality('<') --> [is, less, than].
equality('>') --> [is, greater, than].
equality('=') --> [equals].

% Utility predicates

% and_or_comma ::= and | ,
and_or_comma --> [and].
and_or_comma --> [','].

% or_comma ::= or | ,
or_comma --> [or].
or_comma --> [','].

% Check if a table exists
is_table(TableName) :-
    atom(TableName),
    table(TableName, _).

% Check if a column exists in any table
is_column(ColumnName) :-
    atom(ColumnName),
    table(_, Columns),
    member(ColumnName, Columns).


% evaluate_logical(Query,FilteredTable):- fail.
evaluate_logical(_,_):- fail.

% Parse individual commands and evaluate
parse_and_evaluate(_,[], []).
parse_and_evaluate(part1,[[_,LineSplit]|T], [Query|ResultTail]):- 
                nlp_parse(LineSplit,Query),
                write(Query),nl,
                parse_and_evaluate(part1,T,ResultTail).
                
% parse_and_evaluate(part2,[[Line,LineSplit]|T], [Result|ResultTail]):- 
parse_and_evaluate(part2,[[Line,LineSplit]|T], [_|ResultTail]):- 
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