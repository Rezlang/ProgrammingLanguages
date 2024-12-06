:- [facts].
:- [helper].

:- dynamic nlp_parse/2.

nlp_parse(LineSplit, Query) :-
    phrase(command(Query), LineSplit).

command([command, TableColumnInfo, CommandOperation]) -->
    get, table_column_info(TableColumnInfo), command_operation(CommandOperation), ['.'].

get --> ['Get'].

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

table_column_detail([all, TableName]) -->
    [all, from], table(TableName).
table_column_detail([Columns, TableName]) -->
    columns(Columns), [from], table(TableName).

columns([Column | Rest]) -->
    column(Column), columns_rest(Rest).
columns([Column]) -->
    column(Column).

columns_rest([Column | Rest]) -->
    and_or_comma, column(Column), columns_rest(Rest).
columns_rest([Column]) -->
    and_or_comma, column(Column).
columns_rest([]) --> [].

column(ColumnName) -->
    [ColumnName], { is_column(ColumnName) }.

table(TableName) -->
    [TableName], { is_table(TableName) }.

command_operation(CommandOperation) -->
    join_operation(CommandOperation).
command_operation(CommandOperation) -->
    match_operation(CommandOperation).
command_operation(CommandOperation) -->
    where_operation(CommandOperation).
command_operation([]) --> [].

join_operation([join, TableName, ColumnName]) -->
    [linking], table(TableName), [by, their], column(ColumnName).

join_operation([join, TableName, ColumnName]) -->
    [connecting], table(TableName), [by, their], column(ColumnName).

match_operation(MatchOperation) -->
    [such, that], match_condition(MatchOperation).

match_condition([matches, Values]) -->
    [its, values, are, either], values(Values).

match_condition([matches, ColumnName1, SubQuery]) -->
    column(ColumnName1), [matches, values, within, the], column(ColumnName2), [in], table(TableName), where_operation(WhereOperation),
    { SubQuery = [command, [[ColumnName2, TableName]], WhereOperation] }.

match_condition([matches, ColumnName1, SubQuery]) -->
    column(ColumnName1), [matches, values, within, the], column(ColumnName2), [in], table(TableName),
    { SubQuery = [command, [[ColumnName2, TableName]], []] }.

values([Value | Rest]) -->
    value(Value), values_rest(Rest).
values([Value]) -->
    value(Value).

values_rest([Value | Rest]) -->
    or_comma, value(Value), values_rest(Rest).
values_rest([Value]) -->
    or_comma, value(Value).
values_rest([]) --> [].

value(Value) -->
    [Value].

where_operation([where, WhereCondition]) -->
    [where], where_condition(WhereCondition).

where_condition(Condition) -->
    condition(Condition).
where_condition([or, Condition1, Condition2]) -->
    [either], condition(Condition1), [or], condition(Condition2).
where_condition([and, Condition1, RestConditions]) -->
    condition(Condition1), [and], where_condition(RestConditions).

where_condition([and, Condition1, Condition2]) -->
    condition(Condition1), [and], condition(Condition2).

condition([condition, ColumnName, Equality, Value]) -->
    column(ColumnName), equality(Equality), value(Value).

equality('<') --> [is, less, than].
equality('>') --> [is, greater, than].
equality('=') --> [equals].

and_or_comma --> [and].
and_or_comma --> [','].

or_comma --> [or].
or_comma --> [','].

is_table(TableName) :-
    atom(TableName),
    table(TableName, _).

is_column(ColumnName) :-
    atom(ColumnName),
    table(_, Columns),
    member(ColumnName, Columns).

evaluate_logical([command, TableColumnInfos, CommandOperation], FilteredTables) :-
    findall(
        [TableName, ColumnsToSelect, FilteredSelectedRows],
        (
            member(TableInfo, TableColumnInfos),
            process_table_info(TableInfo, CommandOperation, [TableName, ColumnsToSelect, FilteredSelectedRows])
        ),
        FilteredTables
    ).

process_table_info(TableInfo, CommandOperation, [TableName, ColumnsToSelect, FilteredSelectedRows]) :-
    get_table_info(TableInfo, TableName, ColumnsToSelect),
    get_table_data(TableName, AllColumns, AllRows),
    apply_conditions(TableName, AllColumns, CommandOperation, AllRows, FilteredRows),
    select_columns_from_rows(FilteredRows, AllColumns, ColumnsToSelect, FilteredSelectedRows).

get_table_info([all, TableName], TableName, ColumnsToSelect) :-
    table(TableName, ColumnsToSelect).
get_table_info([Columns, TableName], TableName, ColumnsToSelect) :-
    ColumnsToSelect = Columns.

get_table_data(TableName, AllColumns, AllRows) :-
    table(TableName, AllColumns),
    findall(Row, row(TableName, Row), AllRows).

apply_conditions(_, _, [], AllRows, AllRows).
apply_conditions(_, _, [join|_], _, []).
apply_conditions(_, _, [matches|_], _, []).
apply_conditions(TableName, AllColumns, [where, Conditions], AllRows, FilteredRows) :-
    include(row_satisfies_condition(TableName, AllColumns, Conditions), AllRows, FilteredRows).

row_satisfies_condition(_, AllColumns, [condition, ColumnName, Equality, Value], Row) :-
    column_value(AllColumns, ColumnName, Row, CellValue),
    parse_value(CellValue, ParsedCellValue),
    parse_value(Value, ParsedValue),
    compare_values_by_type(ParsedCellValue, Equality, ParsedValue).

row_satisfies_condition(TableName, AllColumns, [and, Cond1, Cond2], Row) :-
    row_satisfies_condition(TableName, AllColumns, Cond1, Row),
    row_satisfies_condition(TableName, AllColumns, Cond2, Row).

row_satisfies_condition(TableName, AllColumns, [or, Cond1, Cond2], Row) :-
    ( row_satisfies_condition(TableName, AllColumns, Cond1, Row)
    ; row_satisfies_condition(TableName, AllColumns, Cond2, Row)
    ).

column_value(AllColumns, ColumnName, Row, CellValue) :-
    nth0(Index, AllColumns, ColumnName),
    nth0(Index, Row, CellValue).

parse_value(Value, ParsedValue) :-
    ( number(Value) ->
        ParsedValue = Value
    ; atom(Value) ->
        ( atom_string(Value, StringValue),
          ( is_date(StringValue, ParsedDate) ->
              ParsedValue = ParsedDate
          ; number_string(ParsedNumber, StringValue) ->
              ParsedValue = ParsedNumber
          ; ParsedValue = Value
          )
        )
    ; ParsedValue = Value
    ).

compare_values_by_type(Value1, Equality, Value2) :-
    ( number(Value1), number(Value2) ->
        compare_numbers(Value1, Equality, Value2)
    ; Value1 = date(_, _, _), Value2 = date(_, _, _) ->
        compare_dates(Value1, Equality, Value2)
    ; atom(Value1), atom(Value2), Equality = '=' ->
        Value1 = Value2
    ; fail
    ).

compare_numbers(Value1, '=', Value2) :- Value1 =:= Value2.
compare_numbers(Value1, '<', Value2) :- Value1 < Value2.
compare_numbers(Value1, '>', Value2) :- Value1 > Value2.

compare_dates(Date1, Equality, Date2) :-
    date_time_stamp(Date1, Stamp1),
    date_time_stamp(Date2, Stamp2),
    compare_numbers(Stamp1, Equality, Stamp2).

select_columns_from_rows(Rows, AllColumns, ColumnsToSelect, SelectedRows) :-
    find_column_indices(ColumnsToSelect, AllColumns, Indices),
    maplist(select_columns_from_row(Indices), Rows, SelectedRows).

find_column_indices(ColumnsToSelect, AllColumns, Indices) :-
    maplist(column_index(AllColumns), ColumnsToSelect, Indices).

column_index(AllColumns, ColumnName, Index) :-
    nth0(Index, AllColumns, ColumnName).

select_columns_from_row(Indices, Row, SelectedRow) :-
    maplist(nth0_in_row(Row), Indices, SelectedRow).

nth0_in_row(Row, Index, Value) :-
    nth0(Index, Row, Value).

parse_and_evaluate(_,[], []).
parse_and_evaluate(part1,[[_,LineSplit]|T], [Query|ResultTail]):- 
                nlp_parse(LineSplit,Query),
                write(Query),nl,
                parse_and_evaluate(part1,T,ResultTail).
                
parse_and_evaluate(part2,[[Line,LineSplit]|T], [_|ResultTail]):- 
                write(Line),nl,
                nlp_parse(LineSplit,Query),
                evaluate_logical(Query,FilteredTable),
                print_tables(FilteredTable),
                parse_and_evaluate(part2,T,ResultTail).

main :-
    current_prolog_flag(argv, [DataFile, PrintOption|_]),
    open(DataFile, read, Stream),
    read_file(Stream,Lines), 
    close(Stream),
	parse_and_evaluate(PrintOption,Lines,_).
