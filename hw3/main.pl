:- [facts].
:- [helper].

:- dynamic nlp_parse/2.

% The main predicate to parse a natural language query into a structured representation.
nlp_parse(LineSplit, Query) :-
    phrase(command(Query), LineSplit).

% Top-level rule for parsing a command, which consists of the keywords "Get",
% table/column information, a command operation, and ends with a period.
command([command, TableColumnInfo, CommandOperation]) -->
    get, table_column_info(TableColumnInfo), command_operation(CommandOperation), ['.'].

% Keyword "Get" at the start of a command.
get --> ['Get'].

% Parse table and column information, which could be one or more table/column details.
table_column_info([TableColumnDetail | Rest]) -->
    table_column_detail(TableColumnDetail), table_column_info_rest(Rest).
table_column_info([TableColumnDetail]) -->
    table_column_detail(TableColumnDetail).

% Helper for parsing additional table/column details after the first one.
table_column_info_rest([TableColumnDetail | Rest]) -->
    and_or_comma, table_column_detail(TableColumnDetail), table_column_info_rest(Rest).
table_column_info_rest([TableColumnDetail]) -->
    and_or_comma, table_column_detail(TableColumnDetail).
table_column_info_rest([]) --> [].

% Keyword for ending the sentence.
period --> ['.'].

% Handles "all from <table>" or "<columns> from <table>".
table_column_detail([all, TableName]) -->
    [all, from], table(TableName).
table_column_detail([Columns, TableName]) -->
    columns(Columns), [from], table(TableName).

% Parse a list of column names.
columns([Column | Rest]) -->
    column(Column), columns_rest(Rest).
columns([Column]) -->
    column(Column).

% Helper for parsing additional column names.
columns_rest([Column | Rest]) -->
    and_or_comma, column(Column), columns_rest(Rest).
columns_rest([Column]) -->
    and_or_comma, column(Column).
columns_rest([]) --> [].

% Parsing a single column name if it satisfies `is_column/1`.
column(ColumnName) -->
    [ColumnName], { is_column(ColumnName) }.

% Parsing a table name if it satisfies `is_table/1`.
table(TableName) -->
    [TableName], { is_table(TableName) }.

% Command operations: join, match, or where clauses.
command_operation(CommandOperation) -->
    join_operation(CommandOperation).
command_operation(CommandOperation) -->
    match_operation(CommandOperation).
command_operation(CommandOperation) -->
    where_operation(CommandOperation).
command_operation([]) --> [].

% Parsing join operations, e.g., "linking <table> by their <column>".
join_operation([join, TableName, ColumnName]) -->
    [linking], table(TableName), [by, their], column(ColumnName).
join_operation([join, TableName, ColumnName]) -->
    [connecting], table(TableName), [by, their], column(ColumnName).

% Parsing match operations, e.g., "such that its values are either <values>" or column-to-column matches.
match_operation(MatchOperation) -->
    [such, that], match_condition(MatchOperation).

% Match condition with specific values, e.g., "its values are either <value>, <value>".
match_condition([matches, Values]) -->
    [its, values, are, either], values(Values).

% Match condition involving a subquery for column-to-column matches.
match_condition([matches, ColumnName1, SubQuery]) -->
    column(ColumnName1), [matches, values, within, the], column(ColumnName2), [in], table(TableName), where_operation(WhereOperation),
    { SubQuery = [command, [[ColumnName2, TableName]], WhereOperation] }.

% Match condition without a where clause.
match_condition([matches, ColumnName1, SubQuery]) -->
    column(ColumnName1), [matches, values, within, the], column(ColumnName2), [in], table(TableName),
    { SubQuery = [command, [[ColumnName2, TableName]], []] }.

% Parse a list of values, e.g., "value, value".
values([Value | Rest]) -->
    value(Value), values_rest(Rest).
values([Value]) -->
    value(Value).

% Helper for parsing additional values.
values_rest([Value | Rest]) -->
    or_comma, value(Value), values_rest(Rest).
values_rest([Value]) -->
    or_comma, value(Value).
values_rest([]) --> [].

% Parsing a single value.
value(Value) -->
    [Value].

% Parsing where conditions, e.g., "where <condition>".
where_operation([where, WhereCondition]) -->
    [where], where_condition(WhereCondition).

% Logical conditions in a where clause: single, and/or combinations.
where_condition(Condition) -->
    condition(Condition).
where_condition([or, Condition1, Condition2]) -->
    [either], condition(Condition1), [or], condition(Condition2).
where_condition([and, Condition1, RestConditions]) -->
    condition(Condition1), [and], where_condition(RestConditions).
where_condition([and, Condition1, Condition2]) -->
    condition(Condition1), [and], condition(Condition2).

% Single condition in a where clause, e.g., "<column> equals <value>".
condition([condition, ColumnName, Equality, Value]) -->
    column(ColumnName), equality(Equality), value(Value).

% Equality operators: "is less than", "is greater than", "equals".
equality('<') --> [is, less, than].
equality('>') --> [is, greater, than].
equality('=') --> [equals].

% Parsing conjunctions: "and" or ",".
and_or_comma --> [and].
and_or_comma --> [','].

% Parsing disjunctions: "or" or ",".
or_comma --> [or].
or_comma --> [','].

% Validate that a term is a table name.
is_table(TableName) :-
    atom(TableName),
    table(TableName, _).

% Validate that a term is a column name.
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

% Processes information for a single table, extracts its metadata, and filters rows based on conditions.
process_table_info(TableInfo, CommandOperation, [TableName, ColumnsToSelect, FilteredSelectedRows]) :-
    get_table_info(TableInfo, TableName, ColumnsToSelect),
    get_table_data(TableName, AllColumns, AllRows),
    apply_conditions(TableName, AllColumns, CommandOperation, AllRows, FilteredRows),
    select_columns_from_rows(FilteredRows, AllColumns, ColumnsToSelect, FilteredSelectedRows).

% Extracts the table name and columns to select from the input table info.
get_table_info([all, TableName], TableName, ColumnsToSelect) :-
    table(TableName, ColumnsToSelect). % Select all columns if 'all' is specified.
get_table_info([Columns, TableName], TableName, ColumnsToSelect) :-
    ColumnsToSelect = Columns. % Select specified columns.

% Retrieves all column names and row data for a given table from the database.
get_table_data(TableName, AllColumns, AllRows) :-
    table(TableName, AllColumns), % Fetch table metadata (columns).
    findall(Row, row(TableName, Row), AllRows). % Fetch all rows.

% Applies filtering conditions to rows based on the specified operations.
apply_conditions(_, _, [], AllRows, AllRows). % No filtering conditions, return all rows.
apply_conditions(_, _, [join|_], _, []). 
apply_conditions(_, _, [matches|_], _, []).
apply_conditions(TableName, AllColumns, [where, Conditions], AllRows, FilteredRows) :-
    include(row_satisfies_condition(TableName, AllColumns, Conditions), AllRows, FilteredRows). % Filter rows that meet conditions.

% Checks if a row satisfies a specific condition or logical expression.
row_satisfies_condition(_, AllColumns, [condition, ColumnName, Equality, Value], Row) :-
    column_value(AllColumns, ColumnName, Row, CellValue), % Extract column value.
    parse_value(CellValue, ParsedCellValue),
    parse_value(Value, ParsedValue),
    compare_values_by_type(ParsedCellValue, Equality, ParsedValue). % Compare values.

% Handles `and` and `or` conditions for logical expressions.
row_satisfies_condition(TableName, AllColumns, [and, Cond1, Cond2], Row) :-
    row_satisfies_condition(TableName, AllColumns, Cond1, Row),
    row_satisfies_condition(TableName, AllColumns, Cond2, Row).
row_satisfies_condition(TableName, AllColumns, [or, Cond1, Cond2], Row) :-
    ( row_satisfies_condition(TableName, AllColumns, Cond1, Row)
    ; row_satisfies_condition(TableName, AllColumns, Cond2, Row)
    ).

% Extracts the value of a specific column from a row.
column_value(AllColumns, ColumnName, Row, CellValue) :-
    nth0(Index, AllColumns, ColumnName), % Find the index of the column.
    nth0(Index, Row, CellValue). % Retrieve the cell value.

% Parses and normalizes values for comparison.
parse_value(Value, ParsedValue) :-
    ( number(Value) -> % Value is a number.
        ParsedValue = Value
    ; atom(Value) -> % Value is an atom, convert to string and handle special cases (dates, numbers).
        ( atom_string(Value, StringValue),
          ( is_date(StringValue, ParsedDate) ->
              ParsedValue = ParsedDate
          ; number_string(ParsedNumber, StringValue) ->
              ParsedValue = ParsedNumber
          ; ParsedValue = Value
          )
        )
    ; ParsedValue = Value % Default fallback.
    ).

% Compares values based on their types and a comparison operator.
compare_values_by_type(Value1, Equality, Value2) :-
    ( number(Value1), number(Value2) -> % Compare numbers.
        compare_numbers(Value1, Equality, Value2)
    ; Value1 = date(_, _, _), Value2 = date(_, _, _) -> % Compare dates.
        compare_dates(Value1, Equality, Value2)
    ; atom(Value1), atom(Value2), Equality = '=' -> % Compare atoms for equality.
        Value1 = Value2
    ; fail % Unsupported comparison.
    ).

% Number and date comparison utilities.
compare_numbers(Value1, '=', Value2) :- Value1 =:= Value2.
compare_numbers(Value1, '<', Value2) :- Value1 < Value2.
compare_numbers(Value1, '>', Value2) :- Value1 > Value2.
compare_dates(Date1, Equality, Date2) :-
    date_time_stamp(Date1, Stamp1),
    date_time_stamp(Date2, Stamp2),
    compare_numbers(Stamp1, Equality, Stamp2).

% Extracts specific columns from a list of rows.
select_columns_from_rows(Rows, AllColumns, ColumnsToSelect, SelectedRows) :-
    find_column_indices(ColumnsToSelect, AllColumns, Indices),
    maplist(select_columns_from_row(Indices), Rows, SelectedRows).

% Finds indices of columns to select.
find_column_indices(ColumnsToSelect, AllColumns, Indices) :-
    maplist(column_index(AllColumns), ColumnsToSelect, Indices).

% Retrieves the index of a column by name.
column_index(AllColumns, ColumnName, Index) :-
    nth0(Index, AllColumns, ColumnName).

% Extracts values of selected columns from a row.
select_columns_from_row(Indices, Row, SelectedRow) :-
    maplist(nth0_in_row(Row), Indices, SelectedRow).

% Utility to retrieve a value from a specific index in a row.
nth0_in_row(Row, Index, Value) :-
    nth0(Index, Row, Value).

parse_and_evaluate(_, [], []).

parse_and_evaluate(part1, [[_, LineSplit] | T], [Query | ResultTail]) :- 
    nlp_parse(LineSplit, Query),
    write(Query), nl, 
    parse_and_evaluate(part1, T, ResultTail).

parse_and_evaluate(part2, [[Line, LineSplit] | T], [_ | ResultTail]) :- 
    write(Line), nl, 
    nlp_parse(LineSplit, Query), 
    evaluate_logical(Query, FilteredTable), 
    print_tables(FilteredTable), 
    parse_and_evaluate(part2, T, ResultTail).

main :-
    current_prolog_flag(argv, [DataFile, PrintOption | _]), 
    open(DataFile, read, Stream), 
    read_file(Stream, Lines),
    close(Stream),
    parse_and_evaluate(PrintOption, Lines, _). 