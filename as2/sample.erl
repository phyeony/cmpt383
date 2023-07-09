-module(sample).
-export([matrix_mult/2, main/0]).
-compile(export_all).

main() ->
    % Matrix Multiplication Example
    MatrixA1 = [[1, 2, 3], [4, 5, 6]], % 2x3
    MatrixB1 = [[7, 8], [9, 10], [11, 12]], % 3x2
    case matrix_mult(MatrixA1, MatrixB1) of
        {ok, Result1} ->
            io:format("Matrix multiplication result 1:~n"),
            print_matrix(Result1);
        {error, Reason1} ->
            io:format("Error: ~s~n", [Reason1])
    end,
    % Expected Result: {ok, [[58, 64], [139, 154]]} % 2x2

    MatrixA2 = [[1, 2], [3, 4], [5, 6]],
    MatrixB2 = [[-1, -2, -3], [-4, -5, -6]],
    case matrix_mult(MatrixA2, MatrixB2) of
        {ok, Result2} ->
            io:format("Matrix multiplication result 2:~n"),
            print_matrix(Result2);
        {error, Reason2} ->
            io:format("Error: ~s~n", [Reason2])
    end,
    % Expected Result: {ok, [[-9, -12, -15], [-19, -26, -33], [-29, -40, -51]]}

    MatrixA3 = [
        [1, 2, 3, 4, 5],
        [6, 7, 8, 9, 10],
        [11, 12, 13, 14, 15],
        [16, 17, 18, 19, 20],
        [21, 22, 23, 24, 25]
    ],
    MatrixB3 = [
        [1, 2, 3, 4, 5],
        [6, 7, 8, 9, 10],
        [11, 12, 13, 14, 15],
        [16, 17, 18, 19, 20],
        [21, 22, 23, 24, 25]
    ],
    case matrix_mult(MatrixA3, MatrixB3) of
        {ok, Result3} ->
            io:format("Matrix multiplication result:~n"),
            print_matrix(Result3);
        {error, Reason3} ->
            io:format("Error: ~s~n", [Reason3])
    end,
    % Matrix multiplication result:
    % [
    % [215    230     245     260     275     ]
    % [490    530     570     610     650     ]
    % [765    830     895     960     1025    ]
    % [1040   1130    1220    1310    1400    ]
    % [1315   1430    1545    1660    1775    ]
    % ]

    % Incorporate processes and sending messages from Erlang
    MatrixA4 = [
        [1, 2, 3, 4, 5],
        [6, 7, 8, 9, 10],
        [11, 12, 13, 14, 15],
        [16, 17, 18, 19, 20],
        [21, 22, 23, 24, 25]
    ],
    MatrixB4 = [
        [1, 2, 3, 4, 5],
        [6, 7, 8, 9, 10],
        [11, 12, 13, 14, 15],
        [16, 17, 18, 19, 20],
        [21, 22, 23, 24, 25]
    ],
    case concurrent_matrix_mult(MatrixA4, MatrixB4) of
        {ok, Result4} ->
            io:format("Matrix multiplication result:~n"),
            print_matrix(Result4);
        {error, Reason4} ->
            io:format("Error: ~s~n", [Reason4])
    end.


% Data Crunching Example
% Matrix multiplication in pure Erlang should be worse as Erlang is not intended to perform a lot of data crunching.
% However, there has been Elixir/Erlang libraries implemented in C under the hood to compensate slow matrix multiplication. 
% https://github.com/versilov/matrex

% Inputs are 2D arrays with number. Returns an atom "ok" with the result or an atom "error" with a string for reason.
% Referred https://en.wikipedia.org/wiki/Matrix_multiplication
-spec matrix_mult([[number()]], [[number()]]) -> {ok, [[number()]]} | {error, string()}.
matrix_mult(MatrixA, MatrixB) ->
    ColumnSizeA = length(hd(MatrixA)),
    RowSizeB = length(MatrixB),
    ColumnSizeB = length(hd(MatrixB)),
    case ColumnSizeA == RowSizeB of
        true ->
            Result = [ computeRow(RowA, MatrixB, ColumnSizeB)  || RowA <- MatrixA ],
            {ok, Result};
        false ->
            {error, "Matrix A column size is not equal to Matrix B row size. Incompatible Matrices."}
    end.


computeRow(RowA, MatrixB, ColumnSizeB) ->
    % List index start with 1.
    [computeCell(RowA, MatrixBColumnIdx, MatrixB) || MatrixBColumnIdx <-  lists:seq(1, ColumnSizeB)].

computeCell(CellA, MatrixBColumnIdx, MatrixB) ->
    % io:format("~w, ", [MatrixB]),
    % io:format("~w, ", [CellA]),
    % io:format("~w, ", [MatrixBColumnIdx]),
    % io:format("~n"),
    {_, NewCell} = lists:foldl(
        fun(RowB, {Index, Acc}) ->
            ElementA = lists:nth(Index, CellA),
            ElementB = lists:nth(MatrixBColumnIdx, RowB),
            {Index + 1, Acc + ElementA * ElementB}
        end,
        {1, 0},
        MatrixB
    ),
    NewCell.

% Function to print a matrix
print_matrix(Matrix) ->
    io:format("[\n"),
    lists:foreach(fun(Row) -> print_row(Row), io:nl() end, Matrix),
    io:format("]\n").

print_row(Row) ->
    io:format("["),
    lists:foreach(fun(Element) -> io:format("~p\t", [Element]) end, Row),
    io:format("]").
    


concurrent_matrix_mult(MatrixA, MatrixB) ->
    ColumnSizeA = length(hd(MatrixA)),
    RowSizeB = length(MatrixB),
    ColumnSizeB = length(hd(MatrixB)),
    case ColumnSizeA == RowSizeB of
        true ->
            Result = [ concurrent_computeRow(RowA, MatrixB, ColumnSizeB)  || RowA <- MatrixA ],
            {ok, Result};
        false ->
            {error, "Matrix A column size is not equal to Matrix B row size. Incompatible Matrices."}
    end.

concurrent_computeRow(RowA, MatrixB, ColumnSizeB) ->
    Pids = [spawn_link(fun() -> concurrent_computeCell(RowA, MatrixBColumnIdx, MatrixB, self()) end) || MatrixBColumnIdx <- lists:seq(1, ColumnSizeB)],
    Results = [receive {Pid, Result} -> Result end || Pid <- Pids],
    print_row([Results]),
    Results.


concurrent_computeCell(CellA, MatrixBColumnIdx, MatrixB, Pid) ->
    {_, NewCell} = lists:foldl(
        fun(RowB, {Index, Acc}) ->
            ElementA = lists:nth(Index, CellA),
            ElementB = lists:nth(MatrixBColumnIdx, RowB),
            {Index + 1, Acc + ElementA * ElementB}
        end,
        {1, 0},
        MatrixB
    ),
    Pid ! {self(), NewCell}.