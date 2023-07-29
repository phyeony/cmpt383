-module(exers).
-export([divisors/1, primes/1, join/2, pythagorean/1, merge/2, mergeSort/1, isPrimeDay/1, isFriday/1, main/0]).

% You can run any functions that are exported with exers:<functionName>(arg) e.g. exers:divisors(24).
main() ->
    D = divisors(30), % Runs divisors function
    io:format("Divisors: ~p~n", [D]),

    P = primes(7),
    io:format("Primes: ~p~n", [P]),

    PY = pythagorean(10),
    io:format("Pythagorean Triples: ~p~n", [PY]),

    Result = join(", ", ["one", "two", "three"]),
    io:format("Join Result: ~p~n", [Result]),

    MergeResult = merge([4,5,7,8], [1,2,3,6,9]),
    io:format("Merge Result: ~p~n", [MergeResult]),

    MergeSortResult = mergeSort([6,2,4,8,9,5,3,1,7,10]),
    io:format("Merge Sort Result: ~p~n", [MergeSortResult]),

    % Should return true
    IsPrimeDayTrue = isPrimeDay(calendar:date_to_gregorian_days({2018, 5, 13})),
    io:format("isPrimeDay Result should be true: ~p~n", [IsPrimeDayTrue]),

    % Should return false
    IsPrimeDayFalse = isPrimeDay(calendar:date_to_gregorian_days({2018, 5, 14})),
    io:format("isPrimeDay Result should be false: ~p~n", [IsPrimeDayFalse]),

    % Should return true
    IsFridayTrue = isFriday(calendar:date_to_gregorian_days({2018, 5, 18})),
    io:format("isFriday Result should be true: ~p~n", [IsFridayTrue]),

    % Should return false
    IsFridayFalse = isFriday(calendar:date_to_gregorian_days({2018, 5, 17})),
    io:format("isFriday Result should be false: ~p~n", [IsFridayFalse]).


    

% Exercise 2: the divisors, primes, join, and pythagorean functions.
divisors(N) ->
    [I || I <- lists:seq(2, N div 2), N rem I =:= 0 ].

primes(N) -> 
    [I || I <- lists:seq(2, N), divisors(I) =:= []].
    
join(_, []) -> "";
join(_,[Y]) -> Y;
join(X,[Y|YS]) -> Y ++ X ++ join(X, YS).

pythagorean(N) ->
    [{A, B, C}|| C <- lists:seq(1,N), A<-lists:seq(1,N), B <-lists:seq(1,N), A*A + B*B =:= C*C, A<B, B<C].

% Exercise 3: mergesort (and thus probably also merge).
% For mergesort, be as general as you can with one implementation in your language
% : don't worry about being able to sort every type if that's difficult.

% Merging
% x, y are already sorted lists
merge([], []) -> [];
merge(Xs, []) -> Xs;
merge([], Ys) -> Ys;
merge([X|Xs], [Y|Ys]) when X < Y -> [X | merge(Xs,[Y|Ys])];
merge([X|Xs], [Y|Ys]) -> [Y | merge([X|Xs], Ys)].

mergeSort([]) -> [];
mergeSort([X]) -> [X];
mergeSort(N) -> 
    {ListA, ListB} = lists:split(length(N) div 2, N),
    merge(mergeSort(ListA),mergeSort(ListB)).


% Also Exercise 3: isPrimeDay and isFriday, using whatever date types are available in your language
isPrimeDay(D) ->
    {_,_,Day} = calendar:gregorian_days_to_date(D),
    divisors(Day) =:= [].

% monday == 1, tuesday==2,....,friday ==5
isFriday(D) ->
    Date = calendar:gregorian_days_to_date(D),
    calendar:day_of_the_week(Date) =:= 5.
