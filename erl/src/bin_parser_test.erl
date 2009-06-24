-module(bin_parser_test).
-include_lib("eunit/include/eunit.hrl").

small_int_test() -> 
    test_single_term(1).

int_test() -> 
    test_single_term(300).

small_big_test() ->
    test_single_term(6898375242683851459).

tuple_test() ->
    [ test_single_term({}),
      test_single_term({1}),
      test_single_term({1,2}),
      test_single_term({1,2,3}),
      test_single_term({1,{2,3}}),
      test_single_term({{1,2},3})
      ].

list_test() ->
    [ test_single_term([]),
      test_single_term([1]),
      test_single_term([1,2]),
      test_single_term([1,2,3]),
      test_single_term([1,[2,3]]),
      test_single_term([[1,2],3])
      ].

tuple_list_combo_test() ->
    [ test_single_term([1,{2,3}]),
      test_single_term([{1,2},3]),
      test_single_term({1,[2,3]}),
      test_single_term({[1,2],3})
      ].

partial_seperator_test() -> test_partial(<<131>>).

partial_small_int_test() -> test_partial(<<131,97>>).

partial_int_test() -> 
    [ test_partial(<<131,98>>),
      test_partial(<<131,98,0>>),
      test_partial(<<131,98,0,0>>),
      test_partial(<<131,98,0,0,0>>)
      ]. 

partial_small_big_test() ->
    [ 
      test_partial(<<131,110>>),
      test_partial(<<131,110,8>>),
      test_partial(<<131,110,8,0>>),
      test_partial(<<131,110,8,0,195>>),
      test_partial(<<131,110,8,0,195,34>>),
      test_partial(<<131,110,8,0,195,34,133>>),
      test_partial(<<131,110,8,0,195,34,133,235>>),
      test_partial(<<131,110,8,0,195,34,133,235,187>>),
      test_partial(<<131,110,8,0,195,34,133,235,187,243>>),
      test_partial(<<131,110,8,0,195,34,133,235,187,243,187>>)
     ].

partial_atom_test() ->
    [ test_partial(<<131,100>>),
      test_partial(<<131,100,0>>),
      test_partial(<<131,100,0,3>>),
      test_partial(<<131,100,0,3,97>>),
      test_partial(<<131,100,0,3,97,98>>)
      ].

partial_small_tuple_test() ->
    [ test_partial(<<131,104>>),
      test_partial(<<131,104,3>>),
      test_partial(<<131,104,3,97>>),
      test_partial(<<131,104,3,97,1>>),
      test_partial(<<131,104,3,97,1,97>>),
      test_partial(<<131,104,3,97,1,97,2>>),
      test_partial(<<131,104,3,97,1,97,2,97>>)
      ].

partial_string_test() ->
    [ test_partial(<<131,107>>),
      test_partial(<<131,107,0>>),
      test_partial(<<131,107,0,3>>),
      test_partial(<<131,107,0,3,97>>),
      test_partial(<<131,107,0,3,97,98>>)
     ].
	
partial_list_test() ->	   
    [ test_partial(<<131,108>>),
      test_partial(<<131,108,0>>),
      test_partial(<<131,108,0,0>>),
      test_partial(<<131,108,0,0,0>>),
      test_partial(<<131,108,0,0,0,3>>),
      test_partial(<<131,108,0,0,0,3,97>>),
      test_partial(<<131,108,0,0,0,3,97,1>>),
      test_partial(<<131,108,0,0,0,3,97,1,97>>),
      test_partial(<<131,108,0,0,0,3,97,1,97,2>>),
      test_partial(<<131,108,0,0,0,3,97,1,97,2,97>>)
      ].

random_test() ->
    [ 
%      {ok,[{{3,5},[1,1,1,1,1,1,1]}],<<>>} = bin_parser:parse_binary(<<16#83,16#6c,16#00,16#00,16#00,16#01,16#68,16#02,16#68,16#02,16#61,16#03,16#61,16#05,16#6b,16#00,16#07,16#01,16#01,16#01,16#01,16#01,16#01,16#01,16#6a>>),
%      test_single_term({6898375242683851459,10})
      ]. 
%% utility

test_single_term(Term) ->
    io:format("test ~w\n",[Term]),
    { ok, Term, Data } = bin_parser:parse_binary(term_to_binary(Term)),
    <<>> = Data.

test_partial(Binary) ->
    io:format("test ~w\n",[Binary]),
    { partial, Binary } = bin_parser:parse_binary(Binary).
     



