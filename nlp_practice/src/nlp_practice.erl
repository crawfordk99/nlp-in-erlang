-module(nlp_practice).

-export([word_freq/1, sentiment_advice/1, stem/1, pos_tag/1]).

% Split each word in the string first by any punctuation or space, make sure 
% Text comes from list of Erlang strings, and returns a list with the split words
tokenize(Text) when is_list(Text) ->
    re:split(Text, "[ ,.!?:;\"()\\[\\]{}]+", [{return, list}]). 

% Count word frequency using the tokens generated from the previous function
% foldl takes a word, and an accumulator, and updates a map with the word as the key
% and the Acc as the value to keep track of frequency

word_freq(Text) ->
    Tokens = tokenize(Text), % tokenize the text first into a list of words
    lists:foldl(fun(Word, Acc) -> 
        maps:update_with(Word, fun(N) -> N + 1 end, 1, Acc) % Add 1 to the count for the word
    end, #{}, Tokens). % Word => Acc is how the map looks

% Analyze the sentiment of the text by searching for positive, neutral, negative words
sentiment(Text) ->
    PositiveWords = ["happy", "good", "great", "excited"],
    NeutralWords = ["okay", "so-so", "fifty-fifty", "not bad"],
    NegativeWords = ["sad", "depressed", "not today"],
    Tokens = tokenize(Text),
    Score = lists:foldl(fun(Word, Acc) -> % Checks to see if the word is a member of any of the lists, and gives a score
        case lists:member(Word, PositiveWords) of % Checks to see if the word is a member of any of the lists
            true -> Acc + 1; % Gives it a value if it is to the acc
            false -> 
                case lists:member(Word, NeutralWords) of
                    true -> Acc + 0.5;
                    false -> 
                        case lists:member(Word, NegativeWords) of
                            true -> Acc - 1;
                            false -> Acc
                        end
                end
        end
    end, 0, Tokens), % Returns the list of Tokens after being searched through
    case Score of % Checks the score and gives an output
        N when N > 1 -> positive;
        N when N >= 0 andalso N =< 1  -> neutral; % andalso makes sure the right half is only evaluated if the left is true
        N when N < 0 -> negative
    end.

% Generate a response based on the analysis of the text
sentiment_advice(Text) ->
    PositiveAdvice = ["Keep doing what you're doing!"],
    NeutralAdvice = ["Life is what you make it"],
    SadAdvice = ["If you need a shoulder to cry on, I'm here."],
    case sentiment(Text) of
        positive -> io:format("~p~n", PositiveAdvice);
        neutral -> io:format("~p~n", NeutralAdvice);
        negative -> io:format("~p~n", SadAdvice)
    end.



% Allows for root word analysis by removing the suffix
stem_suffix(Word) ->
    case lists:reverse(Word) of % removes the end, and returns the rest
        "gni" ++ Rest -> lists:reverse(Rest);
        "de" ++ Rest -> lists:reverse(Rest);
        "s" ++ Rest -> lists:reverse(Rest);
        "yl" ++ Rest -> lists:reverse(Rest);
        _ -> Word % if none of these suffixes are present, return the word as is
    end.
    


% Allows for root word analysis by removing the prefix
stem_prefix(Word) ->
    case Word of
        "pre" ++ Rest -> Rest;
        "un" ++ Rest -> Rest;
        "im" ++ Rest -> Rest;
        _ -> Word % if none of these prefixes are present, return the word as is
    end.

% Finds the stem by using the stem_suffix, and stem_prefix functions to remove them
stem(Text) ->
    Tokens = tokenize(Text),
    lists:map(fun(Word) -> 
        stem_suffix(stem_prefix(Word)),
        io:format("The stem of the word is: ~p~n", [Word]) % Prints each stem after being processed to the console
    end, Tokens).


% Simple dictionary for POS tagging
pos_dict() -> 
    #{
        "cat" => noun, "dog" => noun, "apple" => noun, "Erlang" => noun,
        "run" => verb, "jumps" => verb, "eats" => verb,
        "happy" => adj, "fast" => adj, "red" => adj,
        "quickly" => adv, "slowly" => adv
    }.

% POS tagging function, similar to word_freq, except instead of an accumulator, it's checking
% to see if the word is in the pos dictionary, and then passing the value if it is in there
pos_tag(Text) ->
    Tokens = tokenize(Text),  
    lists:map(fun(Word) -> {Word, maps:get(Word, pos_dict(), unknown)} end, Tokens).

    -ifdef(EUNIT).
%
% Unit tests go here. 
%

-include_lib("eunit/include/eunit.hrl").

% This is how you would write a basic unit test for lists. _assert checks the type, and
% _assertEqual checks if it's returning the right values from the function
word_freq_test_() ->
    Tokens = tokenize("Hello Hello I am Hello"),
    [?_assert(is_list(Tokens)),
    ?_assertEqual(8, length(Tokens)),
    ?'_assertEqual'(#{"Hello" => 3, "I" => 1, "am" => 1}, Expr)
    ].
    
    
-endif
