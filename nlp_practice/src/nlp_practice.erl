-module(nlp_practice).

-export([word_freq/1, sentiment/1, stem_suffix/1]).

% Split each word in the string first by any punctuation or space, make sure 
% Text comes from list of Erlang strings, and returns a list with the split words
tokenize(Text) when is_list(Text) ->
    re:split(Text, "[ ,.!?:;\"()\\[\\]{}]+", [{return, list}]). 

% Count word frequency using the tokens generated from the previous function
% foldl takes a word, and an accumulator
% update_with tak
word_freq(Text) ->
    Tokens = tokenize(Text),
    lists:foldl(fun(Word, Acc) -> 
        maps:update_with(Word, fun(N) -> N + 1 end, 1, Acc) 
    end, #{}, Tokens).

% Analyze the sentiment of the text by searching for positive, neutral, negative words
sentiment(Text) ->
    PositiveWords = ["happy", "good", "great", "excited"],
    NeutralWords = ["okay", "so-so", "fifty-fifty", "not bad"],
    NegativeWords = ["sad", "depressed", "not today"],
    Tokens = tokenize(Text),
    Score = lists:foldl(fun(Word, Acc) -> % Checks to see if the word is a member of any of the lists, and gives a score
        case lists:member(Word, PositiveWords) of
            true -> Acc + 1;
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


% Allows for root word analysis by removing the suffix
stem_suffix(Text) ->
    Tokens = tokenize(Text),
    lists:foldl(fun(Word) ->
        case lists:reverse(Word) of % removes the end, and returns the rest
            "gni" ++ Rest -> lists:reverse(Rest);
            "de" ++ Rest -> lists:reverse(Rest);
            "s" ++ Rest -> lists:reverse(Rest);
            "yl" ++ Rest -> lists:reverse(Rest);
            _ -> Word % if none of these suffixes are present, return the word as is
        end
    end, Tokens).

stem_prefix(Text) ->
    Tokens = tokenize(Text),
    lists:foldl(fun(Word) ->
        

)

    
    

