# nlp-in-erlang

# Overview

I made a basic natural language processor in erlang to better understand how nlps work. Erlang does not have its own modules for nlp, and so I built a bunch of functions to handle basic nlp actions like tokenizing, sentiment analysis, word frequency, and part-of-speech tagging. This was also my first opportunity to work with a functional programming language. Every function is designed to produce an output from the inputs, and the scope is just within the function. This was actually nice for atleast making basic nlp functions. It helped me to break apart the tasks, and understand what nlp means.

[Software Demo Video](https://youtu.be/WA78GYcscP8)

# Development Environment

Just downloaded the latest erlang language and compiled it through VS code. 

I did import the eunit library to see how tests work in erlang.

# Useful Websites

* [Programming Erlang Book](https://learning.oreilly.com/library/view/programming-erlang-2nd/9781941222454/f_0108.html)
* [Stack Overflow](https://stackoverflow.com/)
* [Erlang Docs](https://www.erlang.org/docs/20/)
* [ChatGPT](https://chatgpt.com)

# Future Work

* Python imports for better processing to handle larger amounts of data
* Implement concurrency to take advantage of erlang's greatest strength
* Implement math processing