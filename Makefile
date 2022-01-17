all: pl0 test

%: %.cc
	g++ -std=c++20 -Wall -Wextra -O3 -o $@ $< -lfmt -ggdb
