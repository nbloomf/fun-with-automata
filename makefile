all: CA.lhs
	@pandoc --mathjax -s -H docs/style.txt -o docs/CA.html $<
