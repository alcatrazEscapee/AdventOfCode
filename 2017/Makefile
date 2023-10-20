
EXPECTED := ./expected.txt
ACTUAL   := ./actual.txt

DAYS_COR := $(shell find src -name 'day\*.cor')
DAYS     := $(DAYS_COR:src/day%.cor=%)

.DEFAULT_GOAL := run

CORDY := cordy -o --link day15=out/day15.so
GCC   := gcc -shared -O3

FORCE :

.PHONY : run
run : out/day15.so
	@$(CORDY) src/day$(day).cor

.PHONY : test
test : $(ACTUAL)
	@diff --strip-trailing-cr $(EXPECTED) $(ACTUAL) >> /dev/null || ( \
		printf "\n=== Tests Failed! ===\n\n" && \
		diff --strip-trailing-cr -u $(EXPECTED) $(ACTUAL))
	@printf "\n=== Tests Passed! ===\n"

$(ACTUAL) : $(DAYS_COR) FORCE
	@printf "Running tests ...\n"
	@printf "Advent of Code 2016\n" > $(ACTUAL)
	@for day in $(DAYS) ; do \
		printf "Day $$day: " ; \
		printf "Day $$day\n" >> $(ACTUAL) ; \
		TIMEFORMAT='%3lR' ; \
		time $(CORDY) src/day$$day.cor >> $(ACTUAL) ; \
	done

out/day15.so : src/day15.c src/cordy.h
	@printf "Compiling src/day15.c ...\n"
	@mkdir -p out
	@$(GCC) -o out/day15.so src/day15.c
