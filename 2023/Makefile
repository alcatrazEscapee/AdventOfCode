
EXPECTED := ./inputs/expected.txt
ACTUAL   := ./inputs/actual.txt

DAYS_COR   := $(shell find src -name 'day\*.cor')
DAYS_COR_N := $(DAYS_COR:src/day%.cor=%)

DAYS_HS    := $(shell find src -name 'day\*.hs')
DAYS_HS_N  := $(DAYS_HS:src/day%.hs=%)
DAYS_HS_O  := $(DAYS_HS:src/%=out/%.o)

DAYS_CPP   := $(shell find src -name 'day\*.cpp')
DAYS_CPP_N := $(DAYS_CPP:src/day%.cpp=%)
DAYS_CPP_O := $(DAYS_CPP:src/%=out/%.o)

.DEFAULT_GOAL := run

MAKE_DIRS = @mkdir -p $(@D)

CORDY := cordy -o
GHC   := ghc -O2 --make -odir out -hidir out
CPP   := g++ -std=c++20 -lstdc++ -Wall -Werror -O3

FORCE :

.PHONY : run
run : FORCE
	@$(CORDY) src/day$(day).cor

.PHONY : test
test : $(ACTUAL)
	@diff --strip-trailing-cr $(EXPECTED) $(ACTUAL) >> /dev/null || ( \
		printf "\n=== Tests Failed! ===\n\n" && \
		diff --strip-trailing-cr -u $(EXPECTED) $(ACTUAL))
	@printf "\n=== Tests Passed! ===\n"

$(ACTUAL) : $(DAYS_COR) $(DAYS_HS_O) $(DAYS_CPP_O) FORCE
	@printf "Running tests (Cordy) ...\n\n"
	@printf "Advent of Code 2016\n" > $(ACTUAL)
	@for day in $(DAYS_COR_N) ; do \
		printf "  Day $$day: " ; \
		printf "Day $$day\n" >> $(ACTUAL) ; \
		TIMEFORMAT='%3lR' ; \
		time $(CORDY) src/day$$day.cor >> $(ACTUAL) ; \
	done
	@printf "\nRunning tests (Haskell) ...\n\n"
	@for day in $(DAYS_HS_N) ; do \
		printf "  Day $$day: " ; \
		printf "Haskell Day $$day\n" >> $(ACTUAL) ; \
		TIMEFORMAT='%3lR' ; \
		time cat ./inputs/day$$day.txt | ./out/day$$day.hs.o >> $(ACTUAL) ; \
	done
	@printf "\nRunning tests (C++) ...\n\n"
	@for day in $(DAYS_CPP_N) ; do \
		printf "  Day $$day: " ; \
		printf "C++ Day $$day\n" >> $(ACTUAL) ; \
		TIMEFORMAT='%3lR' ; \
		time cat ./inputs/day$$day.txt | ./out/day$$day.cpp.o >> $(ACTUAL) ; \
	done


# Haskell
.PHONY : haskell
haskell : out/day$(day).hs.o
	@cat ./inputs/day$(day).txt | ./out/day$(day).hs.o

out/%.hs.o : src/%.hs
	$(MAKE_DIRS)
	-@rm -f out/Main.hi
	-@rm -f out/Main.o
	@$(GHC) -o $@ $<

# C++
.PHONY : cpp
cpp : out/day$(day).cpp.o
	@cat ./inputs/day$(day).txt | ./out/day$(day).cpp.o

out/%.cpp.o : src/%.cpp
	$(MAKE_DIRS)
	-@printf "Compiling $< ...\n"
	@$(CPP) -o $@ $<