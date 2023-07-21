SHELL := /bin/bash

INPUT_DIR  := inputs
OUT_DIR    := out
SRC_DIR    := src
LIB_DIR    := lib

DAYS_TXT   := $(shell find $(INPUT_DIR) -name 'day\*.txt')
DAYS_O     := $(DAYS_TXT:$(INPUT_DIR)/day%.txt=$(OUT_DIR)/day%.o)
DAYS       := $(DAYS_TXT:$(INPUT_DIR)/day%.txt=%)

GCC        := -Wall -Werror -O3

C          := gcc -std=c11 $(GCC)
CPP        := g++ -std=c++17 -lstdc++ $(GCC)

AOC_H      := $(SRC_DIR)/aoc.h
OUT_TXT    := $(OUT_DIR)/test.txt
EXP_TXT    := $(INPUT_DIR)/answers.txt

MAKE_DIRS = @mkdir -p $(@D)

.DEFAULT_GOAL := run
.PRECIOUS : $(INPUT_DIR)/%.txt $(BUILD_DIR)/answers.txt

FORCE :

help :
	@echo "Advent of Code 2018 in C/C++"
	@echo "Put inputs in /inputs/dayXX.txt and expected ouptut in /inputs/answers.txt"
	@echo "  make day=XX  Runs day XX"
	@echo "  make test    Tests each day against expected answers"

.PHONY : clean
clean : FORCE
	rm -rf $(OUT_DIR)

.PHONY : run
run : $(OUT_DIR)/day$(day).o
	-@if [ ! -z "$(day)" ]; then \
		cat $(INPUT_DIR)/day$(day).txt | $(OUT_DIR)/day$(day).o ; \
	fi

.PHONY : test
test : $(DAYS_O) FORCE
	@printf "Running tests ...\n"
	@printf "Advent of Code 2016\n" > $(OUT_TXT)
	@for day in $(DAYS) ; do \
		printf "Day $$day: " ; \
		printf "Day $$day\n" >> $(OUT_TXT) ; \
		TIMEFORMAT='%3lR' ; \
		time cat $(INPUT_DIR)/day$$day.txt | $(OUT_DIR)/day$$day.o >> $(OUT_TXT) ; \
	done
	@diff --strip-trailing-cr $(EXP_TXT) $(OUT_TXT) >> /dev/null || ( \
		printf "\n=== Tests Failed! ===\n\n" && \
		diff --strip-trailing-cr -u $(EXP_TXT) $(OUT_TXT))
	@printf "\n=== Tests Passed! ===\n"

$(OUT_DIR)/day.o : FORCE
	-@printf "No day provided - run make day=XX\n"

$(OUT_DIR)/%.o : $(SRC_DIR)/%.c $(AOC_H)
	$(MAKE_DIRS)
	-@printf "Compiling $< ...\n"
	@$(C) -o $@ $<

$(OUT_DIR)/%.o : $(SRC_DIR)/%.cpp $(AOC_H)
	$(MAKE_DIRS)
	-@printf "Compiling $< ...\n"
	@$(CPP) -o $@ $<

$(OUT_DIR)/day14.o : $(LIB_DIR)/vec.h
$(OUT_DIR)/day15.o : $(LIB_DIR)/point.h
$(OUT_DIR)/day18.o : $(LIB_DIR)/set.h
$(OUT_DIR)/day20.o : $(LIB_DIR)/point.h
$(OUT_DIR)/day21.o : $(LIB_DIR)/vec.h $(LIB_DIR)/set.h
