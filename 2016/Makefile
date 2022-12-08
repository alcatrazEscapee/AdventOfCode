SHELL := /bin/bash

INPUT_DIR  := inputs
BUILD_DIR  := out
SRC_DIR    := src

MAKE_DIRS = @mkdir -p $(@D)

DAYS_HS    := $(shell find $(SRC_DIR) -name '\*.hs')
DAYS_INPUT := $(shell find $(INPUT_DIR) -name '\*.txt') 
DAYS_EXE   := $(DAYS_HS:$(SRC_DIR)/%.hs=$(BUILD_DIR)/%.exe)
DAYS_LOG   := $(DAYS_HS:$(SRC_DIR)/%.hs=$(BUILD_DIR)/%.log)

DAYS       := $(DAYS_HS:$(SRC_DIR)/day%.hs=%)

.DEFAULT_GOAL := run
.PRECIOUS : $(INPUT_DIR)/%.txt $(BUILD_DIR)/%.exe $(BUILD_DIR)/%.log

FORCE :

help :
	@echo "Advent of Code 2016 in Haskell"
	@echo "Put inputs in ./inputs/dayXX.txt and expected answers in ./inputs/answers.txt"
	@echo "  make day=XX  Runs day XX"
	@echo "  make test    Tests each day against expected answers"

.PHONY : run
run : $(BUILD_DIR)/day$(day).log

.PHONY : clean
clean : FORCE
	-@rm -rf out

.PHONY : test
test : $(BUILD_DIR)/all.log
	@diff --strip-trailing-cr $(INPUT_DIR)/answers.txt $(BUILD_DIR)/all.log >> /dev/null || ( \
		printf "\n=== Tests Failed! ===\n\n" && \
		diff --strip-trailing-cr -u $(INPUT_DIR)/answers.txt $(BUILD_DIR)/all.log)
	@printf "\n=== Tests Passed! ===\n"

$(BUILD_DIR)/day.log : FORCE
	-@printf "No day provided - run make day=XX"

$(BUILD_DIR)/all.log : $(DAYS_LOG)
	-@rm -f $(BUILD_DIR)/all.log
	-@touch $(BUILD_DIR)/all.log
	@for day in $(DAYS) ; do \
		printf "Day $$day\n" >> $(BUILD_DIR)/all.log; \
		cat $(BUILD_DIR)/day$$day.log >> $(BUILD_DIR)/all.log ; \
	done

# Day 12 outputs a C file which must be compiled and executed
$(BUILD_DIR)/day12.log : FORCE $(BUILD_DIR)/day12.o
	-@rm -f $(BUILD_DIR)/day12.log
	@$(BUILD_DIR)/day12.o | tee $(BUILD_DIR)/day12.log

$(BUILD_DIR)/day12.o : $(BUILD_DIR)/day12.exe $(INPUT_DIR)/day12.txt
	@cat $(INPUT_DIR)/day12.txt | $(BUILD_DIR)/day12.exe > $(BUILD_DIR)/day12.c
	@printf "Compiling out/day12.c ...\n"
	@gcc -std=c11 -O3 -o $(BUILD_DIR)/day12.o $(BUILD_DIR)/day12.c

# Day 16 is written in C entirely because haskell is annoying to optimize and it's just ovbious in C
$(BUILD_DIR)/day16.log : FORCE $(BUILD_DIR)/day16.o $(INPUT_DIR)/day16.txt
	-@rm -f $(BUILD_DIR)/day16.log
	@cat $(INPUT_DIR)/day16.txt | $(BUILD_DIR)/day16.o | tee $(BUILD_DIR)/day16.log

$(BUILD_DIR)/day16.o : $(SRC_DIR)/day16.c
	$(MAKE_DIRS)
	@printf "Compiling src/day16.c ...\n"
	@gcc -std=c11 -Werror -Wall -Wno-implicit-function-declaration -O3 -o $(BUILD_DIR)/day16.o $(SRC_DIR)/day16.c

# Pattern rule which matches the output of most days
$(BUILD_DIR)/day%.log : FORCE $(BUILD_DIR)/day%.exe $(INPUT_DIR)/day%.txt
	-@rm -f $(BUILD_DIR)/day$*.log
	@printf "Day $*\n" | tee $(BUILD_DIR)/day$*.log
	@cat $(INPUT_DIR)/day$*.txt | $(BUILD_DIR)/day$*.exe | tee $(BUILD_DIR)/day$*.log

$(BUILD_DIR)/%.exe : $(SRC_DIR)/%.hs
	$(MAKE_DIRS)
	-@rm -f $(BUILD_DIR)/Main.hi
	-@rm -f $(BUILD_DIR)/Main.o
	@ghc -O2 --make -o $@ -odir $(BUILD_DIR) -hidir $(BUILD_DIR) $<

$(INPUT_DIR)/%.txt:
	$(MAKE_DIRS)
	touch $@
