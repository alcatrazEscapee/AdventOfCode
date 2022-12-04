SHELL := /bin/bash

INPUT_DIR := inputs
BUILD_DIR := out
SRC_DIR   := src

MAKE_DIRS = @mkdir -p $(@D)

DAYS_HS   := $(shell find $(SRC_DIR) -name 'day*.hs')
DAYS_EXE  := $(DAYS_HS:$(SRC_DIR)/%.hs=$(BUILD_DIR)/%.exe)
DAYS      := $(DAYS_HS:$(SRC_DIR)/day%.hs=%)

.DEFAULT_GOAL := run
.PRECIOUS : $(INPUT_DIR)/%.txt

help :
	@echo "Advent of Code 2016 in Haskell"
	@echo "Put inputs in ./inputs/dayXX.txt and expected answers in ./inputs/answers.txt"
	@echo "  make day=XX  Runs day XX"
	@echo "  make test    Tests each day against expected answers"

.PHONY : run
run : $(BUILD_DIR)/day$(day).exe $(INPUT_DIR)/day$(day).txt
	-@if [ "$(day)" = "" ]; then \
		echo "No day provided - try with make day=XX" ; \
	else \
		cat $(INPUT_DIR)/day$(day).txt | $(BUILD_DIR)/day$(day).exe ; \
	fi

.PHONY : clean
clean :
	rm -rf out

.PHONY : test
test : $(DAYS_EXE)
	rm $(BUILD_DIR)/log.txt
	touch $(BUILD_DIR)/log.txt
	for day in $(DAYS) ; do \
		printf "Day $$day\n" >> $(BUILD_DIR)/log.txt ; \
		cat $(INPUT_DIR)/day$$day.txt | $(BUILD_DIR)/day$$day.exe >> $(BUILD_DIR)/log.txt ; \
	done
	python test.py


$(BUILD_DIR)/%.exe : $(SRC_DIR)/%.hs
	$(MAKE_DIRS)
	rm -f $(BUILD_DIR)/Main.hi
	rm -f $(BUILD_DIR)/Main.o
	ghc --make -o $@ -odir $(BUILD_DIR) -hidir $(BUILD_DIR) $<

$(INPUT_DIR)/%.txt:
	$(MAKE_DIRS)
	touch $@
