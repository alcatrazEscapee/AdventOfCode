SHELL := /bin/bash

INPUT_DIR := inputs
BUILD_DIR := out
SRC_DIR   := src


.DEFAULT_GOAL = run

help :
	@echo "Advent of Code 2016 in Haskell"

.PHONY : run
run : $(BUILD_DIR)/day$(day).exe
	-@if [ "$(day)" = "" ]; then \
		echo "No day provided - try with make day=XX" ; \
	else \
		cat $(INPUT_DIR)/day$(day).txt | $(BUILD_DIR)/day$(day).exe ; \
	fi

.PHONY : clean
clean :
	rm -rf out

$(BUILD_DIR)/%.exe : $(SRC_DIR)/%.hs $(INPUT_DIR)/%.txt
	mkdir -p $(BUILD_DIR)
	ghc --make -o $@ -odir $(BUILD_DIR) -hidir $(BUILD_DIR) $<
