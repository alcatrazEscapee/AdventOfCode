SHELL := /bin/bash

INPUT_DIR := inputs
BUILD_DIR := out
SRC_DIR   := src

MAKE_DIRS = @mkdir -p $(@D)


.DEFAULT_GOAL := run
.PRECIOUS : $(INPUT_DIR)/%.txt

help :
	@echo "Advent of Code 2016 in Haskell"

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

$(BUILD_DIR)/%.exe : $(SRC_DIR)/%.hs
	$(MAKE_DIRS)
	rm $(BUILD_DIR)/Main.hi
	rm $(BUILD_DIR)/Main.o
	ghc --make -o $@ -odir $(BUILD_DIR) -hidir $(BUILD_DIR) $<

$(INPUT_DIR)/%.txt:
	$(MAKE_DIRS)
	touch $@
