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

$(BUILD_DIR)/%.exe : $(BUILD_DIR)/%.hs
	$(MAKE_DIRS)
	ghc --make -o $@ -odir $(BUILD_DIR) -hidir $(BUILD_DIR) $<

$(BUILD_DIR)/%.hs : $(SRC_DIR)/%.hs $(SRC_DIR)/utils.hs
	$(MAKE_DIRS)
	cp $< $@
	cat $(SRC_DIR)/utils.hs >> $@

$(INPUT_DIR)/%.txt:
	$(MAKE_DIRS)
	touch $@
