SHELL := /bin/bash

INPUT_DIR  := inputs
BUILD_DIR  := out
SRC_DIR    := src
RUST_DIR   := target/release

MAKE_DIRS = @mkdir -p $(@D)

DAYS_SRC   := $(shell find $(INPUT_DIR) -name 'day\*.txt')
DAYS_RS    := $(shell find $(SRC_DIR) -name 'day\*.rs')
DAYS_LOG   := $(DAYS_SRC:$(INPUT_DIR)/%.txt=$(BUILD_DIR)/%.log)
DAYS_RUST  := $(DAYS_RS:$(SRC_DIR)/%.rs=$(RUST_DIR)/%.exe)

DAYS       := $(DAYS_SRC:$(INPUT_DIR)/day%.txt=%)

GCC        := gcc -std=c11 -Werror -Wall -Wno-implicit-function-declaration -Wno-unused-label -O3
GHC        := ghc -O2 --make -odir $(BUILD_DIR) -hidir $(BUILD_DIR)

.DEFAULT_GOAL := run
.PRECIOUS : $(INPUT_DIR)/%.txt $(BUILD_DIR)/%.hs.o $(BUILD_DIR)/%.c.o $(BUILD_DIR)/%.c $(BUILD_DIR)/%.log

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
	-@rm -rf $(BUILD_DIR)
	-@rm -rf $(RUST_DIR)

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

# $(eval $(call build,...)) where arguments ... are day (in rule), day (in targets), and executable extension
define build = 
$(BUILD_DIR)/day$(2).log : FORCE $(BUILD_DIR)/day$(2)$(3) $(INPUT_DIR)/day$(2).txt
	@mkdir -p $$(@D)
	-@rm -f $(BUILD_DIR)/day$(1).log
	@printf "Day $(1)\n" | tee $(BUILD_DIR)/day$(1).log
	@cat $(INPUT_DIR)/day$(1).txt | $(BUILD_DIR)/day$(1)$(3) | tee $(BUILD_DIR)/day$(1).log
endef

$(eval $(call build,05,05,.rs.o)) # Day 05, 14, 17 implemented in Rust due needing MD5 (and high performance), and not wanting to do that in either C or Haskell
$(eval $(call build,14,14,.rs.o))
$(eval $(call build,17,17,.rs.o))

$(eval $(call build,12,12,.out.c.o))  # Day 12 outputs a C file to out/ which is compiled

$(eval $(call build,16,16,.c.o))  # Day 16, 18 implemented directly in C
$(eval $(call build,18,18,.c.o))

$(eval $(call build,$$*,%,.hs.o))  # All other days implemented in haskell and are caught by the pattern rule


# Build Artifacts

$(BUILD_DIR)/%.c.o : $(SRC_DIR)/%.c
	$(MAKE_DIRS)
	@printf "Compiling $< ...\n"
	@$(GCC) -o $@ $<

$(BUILD_DIR)/%.out.c.o : $(BUILD_DIR)/%.hs.o $(INPUT_DIR)/%.txt
	$(MAKE_DIRS)
	@cat $(INPUT_DIR)/$*.txt | $(BUILD_DIR)/$*.hs.o > $(BUILD_DIR)/$*.c
	@printf "Compiling out/$*.c ...\n"
	@$(GCC) -o $(BUILD_DIR)/$*.out.c.o $(BUILD_DIR)/$*.c

$(BUILD_DIR)/%.hs.o : $(SRC_DIR)/%.hs
	$(MAKE_DIRS)
	-@rm -f $(BUILD_DIR)/Main.hi
	-@rm -f $(BUILD_DIR)/Main.o
	@$(GHC) -o $@ $<

$(BUILD_DIR)/%.rs.o : $(RUST_DIR)/%.exe
	$(MAKE_DIRS)
	@cp $< $@

$(DAYS_RUST) : $(DAYS_RS) Cargo.toml Cargo.lock
	@printf "Compiling $(DAYS_RS) ...\n"
	@cargo build --release

$(INPUT_DIR)/%.txt:
	$(MAKE_DIRS)
	@touch $@
