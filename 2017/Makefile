
EXPECTED := ./expected.txt
ACTUAL   := ./actual.txt

DAYS_COR := $(shell find src -name 'day\*.cor')
DAYS     := $(DAYS_COR:src/day%.cor=%)

.DEFAULT_GOAL := run

FORCE :

.PHONY : run
run :
	@cordy -o src/day$(day).cor

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
		time cordy -o src/day$$day.cor >> $(ACTUAL) ; \
	done