
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
	@diff --strip-trailing-cr $(ACTUAL) $(EXPECTED) >> /dev/null || ( \
		printf "\n=== Tests Failed! ===\n\n" && \
		diff --strip-trailing-cr -u $(ACTUAL) $(EXPECTED))
	@printf "\n=== Tests Passed! ===\n"

$(ACTUAL) : $(DAYS_COR) FORCE
	-@rm -f $(ACTUAL)
	-@touch $(ACTUAL)
	@for day in $(DAYS) ; do \
		printf "Test Day $$day\n" ; \
		printf "Day $$day\n" >> $(ACTUAL) ; \
		cordy -o src/day$$day.cor >> $(ACTUAL) ; \
	done