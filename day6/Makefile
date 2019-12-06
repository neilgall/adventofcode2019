
.PHONY: all
all: part1 part2

.PHONY: example
example:
	gprolog --consult-file example --query-goal "total(X)" </dev/null

.PHONY: part1
part1: rules.pl day6.pl
	gprolog --consult-file rules --consult-file day6 --query-goal "total(Total)" </dev/null

.PHONY: part2
part2: rules.pl day6.pl
	gprolog --consult-file rules --consult-file day6 --query-goal "part2(Length)" </dev/null

rules.pl: input.txt
	perl -n -e '/^([A-Za-z0-9]+)\)([A-Za-z0-9]+)/ && print "orbits(object_$$1, object_$$2).\n"' <$^ >$@

