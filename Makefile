# The command-line Scheme interpreter
SCHEME=plt-r5rs

# Tests for use with test-token-types.scm: all files that end with
# tokens.in
# Expected output must have the .in replaced with .expect
TEST_TOKEN_INPUTS := $(wildcard *tokens.in)

# Error tests for use with test-token-types.scm: all files that end
# with tokens.err
TEST_TOKEN_ERRORS := $(wildcard *tokens.err)

# Tests for use with test-tokenize.scm
# More files can be added after distribution.scm, separated by spaces
# Expected output must have the filename with .scm replaced by
# .tokens.expect (e.g. distribution.tokens.expect)
TEST_TOKENIZE_INPUTS := distribution.scm

# Tests for use with test-read-simple.scm: all files that end with
# simple.in
# Expected output must have the .in replaced with .expect
TEST_SIMPLE_INPUTS := $(wildcard *simple.in)

# Tests for use with test-read-compound.scm: all files that end with
# compound.in
# Expected output must have the .in replaced with .expect
TEST_COMPOUND_INPUTS := $(wildcard *compound.in)

# Tests for use with test-read-datum.scm: all files that end with
# datum.in
# Expected output must have the .in replaced with .expect
TEST_DATUM_INPUTS := $(wildcard *datum.in)

# Error tests for use with test-read-datum.scm: all files that end
# with datum.err
TEST_DATUM_ERRORS := $(wildcard *datum.err)

# Tests for use with test-parse.scm
# More files can be added after distribution.scm, separated by spaces
# Expected output must have the filename with .scm replaced by
# .datums.expect (e.g. distribution.datums.expect)
TEST_PARSE_INPUTS := distribution.scm

all: done

correct-tests: test-tokens test-tokenize test-simple test-compound test-datum test-parse

error-tests: error-tokens error-datum

phase1: test-tokens error-tokens

phase2: test-tokenize

phase3: test-simple test-compound test-datum test-parse

test-tokens: $(TEST_TOKEN_INPUTS:.in=.tokens)

%.tokens:
	$(SCHEME) test-token-types.scm < $(@:.tokens=.in) > $(@:.tokens=.out)
	diff -q $(@:.tokens=.out) $(@:.tokens=.expect)

test-tokenize: $(TEST_TOKENIZE_INPUTS:%.scm=%.tokenize)

%.tokenize:
	$(SCHEME) test-tokenize.scm < $(@:%.tokenize=%.scm) > $(@:.tokenize=.tokens.out)
	diff -q $(@:.tokenize=.tokens.out) $(@:.tokenize=.tokens.expect)

test-simple: $(TEST_SIMPLE_INPUTS:.in=.simple)

%.simple:
	$(SCHEME) test-read-simple.scm < $(@:.simple=.in) > $(@:.simple=.out)
	diff -q $(@:.simple=.out) $(@:.simple=.expect)

test-compound: $(TEST_COMPOUND_INPUTS:.in=.compound)

%.compound:
	$(SCHEME) test-read-compound.scm < $(@:.compound=.in) > $(@:.compound=.out)
	diff -q $(@:.compound=.out) $(@:.compound=.expect)

test-datum: $(TEST_DATUM_INPUTS:.in=.datum)

%.datum:
	$(SCHEME) test-read-datum.scm < $(@:.datum=.in) > $(@:.datum=.out)
	diff -q $(@:.datum=.out) $(@:.datum=.expect)

test-parse: $(TEST_TOKENIZE_INPUTS:%.scm=%.parse)

%.parse:
	$(SCHEME) test-parse.scm < $(@:%.parse=%.scm) > $(@:.parse=.datums.out)
	diff -q $(@:.parse=.datums.out) $(@:.parse=.datums.expect)

error-tokens: $(TEST_TOKEN_ERRORS:.err=.err-tokens)

%.err-tokens:
	$(SCHEME) test-token-types.scm < $(@:.err-tokens=.err) > $(@:.err-tokens=.out)
	diff -q -I "Error: .*" $(@:.err-tokens=.out) $(@:.err-tokens=.expect)
	grep -q '^Error: ' $(@:.err-tokens=.out)
	echo PASS

error-datum: $(TEST_DATUM_ERRORS:.err=.err-datum)

%.err-datum:
	$(SCHEME) test-read-datum.scm < $(@:.err-datum=.err) > $(@:.err-datum=.out)
	diff -q -I "Error: .*" $(@:.err-datum=.out) $(@:.err-datum=.expect)
	grep -q '^Error: ' $(@:.err-datum=.out)
	echo PASS

done: correct-tests error-tests
	@echo '##### All tests passed! #####'

clean:
	rm -vf *.out
