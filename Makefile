UNIT_TESTS=dist/build/unit-tests/test-unit-tests

$(UNIT_TESTS):
	obuild configure --enable-tests
	obuild build


.PHONY: clean test
clean:
	rm -rf dist

test: $(UNIT_TESTS)
	obuild test --output
