dist/build/unit-tests/unit-tests:
	obuild configure
	obuild build

.PHONY: clean
clean:
	rm -rf dist
