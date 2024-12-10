# Test makefile for scipunch build tools

# Foo target
foo:
	echo "foo"

bar: foo
	echo "bar"

# Comment for the baz target
baz: foo bar
	echo "baz"
