
EXAMPLES = \
	example fib infinite neg newton warnings fact helloworld infrec \
	testdiv yes


all:
	for f in $(EXAMPLES); \
	do \
	../lisc -co -o $$f $$f.lis; \
	done

clean:
	for f in $(EXAMPLES); \
	do \
		rm $$f; \
	done
