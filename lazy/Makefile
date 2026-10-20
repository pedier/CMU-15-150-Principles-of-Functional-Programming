FILES = code/bbs/StreamRand.sml code/streamfns/StreamFns.sml code/fractions/ContFraction.sml code/trees/LazyTree.sml collab.txt

.PHONY: all clean


all: handin.zip

clean:
	rm -f handin.zip

handin.zip: $(FILES)
	zip $@ $^
