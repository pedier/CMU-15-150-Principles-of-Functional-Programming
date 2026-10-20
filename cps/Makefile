FILES = code/detypify/types.sml code/findn/findn.sml code/findPartition/findPartition.sml code/dpll/unitprop.sml code/dpll/dpll.sml collab.txt

.PHONY: all clean


all: handin.zip

clean:
	rm -f handin.zip

handin.zip: $(FILES)
	zip $@ $^
