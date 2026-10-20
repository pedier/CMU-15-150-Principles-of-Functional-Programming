FILES = code/multilingual/languages.sml code/trains/trains.sml code/intersect/intersect.sml code/derivative/derivative.sml collab.txt

.PHONY: all clean


all: handin.zip

clean:
	rm -f handin.zip

handin.zip: $(FILES)
	zip $@ $^
