FILES = code/errors/error1.sml code/errors/error2.sml code/errors/error3.sml code/errors/error4.sml code/errors/error5.sml collab.txt

.PHONY: all clean


all: handin.zip

clean:
	rm -f handin.zip

handin.zip: $(FILES)
	zip $@ $^
