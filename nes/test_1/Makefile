

OBJECTS=main.o foo.o

default: foo.nes


.c.o:
	cc65 -O -Osri $^
	ca65 $*.s

.s.o:
	ca65 $^

prgrom.bin: $(OBJECTS)
	ld65 -v -C nes_simple.ld65 $^

foo.nes: prgrom.bin
	cat nes_header.bin $^ nes_footer.bin > $@

clean:
	$(RM) $(OBJECTS) foo.nes prgrom.bin
