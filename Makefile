GAME=nesmon
ASSEMBLER=ca65
LINKER=ld65

OBJ_FILES=$(GAME).o

all: $(GAME).nes

$(GAME).$(FORMAT): $(OBJ_FILES) nes.cfg
	$(LINKER) -o $(GAME).nes -C nes.cfg $(OBJ_FILES) -m $(GAME).map.txt -Ln $(GAME).labels.txt --dbgfile $(GAME).dbg

.PHONY: clean

clean:
	rm -f *.o *.nes *.dbg *.nl *.map.txt *.labels.txt

$(OBJ_FILES): *.s chars.chr

%.o:%.s
	$(ASSEMBLER) $< -g -o $@
