# Try to detect the operating system to set flags for the assembler.
# OSX uses macho64 binary format, and needs an _ prefixed before all global
# symbols. Linux uses elf64 binary format, and the symbols are all fine as they
# are.
OS := $(shell uname)
ifeq ($(OS), Darwin)
FORMAT = macho64
PREFIX = --prefix _
else
FORMAT = elf64
PREFIX =
endif

COMPILER := ../target/release/sample_compiler
EXEC_FILES := $(patsubst %.expl,%.exe,$(wildcard *.expl))

.PHONY : all clean
all : $(EXEC_FILES)

%.s %.dot : %.expl $(COMPILER)
	$(COMPILER) $(OPTIONS) $<

%.o : %.s
	nasm -f $(FORMAT) $(PREFIX) $<

%.o : %.c

# Use gcc to do the linking since we want to include libc, and our
# object code starts from  a C-style main function. We do not want to
# enter at _start (which is where the OS dumps you on process
# creation), because there's a bunch of book keeping and alignment
# stuff to sort out. gcc/libc include code to do that for us.
%.exe : %.o ../runtime/runtime.a
	gcc $< ../runtime/runtime.a -o $@

%.pdf : %.dot
	dot -Tpdf $< > $@

.PRECIOUS : %.s

clean :
	rm -f *.o *.s $(EXEC_FILES) *.pdf *.dot
