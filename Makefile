SRCDIR = src
EBINDIR = ebin

# Find all .erl files in the src directory
SOURCES = $(wildcard $(SRCDIR)/*.erl)
BEAMS = $(SOURCES:$(SRCDIR)/%.erl=$(EBINDIR)/%.beam)

# Default target
all: $(EBINDIR) $(BEAMS)

# Create ebin directory if it doesn't exist
$(EBINDIR):
	mkdir -p $(EBINDIR)

# Compile .erl files to .beam files
$(EBINDIR)/%.beam: $(SRCDIR)/%.erl | $(EBINDIR)
	erlc -o $(EBINDIR) $<

# Clean compiled files
clean:
	rm -rf $(EBINDIR)

# Rebuild everything
rebuild: clean all

test: lux-dep lux_test

lux_test:
	(cd test; ../deps/lux/bin/lux readme.lux)

clean-deps:
	rm -rf deps

lux-dep:
	@if [ ! -d deps/lux ]; then \
		mkdir -p deps; \
        cd deps; \
        git clone https://github.com/hawk/lux.git; \
        cd lux; \
        autoconf; \
        ./configure; \
        make; \
    fi
        
.PHONY: all clean rebuild lux_test lux-dep clean-deps
