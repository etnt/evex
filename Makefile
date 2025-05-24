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

.PHONY: all clean rebuild
