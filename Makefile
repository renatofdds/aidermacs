# Makefile for testing aidermacs across Emacs versions

EMACS ?= emacs
BATCH = $(EMACS) -Q -batch

.PHONY: test compile clean

# Main test target
test: compile lint-checkdoc

# Compile all .el files to check for byte-compilation errors
compile:
	@echo "Compiling Elisp files..."
	$(BATCH) -L . --eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile *.el

# Run checkdoc to verify documentation standards
lint-checkdoc:
	@echo "Running checkdoc..."
	$(BATCH) --eval "(require 'checkdoc)" \
		--eval "(setq checkdoc-arguments-in-order-flag nil)" \
		--eval "(setq checkdoc-verb-check-experimental-flag nil)" \
		--eval "(setq sentence-end-double-space nil)" \
		--eval "(checkdoc-file \"aidermacs.el\")" \
		--eval "(checkdoc-file \"aidermacs-backends.el\")" \
		--eval "(checkdoc-file \"aidermacs-backend-comint.el\")" \
		--eval "(checkdoc-file \"aidermacs-backend-vterm.el\")" \
		--eval "(checkdoc-file \"aidermacs-models.el\")"

# Clean compiled files
clean:
	@echo "Cleaning compiled files..."
	rm -f *.elc
