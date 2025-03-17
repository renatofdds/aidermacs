# Makefile for testing aidermacs across Emacs versions

EMACS ?= emacs
BATCH = $(EMACS) -Q -batch

.PHONY: test

# Main test target
test:
	@echo "Testing aidermacs..."
	$(BATCH) -L . --eval "(require 'aidermacs)"
