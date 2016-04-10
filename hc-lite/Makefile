REPO            ?= machi
PKG_REVISION    ?= $(shell git describe --tags)
PKG_BUILD        = 1
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR := $(shell which rebar)
ifeq ($(REBAR),)
REBAR = $(BASE_DIR)/rebar
endif
OVERLAY_VARS    ?=
EUNIT_OPTS       = -v

.PHONY: rel stagedevrel deps package pkgclean edoc

all: deps compile

compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) -r clean

edoc: edoc-clean
	$(REBAR) skip_deps=true doc

edoc-clean:
	rm -f edoc/*.png edoc/*.html edoc/*.css edoc/edoc-info

PULSE_TESTS = hcl_projection_store_eqc

pulse:
	@rm -rf $(BASE_DIR)/.eunit
	BITCASK_PULSE=1 $(REBAR) clean compile
	env USE_PULSE=1 $(REBAR) -D PULSE eunit skip_deps=true suites=$(PULSE_TESTS) ; \
	if [ $$? -ne 0 ]; then \
		echo PULSE test FAILED; \
		cp ./.eunit/current_counterexample.eqc $(COUNTER); \
		cp ./.eunit/.eqc-info $(EQCINFO); \
		echo See files $(COUNTER) and $(EQCINFO); \
		exit 1; \
	else \
		exit 0; \
	fi

include tools.mk
