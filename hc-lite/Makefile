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

pulse: compile
	@echo Sorry, PULSE tests not available yet

include tools.mk
