# PureScript to native binary (via C++) Makefile
#
# Run 'make' or 'make release' to build an optimized release build
# Run 'make debug' to build a non-optimized build suitable for debugging
#
# You can also perform a parallel build with 'make -jN', where N is the
# number of cores to use.
#
# PCC, SRC, OUTPUT, and BIN can all be overridden with the
# command itself. For example: 'make BIN=myutil'
#
# Flags can be added to either the codegen or native build phases.
# For example: 'make PCCFLAGS=--no-tco CXXFLAGS=-DDEBUG LDFLAGS=lobjc'
#
# You can also edit the generated version of this file directly.
#
PCC    := {{path}}/pcc
SRC    := src
OUTPUT := output
BIN    := main

override PCCFLAGS += --comments
override CXXFLAGS += -std=c++11
override LDFLAGS  +=

ifeq ($(GC),yes)
  override CXXFLAGS += -DUSE_GC
  override LDFLAGS += -lgc
endif

DEBUG := "-DDEBUG -g"
RELEASE := "-O3 -flto"

INCLUDES := -I $(OUTPUT)
BIN_DIR := $(OUTPUT)/bin

PSC_PKG_BIN := psc-package
PSC_PACKAGE := {{path}}/$(PSC_PKG_BIN)

ifeq ("$(wildcard $(PSC_PACKAGE))","")
	PSC_PACKAGE := $(PSC_PKG_BIN)
endif

PACKAGE_SOURCES = $(shell $(PSC_PACKAGE) sources)
PURESCRIPT_PKGS := $(firstword $(subst /, ,$(PACKAGE_SOURCES)))

## Not all environments support globstar (** dir pattern)
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

release: codegen
	@$(MAKE) $(BIN) CXXFLAGS+=$(RELEASE)

debug: codegen
	@$(MAKE) $(BIN) CXXFLAGS+=$(DEBUG)

codegen: PURESCRIPT_PKG_SRCS=$(foreach d,$(PACKAGE_SOURCES),$(call rwildcard,$(firstword $(subst *, ,$(d))),*.purs))
codegen: PURESCRIPT_SRCS=$(call rwildcard,$(SRC)/,*.purs)
#
codegen: $(PURESCRIPT_PKGS)
	@$(PCC) $(PCCFLAGS) --output $(OUTPUT) $(PURESCRIPT_PKG_SRCS) $(PURESCRIPT_SRCS)

$(PURESCRIPT_PKGS):
	@echo "Getting packages using" $(PSC_PACKAGE) "..."
	@$(PSC_PACKAGE) update

SRCS := $(call rwildcard,$(OUTPUT)/,*.cc)

OBJS = $(SRCS:.cc=.o)
DEPS = $(SRCS:.cc=.d)

$(BIN): $(OBJS)
	@echo "Linking" $(BIN_DIR)/$(BIN)
	@mkdir -p $(BIN_DIR)
	@$(CXX) $^ -o $(BIN_DIR)/$@ $(LDFLAGS)

-include $(DEPS)

%.o: %.cc
	@echo "Creating" $@
	@$(CXX) $(CXXFLAGS) $(INCLUDES) -MMD -MP -c $< -o $@

.PHONY: all
all: release

.PHONY: clean
clean:
	@-rm -rf $(OUTPUT)

.PHONY: run
run:
	@$(BIN_DIR)/$(BIN) $(ARGS)
