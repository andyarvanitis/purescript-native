# PureScript to native binary (via C++) Makefile
#
# Run 'make' or 'make release' to build an optimized release build
# Run 'make debug' to build a non-optimized build suitable for debugging
#
# You can also perform a parallel build with 'make -jN', where N is the
# number of cores to use.
#
# PURS, SRC, OUTPUT, and BIN can all be overridden with the
# command itself. For example: 'make BIN=myutil'
#
# Flags can be added to either the codegen or native build phases.
# For example: 'make PURSFLAGS=--codegen,js CFLAGS=-DDEBUG LDFLAGS=lgmp'
#
# You can also edit the generated version of this file directly.
#
PURS        := purs
PSC_PACKAGE := psc-package
PSOBJC      := purecc
SRC         := src
OUTPUT      := output
CC_SRC      := $(OUTPUT)/src
FFI_SRC     := ffi
BIN         := main

override PURSFLAGS += compile --codegen corefn
override CFLAGS += --std=c++11

DEBUG := "-DDEBUG -g"
RELEASE := "-O3"

INCLUDES := -I $(CC_SRC)
BIN_DIR := $(OUTPUT)/bin

PACKAGE_SOURCES = $(subst \,/,$(shell $(PSC_PACKAGE) sources))
PURESCRIPT_PKGS := $(firstword $(subst /, ,$(PACKAGE_SOURCES)))

## Not all environments support globstar (** dir pattern)
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

release: codegen
	@$(MAKE) $(BIN) CFLAGS+=$(RELEASE)

debug: codegen
	@$(MAKE) $(BIN) CFLAGS+=$(DEBUG)

.PHONY: corefn
corefn: PURESCRIPT_PKG_SRCS=$(foreach d,$(PACKAGE_SOURCES),$(call rwildcard,$(firstword $(subst *, ,$(d))),*.purs))
corefn: PURESCRIPT_SRCS=$(call rwildcard,$(SRC)/,*.purs)
corefn: $(PURESCRIPT_PKGS)
	@$(PURS) $(PURSFLAGS) --output $(OUTPUT) $(PURESCRIPT_PKG_SRCS) $(PURESCRIPT_SRCS)

.PHONY: codegen
codegen: COREFN_SRCS=$(call rwildcard,$(OUTPUT)/,corefn.json)
codegen: corefn
	@$(PSOBJC) $(COREFN_SRCS)

$(PURESCRIPT_PKGS):
	@echo "Getting packages using" $(PSC_PACKAGE) "..."
	@$(PSC_PACKAGE) update

SRCS := $(call rwildcard,$(CC_SRC)/,*.cpp)
SRCS += $(call rwildcard,$(FFI_SRC)/,*.cpp)
SRCS += $(call rwildcard,$(FFI_SRC)/,*.c)

OBJS1 = $(SRCS:.cpp=.o)
OBJS  = $(OBJS1:.c=.o)
DEPS  = $(OBJS:.o=.d)

$(BIN): $(OBJS)
	@echo "Linking" $(BIN_DIR)/$(BIN)
	@mkdir -p $(BIN_DIR)
	@$(CXX) $^ -o $(BIN_DIR)/$@ $(LDFLAGS)

-include $(DEPS)

%.o: %.cpp
	@echo "Creating" $@
	@$(CXX) $(CFLAGS) $(INCLUDES) -MMD -MP -c $< -o $@

%.o: %.c
	@echo "Creating" $@
	@$(CXX) $(CFLAGS) $(INCLUDES) -MMD -MP -c $< -o $@

.PHONY: all
all: release

.PHONY: clean
clean:
	@-rm -rf $(OUTPUT)

.PHONY: run
run:
	@$(BIN_DIR)/$(BIN) $(ARGS)