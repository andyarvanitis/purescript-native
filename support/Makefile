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
# For example: 'make PURSFLAGS=--codegen,js CXXFLAGS=-DDEBUG LDFLAGS=lgmp'
#
# You can also edit the generated version of this file directly.
#
PURS        := purs
PSC_PACKAGE := psc-package
PSCPP       := pscpp
SRC         := src
SRCINT      := srcint
OUTPUT      := output
CC_SRC      := $(OUTPUT)/src
FFI_SRC     := ffi
BIN         := main

override PURSFLAGS += compile --codegen corefn
override CXXFLAGS += --std=c++11

CFLAGS += -Os -pedantic -std=c99 -Wall -fstrict-aliasing -fomit-frame-pointer
LDFLAGS += -lm
LDSHAREDFLAGS = -shared

CXXVERSION = $(shell $(CXX) --version)
ifneq (,$(findstring g++,$(CXXVERSION)))
  PSCPPFLAGS += "--ucns"
endif

DEBUG := "-DDEBUG -g"
RELEASE := "-DNDEBUG -O3"

INCLUDES := -I $(CC_SRC) 
BIN_DIR := $(OUTPUT)/bin

PACKAGE_SOURCES = $(subst \,/,$(shell $(PSC_PACKAGE) sources))
PURESCRIPT_PKGS := $(firstword $(subst /, ,$(PACKAGE_SOURCES)))

PURESCRIPT_PKG_SRCS=$(foreach d,$(PACKAGE_SOURCES),$(call rwildcard,$(firstword $(subst *, ,$(d))),*.purs))

## Not all environments support globstar (** dir pattern)
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

debug: codegen
	@$(MAKE) $(BIN) CFLAGS+=$(DEBUG) CXXFLAGS+=$(DEBUG)

release: codegen
	@$(MAKE) $(BIN_DIR)/$(BIN) CFLAGS+=$(RELEASE) CXXFLAGS+=$(RELEASE)

.PHONY: corefn
corefn: PURESCRIPT_SRCS=$(call rwildcard,$(SRC)/,*.purs)
corefn: $(PURESCRIPT_PKGS)
	@$(PURS) $(PURSFLAGS) --output $(OUTPUT) $(PURESCRIPT_PKG_SRCS) $(PURESCRIPT_SRCS)

.PHONY: codegen
codegen: COREFN_SRCS=$(call rwildcard,$(OUTPUT)/,corefn.json)
codegen: corefn
	@$(PSCPP) $(PSCPPFLAGS) $(COREFN_SRCS)

$(PURESCRIPT_PKGS):
	@echo "Getting packages using" $(PSC_PACKAGE) "..."
	@$(PSC_PACKAGE) update

# the core modules 
PSC_MODULES = $(shell sed -sE -e "s/--.*//g" \
  -ne "0,/module/{s/^[[:space:]]*module[[:space:]]+([A-Za-z.]*).*/\1/p}" $(PURESCRIPT_PKG_SRCS) \
  | sed -e "s/\./_/g" )

APP_MODULES =  # use it for your application specific modules
MAIN_MODULE = Main

PSC_SRCS := $(foreach m,$(PSC_MODULES),$(call rwildcard,$(CC_SRC)/$(m)/,*.cpp)) 
APP_SRCS := $(foreach m,$(APP_MODULES),$(call rwildcard,$(CC_SRC)/$(m)/,*.cpp))
FFI_SRCS := $(call rwildcard,$(FFI_SRC)/,*.cpp)
MAIN_SRCS:= $(call rwildcard,$(CC_SRC)/$(MAIN_MODULE)/,*.cpp)  $(CC_SRC)/purescript.cpp

PSC_OBJS = $(PSC_SRCS:.cpp=.o)
APP_OBJS = $(APP_SRCS:.cpp=.o)
FFI_OBJS = $(FFI_SRCS:.cpp=.o)
MAIN_OBJS= $(MAIN_SRCS:.cpp=.o)

PSC_LIBNAME = psc
FFI_LIBNAME = ffi
APP_LIBNAME = app
PSC_LIB = $(BIN_DIR)/lib$(PSC_LIBNAME).so
FFI_LIB = $(BIN_DIR)/lib$(FFI_LIBNAME).so
APP_LIB = $(BIN_DIR)/lib$(APP_LIBNAME).so

DEPS  = $(PSC_OBJS:.o=.d) $(APP_OBJS:.o=.d) $(FFI_OBJS:.o=.d) $(MAIN_OBJS:.o=.d)

$(PSC_LIB): $(PSC_OBJS)
	@echo "build shared lib:" $(PSC_LIB)
	@mkdir -p $(BIN_DIR)
	@$(CXX) $^ -o $(PSC_LIB) $(LDSHAREDFLAGS)

$(FFI_LIB): $(FFI_OBJS)
	@echo "build shared lib:" $(FFI_LIB)
	@mkdir -p $(BIN_DIR)
	@$(CXX) $^ -o $(FFI_LIB) $(LDSHAREDFLAGS)

$(BIN_DIR)/$(BIN): $(MAIN_OBJS) $(PSC_LIB) $(FFI_LIB)
	@echo "Linking" $@
	@mkdir -p $(BIN_DIR)
	@$(CXX) $(MAIN_OBJS) -o $@ -L$(BIN_DIR)/ -l$(PSC_LIBNAME) -l$(FFI_LIBNAME) $(LDFLAGS)

$(BIN_DIR)/$(BIN)_s: $(MAIN_OBJS) $(PSC_OBJS) $(FFI_OBJS)
	@echo "Linking" $@
	@mkdir -p $(BIN_DIR)
	@$(CXX) $^ -o $@ $(LDFLAGS)

-include $(DEPS)

%.o: %.cpp
	@echo "Creating" $@ "(C++)"
	@$(CXX) $(CXXFLAGS) $(INCLUDES)  -MMD -MP -c $< -o $@ -fPIC

.PHONY: all
all: release

.PHONY: clean
clean:
	@-rm -f $(PSC_LIB) $(PSC_OBJS) $(FFI_LIB) $(FFI_OBJS) $(MAIN_OBJS) $(DEPS)
	@-rm -rf $(OUTPUT)

.PHONY: run
run:
	@$(BIN_DIR)/$(BIN) $(ARGS)
