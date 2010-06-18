### INCLUDES
include ../app.mk

###-----------------------------------------------------------------------------
### APPLICATION LAYOUT
###-----------------------------------------------------------------------------
TOP_DIR = ..
CONFSRC = $(patsubst conf/%.conf.src,%.conf.src,$(wildcard conf/*.conf.src))
CONF = $(CONFSRC:.conf.src=.conf)
SPECSRC = $(patsubst conf/%.spec.src,%.spec.src,$(wildcard conf/*.spec.src))
SPEC = $(SPECSRC:.spec.src=.spec)
ERLS = $(patsubst ../src/%.erl,%.erl,$(wildcard ../src/*.erl))
BEAMS = $(ERLS:.erl=.beam)
MODS = $(BEAMS:.beam=)
STUB_ERLS = $(patsubst stubs/%.erl,%.erl,$(wildcard stubs/*.erl))
STUB_BEAMS = $(STUB_ERLS:.erl=.beam)

### DEPENDENCY SEARCH PATHS
VPATH = conf:stubs

.PHONY: all clean test
.SUFFIXES: .erl .hrl .beam .conf.src .conf .spec.src .spec

###-----------------------------------------------------------------------------
### TARGETS
###-----------------------------------------------------------------------------
all: log $(STUB_BEAMS) $(CONF) $(SPEC) test

test:
	cd $(TOP_DIR); $(MAKE) clean; $(MAKE) DEBUG_INFO='true'
	$(MV) ../ebin/*.beam .
	$(MV) ../ebin/*.app .
	$(RUN_TEST) -dir . -spec ./conf/test.spec -cover ./conf/cover.conf

log:
	$(MKDIR) log

clean:
	$(RM) *.beam
	$(RM) *.app
	$(RM) stubs/*.beam
	$(RM) variables-ct*
	$(RM) -R log/*
	$(RM) -R conf/*.conf
	$(RM) -R conf/*.spec

%.beam: %.erl
	@$(ECHO) "$(ERLC) $(STUB_EFLAGS) $^"
	@$(ERLC) $(STUB_EFLAGS) $^

%.conf: %.conf.src
	@$(ECHO) "Generating conf/$@"
	@$(SED) "s|%MODULES%|`echo $(MODS) | tr '[:blank:]' ','`|g" $^ | \
	$(SED) "s|%TEST_DIR%|`pwd`|g" > conf/$@

%.spec: %.spec.src
	@$(ECHO) "Generating conf/$@"
	@$(SED) "s|%TEST_DIR%|`pwd`|g" $^ > conf/$@
