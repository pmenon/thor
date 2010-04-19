
SRC_DIR=src
INCLUDE_DIR=include
EBIN_DIR=ebin

ERLC_OPTS=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall -v +debug_info 

SOURCES=$(wildcard $(SRC_DIR)/*.erl)
BEAM_TARGETS=$(patsubst $(SRC_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(SOURCES))

TARGETS=$(BEAM_TARGETS)

all: $(TARGETS)

$(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl
	erlc $(ERLC_OPTS) -pa $(EBIN_DIR) $<

