MFKILE_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
BIN:=$(MFKILE_DIR)/bin
SRC:=$(MFKILE_DIR)/src
make:
	cd $(SRC) && ghc Main.hs;
	[ -d $(BIN) ] || mkdir -p $(BIN);
	mv $(SRC)/Main $(BIN)/ARhelper;