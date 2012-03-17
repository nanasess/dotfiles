SITE_DIR=C:/Users/Kentaro\ Ohkouchi/.emacs.d
EMACS=C:/emacs-23.4/bin/emacs.exe

all: compile
compile:
	cd $(SITE_DIR) && find . -name '*.el' | xargs $(EMACS) -Q -batch -l $(SITE_DIR)/init.el -f batch-byte-compile

clean:
	cd $(SITE_DIR) && find . -name '*.elc' -delete -print