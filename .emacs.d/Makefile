SITE_DIR=~/.emacs.d
EMACS=~/Applications/Emacs.app/Contents/MacOS/Emacs

all: compile
compile:
	cd $(SITE_DIR) && find . -name '*.el' | xargs $(EMACS) -Q -batch -l $(SITE_DIR)/init.el -f batch-byte-compile

clean:
	cd $(SITE_DIR) && find . -name '*.elc' -delete -print
