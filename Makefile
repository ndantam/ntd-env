
FILES := $(shell git ls-tree  HEAD -r ntd | cut -f 2 | sed -e 's!^ntd/!!')

HOME_FILES := $(addprefix $(HOME)/, $(FILES))

install: $(HOME_FILES)

list:
	@echo $(FILES)
	@echo $(HOME_FILES)

$(HOME)/%: ntd/%
	@mkdir -vp `dirname $@`
	@if [ ! -L $@ ] ; then ln -sv `pwd`/$< $@; fi

clean:
	rm -f $(HOME_FILES)


WL_VARS := LISPDIR=~/.emacs.d/site-lisp VERSION_SPECIFIC_LISPDIR=~/.emacs.d/site-lisp PIXMAPDIR=~/.emacs.d/pixmaps

apel:
	 $(MAKE) -C emacs/$@
	 $(MAKE) -C emacs/$@ install $(WL_VARS)


flim:
	 $(MAKE) -C emacs/$@ $(WL_VARS)
	 $(MAKE) -C emacs/$@ install $(WL_VARS)

semi:
	 $(MAKE) -C emacs/$@ $(WL_VARS)
	 $(MAKE) -C emacs/$@ install $(WL_VARS)

wanderlust:
	echo '(setq load-path (append (list "/home/ntd/.emacs.d/site-lisp/apel" "/home/ntd/.emacs.d/site-lisp/flim" "/home/ntd/.emacs.d/site-lisp/semi") load-path))' > emacs/wanderlust/WL-CFG
	echo '(setq wl-install-utils t)' >> emacs/wanderlust/WL-CFG
	$(MAKE) -C emacs/$@ $(WL_VARS)
	$(MAKE) -C emacs/$@ install $(WL_VARS)
