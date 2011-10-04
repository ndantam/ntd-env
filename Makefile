
FILES := .cshrc  .emacs   .env.sh  .gitconfig  .gitignore  .gvimrc  .mechsoph_bash  .ssh/config  .vimrc  .viper  .zshrc .emacs.d/site-lisp/magit

HOME_FILES := $(addprefix $(HOME)/, $(FILES))

install: $(HOME_FILES)

$(HOME)/%: ntd/%
	@mkdir -vp `dirname $@`
	@if [ ! -L $@ ] ; then ln -sv `pwd`/$< $@; fi

clean:
	rm -f $(HOME_FILES)
