
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
