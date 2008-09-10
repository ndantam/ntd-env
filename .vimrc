"colorscheme evening
colorscheme ron
set tabstop=4
"et shiftwidth=4
set showmatch
set encoding=utf-8



augroup filetype
        au! BufRead,BufNewFile *.jm4    set filetype=java
        au! BufRead,BufNewFile *.ho,*.m    set filetype=objc
        au! BufRead,BufNewFile *.lm4    set filetype=lex
        au! BufRead,BufNewFile *.ym4    set filetype=yacc
        au! BufRead,BufNewFile *.cp4    set filetype=cpp
        au! BufRead,BufNewFile *.cm4    set filetype=c
        au! BufRead,BufNewFile *.nqc4   set filetype=nqc
        au! BufRead,BufNewFile *.htm4   set filetype=html
        au! BufRead,BufNewFile *.ds   set filetype=lisp
        au! BufRead,BufNewFile *.seml,*.sem4   set filetype=seml
        au! BufRead,BufNewFile *.tex    highlight SpellErrors ctermfg=Red guifg=Red
 	   \ cterm=underline gui=underline term=reverse
augroup END

let spell_auto_type="latex,tex,txt,html"

highlight SpellErrors ctermfg=Red guifg=Red
 	   \ cterm=underline gui=underline term=reverse

syntax on
