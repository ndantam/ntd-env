"colorscheme evening
colorscheme ron
set showmatch
set encoding=utf-8
"set tabstop=8
"set shiftwidth=8
"set expandtab
"set autoindent
"imap <tab> <esc>==i
"autocmd FileType make setlocal noexpandtab
"autocmd FileType make imap <tab> <tab>

augroup filetype
        au! BufRead,BufNewFile *.jm4    set filetype=java
        "au! BufRead,BufNewFile *.ho,*.m    set filetype=objc
        au! BufRead,BufNewFile *.m      set filetype=matlab
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
        au! BufRead,BufNewFile *.tsm,*.tsj   set filetype=mail
augroup END

autocmd Filetype mail set textwidth=68

let spell_auto_type="latex,tex,txt,html"

highlight SpellErrors ctermfg=Red guifg=Red
 	   \ cterm=underline gui=underline term=reverse

syntax on
set modeline
set modelines=15


" Removes trailing spaces
function TrimWhiteSpace()
  %s/\s*$//
  ''
:endfunction

"set list listchars=trail:.,extends:>
autocmd FileWritePre * :call TrimWhiteSpace()
autocmd FileAppendPre * :call TrimWhiteSpace()
autocmd FilterWritePre * :call TrimWhiteSpace()
autocmd BufWritePre * :call TrimWhiteSpace()
