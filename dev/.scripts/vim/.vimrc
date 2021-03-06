""https://github.com/garybernhardt/dotfiles/blob/master/.vimrc
filetype plugin indent on
syntax on
set history=10000
set relativenumber
set ruler
set tabstop=4
set shiftwidth=4
set hlsearch
set viminfo='100,<1000,s10,h
highlight _r                                        ctermfg=red
highlight _dr                                       ctermfg=darkred
highlight r   ctermbg=red          guibg=red        ctermfg=White
highlight dr  ctermbg=darkred      guibg=darkred    ctermfg=White
highlight lr  ctermbg=lightred     guibg=lightred
highlight _dy                                       ctermfg=darkyellow
highlight y   ctermbg=yellow       guibg=yellow
highlight dy  ctermbg=darkyellow   guibg=darkyellow
highlight ly  ctermbg=lightyellow  guibg=lightyellow
highlight B   ctermbg=brown        guibg=brown
highlight _g                                        ctermfg=green
highlight _dg                                       ctermfg=darkgreen
highlight g   ctermbg=green        guibg=green
highlight dg  ctermbg=darkgreen    guibg=darkgreen
highlight lg  ctermbg=lightgreen   guibg=lightgreen
highlight _b                                        ctermfg=blue
highlight _db                                       ctermfg=darkblue
highlight b   ctermbg=blue         guibg=blue
highlight db  ctermbg=darkblue     guibg=darkblue
highlight lb  ctermbg=lightblue    guibg=lightblue
highlight _m                                          ctermfg=magenta
highlight _dm                                         ctermfg=darkmagenta
highlight m   ctermbg=magenta      guibg=magenta      ctermfg=darkblue
highlight dm  ctermbg=darkmagenta  guibg=darkmagenta  ctermfg=darkblue
highlight lm  ctermbg=lightmagenta guibg=lightmagenta ctermfg=darkblue
highlight _c                                          ctermfg=cyan
highlight _dc                                         ctermfg=darkcyan
highlight cy  ctermbg=cyan         guibg=cyan
highlight dc  ctermbg=darkcyan     guibg=darkcyan
highlight lc  ctermbg=lightcyan    guibg=lightcyan
"":runtime syntax/colortest.vim

function! MatchSplunkGeneral()
  so ~/.vimrc
  call AspectSplunkGeneral()
endfunction
function! MatchProphecy()
  so ~/.vimrc
  call AspectProphecyVCS()
endfunction

function! MatchLloyds()
  so ~/.vimrc
  call AspectLloyds()
endfunction

function! AspectProphecyVCS()
	let rank = 20
	%s/\\s/\//g
	%s/\\r\\n/\r/g
	%s/\\n/\r/g
	call AspectSplunkGeneral()
endfunction

function! AspectLloyds()
	let rank = 20
    call matchadd('_b', '__AphroditeMsg__' ,rank)
	call AspectSplunkGeneral()
endfunction

function! AspectSplunkGeneral()
	let rank = 21
    call matchadd('_dg','SIP message([io]):'         ,rank) 
    call matchadd('_dg','^INVITE'                    ,rank) 
    call matchadd('_dg','^SIP/2.0 180 Ringing'       ,rank) 
    call matchadd('_dg','^SIP/2.0 183 Call progress' ,rank) 
    call matchadd('_dg','^SIP/2.0 183 Call progress' ,rank) 
    call matchadd('_dg','^SIP/2.0 200 OK'            ,rank) 
    call matchadd('_dg','^ACK'                       ,rank) 
    call matchadd('_dg','^BYE'                       ,rank) 

    call matchadd('_dy','[a-z0-9]\+\.[a-z]\+\.voxeo.net' ,rank) 

    call matchadd('_b', '\/log: .*'        ,rank)

    call matchadd('_r', '\/warning:.*\c',rank)
    call matchadd('_r', '\/SESSION_START: .*',rank)
    call matchadd('_r', '\/SESSION_END: .*',rank)
    call matchadd('_r', '\/CALL_START: ',rank)

    call matchadd('dr', 'fail\c' ,rank)
    call matchadd('dr', 'error\c',rank)

    call matchadd('dm', '\/event: [^ ]\+\c',rank)

    call matchadd('m',  '\/action: [^ ]\+\c',rank)
endfunction
