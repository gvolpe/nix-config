set directory=~/.vim/backup
set backupdir=~/.vim/backup " keep swap files here
filetype off                " required

call plug#begin('~/.local/share/nvim/plugged')

Plug 'tpope/vim-fugitive'                                         " git plugin
Plug 'tpope/vim-repeat'                                           " repeat plugin commands with (.)
Plug 'easymotion/vim-easymotion'                                  " highlights keys to move quickly
Plug 'vim-airline/vim-airline'                                    " bottom status bar
Plug 'vim-airline/vim-airline-themes'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' } " fuzzy finder conf
Plug 'junegunn/fzf.vim'                                           " fuzzy finder
Plug 'ryanoasis/vim-devicons'                                     " dev icons shown in the tree explorer
Plug 'scrooloose/nerdtree'                                        " tree explorer
Plug 'scrooloose/nerdcommenter'                                   " code commenter
Plug 'jacoborus/tender.vim'                                       " my favorite theme so far :)
Plug 'kien/rainbow_parentheses.vim'                               " for nested parentheses
Plug 'tpope/vim-surround'                                         " quickly edit surroundings (brackets, html tags, etc)
Plug 'junegunn/vim-easy-align'                                    " alignment plugin
Plug 'neomake/neomake'                                            " run programs asynchronously and highlight errors
Plug 'Valloric/MatchTagAlways'                                    " highlights html enclosing tags
Plug 'Twinside/vim-hoogle'                                        " Hoogle search (Haskell) in Vim
Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }                " ghcid for Haskell
Plug 'vmchale/dhall-vim'                                          " Syntax highlighting for Dhall lang
Plug 'terryma/vim-multiple-cursors'                               " Multiple cursors selection, etc
Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install()}}        " LSP client + autocompletion plugin
Plug 'itchyny/lightline.vim'                                      " configurable status line (can be used by coc)
Plug 'derekwyatt/vim-scala'                                       " scala plugin
Plug 'dyng/ctrlsf.vim'                                            " edit file in place after searching with ripgrep
Plug 'jremmen/vim-ripgrep'                                        " blazing fast search using ripgrep
Plug 'stefandtw/quickfix-reflector.vim'                           " make modifications right in the quickfix window
Plug 'Xuyuanp/nerdtree-git-plugin'                                " shows files git status on the NerdTree
Plug 'LnL7/vim-nix'                                               " nix support (highlighting, etc)
Plug 'skywind3000/asyncrun.vim'                                   " run async commands, show result in quickfix window
Plug 'tmux-plugins/vim-tmux'                                      " syntax highlighting for tmux conf file and more
Plug 'justinmk/vim-gtfo'                                          " go to terminal or file manager
Plug 'ap/vim-css-color'                                           " preview css colors

call plug#end()

" End of plugins here
" ===================

" shows list of yanked text (coc-yank plugin)
nnoremap <silent> <space>y  :<C-u>CocList -A --normal yank<cr>

" ripgrep smartcase (search with case insensitive)
let g:rg_command = 'rg --vimgrep -S'

" search work under cursor with CtrlSF (it uses ripgrep as the engine)
nmap <silent> <M-f> <Plug>CtrlSFCwordPath <CR>

" open quickfix windows when running AsyncRun
let g:asyncrun_open = 8

" close quickfix window
nnoremap <Esc> :cclose<CR>

" airline: status bar at the bottom
let g:airline_powerline_fonts=1

let g:airline_section_error = '%{airline#util#wrap(airline#extensions#coc#get_error(),0)}'
let g:airline_section_warning = '%{airline#util#wrap(airline#extensions#coc#get_warning(),0)}'

" Highlighting for jsonc filetype
autocmd FileType json syntax match Comment +\/\/.\+$+

" EasyMotion search with highlighting
map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)

" Hoogle config
let g:hoogle_search_count = 20
au BufNewFile,BufRead *.hs map <silent> <F1> :Hoogle<CR>
au BufNewFile,BufRead *.hs map <silent> <C-c> :HoogleClose<CR>

" Nerd commenter
filetype plugin on

" Better Unix support
set viewoptions=folds,options,cursor,unix,slash
set encoding=utf-8

" Relative numbers
set relativenumber

" Handle window actions with Meta instead of <C-w>
" Switching
nnoremap <M-h> <C-w>h
nnoremap <M-j> <C-w>j
nnoremap <M-k> <C-w>k
nnoremap <M-l> <C-w>l

" Moving
nnoremap <M-H> <C-w>H
nnoremap <M-J> <C-w>J
nnoremap <M-K> <C-w>K
nnoremap <M-L> <C-w>L
nnoremap <M-x> <C-w>x

" Resizing
nnoremap <M-=> <C-w>=
nnoremap <M-+> <C-w>+
nnoremap <M--> <C-w>-
nnoremap <M-<> <C-w><
nnoremap <M->> <C-w>>

" Clear search highlighting
nnoremap <C-z> :nohlsearch<CR>

" Terminal mode exit shortcut
:tnoremap <Esc> <C-\><C-n>

" Nerdtree git plugin symbols
let g:NERDTreeIndicatorMapCustom = {
    \ "Modified"  : "ᵐ",
    \ "Staged"    : "ˢ",
    \ "Untracked" : "ᵘ",
    \ "Renamed"   : "ʳ",
    \ "Unmerged"  : "ᶴ",
    \ "Deleted"   : "ˣ",
    \ "Dirty"     : "˜",
    \ "Clean"     : "ᵅ",
    \ "Unknown"   : "?"
    \ }

function! TrimWhitespace()
    let l:save_cursor = getpos('.')
    %s/\s\+$//e
    call setpos('.', l:save_cursor)
endfun

command! TrimWhitespace call TrimWhitespace() " Trim whitespace with command
autocmd BufWritePre * :call TrimWhitespace()  " Trim whitespace on every save

" Non-mapped function for tab toggles
function! TabToggle()
  if &expandtab
    set noexpandtab
  else
    set expandtab
  endif
endfunc

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

"    Nerdtree
map <C-F> :NERDTreeToggle<CR>
map <C-S> :NERDTreeFind<CR>

" Other options
let mapleader=','
set backspace=2
colorscheme tender
syntax on
set shell=/bin/bash
set laststatus=2
set noshowmode

" Draw a line at 120 columns
" set colorcolumn=120
" highlight ColorColumn ctermbg=235 guibg=#2c2d27

" Fixes broken cursor on Linux
set guicursor=

" NerdTree config
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

let g:NERDTreeMinimalUI = 1
let g:NERDTreeDirArrows = 1

                            " General editor options
set hidden                  " Hide files when leaving them.
set number                  " Show line numbers.
set numberwidth=1           " Minimum line number column width.
set cmdheight=2             " Number of screen lines to use for the commandline.
set textwidth=120           " Lines length limit (0 if no limit).
set formatoptions=jtcrq     " Sensible default line auto cutting and formatting.
set linebreak               " Don't cut lines in the middle of a word .
set showmatch               " Shows matching parenthesis.
set matchtime=2             " Time during which the matching parenthesis is shown.
set background=dark         " Color adapted to dark background.
set listchars=tab:▸\ ,eol:¬ " Invisible characters representation when :set list.
set clipboard=unnamedplus   " Copy/Paste to/from clipboard
set cursorline              " Highlight line cursor is currently on
set completeopt+=noinsert   " Select the first item of popup menu automatically without inserting it

" Search
set incsearch  " Incremental search.
set ignorecase " Case insensitive.
set smartcase  " Case insensitive if no uppercase letter in pattern, case sensitive otherwise.
set nowrapscan " Don't go back to first match after the last match is found.

" Fold
" set foldmethod=indent
" set foldlevelstart=1

" Tabs
set expandtab     " Tab transformed in spaces
set tabstop=2     " Sets tab character to correspond to x columns.
                  " x spaces are automatically converted to <tab>.
                  " If expandtab option is on each <tab> character is converted to x spaces.
set softtabstop=2 " column offset when PRESSING the tab key or the backspace key.
set shiftwidth=2  " column offset when using keys '>' and '<' in normal mode.

" Toggle display of tabs and EOF
nnoremap <leader>l :set list!<CR>

" Auto-commands
" Vimscript
augroup vimscript_augroup
  autocmd!
  autocmd FileType vim nnoremap <buffer> <M-z> :execute "help" expand("<cword>")<CR>
augroup END

" Spell check for markdown files
au BufNewFile,BufRead *.md set spell

" Fuzzy finder shortcut
nnoremap <C-p> :FZF<CR>

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(), <bang>0)

command! -bang -nargs=? -complete=dir Files
    \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)
" Always enable preview window on the right with 60% width
"let g:fzf_preview_window = 'right:60%'

" Disable the annoying and useless ex-mode
nnoremap Q <Nop>
nnoremap gQ <Nop>

" Disable arrow keys and page up / down
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>
inoremap <Up> <nop>
inoremap <Down> <nop>
inoremap <Left> <nop>
inoremap <Right> <nop>
vnoremap <Up> <nop>
vnoremap <Down> <nop>
vnoremap <Left> <nop>
vnoremap <Right> <nop>
noremap <PageUp> <nop>
inoremap <PageUp> <nop>
vnoremap <PageUp> <nop>
noremap <PageDown> <nop>
inoremap <PageDown> <nop>
vnoremap <PageDown> <nop>

" Disable mouse / touchpad (only in vim)
set mouse=nicr
inoremap <ScrollWheelUp> <nop>
inoremap <S-ScrollWheelUp> <nop>
inoremap <C-ScrollWheelUp> <nop>
inoremap <ScrollWheelDown> <nop>
inoremap <S-ScrollWheelDown> <nop>
inoremap <C-ScrollWheelDown> <nop>
inoremap <ScrollWheelLeft> <nop>
inoremap <S-ScrollWheelLeft> <nop>
inoremap <C-ScrollWheelLeft> <nop>
inoremap <ScrollWheelRight> <nop>
inoremap <S-ScrollWheelRight> <nop>
inoremap <C-ScrollWheelRight> <nop>

" vim-scala
au BufRead,BufNewFile *.sbt set filetype=scala

" ------------------- COC config -----------------------

" Smaller updatetime for CursorHold & CursorHoldI
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

" Some server have issues with backup files, see #649
set nobackup
set nowritebackup

" Better display for messages
set cmdheight=2

" Use <c-space> for trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> for confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[c` and `]c` for navigate diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Remap for do codeAction of current line
nmap <leader>ac <Plug>(coc-codeaction)

" Remap for do action format
"nmap <silent> F <Plug>(coc-action-format) "does not work
nnoremap <silent> F :call CocAction('format')<CR>

" Temporary command to run brittany in Haskell projects (ghcide does not support it yet)
nnoremap <leader>af :r !brittany --write-mode=inplace %:p<CR>

" Temporary command to run hlint in Haskell projects (ghcide does not support it yet)
nnoremap <leader>al :AsyncRun hlint %:p<CR>

" Show signature help
autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')

" Use K for show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

nnoremap <silent> <M-B> :call CocRequest('metals', 'workspace/executeCommand', { 'command': 'build-import' })<CR>
"nnoremap <silent> <M-Z> :ccl<CR>

" COC Snippets

" Use <C-l> for trigger snippet expand.
imap <C-l> <Plug>(coc-snippets-expand)

" Use <C-j> for select text for visual placeholder of snippet.
vmap <C-j> <Plug>(coc-snippets-select)

" Use <C-j> for jump to next placeholder, it's default of coc.nvim
let g:coc_snippet_next = '<c-j>'

" Use <C-k> for jump to previous placeholder, it's default of coc.nvim
let g:coc_snippet_prev = '<c-k>'

" Use <C-j> for both expand and jump (make expand higher priority.)
imap <C-j> <Plug>(coc-snippets-expand-jump)
