" Plugins
call plug#begin('~/.config/nvim/autoload/plug')

  Plug 'jeffkreeftmeijer/vim-numbertoggle'
  Plug 'ryanoasis/vim-devicons'
  Plug 'scrooloose/nerdtree'
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
  Plug 'scrooloose/nerdcommenter'
  Plug 'nvim-lua/plenary.nvim'
  Plug 'nvim-telescope/telescope.nvim'
  Plug 'neoclide/coc.nvim', { 'branch': 'release' }
  Plug 'airblade/vim-gitgutter'

  " Themes
  Plug 'mhinz/vim-startify'
  Plug 'morhetz/gruvbox'

call plug#end()

set nocompatible            " disable compatibility to old-time vi
set showmatch               " show matching
set ignorecase              " case insensitive
set mouse=v                 " middle-click paste with
set nohlsearch              " no highlight search
set hidden                  " save buffer on background
set noerrorbells            " no errorbells
set nowrap                  " no wrap line
set incsearch               " incremental search
set tabstop=2               " number of columns occupied by a tab
set softtabstop=2           " see multiple spaces as tabstops so <BS> does the right thing
set expandtab               " converts tabs to white space
set shiftwidth=2            " width for autoindents
set autoindent              " indent a new line the same amount as the line just typed
set number                  " add line numbers
set wildmode=longest,list   " get bash-like tab completions
set signcolumn=yes          " add column at the begining
filetype plugin on          " allow auto-indenting depending on file type
syntax on                   " syntax highlighting
set mouse=a                 " enable mouse click
set clipboard=unnamedplus   " using system clipboard
filetype plugin on
set foldmethod=indent

let mapleader = ' '

" NERDTree Configs
nmap <C-t>  :NERDTreeToggle<CR>

let NERDTreeShowHidden=1
let NERDTreeIgnore = ['__pycache__']

" Telescope Configs
nnoremap ff     <cmd>Telescope find_files<cr>
nnoremap fg     <cmd>Telescope live_grep<cr>
nnoremap fb     <cmd>Telescope buffers<cr>
nnoremap fh     <cmd>Telescope help_tags<cr>

lua << EOF
require('telescope').setup{
  defaults = {
    file_ignore_patterns = { "node_modules", "%.class" }
  }
}
EOF

fun! Trimwhitespace()
  let l:save = winsaveview()
  keeppatterns %s/\s\+$//e
  call winrestview(l:save)
endfun

augroup DEFAULT
  autocmd!
  autocmd BufWritePre * :call Trimwhitespace()
augroup END


" Colorscheme
set termguicolors
colorscheme gruvbox
highlight Normal guibg=none
