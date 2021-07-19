" Plugins
call plug#begin('~/.config/nvim/autoload/plug')

  " Airline/Powerline
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  " File Explorer
  Plug 'scrooloose/NERDTree'
  " Markdown Preview
  " Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && yarn install'  }
  " Line Number
  Plug 'jeffkreeftmeijer/vim-numbertoggle'

call plug#end()


" Configurations
set number " Show line numbers

" Set tab and spaces
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set autoindent

" Use system clipboard
set clipboard=unnamedplus


" NERDTree Configs
nmap <C-t>  :NERDTreeToggle<CR>

let NERDTreeShowHidden=1
let NERDTreeIgnore = ['__pycache__']


" Airline Configs
let g:airline_theme='powerlineish'
