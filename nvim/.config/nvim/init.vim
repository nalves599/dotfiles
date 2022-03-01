
set path+=**

" Ignore files
set wildignore+=*.pyc
set wildignore+=*_build/*
set wildignore+=**/coverage/*
set wildignore+=**/node_modules/*
set wildignore+=**/android/*
set wildignore+=**/ios/*
set wildignore+=**/.git/*

" Plugins
call plug#begin('~/.config/nvim/autoload/plug')
  " LSP Plugins
  Plug 'neovim/nvim-lspconfig'
  Plug 'hrsh7th/cmp-nvim-lsp'
  Plug 'hrsh7th/cmp-buffer'
  Plug 'hrsh7th/cmp-path'
  Plug 'hrsh7th/nvim-cmp'

  " Misc plugins
  Plug 'jeffkreeftmeijer/vim-numbertoggle'
  Plug 'ryanoasis/vim-devicons'
  Plug 'scrooloose/nerdcommenter'
  Plug 'sbdchd/neoformat'
  Plug 'nvim-lualine/lualine.nvim'
  "Plug 'glepnir/dashboard-nvim'

  " Themes
  Plug 'gruvbox-community/gruvbox'
  "Plug 'Mofiqul/dracula.nvim'
  "Plug 'NTBBloodbath/doom-one.nvim'

  " Snippets
  Plug 'L3MON4D3/LuaSnip'
  Plug 'rafamadriz/friendly-snippets'

  " Treesitter - syntax highlight
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
  Plug 'nvim-treesitter/playground'

  " Harpoon - file navigation
  Plug 'nvim-lua/plenary.nvim'
  Plug 'nvim-lua/popup.nvim'
  Plug 'theprimeagen/harpoon'

  " Git worktree
  Plug 'theprimeagen/git-worktree.nvim'

  " Fugitive Vim - git
  Plug 'tpope/vim-fugitive'
  Plug 'junegunn/gv.vim'

  " Telescope
  Plug 'nvim-telescope/telescope.nvim'
  Plug 'nvim-telescope/telescope-fzy-native.nvim'

call plug#end()

" Load my plugins configs
lua require('my-plugins-config')
"lua require('doom-one').setup({ transparent_background = true })

let g:neoformat_try_node_exe = 1

set nocompatible                " disable compatibility to old-time vi
set showmatch                   " show matching
set ignorecase                  " case insensitive
set mouse=v                     " middle-click paste with
set nohlsearch                  " no highlight search
set hidden                      " save buffer on background
set noerrorbells                " no errorbells
set nowrap                      " no wrap line
set incsearch                   " incremental search
set tabstop=2                   " number of columns occupied by a tab
set softtabstop=2               " see multiple spaces as tabstops so <BS> does the right thing
set colorcolumn=80              " add 80 columns line
set expandtab                   " converts tabs to white space
set shiftwidth=2                " width for autoindents
set autoindent                  " indent a new line the same amount as the line just typed
set number                      " add line numbers
set wildmode=longest,list,full  " get bash-like tab completions
set signcolumn=yes              " add column at the begining
set scrolloff=8                 " when scrolling set 8 lines away
filetype plugin on              " allow auto-indenting depending on file type
syntax on                       " syntax highlighting
set mouse=a                     " enable mouse click
set clipboard=unnamedplus       " using system clipboard
filetype plugin on
set foldmethod=indent

let mapleader = ' '

" Remaps
vnoremap K :m '<-2<CR>gv=gv
vnoremap J :m '>+1<CR>gv=gv
nnoremap Y yg$
nnoremap J mzJ`z
inoremap <C-c> <esc>
nmap <leader>wv <C-w>v
nmap <leader>wl <C-w>l
nmap <leader>wh <C-w>h
nmap <leader>wk <C-w>k
nmap <leader>wj <C-w>j

fun! EmptyRegisters()
    let regs=split('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/-"', '\zs')
    for r in regs
        call setreg(r, [])
    endfor
endfun

augroup Default
  autocmd!
  autocmd BufWritePre *.ts Neoformat
  autocmd BufWritePre * %s/\s\+$//e
augroup END

" Colorscheme
set termguicolors
colorscheme gruvbox
highlight Normal guibg=none
