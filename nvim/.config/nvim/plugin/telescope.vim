nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>ff <cmd>Telescope find_files find_command=rg,-S,--hidden,--files,-g,!.git<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fr <cmd>Telescope oldfiles<cr>
nnoremap <leader>fw <cmd>lua require('telescope').extensions.git_worktree.git_worktrees()<cr>

" Open file explorer
nnoremap <leader>fe <cmd>Ex<cr>
