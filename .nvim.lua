-- hls
vim.api.nvim_create_autocmd('FileType', {
  pattern = { 'haskell', 'lhaskell' },
  callback = function()
    vim.lsp.start {
      name = 'hls',
      cmd = { 'haskell-language-server-wrapper', '--lsp' },
      root_dir = vim.fs.root(0, { 'stack.yaml' }),
    }
  end,
})

vim.api.nvim_create_autocmd('BufEnter', {
  callback = function()
    vim.cmd.compiler 'stack'
    vim.b.dispatch = 'stack build'
    vim.b.start = 'stack ghci'
  end,
})
