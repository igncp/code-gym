let s:path_of_vim_script_dir = fnamemodify(resolve(expand('<sfile>:p')), ':h')

fun! Basic03GetCommand()
  execute ":'<,'>w! /tmp/vim-file"
  let s:result = system('node ' . s:path_of_vim_script_dir . '/script.js')

  " call setline('.', s:result)
  " execute ':%s/\%x00//g'
endfunction
