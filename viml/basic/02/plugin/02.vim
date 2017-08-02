let s:path_of_vim_script_dir = fnamemodify(resolve(expand('<sfile>:p')), ':h')

fun! Basic02GetCommand()
  let s:result = system('sh ' . s:path_of_vim_script_dir . '/script.sh')

  echo s:result
endfunction
