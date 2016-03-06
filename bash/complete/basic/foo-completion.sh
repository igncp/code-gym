# http://www.tldp.org/LDP/abs/html/tabexpansion.html

_fooCompletion () {
  local current_word

  COMPREPLY=()
  current_word=${COMP_WORDS[COMP_CWORD]}

  case "$current_word" in
    -*)
    COMPREPLY=( $( compgen -W '--option-foo --option-bar  --option-baz' -- $current_word ) );;
  esac

  return 0
}

complete -F _fooCompletion -o filenames ./foo.sh
