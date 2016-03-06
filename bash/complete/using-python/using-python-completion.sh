# https://www.debian-administration.org/article/317/An_introduction_to_bash_completion_part_2

_usingPython()  {
  local cur prev opts

  COMPREPLY=( $(./using-python-completion.py $COMP_WORDS $COMP_CWORD | xargs echo) )
  return 0
}

complete -F _usingPython using-python.sh