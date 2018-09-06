if [ "$#" -eq 0 ]; then
  /usr/bin/env
elif [ "$1" == "--null" ]; then
  /usr/bin/env | tr '\n' '\000'
else
  /usr/bin/env "$@"
fi