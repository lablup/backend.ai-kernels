#!/bin/bash
while read -p "Backend.AI >> " input
do
  if [ "${input}" == "clear" ]; then
    clear
  elif [ "${input}" == "quit" ]; then
    break
  elif [ "${input}" == "exit" ]; then
    break
  elif [ "${input}" == "" ]; then
    :
  else
    backend.ai ${input}
  fi
done
