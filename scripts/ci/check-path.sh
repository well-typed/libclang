#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
#set -o xtrace

if [ $# -gt 0 ] ; then
  echo "Usage: $(basename "$0")"
  echo
  echo 'Check directories in PATH for the following conditions:'
  echo
  echo '* noexist - directory does not exist'
  echo '* empty   - directory is empty'
  echo '* llvm*   - directory contains entries starting with llvm'
  echo '* clang*  - directory contains entries starting with clang'
  exit 2
fi

while IFS=$'\n' read -r dir ; do
  if [ ! -d "${dir}" ] ; then
    echo "noexist ${dir}"
    continue
  fi
  if [ -z "$(ls -A "${dir}")" ] ; then
    echo "empty   ${dir}"
    continue
  fi
  if ls "${dir}/llvm"* >/dev/null 2>&1 ; then
    echo "llvm*   ${dir}"
  fi
  if ls "${dir}/clang"* >/dev/null 2>&1 ; then
    echo "clang*  ${dir}"
  fi
done < <(echo "${PATH}" | tr ':' '\n')
