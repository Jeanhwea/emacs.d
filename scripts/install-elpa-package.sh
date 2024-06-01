#!/usr/bin/env bash
EMACS=emacs

MACUNAME=$(uname -s | grep -i darwin)
if [ X"$MACUNAME" != X"" ]; then
    EMACS="$HOME/Applications/Emacs.app/Contents/MacOS/Emacs"
fi

$EMACS --script ~/.emacs.d/scripts/instpkg.el
