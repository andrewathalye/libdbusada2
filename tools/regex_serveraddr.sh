#!/bin/sh

KEY='[a-z]+'
DIRECT='[-0-9A-Za-z_\/.\*]'
ESCAPED='(%[0-9A-Fa-f]{2})'

VALUE="($DIRECT|$ESCAPED)+"
ADDRESS="$KEY:($KEY=$VALUE(,$KEY=$VALUE)*)?"
EXPR="($ADDRESS)(;$ADDRESS)*"

echo $EXPR
