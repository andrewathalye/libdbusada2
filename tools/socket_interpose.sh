#!/bin/sh
####################
# SOCKET_INTERPOSE #
####################
# Prints all data that would go to D-Bus Socket and redirects it through xd

rm -r dat>/dev/null
mkdir dat
echo "Create socket"
socat -r dat/todbus.dat -R dat/fromdbus.dat UNIX-LISTEN:dat/interpose.sock UNIX-CONNECT:/run/user/$UID/bus &
sleep 0.25
echo "Run command. Find data in dat/"
DBUS_SESSION_BUS_ADDRESS=unix:path=$PWD/dat/interpose.sock "$@"
