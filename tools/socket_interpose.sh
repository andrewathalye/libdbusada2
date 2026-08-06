#!/bin/sh
####################
# SOCKET_INTERPOSE #
####################
# Prints all data that would go to D-Bus Socket and redirects it through xd

rm interpose.sock todbus.dat fromdbus.dat
echo "Create socket"
socat -r todbus.dat -R fromdbus.dat UNIX-LISTEN:interpose.sock UNIX-CONNECT:/run/user/$UID/bus &
sleep 0.25
echo "Run command. Find data in todbus.dat and fromdbus.dat."
DBUS_SESSION_BUS_ADDRESS=unix:path=$PWD/interpose.sock "$@"
