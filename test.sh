#!/bin/sh
function testreal {
   socat -v unix:outsock exec:cat &
   sleep 1
   kill $TESTPID
}

function testsave {
   socat -u -v unix:outsock create:out.dat &
   sleep 1
   kill $TESTPID
   echo
   hexdump -C out.dat
   rm out.dat
}

gprbuild -Pgpr/tests || exit -1
./tests &
TESTPID=$!
sleep 1
#testreal
testsave

