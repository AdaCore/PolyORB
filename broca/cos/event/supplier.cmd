create channel c
create pushsupplier pss
create pullsupplier pls
connect pss to c
connect pls to c
sleep 6
produce "running test 1" in pss
sleep 6
produce "running test 2" in pss
sleep 6
produce "running test 3" in pls
sleep 6
produce "running test 4" in pls

