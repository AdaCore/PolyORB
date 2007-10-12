create channel c
create pushconsumer psc
create pullconsumer plc
connect psc to c
connect plc to c
sleep 3
consume in psc
sleep 6
consume in plc
sleep 6
consume in psc
sleep 6
consume in plc

