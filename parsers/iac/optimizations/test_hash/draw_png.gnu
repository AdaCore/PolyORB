set terminal png small 
set output 'order.png'

set title 'Fetching times (ordered)'

set xlabel 'Operation index'
set ylabel 'Implementation fetching time (sec)'

#set yrange [19e-05:23e-05]

plot    'no_opt.order'  using 1:2  title 'No_Opt'  with lines,\
	'opt_cpu.order' using 1:2  title 'CPU_Opt' with lines,\
	'opt_mem.order' using 1:2  title 'Mem_Opt' with lines


set terminal png small 
set output 'random.png'

set title 'Fetching times (random)'

set xlabel 'Operation index'
set ylabel 'Implementation fetching time (sec)'

#set yrange [19e-05:23e-05]

plot    'no_opt.random'  using 1:2  title 'No_Opt'  with lines,\
	'opt_cpu.random' using 1:2  title 'CPU_Opt' with lines,\
	'opt_mem.random' using 1:2  title 'Mem_Opt' with lines

