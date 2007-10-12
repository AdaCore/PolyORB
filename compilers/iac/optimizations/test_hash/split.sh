# !/bin/sh

rm -f opt_cpu.order  > /dev/null
rm -f opt_cpu.random > /dev/null 
rm -f opt_mem.order > /dev/null
rm -f opt_mem.random > /dev/null
rm -f no_opt.order > /dev/null
rm -f no_opt.random > /dev/null

tail -200 no_opt | head -100 > no_opt.order 
tail -100 no_opt | head -100 > no_opt.random
tail -200 opt_cpu | head -100 > opt_cpu.order
tail -100 opt_cpu | head -100 > opt_cpu.random
tail -200 opt_mem | head -100 > opt_mem.order
tail -100 opt_mem | head -100 > opt_mem.random

gnuplot draw_png.gnu
gnuplot draw_eps.gnu


