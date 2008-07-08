# !/bin/sh

rm=/bin/rm
cp=/bin/cp
cat=/bin/cat

if [ $# -ne 2 ] ; then
  echo "Usage: $0 working_directory n_iterations" ;
  exit 1;
fi;


# Number of iterations
n_iter=$2

# Working directoty
w_d=$1
mkdir $w_d

# First Test
test_mode=none
res_file=no_opt
# Build the non optimization test
./build_test.sh $test_mode


# Running the server
./server $n_iter > "$w_d/$res_file" &
pid_serv=$!

sleep 1

#running the client
./client $n_iter # > "$w_d/$res_file_total"

# Kill the server
kill -9 $pid_serv

# Second Test
test_mode=cpu
res_file=opt_cpu
# Build the non optimization test
./build_test.sh $test_mode

# Running the server
./server $n_iter > "$w_d/$res_file" &
pid_serv=$!

sleep 1

#running the client
./client $n_iter # > "$w_d/$res_file_total"

# Kill the server
kill -9 $pid_serv

# Third Test
test_mode=mem
res_file=opt_mem
# Build the non optimization test
./build_test.sh $test_mode

# Running the server
./server $n_iter > "$w_d/$res_file" &
pid_serv=$!

sleep 1

#running the client
./client $n_iter # > "$w_d/$res_file_total"

# Kill the server
kill -9 $pid_serv

# Graphic building
$cp split.sh draw_eps.gnu draw_png.gnu $w_d
cd $w_d
./split.sh
cd ..

echo "Done!"
exit 0

