#include <stdlib.h>
#include <iostream>
#include "n_iterations.hh"

using namespace std;

static const int N_OPERATIONS = 100;

static int actualIterationNumber;

static double durations[N_OPERATIONS];
static double times[N_OPERATIONS];
static bool initialized = false;

void mayBeInitializeStats (void) {
  if (!initialized) {
    initialized = true;
    actualIterationNumber = 0;
    for (int i; i < N_OPERATIONS; i++) {
      times[i] = 0;
    };
  };
}

void printTable (void) {
  for (int i; i < N_OPERATIONS; i++) {
    cout << i 
	 << "   " 
	 << durations[i]
	 << endl;
  };
}

void insertDuration (double d, int index) {
  actualIterationNumber++;
  
  //  If the operation is already called, the new duration is the
  //  average of all the durations

  durations[index] = ((durations[index] * times[index]) + d) / 
    (times[index] + 1);
  times[index] = times[index] + 1;

  if (actualIterationNumber == N_ITERATIONS) {
    printTable ();
    initialized = false;
  };
}

