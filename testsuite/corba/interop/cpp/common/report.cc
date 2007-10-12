#include <stdlib.h>
#include <iostream>

using namespace std;

const int MAX = 60;
bool passed = true;

void output(char *s, bool pass)
{
  char Line[MAX + 1];
  unsigned int len = strlen(s);
  unsigned int i;

  for (i = 0 ; i < sizeof Line - 1; i++) 
    Line [i] = (i < len) ? s [i] : '.';
  
  Line [sizeof Line - 1] = '\0';

  cout << Line << (pass ? " : PASSED" : " : FAILED") << endl;

  passed = passed && pass;
}

void new_test (char *s)
{
  cout << "==> Begin test " << s << "<==" << endl;
}

void end_report ()
{
  output ("END TESTS", passed);
}
