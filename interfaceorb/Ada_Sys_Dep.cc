#include <iostream>

class myException {};

int main ()
{
  if (sizeof(long) != 4)
    {
      cerr << "The size of type C++ Long is not standard." << endl;
      cerr << "AdaBroker cannot be compiled." << endl;
      throw new myException;
    };
  return 0;
}
