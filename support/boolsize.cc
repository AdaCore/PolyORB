//
// Print the Ada type corresponding to the C++ bool
//

#include <ostream.h>

int
main()
{
  cout << "Interfaces.Unsigned_" << 8*sizeof(bool) << "\n";
}
