////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class is as well a C class as an Ada Class (see           ////
////     omniObject.ads). It is wrapped around Rope in order to         ////
////     the presence of non default constructors.                      ////
////     So, it provides the same functions Rope except that cons-      ////
////     tructors are replaced by Init functions.                       ////
////     It has also a pointer on the underlining Rope Object           ////
////                                                                    ////
////                                                                    ////
////                Date : 02/17/99                                     ////
////                                                                    ////
////                authors : Sebastien Ponce                           ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////


#include "Ada_Rope.hh"

void
Ada_Rope::Init (Anchor *a,
		unsigned int maxStrands,
		_CORBA_Boolean heapAllocated);
{
  // Creation of the underlining omniobject_C2Ada object
  C_Rope = new Rope (a,maxStrands,heapAllocated);
  // updating of Init_OK flag
  Init_Ok = true;
  return;  
}


Ada_Rope::Ada_Rope ()
{
  Init_Ok = false;
};
// default constructor


extern void
Ada_Rope::raise_ada_exception (const char *msg);
// See implementation in omniobject.adb

