////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class is both a C class and an Ada Class (see             ////
////     omniObject.ads). It is wrapped around Rope in order to         ////
////     the presence of non default constructors.                      ////
////     So, it provides the same functions Rope except that cons-      ////
////     tructors are replaced by Init functions.                       ////
////     It has also a pointer on the underlying Rope Object            ////
////                                                                    ////
////                                                                    ////
////                Date : 02/17/99                                     ////
////                                                                    ////
////                authors : Sebastien Ponce                           ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////


#include "Ada_Rope.hh"

Ada_Rope::Ada_Rope ()
{
  Init_Ok = false;
  C_Rope = NULL;
};
// default constructor


