///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
////                                                               ////
////                         AdaBroker                             ////
////                                                               ////
////                 class Ada_OmniObject                          ////
////                                                               ////
////                                                               ////
////   Copyright (C) 1999 ENST                                     ////
////                                                               ////
////   This file is part of the AdaBroker library                  ////
////                                                               ////
////   The AdaBroker library is free software; you can             ////
////   redistribute it and/or modify it under the terms of the     ////
////   GNU Library General Public License as published by the      ////
////   Free Software Foundation; either version 2 of the License,  ////
////   or (at your option) any later version.                      ////
////                                                               ////
////   This library is distributed in the hope that it will be     ////
////   useful, but WITHOUT ANY WARRANTY; without even the implied  ////
////   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ////
////   PURPOSE.  See the GNU Library General Public License for    ////
////   more details.                                               ////
////                                                               ////
////   You should have received a copy of the GNU Library General  ////
////   Public License along with this library; if not, write to    ////
////   the Free Software Foundation, Inc., 59 Temple Place -       ////
////   Suite 330, Boston, MA 02111-1307, USA                       ////
////                                                               ////
////                                                               ////
////                                                               ////
////   Description                                                 ////
////   -----------                                                 ////
////    This file contains wrapper functions around functions      ////
////   defined in CORBA.h They are here to handle C++ exceptions   ////
////   which could make the Ada program halt.                      ////
////                                                               ////
////                                                               ////
////   authors : Sebastien Ponce, Fabien Azavant                   ////
////   date    : 02/28/99                                          ////
////                                                               ////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////

#include <omniORB2/CORBA.h>

	      
// Ada_ORB_init
//-------------
CORBA::ORB_ptr
Ada_ORB_init(int argc, char **argv,const char *orb_identifier) {

  cerr << "*** Entering Ada_ORB_init" << endl ;
  cerr << "argc : " << argc << endl ;
  cerr << "orb_id  : " << orb_identifier << endl ;
  
  for(int j=0 ; j<argc ; j++) {
    cerr << argv[j] << endl ;
  }

  try {
    return CORBA::ORB_init(argc, argv, orb_identifier) ;
  } catch (...) {
    cerr << "caught C++ exception" << endl ;
  }
  
}


// Ada_BOA_init
//-------------
CORBA::BOA_ptr
Ada_BOA_init(CORBA::ORB_ptr orb,
	     int argc,
	     char **argv,
	     const char *boa_identifier) {

  cerr << "*** Entering Ada_BOA_init" << endl ;
  cerr << "argc : " << argc << endl ;
  cerr << "boa_id  : " << boa_identifier << endl ;
  
  for(int j=0 ; j<argc ; j++) {
    cerr << argv[j] << endl ;
  }

  try {
    return orb->BOA_init(argc, argv, boa_identifier) ;
  } catch (...) {
    cerr << "caught C++ exception" << endl ;
  }
  
}
