///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
////                                                               ////
////                         AdaBroker                             ////
////                                                               ////
////                 class Ada_Corba_Exceptions                    ////
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
////     This class interfaces C and Ada classes for exceptions.   ////
////     It provides some methods corresponding to these of        ////
////     CORBA.h file.                                             ////
////     The methods of CORBA.h take arguments of type             ////
////     system_exception. Since this type does not exist in Ada   ////
////     the methods defined here have different parameters.       ////
////     These methods are then used in Ada by importing them.     ////
////     (see corba-exceptions.ads/b)                              ////
////                                                               ////
////                                                               ////
////                                                               ////
////   authors : Sebastien Ponce, Fabien Azavant                   ////
////   date    : 02/28/99                                          ////
////                                                               ////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////


#include <omniORB2/CORBA.h>

CORBA::Boolean
_omni_callTransientExceptionHandler(omniObject* omniobj,
				    CORBA::ULong retries,
				    CORBA::ULong minor,
				    CORBA::CompletionStatus status);
// equivalent of _omni_callTransientExceptionHandler(omniObject*,
//             				             CORBA::ULong,
//				                     const CORBA::TRANSIENT&);
// (see CORBA.h L 2702)


CORBA::Boolean
_omni_callCommFailureExceptionHandler(omniObject*,
				      CORBA::ULong,
				      CORBA::ULong minor,
				      CORBA::CompletionStatus status);
// equivalent of _omni_callCommFailureExceptionHandler(omniObject*,
//             				               CORBA::ULong,
//				                       const CORBA::COMM_FAILURE&);
// (see CORBA.h L 2707)


CORBA::Boolean
_omni_callSystemExceptionHandler(omniObject*,
				 CORBA::ULong,
				 CORBA::ULong minor,
				 CORBA::CompletionStatus status);
// equivalent of _omni_callSystemExceptionHandler(omniObject*,
//             				          CORBA::ULong,
//				                  const CORBA::SystemException&);
// (see CORBA.h L 2712)
