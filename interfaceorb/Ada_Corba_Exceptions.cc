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


#include "Ada_Corba_Exceptions.hh"
#include "Ada_exceptions.hh"


CORBA::Boolean
_omni_callTransientExceptionHandler(omniObject* omniobj,
				    CORBA::ULong retries,
				    CORBA::ULong minor,
				    CORBA::CompletionStatus status)
{
ADABROKER_TRY
  // creates an exception object
  CORBA::TRANSIENT ex (minor, status);
  // throws it
  return _omni_callTransientExceptionHandler (omniobj,
					      retries,
					      ex);
ADABROKER_CATCH
};


CORBA::Boolean
_omni_callCommFailureExceptionHandler(omniObject* omniobj,
				      CORBA::ULong retries,
				      CORBA::ULong minor,
				      CORBA::CompletionStatus status)
{
ADABROKER_TRY
  // creates an exception object
  CORBA::COMM_FAILURE ex (minor, status);
  // throws it
  return _omni_callCommFailureExceptionHandler (omniobj,
						retries,
						ex);
ADABROKER_CATCH
};


CORBA::Boolean
_omni_callSystemExceptionHandler(omniObject* omniobj,
				 CORBA::ULong retries,
				 CORBA::ULong minor,
				 CORBA::CompletionStatus status)
{
ADABROKER_TRY
  // creates an exception object
  CORBA::SystemException ex (minor, status);
  // throws it
  return _omni_callSystemExceptionHandler (omniobj,
					   retries,
					   ex);
ADABROKER_CATCH
};

