#include <omniORB2/CORBA.h>
#include "Ada_OmniObject.hh"

CORBA::Boolean
_omni_callTransientExceptionHandler(Ada_OmniObject* omniobj,
				    CORBA::ULong retries,
				    CORBA::ULong minor,
				    CORBA::CompletionStatus status);
// equivalent of _omni_callTransientExceptionHandler(omniObject*,
//             				             CORBA::ULong,
//				                     const CORBA::TRANSIENT&);
// (see CORBA.h L 2702)


CORBA::Boolean
_omni_callCommFailureExceptionHandler(Ada_OmniObject*,
				      CORBA::ULong,
				      CORBA::ULong minor,
				      CORBA::CompletionStatus status);
// equivalent of _omni_callCommFailureExceptionHandler(omniObject*,
//             				               CORBA::ULong,
//				                       const CORBA::COMM_FAILURE&);
// (see CORBA.h L 2707)


CORBA::Boolean
_omni_callSystemExceptionHandler(Ada_OmniObject*,
				 CORBA::ULong,
				 CORBA::ULong minor,
				 CORBA::CompletionStatus status);
// equivalent of _omni_callSystemExceptionHandler(omniObject*,
//             				          CORBA::ULong,
//				                  const CORBA::SystemException&);
// (see CORBA.h L 2712)
