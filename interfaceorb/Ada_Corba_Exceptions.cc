#include "Ada_Corba_Exceptions.hh"
#include "Ada_exceptions.hh"

CORBA::Boolean
_omni_callTransientExceptionHandler(Ada_OmniObject* omniobj,
				    CORBA::ULong retries,
				    CORBA::ULong minor,
				    CORBA::CompletionStatus status)
{
  ADABROKER_TRY

    // Create an exception object.
    CORBA::TRANSIENT ex (minor, status);

    // Throw it.
    return _omni_callTransientExceptionHandler (omniobj->CPP_Object,
					        retries,
					        ex);
  ADABROKER_CATCH

    // Never reach this code. Just a default return for dummy
    // compilers.
    CORBA::Boolean default_result = false;
    return default_result;
}

CORBA::Boolean
_omni_callCommFailureExceptionHandler(Ada_OmniObject* omniobj,
				      CORBA::ULong retries,
				      CORBA::ULong minor,
				      CORBA::CompletionStatus status)
{
  ADABROKER_TRY

    // Create an exception object.
    CORBA::COMM_FAILURE ex (minor, status);

    // Throw it.
    return _omni_callCommFailureExceptionHandler (omniobj->CPP_Object,
						  retries,
						  ex);
  ADABROKER_CATCH

    // Never reach this. Just a default return for dummy compilers.
    CORBA::Boolean default_result = false;
    return default_result; 
}

CORBA::Boolean
_omni_callSystemExceptionHandler(Ada_OmniObject* omniobj,
				 CORBA::ULong retries,
				 CORBA::ULong minor,
				 CORBA::CompletionStatus status)
{
  ADABROKER_TRY

    // Create an exception object.
    CORBA::SystemException ex (minor, status);

    // Throw it.
    return _omni_callSystemExceptionHandler (omniobj->CPP_Object,
					     retries,
					     ex);
  ADABROKER_CATCH

    // Never reach this code. Just a default return for dummy
    // compilers.
    CORBA::Boolean default_result = false;
    return default_result; 
}

