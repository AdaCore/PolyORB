
--  For each function defined in the IDL file, a descendant of
--  Omniproxycalldesc is created. It is the object in charge of storing the
--  arguments of the function, marshalling them into a bufferedstream, call
--  the remote object, and unmarshall the result.

with CORBA.Object;

with AdaBroker; use AdaBroker;
with AdaBroker.OmniORB;
with AdaBroker.OmniProxyCallDesc;

package AdaBroker.OmniProxyCallWrapper is

   procedure Invoke
     (Obj       : in CORBA.Object.Ref'Class;
      Call_Desc : in out OmniProxyCallDesc.Object'Class);
   --  Reimplemented in Ada to call the C++ ORB (modified by Fabien)
   --
   --  Previous solution : wrapper around void invoke(omniObject* o,
   --  OmniProxyCallDesc& call_desc) in proxyCall.cc L 46

   procedure One_Way
     (Obj       : in CORBA.Object.Ref'Class;
      Call_Desc : in out OmniProxyCallDesc.Object'Class);
   --  Reimplemented in Ada to call the C++ ORB see proxyCall.cc L181

private

   function Omni_Call_Transient_Exception_Handler
     (Obj     : in AdaBroker.OmniORB.OmniObject'Class;
      Retries : in CORBA.Unsigned_Long;
      Minor   : in CORBA.Unsigned_Long;
      Status  : in CORBA.Completion_Status)
      return CORBA.Boolean;
   --  This method is wrapped around C method
   --  _omni_callTransientExceptionHandler ( see Ada_CORBA_Exceptions.hh)

   function Omni_Comm_Failure_Exception_Handler
     (Obj     : in AdaBroker.OmniORB.OmniObject'Class;
      Retries : in CORBA.Unsigned_Long;
      Minor   : in CORBA.Unsigned_Long;
      Status  : in CORBA.Completion_Status)
      return CORBA.Boolean;
   --  This method is wrapped around C method
   --  _omni_commFailureExceptionHandler ( see Ada_CORBA_Exceptions.hh)

   function Omni_System_Exception_Handler
     (Obj     : in AdaBroker.OmniORB.OmniObject'Class;
      Retries : in CORBA.Unsigned_Long;
      Minor   : in CORBA.Unsigned_Long;
      Status  : in CORBA.Completion_Status)
      return CORBA.Boolean;
   --  This method is wrapped around C method
   --  _omni_callSystemExceptionHandler ( see Ada_CORBA_Exceptions.hh)

end AdaBroker.OmniProxyCallWrapper;
