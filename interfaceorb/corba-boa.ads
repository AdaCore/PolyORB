with System;

with AdaBroker.OmniORB;

package CORBA.BOA is

   type Object is private;

   procedure Implementation_Is_Ready
     (Self         : in Object;
      Non_Blocking : in Boolean := False);
   --  Calling this function will cause the BOA to start accepting requests
   --  from other address spaces.  Default behaviour will block
   --  indefinitely on this call if the Non_Blocking argument is not set to
   --  True

   procedure Implementation_Shutdown (Self : in Object);
   pragma Import
     (CPP, Implementation_Shutdown, "impl_shutdown__FPQ25CORBA3BOA");
   --  omniORB2 specific.
   --
   --  This is the reverse of impl_is_ready().  When this call returns, all
   --  the internal threads and network connections will be shutdown.  Any
   --  thread blocking on impl_is_ready is unblocked.  When this call
   --  returns, requests from other address spaces will not be dispatched.
   --  The BOA can be reactivated by impl_is_ready(), it will continue to
   --  use the existing network addresses when reactivated.
   --
   --  Note: This function should not be called in the implementation of a
   --  CORBA interface. Otherwise, this call will be blocked indefinitely
   --  waiting on itself to complete the request.


   procedure Destroy (Self : in Object);
   pragma Import
     (CPP, Destroy, "destroy__FPQ25CORBA3BOA");
   --  omniORB2 specific.
   --
   --  Calling this function will destroy this BOA. The function will call
   --  impl_shutdown() implicitly if it has not been called. When this call
   --  returns, the network addresses (endpoints) where this BOA listens on
   --  will be freed.
   --
   --  Note: After this call, the BOA should not be used directly or
   --  indirectly, otherwise the behaviour is undefined. If there is any
   --  object implementation still registered with the BOA when this
   --  function is called, the object implementation should not be called
   --  afterwards. This function does not call the dispose method of the
   --  implementations.
   --
   --  Note: Initialisation of another BOA using ORB::BOA_init() is not
   --  supported. The behaviour of ORB::BOA_init() after this function is
   --  called is undefined.


   procedure Object_Is_Ready
     (Self : in Object;
      Obj  : in AdaBroker.OmniORB.ImplObject'Class);
   --  Tell the BOA that this object is ready to accept connexions it has
   --  to be done once (and only once) for each local object.  The user HAS
   --  to call this function, it cannot be called automatically.

   procedure Dispose_Object
     (Self : in Object;
      Obj  : in AdaBroker.OmniORB.ImplObject'Class);
   --  Tell the BOA that this object is going to be destroyed and that it
   --  should not accept connexions any longer The user HAS to call this
   --  function, it cannot be called automatically.

private

   type Object is new System.Address;

end CORBA.BOA;

