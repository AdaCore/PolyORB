with Broca.Imp_Object;

package CORBA.BOA is

   procedure Implementation_Is_Ready (Non_Blocking : in Boolean := False)
     renames Broca.Imp_Object.Implementation_Is_Ready;
   --  Calling this function will cause the BOA to start accepting requests
   --  from other address spaces.  Default behaviour will block
   --  indefinitely on this call if the Non_Blocking argument is not set to
   --  True

   procedure Object_Is_Ready
     (Obj  : access Broca.Imp_Object.Implemented_Object'Class)
     renames Broca.Imp_Object.Object_Is_Ready;
   --  Tells the BOA that this object is ready to accept connexions.  It has
   --  to be done once (and only once) for each local object.  The user HAS
   --  to call this function, it cannot be called automatically.

--    procedure Dispose_Object
--      (Self : in Object;
--       Obj  : in OmniObject.Implemented_Object'Class);
--    --  Tells the BOA that this object is going to be destroyed and that it
--    --  should not accept connexions any longer The user HAS to call this
--    --  function, it cannot be called automatically.

end CORBA.BOA;

