with AdaBroker.OmniORB;
with CORBA;
package vehicle.Impl is

   type Object is new AdaBroker.OmniORB.ImplObject with private;

   type Object_Ptr is access all Object;

   function Get_mark
     (Self : access Object)
      return CORBA.String;

   procedure Set_mark
     (Self : access Object;
      To   : in CORBA.String);

   function can_drive
     (Self : access Object;
      age : in CORBA.Unsigned_Short)
      return CORBA.Boolean;

private

   type Object is new AdaBroker.OmniORB.ImplObject with record
      null;
      -- Insert user declarations
   end record;

   procedure Initialize (Self : in out Object);
   procedure Adjust     (Self : in out Object);
   procedure Finalize   (Self : in out Object);

end vehicle.Impl;
