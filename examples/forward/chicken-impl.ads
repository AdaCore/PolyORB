with AdaBroker.OmniORB;
with Egg_Forward;
package Chicken.Impl is

   type Object is new AdaBroker.OmniORB.ImplObject with private;

   type Object_Ptr is access all Object;

   procedure lay
     (Self : access Object;
      number : out CORBA.Unsigned_Short;
      Returns : out Egg_Forward.Ref);

private

   type Object is new AdaBroker.OmniORB.ImplObject with record
      Number : CORBA.Unsigned_Short := 0 ;
   end record;

   procedure Initialize (Self : in out Object);
   procedure Adjust     (Self : in out Object);
   procedure Finalize   (Self : in out Object);

end Chicken.Impl;
