with AdaBroker.OmniORB;
with Chicken_Forward;
package Egg.Impl is

   type Object is new AdaBroker.OmniORB.ImplObject with private;

   type Object_Ptr is access all Object;

   function hatch
     (Self : access Object)
      return Chicken_Forward.Ref;

private

   type Object is new AdaBroker.OmniORB.ImplObject with record
      Already : Boolean := False;
   end record;

   procedure Initialize (Self : in out Object);
   procedure Adjust     (Self : in out Object);
   procedure Finalize   (Self : in out Object);

end Egg.Impl;
