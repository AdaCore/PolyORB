with AdaBroker.OmniORB;
package weapon.Impl is

   type Object is new AdaBroker.OmniORB.ImplObject with private;

   type Object_Ptr is access all Object;

   procedure shoot
     (Self : access Object;
      ranges : in dist);

private

   type Object is new AdaBroker.OmniORB.ImplObject with record
      null;
      -- Insert user declarations
   end record;

   procedure Initialize (Self : in out Object);
   procedure Adjust     (Self : in out Object);
   procedure Finalize   (Self : in out Object);

end weapon.Impl;
