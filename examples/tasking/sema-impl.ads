with AdaBroker.OmniORB;
package Sema.Impl is

   type Object is new AdaBroker.OmniORB.ImplObject with private;

   type Object_Ptr is access all Object;

   procedure Try_P
     (Self : access Object;
      Amount : in CORBA.Long);

   procedure P
     (Self : access Object;
      Amount : in CORBA.Long);

   procedure V
     (Self : access Object;
      Amount : in CORBA.Long);

private

   type Object is new AdaBroker.OmniORB.ImplObject with record
      null;
      -- Insert user declarations
   end record;

   procedure Initialize (Self : in out Object);
   procedure Adjust     (Self : in out Object);
   procedure Finalize   (Self : in out Object);

end Sema.Impl;
