with AdaBroker.OmniORB;
package Counter.Impl is

   type Object is new AdaBroker.OmniORB.ImplObject with private;

   type Object_Ptr is access all Object;

   function Inc
     (Self : access Object;
      Amount : in CORBA.Long)
      return CORBA.Long;

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

end Counter.Impl;
