with AdaBroker.OmniORB;
with CORBA;

package Args.Impl is

   type Object is new AdaBroker.OmniORB.ImplObject with private;

   type Object_Ptr is access all Object;

   function Plus1
     (Self : access Object;
      A : in CORBA.Long)
      return CORBA.Long;

   function Plus2
     (Self : access Object;
      A : in CORBA.Long;
      B : in CORBA.Long)
      return CORBA.Long;


   function Plus3
     (Self : access Object;
      A : in CORBA.Long;
      B : in CORBA.Long;
      C : in CORBA.Long)
      return CORBA.Long;

   procedure Plus_Minus
     (Self : access Object;
      A : in CORBA.Long;
      B : in CORBA.Long;
      P : out CORBA.Long;
      M : out CORBA.Long);


private

   -- You may add fields to this record
   type Object is new AdaBroker.OmniORB.ImplObject with record
      null;
      -- Insert user declarations
   end record;

   procedure Initialize (Self : in out Object);
   procedure Adjust     (Self : in out Object);
   procedure Finalize   (Self : in out Object);

end Args.Impl;
