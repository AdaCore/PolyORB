with CORBA.Object.OmniORB;
with Counter.Skel;
package body Counter.Impl is 

   function Inc
     (Self : access Object;
      Amount : in CORBA.Long)
      return CORBA.Long
   is
   begin 
      -- Insert user code
   end Inc;

   procedure P
     (Self : access Object;
      Amount : in CORBA.Long)
   is
   begin 
      -- Insert user code
   end P;

   procedure V
     (Self : access Object;
      Amount : in CORBA.Long)
   is
   begin 
      -- Insert user code
   end V;

   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   procedure Initialize (Self : in out Object) is
   begin
      AdaBroker.OmniORB.Initialize
        (AdaBroker.OmniORB.ImplObject (Self),
         Counter.Repository_Id);
      -- Add user code *BELOW* this line
   end Initialize;

   procedure Adjust (Self: in out Object) is
   begin
      AdaBroker.OmniORB.Adjust
        (AdaBroker.OmniORB.ImplObject (Self));
      -- Add user code *BELOW* this line
   end Adjust;

   procedure Finalize (Self : in out Object) is
   begin
      -- Add user code *BEFORE* this line
      AdaBroker.OmniORB.Finalize
        (AdaBroker.OmniORB.ImplObject (Self));
   end Finalize;

begin
   CORBA.Object.OmniORB.Register
     (Counter.Repository_Id,
      Counter.Nil_Ref,
      Counter.Skel.Dispatch'Access);
end Counter.Impl;
