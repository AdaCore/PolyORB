with CORBA.Object.OmniORB;
with Echo.Skel;
with CORBA;
package body Echo.Impl is 

   function echoString
     (Self : access Object;
      mesg : in CORBA.String)
      return CORBA.String
   is
   begin 
      return mesg;
   end echoString;

   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   procedure Initialize (Self : in out Object) is
   begin
      AdaBroker.OmniORB.Initialize
        (AdaBroker.OmniORB.ImplObject (Self),
         Echo.Repository_Id);
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
     (Echo.Repository_Id,
      Echo.Nil_Ref,
      Echo.Skel.Dispatch'Access);
end Echo.Impl;
