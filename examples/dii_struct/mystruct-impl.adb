with CORBA.Object.OmniORB;
with Mystruct.Skel;
package body Mystruct.Impl is

   function echoStruct
     (Self : access Object;
      arg : in simple_struct)
      return simple_struct
   is
      Res : Simple_Struct;
   begin
      Res.A := Arg.A;
      Res.B := Arg.B;
      return Res;
   end echoStruct;

   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   procedure Initialize (Self : in out Object) is
   begin
      AdaBroker.OmniORB.Initialize
        (AdaBroker.OmniORB.ImplObject (Self),
         Mystruct.Repository_Id);
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
     (Mystruct.Repository_Id,
      Mystruct.Nil_Ref,
      Mystruct.Skel.Dispatch'Access);
end Mystruct.Impl;
