with CORBA.Object.OmniORB;
with Chicken.Skel;
with Egg_Forward;
with Egg.Impl;
with CORBA.BOA;
with CORBA; use CORBA;
package body Chicken.Impl is 

   procedure lay
     (Self : access Object;
      number : out CORBA.Unsigned_Short;
      Returns : out Egg_Forward.Ref)
   is
      MyEgg : Egg.Impl.Object_Ptr := new Egg.Impl.Object;
   begin
      BOA.Object_Is_Ready (MyEgg.all);
      Self.Number := Self.Number + 1;
      Number := Self.Number;
      Returns := Egg.Convert_Forward.To_Forward (Egg.To_Ref (MyEgg.all));
   end lay;

   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   procedure Initialize (Self : in out Object) is
   begin
      AdaBroker.OmniORB.Initialize
        (AdaBroker.OmniORB.ImplObject (Self),
         Chicken.Repository_Id);
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
     (Chicken.Repository_Id,
      Chicken.Nil_Ref,
      Chicken.Skel.Dispatch'Access);
end Chicken.Impl;
