with CORBA.Object.OmniORB;
with Egg.Skel;
with Chicken_Forward;
with Chicken.Impl;
with CORBA.BOA;
package body Egg.Impl is 

   function hatch
     (Self : access Object)
      return Chicken_Forward.Ref
   is
   begin 
      if Self.Already then
         raise Already_Hatched;
      end if;
      declare
         MyChicken : Chicken.Impl.Object_Ptr := new Chicken.Impl.Object;
      begin
         Self.Already := True ;
         CORBA.BOA.Object_Is_Ready (MyChicken.all);
         return Chicken.Convert_Forward.To_Forward
            (Chicken.To_Ref (MyChicken.all));
      end ;
   end hatch;

   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   procedure Initialize (Self : in out Object) is
   begin
      AdaBroker.OmniORB.Initialize
        (AdaBroker.OmniORB.ImplObject (Self),
         Egg.Repository_Id);
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
     (Egg.Repository_Id,
      Egg.Nil_Ref,
      Egg.Skel.Dispatch'Access);
end Egg.Impl;
