with Egg.Skeleton ;
with Chicken_forward ;
with Chicken ;
with Chicken.Impl ;
with Corba.Boa ;

package body Egg.Impl is


   -----------------------
   -- IDL definitions   --
   -----------------------

   --  hatch
   -------------------------------
   function hatch(Self : access Object) return Chicken_forward.Ref is
   begin
      if Self.all.Already then
         raise Already_Hatched ;
      end if ;
      declare
         Mychicken : Chicken.Impl.Object_Ptr := new Chicken.Impl.Object ;
      begin
         Self.all.Already := True ;
         Chicken.Impl.Set_Boa(Mychicken.all, Self.all.Boa) ;
         Corba.Boa.Object_Is_Ready(Self.all.Boa, Mychicken.all) ;
         return Chicken.Convert_Forward.To_Forward(Chicken.To_Ref(Mychicken.all)) ;
      end ;
   end ;


   -- Set_Boa
   ----------
   procedure Set_Boa(Self : in out Object ;
                     Boa : Corba.Boa.Object) is
   begin
      Self.Boa := Boa ;
   end ;




   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   -- Initialize
   -------------
   procedure Initialize(Self : in out Object) is
   begin
      Omniobject.Initialize(Omniobject.Implemented_Object(Self)) ;
      Init_Local_Object(Self,
                        Repository_Id,
                        Egg.Skeleton.Dispatch'Access,
                        Egg.Is_A'Access) ;
      -- You can add things *BELOW* this line

   end Initialize ;


   -- Adjust
   ---------
   procedure Adjust(Self: in out Object) is
   begin
   Omniobject.Adjust(Omniobject.Implemented_Object(Self)) ;
      -- You can add things *BELOW* this line

   end Adjust ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out Object) is
   begin

      -- You can add things *BEFORE* this line
   Omniobject.Finalize(Omniobject.Implemented_Object(Self)) ;
   end Finalize ;


end Egg.Impl ;
