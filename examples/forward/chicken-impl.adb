with Chicken.Skeleton ;
with Egg_forward ;
with Egg.Impl ;
with Egg ;
with CORBA ;
use type CORBA.Unsigned_Short ;


package body Chicken.Impl is


   -----------------------
   -- IDL definitions   --
   -----------------------

   --  lay
   -------------------------------
   procedure lay(Self : access Object;
                 number : out CORBA.Unsigned_Short;
                 Returns : out Egg_forward.Ref) is
      Myegg : Egg.Impl.Object_Ptr := new Egg.Impl.Object ;
   begin
      Egg.Impl.Set_Boa(Myegg.all, Self.all.Boa) ;
      CORBA.Boa.Object_Is_Ready(Self.all.Boa, Myegg.all) ;
      Self.all.Number := Self.all.Number + 1 ;
      Number := Self.all.Number ;
      Returns := Egg.Convert_Forward.To_Forward(Egg.To_Ref(Myegg.all)) ;
   end;



   -- Set_Boa
   ----------
   procedure Set_Boa(Self : in out Object ;
                     Boa : CORBA.Boa.Object) is
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
      AdaBroker.OmniORB.Initialize(AdaBroker.OmniORB.ImplObject(Self)) ;
      Initialize_Local_Object(Self,
                        Repository_Id,
                        Chicken.Skeleton.Dispatch'Access);
      -- You can add things *BELOW* this line

   end Initialize ;


   -- Adjust
   ---------
   procedure Adjust(Self: in out Object) is
   begin
   AdaBroker.OmniORB.Adjust(AdaBroker.OmniORB.ImplObject(Self)) ;
      -- You can add things *BELOW* this line

   end Adjust ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out Object) is
   begin

      -- You can add things *BEFORE* this line
   AdaBroker.OmniORB.Finalize(AdaBroker.OmniORB.ImplObject(Self)) ;
   end Finalize ;


end Chicken.Impl ;
