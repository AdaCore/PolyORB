with weapon.Skeleton ;
with Text_IO; use Text_IO;


package body weapon.Impl is


   -----------------------
   -- IDL definitions   --
   -----------------------

   procedure shoot(Self : access Object; ranges : in Dist) is
   begin
      null;
   end;





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
                        weapon.Skeleton.Dispatch'Access,
                        weapon.Is_A'Access) ;
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


end weapon.Impl ;




