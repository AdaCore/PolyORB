with vehicle.Skeleton ;
with Corba ; use Corba;
with Text_IO; use Text_IO;

package body vehicle.Impl is
   Mark : Corba.String ;


   -----------------------
   -- IDL definitions   --
   -----------------------

   -- Get_mark
   --------------------------
   function Get_mark(Self : access Object) return Corba.String is
   begin
      return (Mark);
   end;


   -- Set_mark
   --------------------------
   procedure Set_mark(Self : access Object ; To : in Corba.String) is
   begin
      Mark := To;
   end;


   function can_drive(Self : access Object; age : in Corba.Unsigned_Short) return Corba.Boolean is
   begin
      if (Age > 17) then return True;
      else return False;
      end if;
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
                        vehicle.Skeleton.Dispatch'Access,
                        vehicle.Is_A'Access) ;
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


end vehicle.Impl ;
