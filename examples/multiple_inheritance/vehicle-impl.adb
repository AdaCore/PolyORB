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
      AdaBroker.OmniORB.Initialize(AdaBroker.OmniORB.ImplObject(Self)) ;
      Initialize_Local_Object(Self,
                        Repository_Id,
                        vehicle.Skeleton.Dispatch'Access);
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


end vehicle.Impl ;
