with vehicle ;
with tank.Skeleton ;
with vehicle.Impl ;
with weapon.Impl ;
with Corba ;
with Text_IO; use Text_IO;


package body tank.Impl is


   -----------------------------
   -- inheritance from weapon
   -----------------------------

   procedure shoot(Self : access Object; ranges : in Dist) is
   begin
      null;
   end;





   -----------------------
   -- IDL definitions   --
   -----------------------

   function move(Self : access Object; fast : in Weapon.dist) return Corba.String is
   begin
      return (Corba.To_Corba_String(Standard.String'("I turn left !")));
   end ;





   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   -- Initialize
   -------------
   procedure Initialize(Self : in out Object) is
   begin
      Vehicle.Impl.Initialize(Vehicle.Impl.Object(Self)) ;
      Initialize_Local_Object(Self,
                        Repository_Id,
                        tank.Skeleton.Dispatch'Access);
      -- You can add things *BELOW* this line

   end Initialize ;


   -- Adjust
   ---------
   procedure Adjust(Self: in out Object) is
   begin
   vehicle.Impl.Adjust(vehicle.Impl.Object(Self)) ;
      -- You can add things *BELOW* this line

   end Adjust ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out Object) is
   begin

      -- You can add things *BEFORE* this line
   vehicle.Impl.Finalize(vehicle.Impl.Object(Self)) ;
   end Finalize ;


end tank.Impl ;
