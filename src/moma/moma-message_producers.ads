with Ada.Calendar; use Ada.Calendar;
with Temp; use Temp;

package MOMA.Message_Producers is

   --------------------------------
   --  Abstract Message_Producer --
   --------------------------------

   type Message_Producer is abstract tagged private;

   -----------
   -- Close --
   -----------

   procedure Close;

   ---------------------
   --  Get_Persistent --
   ---------------------

   function Get_Persistent return Boolean;

   -------------------
   --  Get_Priority --
   -------------------

   function Get_Priority return Priority;

   -----------------------
   --  Get_Time_To_Live --
   -----------------------

   function Get_Time_To_Live return Time;

   ---------------------
   --  Set_Persistent --
   ---------------------

   procedure Set_Persistent (Persistent : Boolean);

   -------------------
   --  Set_Priority --
   -------------------

   procedure Set_Priority (Value : Priority);

   -----------------------
   --  Set_Time_To_Live --
   -----------------------

   procedure Set_Time_To_Live (TTL : Time);

private
   type Message_Producer is abstract tagged null record;

end MOMA.Message_Producers;
