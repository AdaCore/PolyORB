with PolyORB.Log;
with PolyORB.Types;
with MOMA.Message_Pool.Warehouse;

package body MOMA.Message_Pool.Impl is

   use PolyORB.Log;
   use PolyORB.Types;
   use MOMA.Message_Pool.Warehouse;

   package L is new PolyORB.Log.Facility_Log ("moma.message_pool.impl");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   W : MOMA.Message_Pool.Warehouse.Warehouse;
   --  XXX up to now, one and only one Warehouse,
   --  more warehouse would require message analysis,
   --  => to be done later, after proper message definition

   Message_Id : Natural := 0;
   --  Dummy counter for message_id, to be trashed ...

   -------------
   -- Publish --
   -------------

   function Publish (Message : in PolyORB.Types.String)
                     return PolyORB.Types.String is
      Temp : String := Integer'Image (Message_Id);
      Key : String := "M" & Temp (2 .. Temp'Last);
      --  Dummy Key construction, should be analyzed from message
   begin
      Ensure_Initialization (W);

      Message_Id := Message_Id + 1;

      Register (W, Key, Message);

      return To_PolyORB_String (Key);
   end Publish;

   ---------
   -- Get --
   ---------

   function Get (Message_Id : in PolyORB.Types.String)
                 return PolyORB.Types.String is
   begin
      return Lookup (W, To_String (Message_Id));
   end Get;

end MOMA.Message_Pool.Impl;
