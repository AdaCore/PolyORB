--
--  $Id$
--

with Ada.Streams;
with System.Garlic.Heart;
with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
with System.Garlic.Protocols; use System.Garlic.Protocols;

package body System.Garlic.Loopback is

   use type System.RPC.Partition_ID;
   use type Ada.Streams.Stream_Element_Offset;

   ------------
   -- Create --
   ------------

   function Create return System.Garlic.Protocols.Protocol_Access is
      Self : Protocol_Access := new Loopback_Protocol;
   begin
      Register_Protocol (Self);
      return Self;
   end Create;

   --------------
   -- Get_Info --
   --------------

   function Get_Info (P : access Loopback_Protocol) return String is
   begin
      return "";
   end Get_Info;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (P : access Loopback_Protocol) return String is
   begin
      return "loopback";
   end Get_Name;

   ----------
   -- Send --
   ----------

   procedure Send
     (Protocol  : access Loopback_Protocol;
      Partition : in System.RPC.Partition_ID;
      Data      : access Ada.Streams.Stream_Element_Array) is
   begin
      pragma Assert (Partition = System.Garlic.Heart.Get_My_Partition_ID);
      System.Garlic.Heart.Has_Arrived
        (Partition, Data (Data'First + Unused_Space .. Data'Last));
   end Send;

   -------------------
   -- Set_Boot_Data --
   -------------------

   procedure Set_Boot_Data
     (Protocol         : access Loopback_Protocol;
      Is_Boot_Protocol : in Boolean := False;
      Boot_Data        : in String  := "";
      Is_Master        : in Boolean := False)
   is
   begin
      pragma Assert ((not Is_Boot_Protocol) or else Is_Master);
      pragma Assert (Boot_Data = "");
      null;
   end Set_Boot_Data;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Protocol : access Loopback_Protocol) is
   begin
      null;
   end Shutdown;

end System.Garlic.Loopback;
