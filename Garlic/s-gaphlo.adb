------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--      S Y S T E M . G A R L I C . P H Y S I C A L _ L O C A T I O N       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with System.Garlic.Debug;    use System.Garlic.Debug;
with System.Garlic.Types;    use System.Garlic.Types;
with Unchecked_Deallocation;

package body System.Garlic.Physical_Location is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("PHYSICAL", "(s-gaphlo): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use System.Garlic.Protocols;

   type Location_Body is record
      Protocol : Protocols.Protocol_Access;
      Data     : String_Access;
   end record;

   type Node;
   type Node_Ptr is access Node;
   type Node is record
      Content : Location_Type;
      Next    : Node_Ptr;
   end record;

   type List is record
      First : Node_Ptr;
      Count : Natural := 0;
   end record;

   Partition_ID_To_Location : array (System.RPC.Partition_ID) of List;

   Protocols_List : array (1 .. 10) of Protocol_Access;
   --  This should be enough but may be increased in the future if needed

   procedure Register_Partition
     (P : in System.RPC.Partition_ID;
      L : in Location_Type);
   --  Add a partition into the base

   function Lookup_Protocol (P : String) return Protocol_Access;
   --  Return a protocol or null if no protocol with this name was found

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (L : Location_Type)
     return String
   is
   begin
      return L.Data.all;
   end Get_Data;

   -------------------
   -- Get_Partition --
   -------------------

   function Get_Partition
     (P : System.RPC.Partition_ID)
      return Location_Type
   is
      First : Node_Ptr renames Partition_ID_To_Location (P) .First;
   begin
      if First = null then
         raise No_Such_Location;
      else
         return First.Content;
      end if;
   end Get_Partition;

   -------------------
   -- Get_Partition --
   -------------------

   function Get_Partition
     (P : System.RPC.Partition_ID)
      return String
   is
   begin
      return To_String (Location_Type'(Get_Partition (P)));
   end Get_Partition;

   ------------------
   -- Get_Protocol --
   ------------------

   function Get_Protocol
     (L : Location_Type)
      return Protocol_Access
   is
   begin
      return L.Protocol;
   end Get_Protocol;

   -----------------------------
   -- Location_Read_Attribute --
   -----------------------------

   procedure Location_Read_Attribute
     (P : access Ada.Streams.Root_Stream_Type'Class;
      L : out Location_Type)
   is
      Text : constant String := String'Input (P);
   begin
      L := To_Location (Text);
   end Location_Read_Attribute;

   ------------------------------
   -- Location_Write_Attribute --
   ------------------------------

   procedure Location_Write_Attribute
     (P : access Ada.Streams.Root_Stream_Type'Class;
      L : in Location_Type)
   is
   begin
      String'Output (P, To_String (L));
   end Location_Write_Attribute;

   ---------------------
   -- Lookup_Protocol --
   ---------------------

   function Lookup_Protocol (P : String) return Protocol_Access is
   begin
      for I in Protocols_List'Range loop
         declare
            Current : Protocol_Access renames Protocols_List (I);
         begin
            if Current /= null and then P = Get_Name (Current) then
               return Current;
            end if;
         end;
      end loop;
      return null;
   end Lookup_Protocol;

   ------------------------
   -- Register_Partition --
   ------------------------

   procedure Register_Partition
     (P : in System.RPC.Partition_ID;
      L : in Location_Type)
   is
      Current  : List renames Partition_ID_To_Location (P);
      N        : Node_Ptr := Current.First;
      New_Node : Node_Ptr := new Node'(Content => L, Next => null);
   begin
      if Current.Count = 0 then
         Current.First := New_Node;
      else
         while N.Next /= null loop
            N := N.Next;
         end loop;
         N.Next := New_Node;
      end if;
      Current.Count := Current.Count + 1;
   end Register_Partition;

   ------------------------
   -- Register_Partition --
   ------------------------

   procedure Register_Partition
     (P : in System.RPC.Partition_ID;
      L : in String)
   is
   begin
      Register_Partition (P, To_Location (L));
   end Register_Partition;

   ------------------------
   -- Register_Partition --
   ------------------------

   procedure Register_Partition
     (P : in System.RPC.Partition_ID;
      L : in Locations)
   is
   begin
      for I in L'Range loop
         Register_Partition (P, L (I));
      end loop;
   end Register_Partition;

   -----------------------
   -- Register_Protocol --
   -----------------------

   procedure Register_Protocol (P : in Protocol_Access) is
   begin
      for I in Protocols_List'Range loop
         if Protocols_List (I) = null then
            Protocols_List (I) := P;
            return;
         end if;
      end loop;
      raise Constraint_Error;
   end Register_Protocol;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      pragma Debug (D (D_Debug, "Initiating protocols shutdown"));
      for I in Protocols_List'Range loop
         if Protocols_List (I) /= null then
            Shutdown (Protocols_List (I));
         end if;
      end loop;
      pragma Debug (D (D_Debug, "Protocols shutdown completed"));
   end Shutdown;

   -----------------
   -- To_Location --
   -----------------

   function To_Location (L : String) return Location_Type is
   begin
      for Look_For_Colon in L'Range loop
         if L (Look_For_Colon) = ':' then
            if Look_For_Colon = L'Last then
               return new Location_Body'
                 (Protocol => Lookup_Protocol (L (L'First ..
                                                  Look_For_Colon - 1)),
                  Data     => new String'(""));
            end if;
            if Look_For_Colon + 2 > L'Last or else
              L (Look_For_Colon + 1 .. Look_For_Colon + 2) /= "//" then
               raise Malformed_Location;
            end if;
            return new Location_Body'
              (Protocol => Lookup_Protocol (L (L'First ..
                                               Look_For_Colon - 1)),
               Data     => new String'(L (Look_For_Colon + 3 .. L'Last)));
         end if;
      end loop;
      return new Location_Body'
        (Protocol => Lookup_Protocol (L),
         Data     => new String'(""));
   end To_Location;

   -----------------
   -- To_Location --
   -----------------

   function To_Location
     (P : Protocols.Protocol_Access;
      D : String)
     return Location_Type
   is
   begin
      return new Location_Body'(Protocol => P,
                                Data => new String'(D));
   end To_Location;

   ---------------
   -- To_String --
   ---------------

   function To_String (L : Location_Type) return String is
   begin
      return Get_Name (Get_Protocol (L)) & "://" & Get_Data (L);
   end To_String;

   --------------------------
   -- Unregister_Partition --
   --------------------------

   procedure Unregister_Partition
     (P : in System.RPC.Partition_ID;
      L : in String := "")
   is
      procedure Free is new Unchecked_Deallocation (Node, Node_Ptr);
      Current  : List renames Partition_ID_To_Location (P);
      New_Node : Node_Ptr;
      Old_Node : Node_Ptr;
   begin
      if Current.First /= null and then
        (L = "" or else L = To_String (Current.First.Content)) then
         New_Node := Current.First.Next;
         Free (Current.First);
         Current.First := New_Node;
         Current.Count := Current.Count - 1;
      end if;
      Old_Node := Current.First;
      for I in 1 .. Current.Count - 1 loop
         New_Node := Old_Node;
         Old_Node := Old_Node.Next;
         if L = To_String (Old_Node.Content) then
            New_Node.Next := Old_Node.Next;
            Free (Old_Node);
            Current.Count := Current.Count - 1;
            return;
         end if;
      end loop;
      raise No_Such_Location;
   end Unregister_Partition;

end System.Garlic.Physical_Location;
