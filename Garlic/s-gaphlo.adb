------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--      S Y S T E M . G A R L I C . P H Y S I C A L _ L O C A T I O N       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
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

package body System.Garlic.Physical_Location is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GAPHLO", "(s-gaphlo): ");

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use System.Garlic.Protocols, System.Garlic.Utils;

   function Lookup_Protocol (P : String) return Protocol_Access;
   --  Return a protocol or null if no protocol with this name was found

   --------------------------
   -- Add_Missing_Location --
   --------------------------

   procedure Add_First_Missing_Location
     (List     : in String_Array_Access;
      Current  : in out Natural;
      Protocol : in Protocol_Access;
      Data     : in String_Array_Access) is
   begin
      if Data = null then
         return;
      end if;
      for D in Data'Range loop
         declare
            L : constant String := To_Location (Protocol, Data (D).all);
         begin
            --  Look for L in Location. If not already there, then add it.

            if Missing (L, List (1 .. Current)) then
               Current := Current + 1;
               List (Current) := new String'(L);
               exit;
            end if;
         end;
      end loop;
   end Add_First_Missing_Location;

   ---------------------------
   -- Add_Missing_Locations --
   ---------------------------

   procedure Add_Missing_Locations
     (List     : in String_Array_Access;
      Current  : in out Natural;
      Protocol : in Protocol_Access)
   is
      Data : String_Array_Access;
   begin
      Data := Get_Data (Protocol);
      if Data = null then
         return;
      end if;
      for D in Data'Range loop
         declare
            L : constant String := To_Location (Protocol, Data (D).all);
         begin
            --  Look for L in Location. If not already there, then add it.

            if Missing (L, List (1 .. Current)) then
               Current := Current + 1;
               List (Current) := new String'(L);
            end if;
         end;
      end loop;
      Destroy (Data);
   end Add_Missing_Locations;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Location : in out Location_Type) is
   begin
      Location := Null_Location;
   end Destroy;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (L : Location_Type) return String is
   begin
      return L.Data_Str (1 .. L.Data_Len);
   end Get_Data;

   ------------------
   -- Get_Protocol --
   ------------------

   function Get_Protocol
     (L : Location_Type)
      return Protocol_Access is
   begin
      return L.Protocol;
   end Get_Protocol;

   ----------------------
   -- Get_Support_Data --
   ----------------------

   function Get_Support_Data
     (L : String)
     return String
   is
      Name  : constant String  := Get_Support_Name (L);
      First : constant Natural := L'First + Name'Length;
      Last  : constant Natural := L'Last;

   begin
      if Name'Length = 0
        or else L (First) /= ':'
        or else First + 2 > Last
        or else L (First + 1 .. First + 2) /= "//"
      then
         return Null_String;
      end if;

      return  L (First + 3 .. Last);
   end Get_Support_Data;

   ----------------------
   -- Get_Support_Name --
   ----------------------

   function Get_Support_Name
     (L : String)
     return String
   is
      First : constant Natural := L'First;
      Last  : constant Natural := L'Last;

   begin
      for I in L'Range loop
         if L (I) = ':' then
            if I = Last then
               return L (First .. I - 1);
            end if;
            if I + 2 > Last
              or else L (I + 1 .. I + 2) /= "//"
            then
               return "";
            end if;
            return L (First .. I - 1);
         end if;
      end loop;
      return L;
   end Get_Support_Name;

   ---------------------
   -- Lookup_Protocol --
   ---------------------

   function Lookup_Protocol (P : String) return Protocol_Access is
   begin
      for I in First_Protocol .. Last_Protocol loop
         if Get_Name (Protocol_Table (I)) = P then
            return Protocol_Table (I);
         end if;
      end loop;
      return null;
   end Lookup_Protocol;

   -----------------
   -- To_Location --
   -----------------

   function To_Location (L : String) return Location_Type
   is
      Result : Location_Type := Null_Location;

   begin
      for Colon in L'Range loop
         if L (Colon) = ':' then
            if Colon = L'Last then
               Result.Protocol := Lookup_Protocol (L (L'First .. Colon - 1));
               return Result;
            end if;
            if Colon + 2 > L'Last
              or else L (Colon + 1 .. Colon + 2) /= "//"
            then
               raise Malformed_Location;
            end if;
            Result.Protocol := Lookup_Protocol (L (L'First .. Colon - 1));
            Result.Data_Len := L'Last - Colon - 2;
            Result.Data_Str (1 .. Result.Data_Len) := L (Colon + 3 .. L'Last);
            return Result;
         end if;
      end loop;
      Result.Protocol := Lookup_Protocol (L);
      return Result;
   end To_Location;

   -----------------
   -- To_Location --
   -----------------

   function To_Location
     (P : Protocols.Protocol_Access;
      D : String)
     return Location_Type
   is
      Result : Location_Type := Null_Location;
   begin
      Result.Protocol := P;
      Result.Data_Len := D'Length;
      Result.Data_Str (1 .. Result.Data_Len) := D;
      return Result;
   end To_Location;

   -----------------
   -- To_Location --
   -----------------

   function To_Location
     (P : Protocols.Protocol_Access;
      D : String)
     return String is
   begin
      return Get_Name (P) & "://" & D;
   end To_Location;

   ---------------
   -- To_String --
   ---------------

   function To_String (L : Location_Type) return String is
   begin
      return Get_Name (Get_Protocol (L)) & "://" & Get_Data (L);
   end To_String;

end System.Garlic.Physical_Location;
