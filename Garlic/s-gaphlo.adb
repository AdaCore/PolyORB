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
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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

   use Ada.Finalization, System.Garlic.Protocols, System.Garlic.Utils;

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

   ------------
   -- Adjust --
   ------------

   procedure Adjust (O : in out Location_Type) is
   begin
      if O.Data /= null then
         O.Data := new String'(O.Data.all);
      end if;
   end Adjust;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Location : in out Location_Type) is
   begin
      if Location.Data /= null then
         Destroy (Location.Data);
      end if;
      Location := Null_Location;
   end Destroy;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O : in out Location_Type) is
   begin
      Destroy (O.Data);
   end Finalize;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (L : Location_Type)
     return String_Access is
   begin
      return L.Data;
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

   function To_Location (L : String) return Location_Type is
   begin
      for Look_For_Colon in L'Range loop
         if L (Look_For_Colon) = ':' then
            if Look_For_Colon = L'Last then
               return (Controlled with
                       Protocol => Lookup_Protocol (L (L'First ..
                                                       Look_For_Colon - 1)),
                       Data     => new String'(""));
            end if;
            if Look_For_Colon + 2 > L'Last or else
              L (Look_For_Colon + 1 .. Look_For_Colon + 2) /= "//" then
               raise Malformed_Location;
            end if;
            return (Controlled with
                    Protocol => Lookup_Protocol (L (L'First ..
                                                    Look_For_Colon - 1)),
                    Data     => new String'(L (Look_For_Colon + 3 .. L'Last)));
         end if;
      end loop;
      return (Controlled with
              Protocol => Lookup_Protocol (L),
              Data     => new String'(""));
   end To_Location;

   -----------------
   -- To_Location --
   -----------------

   function To_Location
     (P : Protocols.Protocol_Access;
      D : String)
     return Location_Type is
   begin
      return (Controlled with
              Protocol => P,
              Data => new String'(D));
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
      return Get_Name (Get_Protocol (L)) & "://" & Get_Data (L).all;
   end To_String;

end System.Garlic.Physical_Location;
