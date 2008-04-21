------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . P O A . B A S I C _ P O A                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Basic POA implementation.

with Ada.Streams;

with PolyORB.Log;
with PolyORB.References.IOR;

package body PolyORB.POA.Basic_POA is

   use PolyORB.Errors;
   use PolyORB.Log;
   use PolyORB.Types;

   package L is new Log.Facility_Log ("polyorb.poa.basic_poa");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   ----------------
   -- Create_POA --
   ----------------

   procedure Create_POA
     (Self         : access Basic_Obj_Adapter;
      Adapter_Name :        Standard.String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        POA_Policies.PolicyList;
      POA          :    out Obj_Adapter_Access;
      Error        : in out PolyORB.Errors.Error_Container)
   is
   begin
      POA := new Basic_Obj_Adapter;

      Initialize_POA
        (Obj_Adapter (Self.all)'Access,
         Adapter_Name,
         A_POAManager,
         Policies,
         Obj_Adapter_Access (POA),
         Error);
   end Create_POA;

   --------------------
   -- Set_Proxies_OA --
   --------------------

   procedure Set_Proxies_OA
     (OA         : access Basic_Obj_Adapter;
      Proxies_OA :        Basic_Obj_Adapter_Access) is
   begin
      pragma Assert (OA.Proxies_OA = null
                       and then Proxies_OA /= null);
      OA.Proxies_OA := Proxies_OA;
   end Set_Proxies_OA;

   ------------------
   -- Is_Proxy_Oid --
   ------------------

   function Is_Proxy_Oid
     (OA  : access Basic_Obj_Adapter;
      Oid : access Objects.Object_Id)
     return Boolean is
   begin
      if OA.Proxies_OA = null then
         return False;
      end if;

      declare
         Obj_OA : Obj_Adapter_Access;
         Error  : PolyORB.Errors.Error_Container;

      begin
         Find_POA (OA,
                   Get_Creator (Oid.all),
                   False,
                   Obj_OA,
                   Error);
         if Found (Error) then
            Catch (Error);
            return False;
         end if;

         return Basic_Obj_Adapter_Access (Obj_OA) = OA.Proxies_OA;
      end;
   end Is_Proxy_Oid;

   ------------------
   -- To_Proxy_Oid --
   ------------------

   procedure To_Proxy_Oid
     (OA    : access Basic_Obj_Adapter;
      R     :        References.Ref;
      Oid   :    out Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container) is
   begin
      pragma Debug (C, O ("To_Proxy_Oid: enter"));

      if OA.Proxies_OA = null then
         pragma Debug (C, O ("No Proxies_OA."));
         Oid := null;
         return;
      end if;

      declare
         Oid_Data : aliased Object_Id :=
           Object_Id (References.IOR.Object_To_Opaque (R));

         U_Oid : Unmarshalled_Oid;

      begin
         pragma Debug (C, O ("To_Proxy_Oid: Oid data length:"
                          & Integer'Image (Oid_Data'Length)));

         Assign_Object_Identifier
           (OA.Id_Assignment_Policy.all,
            POA_Types.Obj_Adapter_Access (OA),
            Oid_Data'Unchecked_Access,
            U_Oid,
            Error);

         if Found (Error) then
            return;
         end if;

         pragma Debug (C, O ("To_Proxy_Oid: leave"));

         Oid := U_Oid_To_Oid (U_Oid);
      end;
   end To_Proxy_Oid;

   ------------------
   -- Proxy_To_Ref --
   ------------------

   procedure Proxy_To_Ref
     (OA    : access Basic_Obj_Adapter;
      Oid   : access Objects.Object_Id;
      Ref   : out References.Ref;
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA);
      pragma Warnings (On);

      U_Oid : Unmarshalled_Oid;

   begin
      Oid_To_U_Oid (Oid.all, U_Oid, Error);
      if Found (Error) then
         return;
      end if;

      declare
         use Ada.Streams;
         Oid_Data : aliased Stream_Element_Array :=
                      Stream_Element_Array (
                        Objects.Hex_String_To_Oid (
                          To_Standard_String (U_Oid.Id)));
      begin
         pragma Debug (C, O ("PTR: Oid data length:"
                          & Integer'Image (Oid_Data'Length)));
         Ref := References.IOR.Opaque_To_Object (Oid_Data'Access);
      end;
   end Proxy_To_Ref;

end PolyORB.POA.Basic_POA;
