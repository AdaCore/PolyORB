------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . P O A . B A S I C _ P O A                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Basic POA implementation.

--  $Id$

with Ada.Streams;
with Ada.Unchecked_Conversion;

with PolyORB.Log;
with PolyORB.References.IOR;

package body PolyORB.POA.Basic_POA is

   use PolyORB.Exceptions;
   use PolyORB.Log;
   use PolyORB.Types;

   package L is new Log.Facility_Log ("polyorb.poa.basic_poa");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ----------------
   -- Create_POA --
   ----------------

   procedure Create_POA
     (Self         : access Basic_Obj_Adapter;
      Adapter_Name :        Types.String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        POA_Policies.PolicyList;
      POA          :    out Obj_Adapter_Access;
      Error        : in out PolyORB.Exceptions.Error_Container)
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
         U_Oid : constant Unmarshalled_Oid := Oid_To_U_Oid (Oid);

         Obj_OA : Obj_Adapter_Access;
         Error  : PolyORB.Exceptions.Error_Container;

      begin
         Find_POA (OA,
                   To_Standard_String (U_Oid.Creator),
                   False,
                   Obj_OA,
                   Error);

         if Found (Error) then
            Catch (Error);
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
      Error : in out PolyORB.Exceptions.Error_Container) is
   begin
      pragma Debug (O ("To_Proxy_Oid: enter"));

      if OA.Proxies_OA = null then
         pragma Debug (O ("No Proxies_OA."));
         Oid := null;
         return;
      end if;

      declare
         Oid_Data : aliased Object_Id :=
           Object_Id (References.IOR.Object_To_Opaque (R));

         U_Oid : Unmarshalled_Oid;

      begin
         pragma Debug (O ("To_Proxy_Oid: Oid data length:"
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

         pragma Debug (O ("To_Proxy_Oid: leave"));

         Oid := U_Oid_To_Oid (U_Oid);
      end;
   end To_Proxy_Oid;

   ------------------
   -- Proxy_To_Ref --
   ------------------

   function Proxy_To_Ref
     (OA  : access Basic_Obj_Adapter;
      Oid : access Objects.Object_Id)
     return References.Ref
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA);
      pragma Warnings (On);

      Oid_Data : aliased Object_Id := Objects.To_Oid
        (To_Standard_String (Oid_To_U_Oid (Oid).Id));

      type SEA_Access is access all Ada.Streams.Stream_Element_Array;

      function As_SEA_Access is new Ada.Unchecked_Conversion
        (Object_Id_Access, SEA_Access);

   begin
      pragma Debug (O ("PTR: Oid data length:"
                       & Integer'Image (Oid_Data'Length)));

      return References.IOR.Opaque_To_Object
        (As_SEA_Access (Oid_Data'Unchecked_Access));
   end Proxy_To_Ref;

end PolyORB.POA.Basic_POA;
