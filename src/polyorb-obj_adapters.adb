------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . O B J _ A D A P T E R S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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

--  $Id$

package body PolyORB.Obj_Adapters is

   -------------
   -- Set_ORB --
   -------------

   procedure Set_ORB
     (OA      : access Obj_Adapter;
      The_ORB :        Components.Component_Access) is
   begin
      OA.ORB := The_ORB;
   end Set_ORB;

   --  Default relative URI representation of an object ID:
   --  "/" & hexadecimal representation of oid value.

   --------------------
   -- Oid_To_Rel_URI --
   --------------------

   function Oid_To_Rel_URI
     (OA : access Obj_Adapter;
      Id : access Objects.Object_Id)
     return Types.String
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA);
      pragma Warnings (On);
   begin
      return Types.To_PolyORB_String
        ("/" & Objects.To_String (Id.all));
   end Oid_To_Rel_URI;

   --------------------
   -- Rel_URI_To_Oid --
   --------------------

   function Rel_URI_To_Oid
     (OA  : access Obj_Adapter;
      URI : Types.String)
     return Objects.Object_Id_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA);
      pragma Warnings (On);

      S : constant String := Types.To_Standard_String (URI);
   begin
      if S (S'First) /= '/' then
         raise Constraint_Error;
      end if;

      return new Objects.Object_Id'
        (Objects.To_Oid (S (S'First + 1 .. S'Last)));
   end Rel_URI_To_Oid;

   ------------------
   -- Is_Proxy_Oid --
   ------------------

   function Is_Proxy_Oid
     (OA  : access Obj_Adapter;
      Oid : access Objects.Object_Id)
     return Boolean
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA, Oid);
      pragma Warnings (On);
   begin
      return False;
      --  In the default implementation, proxy object
      --  Ids are not supported, and thus no oid is
      --  a proxy oid.
   end Is_Proxy_Oid;

   ------------------
   -- To_Proxy_Oid --
   ------------------

   procedure To_Proxy_Oid
     (OA    : access Obj_Adapter;
      R     :        References.Ref;
      Oid   :    out Objects.Object_Id_Access;
      Error : in out PolyORB.Exceptions.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA, R, Oid, Error);
      pragma Warnings (On);
   begin
      raise Not_Implemented;
   end To_Proxy_Oid;

   ------------------
   -- Proxy_To_Ref --
   ------------------

   function Proxy_To_Ref
     (OA  : access Obj_Adapter;
      Oid : access Objects.Object_Id)
     return References.Ref
   is
   begin
      raise Not_Implemented;

      pragma Warnings (Off);
      return Proxy_To_Ref (OA, Oid);
      pragma Warnings (On);
   end Proxy_To_Ref;

end PolyORB.Obj_Adapters;
