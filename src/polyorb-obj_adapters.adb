------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . O B J _ A D A P T E R S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

package body PolyORB.Obj_Adapters is

   -------------
   -- Set_ORB --
   -------------

   procedure Set_ORB
     (OA      : access Obj_Adapter;
      The_ORB :        PolyORB.Components.Component_Access)
   is
   begin
      OA.ORB := The_ORB;
   end Set_ORB;

   --  Default relative URI representation of an object ID:
   --  "/" & hexadecimal representation of oid value.

   function Oid_To_Rel_URI
     (OA : access Obj_Adapter;
      Id : access Object_Id)
     return Types.String is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (OA);
      pragma Warnings (On);
      return Types.To_PolyORB_String ("/" & To_String (Id.all));
   end Oid_To_Rel_URI;

   function Rel_URI_To_Oid
     (OA  : access Obj_Adapter;
      URI : Types.String)
     return Object_Id_Access
   is
      S : constant String := Types.To_Standard_String (URI);
   begin
      pragma Warnings (Off);
      pragma Unreferenced (OA);
      pragma Warnings (On);
      if S (S'First) /= '/' then
         raise Constraint_Error;
      end if;
      return new Object_Id'(To_Oid (S (S'First + 1 .. S'Last)));
   end Rel_URI_To_Oid;

   function Is_Proxy_Oid (Id : access Object_Id)
     return Boolean
   is
      pragma Warnings (Off);
      pragma Unreferenced (Id);
      pragma Warnings (On);
   begin
      return False;
      --  In the default implementation, proxy object
      --  Ids are not supported, and thus no oid is
      --  a proxy oid.
   end Is_Proxy_Oid;

   function To_Proxy_Oid (R : References.Ref)
     return Object_Id_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (R);
      pragma Warnings (On);
   begin
      raise Not_Implemented;
      return null;
   end To_Proxy_Oid;

   function To_Ref (Proxy_Oid : access Object_Id)
     return References.Ref
   is
      pragma Warnings (Off);
      pragma Unreferenced (R);
      pragma Warnings (On);
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return To_Ref (Proxy_Oid);
      pragma Warnings (On);
   end To_Ref;

end PolyORB.Obj_Adapters;
