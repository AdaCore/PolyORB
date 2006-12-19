------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . O B J _ A D A P T E R S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

package body PolyORB.Obj_Adapters is

   -------------
   -- Destroy --
   -------------

   procedure Destroy (OA : access Obj_Adapter) is
   begin
      Annotations.Destroy (OA.Notepad);
   end Destroy;

   --  Default relative URI representation of an object ID:
   --  "/" & hexadecimal representation of oid value.

   --------------------
   -- Oid_To_Rel_URI --
   --------------------

   procedure Oid_To_Rel_URI
     (OA    : access Obj_Adapter;
      Id    : access Objects.Object_Id;
      URI   : out Types.String;
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA, Error);
      pragma Warnings (On);
   begin
      URI := Types.To_PolyORB_String
        ("/" & Objects.Oid_To_Hex_String (Id.all));
      --  XXX should URI_Encode the oid, not hexify it!
   end Oid_To_Rel_URI;

   --------------------
   -- Rel_URI_To_Oid --
   --------------------

   function Rel_URI_To_Oid
     (OA  : access Obj_Adapter;
      URI : String)
     return Objects.Object_Id_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA);
      pragma Warnings (On);

   begin
      if URI (URI'First) /= '/' then
         raise Constraint_Error;
      end if;

      return new Objects.Object_Id'
        (Objects.Hex_String_To_Oid (URI (URI'First + 1 .. URI'Last)));
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
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (OA, R, Oid);

      use PolyORB.Errors;

   begin
      Throw (Error, No_Implement_E,
             System_Exception_Members'
               (Minor => 0, Completed => Completed_Maybe));
   end To_Proxy_Oid;

   ------------------
   -- Proxy_To_Ref --
   ------------------

   procedure Proxy_To_Ref
     (OA    : access Obj_Adapter;
      Oid   : access Objects.Object_Id;
      Ref   : out References.Ref;
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (OA, Ref, Oid);

      use PolyORB.Errors;

   begin
      Throw (Error, No_Implement_E,
             System_Exception_Members'
               (Minor => 0, Completed => Completed_Maybe));
   end Proxy_To_Ref;

   ----------------
   -- Notepad_Of --
   ----------------

   function Notepad_Of
     (OA : access Obj_Adapter)
     return Annotations.Notepad_Access
   is
   begin
      return OA.Notepad'Access;
   end Notepad_Of;

end PolyORB.Obj_Adapters;
