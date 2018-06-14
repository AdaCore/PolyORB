------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . O B J _ A D A P T E R S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

package body PolyORB.Obj_Adapters is

   -------------
   -- Destroy --
   -------------

   procedure Destroy (OA : access Obj_Adapter) is
   begin
      Annotations.Destroy (OA.Notepad);
   end Destroy;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (OA : in out Obj_Adapter) is
   begin
      --  Use Unchecked_Access so that passed value can be freely converted
      --  to named access type within the processing for Destroy.

      Destroy (Obj_Adapter'Class (OA)'Unchecked_Access);
   end Finalize;

   --------------------
   -- Oid_To_Rel_URI --
   --------------------

   --  Default relative URI representation of an object ID:
   --  "/" & hexadecimal representation of oid value.

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
