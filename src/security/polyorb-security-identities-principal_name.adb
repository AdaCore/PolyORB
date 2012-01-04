------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.SECURITY.IDENTITIES.PRINCIPAL_NAME                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.Security.Identities.Principal_Name is

   function Create_Empty_Principal_Name_Identity return Identity_Access;

   procedure Initialize;

   ------------------------------------------
   -- Create_Empty_Principal_Name_Identity --
   ------------------------------------------

   function Create_Empty_Principal_Name_Identity return Identity_Access is
   begin
      return new Principal_Name_Identity_Type;
   end Create_Empty_Principal_Name_Identity;

   ------------------------------------
   -- Create_Principal_Name_Identity --
   ------------------------------------

   function Create_Principal_Name_Identity
     (Principal_Name : PolyORB.Security.Exported_Names.Exported_Name_Access)
      return Identity_Access
   is
   begin
      return
        new Principal_Name_Identity_Type'(Principal_Name => Principal_Name);
   end Create_Principal_Name_Identity;

   ------------
   -- Decode --
   ------------

   procedure Decode
     (Self  : access Principal_Name_Identity_Type;
      Item  :        Ada.Streams.Stream_Element_Array;
      Error : in out PolyORB.Errors.Error_Container)
   is
   begin
      PolyORB.Security.Exported_Names.Decode
        (Item,
         Self.Principal_Name,
         Error);
   end Decode;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate
     (Self : access Principal_Name_Identity_Type)
      return Identity_Access
   is
   begin
      return
        new Principal_Name_Identity_Type'
        (Principal_Name =>
           PolyORB.Security.Exported_Names.Duplicate (Self.Principal_Name));
   end Duplicate;

   ------------
   -- Encode --
   ------------

   function Encode
     (Self : access Principal_Name_Identity_Type)
      return Ada.Streams.Stream_Element_Array
   is
   begin
      return PolyORB.Security.Exported_Names.Encode (Self.Principal_Name);
   end Encode;

   ------------------------
   -- Get_Printable_Name --
   ------------------------

   function Get_Printable_Name
     (Self : access Principal_Name_Identity_Type)
      return String
   is
   begin
      return
        PolyORB.Security.Exported_Names.Get_Printable_Name
        (Self.Principal_Name);
   end Get_Printable_Name;

   --------------------
   -- Get_Token_Type --
   --------------------

   function Get_Token_Type
     (Self : access Principal_Name_Identity_Type)
      return PolyORB.Security.Types.Identity_Token_Type
   is
      pragma Unreferenced (Self);

   begin
      return PolyORB.Security.Types.ITT_Principal_Name;
   end Get_Token_Type;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register
        (PolyORB.Security.Types.ITT_Principal_Name,
         Create_Empty_Principal_Name_Identity'Access);
   end Initialize;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents
     (Self : access Principal_Name_Identity_Type)
   is
   begin
      PolyORB.Security.Exported_Names.Destroy (Self.Principal_Name);
   end Release_Contents;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"polyorb.security.identities.principal_name",
          Conflicts => Empty,
          Depends   => Empty,
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.Security.Identities.Principal_Name;
