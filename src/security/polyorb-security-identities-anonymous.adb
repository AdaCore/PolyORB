------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SECURITY.IDENTITIES.ANONYMOUS                   --
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

package body PolyORB.Security.Identities.Anonymous is

   function Create_Empty_Anonymous_Identity return Identity_Access;

   procedure Initialize;

   -------------------------------
   -- Create_Anonymous_Identity --
   -------------------------------

   function Create_Anonymous_Identity return Identity_Access is
   begin
      return new Anonymous_Identity_Type;
   end Create_Anonymous_Identity;

   -------------------------------------
   -- Create_Empty_Anonymous_Identity --
   -------------------------------------

   function Create_Empty_Anonymous_Identity return Identity_Access is
   begin
      return new Anonymous_Identity_Type;
   end Create_Empty_Anonymous_Identity;

   ------------
   -- Decode --
   ------------

   overriding procedure Decode
     (Self  : access Anonymous_Identity_Type;
      Item  :        Ada.Streams.Stream_Element_Array;
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Self);

      use PolyORB.Errors;

   begin
      if Item'Length /= 0 then
         Throw
           (Error,
            Marshal_E,
            System_Exception_Members'(Minor => 0, Completed => Completed_No));
      end if;
   end Decode;

   ---------------
   -- Duplicate --
   ---------------

   overriding function Duplicate
     (Self : access Anonymous_Identity_Type)
      return Identity_Access
   is
      pragma Unreferenced (Self);

   begin
      return new Anonymous_Identity_Type;
   end Duplicate;

   ------------
   -- Encode --
   ------------

   overriding function Encode
     (Self : access Anonymous_Identity_Type)
      return Ada.Streams.Stream_Element_Array
   is
      pragma Unreferenced (Self);

   begin
      return Ada.Streams.Stream_Element_Array'(1 .. 0 => 0);
   end Encode;

   ------------------------
   -- Get_Printable_Name --
   ------------------------

   overriding function Get_Printable_Name
     (Self : access Anonymous_Identity_Type)
      return String
   is
      pragma Unreferenced (Self);

   begin
      return "ANONYMOUS";
   end Get_Printable_Name;

   --------------------
   -- Get_Token_Type --
   --------------------

   overriding function Get_Token_Type
     (Self : access Anonymous_Identity_Type)
      return PolyORB.Security.Types.Identity_Token_Type
   is
      pragma Unreferenced (Self);

   begin
      return PolyORB.Security.Types.ITT_Anonymous;
   end Get_Token_Type;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register
        (PolyORB.Security.Types.ITT_Anonymous,
         Create_Empty_Anonymous_Identity'Access);
   end Initialize;

   ----------------------
   -- Release_Contents --
   ----------------------

   overriding procedure Release_Contents
     (Self : access Anonymous_Identity_Type)
   is
      pragma Unreferenced (Self);

   begin
      null;
   end Release_Contents;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"polyorb.security.identities.anonymous",
          Conflicts => Empty,
          Depends   => Empty,
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.Security.Identities.Anonymous;
