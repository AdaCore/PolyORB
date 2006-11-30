------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.SECURITY.IDENTITIES.DISTINGUISHED_NAME               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.Security.Identities.Distinguished_Name is

   use PolyORB.Errors;
   use PolyORB.X509;

   function Create_Empty_Distinguished_Name_Identity return Identity_Access;

   procedure Initialize;

   ------------
   -- Create --
   ------------

   function Create (The_Name : PolyORB.X509.Name) return Identity_Access is
   begin
      return new Distinguished_Name_Identity_Type'(Name => The_Name);
   end Create;

   ----------------------------------------------
   -- Create_Empty_Distinguished_Name_Identity --
   ----------------------------------------------

   function Create_Empty_Distinguished_Name_Identity return Identity_Access is
   begin
      return new Distinguished_Name_Identity_Type;
   end Create_Empty_Distinguished_Name_Identity;

   ------------
   -- Decode --
   ------------

   procedure Decode
     (Self  : access Distinguished_Name_Identity_Type;
      Item  :        Ada.Streams.Stream_Element_Array;
      Error : in out PolyORB.Errors.Error_Container)
   is
   begin
      Self.Name := Decode (Item);

   exception
      when X509_Error =>
         Throw
           (Error,
            Marshal_E,
            System_Exception_Members'(Minor => 0, Completed => Completed_No));
   end Decode;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate
     (Self : access Distinguished_Name_Identity_Type) return Identity_Access
   is
   begin
      return
        new Distinguished_Name_Identity_Type'(Name => Duplicate (Self.Name));
   end Duplicate;

   ------------
   -- Encode --
   ------------

   function Encode
     (Self : access Distinguished_Name_Identity_Type)
      return Ada.Streams.Stream_Element_Array
   is
   begin
      return Encode (Self.Name);
   end Encode;

   ------------------------
   -- Get_Printable_Name --
   ------------------------

   function Get_Printable_Name
     (Self : access Distinguished_Name_Identity_Type)
      return String
   is
   begin
      return "[DN]" & To_String (Self.Name);
   end Get_Printable_Name;

   --------------------
   -- Get_Token_Type --
   --------------------

   function Get_Token_Type
     (Self : access Distinguished_Name_Identity_Type)
      return PolyORB.Security.Types.Identity_Token_Type
   is
      pragma Unreferenced (Self);

   begin
      return PolyORB.Security.Types.ITT_Distinguished_Name;
   end Get_Token_Type;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register
        (PolyORB.Security.Types.ITT_Distinguished_Name,
         Create_Empty_Distinguished_Name_Identity'Access);
   end Initialize;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents
     (Self : access Distinguished_Name_Identity_Type)
   is
   begin
      Destroy (Self.Name);
   end Release_Contents;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"polyorb.security.identities.distinguished_name",
          Conflicts => Empty,
          Depends   => +"x509",
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.Security.Identities.Distinguished_Name;
