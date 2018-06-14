------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.REPRESENTATIONS.CDR.GIOP_1_0                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;
with PolyORB.Representations.CDR.Common;
with PolyORB.Utils.Strings;

package body PolyORB.Representations.CDR.GIOP_1_0 is

   use PolyORB.Errors;
   use PolyORB.Representations.CDR.Common;

   function Create return CDR_Representation_Access;

   procedure Deferred_Initialization;

   ------------
   -- Create --
   ------------

   function Create return CDR_Representation_Access is
   begin
      return new GIOP_1_0_CDR_Representation;
   end Create;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      Register_Factory (1, 0, Create'Access);
   end Deferred_Initialization;

   --------------
   -- Marshall --
   --------------

   overriding procedure Marshall
     (R      : GIOP_1_0_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.Char;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (R);
      pragma Unreferenced (Error);

   begin
      Marshall_Latin_1_Char (Buffer, Data);
   end Marshall;

   --------------
   -- Marshall --
   --------------

   overriding procedure Marshall
     (R      : GIOP_1_0_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.String;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (R);
      pragma Unreferenced (Error);

   begin
      Marshall_Latin_1_String (Buffer, Data);
   end Marshall;

   --------------
   -- Marshall --
   --------------

   overriding procedure Marshall
     (R      : GIOP_1_0_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.Wchar;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (R);
      pragma Unreferenced (Buffer);
      pragma Unreferenced (Data);

   begin
      Throw
        (Error,
         Marshal_E,
         System_Exception_Members'(5, Completed_No));
   end Marshall;

   --------------
   -- Marshall --
   --------------

   overriding procedure Marshall
     (R      : GIOP_1_0_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.Wide_String;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (R);
      pragma Unreferenced (Buffer);
      pragma Unreferenced (Data);

   begin
      Throw
        (Error,
         Marshal_E,
         System_Exception_Members'(5, Completed_No));
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   overriding procedure Unmarshall
     (R      : GIOP_1_0_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Char;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (R);
      pragma Unreferenced (Error);

   begin
      Data := Unmarshall_Latin_1_Char (Buffer);
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   overriding procedure Unmarshall
     (R      : GIOP_1_0_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.String;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (R);
      pragma Unreferenced (Error);

   begin
      Data := Unmarshall_Latin_1_String (Buffer);
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   overriding procedure Unmarshall
     (R      : GIOP_1_0_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Wchar;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (R);
      pragma Unreferenced (Buffer);
      pragma Unreferenced (Data);

   begin
      Throw
        (Error,
         Marshal_E,
         System_Exception_Members'(5, Completed_No));
      --  XXX The minor code different for client and server
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   overriding procedure Unmarshall
     (R      : GIOP_1_0_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Wide_String;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (R);
      pragma Unreferenced (Buffer);
      pragma Unreferenced (Data);

   begin
      Throw
        (Error,
         Marshal_E,
         System_Exception_Members'(5, Completed_No));
      --  XXX The minor code different for client and server
   end Unmarshall;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"representations.cdr.giop_1_0",
          Conflicts => Empty,
          Depends   => Empty,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access,
          Shutdown  => null));
   end;
end PolyORB.Representations.CDR.GIOP_1_0;
