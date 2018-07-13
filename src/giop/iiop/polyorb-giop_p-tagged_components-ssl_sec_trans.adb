------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.GIOP_P.TAGGED_COMPONENTS.SSL_SEC_TRANS               --
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

pragma Ada_2012;

with PolyORB.Initialization;
with PolyORB.Representations.CDR.Common;
with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Tagged_Components.SSL_Sec_Trans is

   use PolyORB.Representations.CDR.Common;

   function Create_Empty_Component return Tagged_Component_Access;

   procedure Initialize;

   ----------------------------
   -- Create_Empty_Component --
   ----------------------------

   function Create_Empty_Component return Tagged_Component_Access is
   begin
      return new TC_SSL_Sec_Trans;
   end Create_Empty_Component;

   ---------------
   -- Duplicate --
   ---------------

   overriding function Duplicate
     (C : TC_SSL_Sec_Trans)
     return Tagged_Component_Access
   is
      Result : constant Tagged_Component_Access := new TC_SSL_Sec_Trans;

   begin
      TC_SSL_Sec_Trans (Result.all).Target_Supports := C.Target_Supports;
      TC_SSL_Sec_Trans (Result.all).Target_Requires := C.Target_Requires;
      TC_SSL_Sec_Trans (Result.all).Port            := C.Port;

      return Result;
   end Duplicate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register
        (Tag_SSL_Sec_Trans,
         Create_Empty_Component'Access,
         null);
   end Initialize;

   -----------------------------
   -- Marshall_Component_Data --
   -----------------------------

   overriding procedure Marshall_Component_Data
     (C      : access TC_SSL_Sec_Trans;
      Buffer : access Buffer_Type)
   is
      Temp_Buf : Buffer_Access := new Buffer_Type;

   begin
      Start_Encapsulation (Temp_Buf);

      Marshall (Temp_Buf, Types.Unsigned_Short (C.Target_Supports));
      Marshall (Temp_Buf, Types.Unsigned_Short (C.Target_Requires));
      Marshall (Temp_Buf, Types.Unsigned_Short (C.Port));

      Marshall (Buffer, Encapsulate (Temp_Buf));
      Release (Temp_Buf);
   end Marshall_Component_Data;

   ----------------------
   -- Release_Contents --
   ----------------------

   overriding procedure Release_Contents (C : access TC_SSL_Sec_Trans) is
      pragma Unreferenced (C);
   begin
      null;
   end Release_Contents;

   -------------------------------
   -- Unmarshall_Component_Data --
   -------------------------------

   overriding procedure Unmarshall_Component_Data
     (C      : access TC_SSL_Sec_Trans;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use PolyORB.Errors;

      Tag_Body : aliased Encapsulation := Unmarshall (Buffer);

      Temp_Buf : Buffer_Access := new Buffer_Type;
   begin
      Decapsulate (Tag_Body'Access, Temp_Buf);

      C.Target_Supports :=
        Association_Options
        (Types.Unsigned_Short'(Unmarshall (Temp_Buf)));

      C.Target_Requires :=
        Association_Options
        (Types.Unsigned_Short'(Unmarshall (Temp_Buf)));

      C.Port := Sockets.Port_Type
        (Types.Unsigned_Short'(Unmarshall (Temp_Buf)));

      pragma Assert (Remaining (Temp_Buf) = 0);
      Release (Temp_Buf);

   exception
      when others =>
         Release (Temp_Buf);
         Throw (Error,
                Bad_Param_E,
                System_Exception_Members'(10, Completed_No));
   end Unmarshall_Component_Data;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tagged_components.ssl_sec_trans",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => PolyORB.Initialization.String_Lists.Empty,
       Provides  => PolyORB.Initialization.String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.GIOP_P.Tagged_Components.SSL_Sec_Trans;
