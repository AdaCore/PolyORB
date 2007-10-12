------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.REPRESENTATIONS.CDR.GIOP_1_0                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

   procedure Marshall
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

   procedure Marshall
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

   procedure Marshall
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

   procedure Marshall
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

   procedure Unmarshall
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

   procedure Unmarshall
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

   procedure Unmarshall
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

   procedure Unmarshall
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
