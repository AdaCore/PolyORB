------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.GIOP_P.TAGGED_COMPONENTS.SSL_SEC_TRANS               --
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

   function Duplicate (C : TC_SSL_Sec_Trans) return Tagged_Component_Access is
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

   procedure Marshall_Component_Data
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

   procedure Release_Contents (C : access TC_SSL_Sec_Trans) is
      pragma Unreferenced (C);
   begin
      null;
   end Release_Contents;

   -------------------------------
   -- Unmarshall_Component_Data --
   -------------------------------

   procedure Unmarshall_Component_Data
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
