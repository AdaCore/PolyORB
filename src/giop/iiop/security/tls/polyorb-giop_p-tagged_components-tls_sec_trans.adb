------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.GIOP_P.TAGGED_COMPONENTS.TLS_SEC_TRANS               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2013, Free Software Foundation, Inc.          --
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

with PolyORB.GIOP_P.Tagged_Components.CSI_Sec_Mech_List;
with PolyORB.Initialization;
with PolyORB.Representations.CDR.Common;
with PolyORB.Security.Transport_Mechanisms.TLS;
with PolyORB.Transport.Connected.Sockets.TLS;
with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Tagged_Components.TLS_Sec_Trans is

   use PolyORB.Representations.CDR.Common;
   use PolyORB.Security.Transport_Mechanisms;
   use PolyORB.Security.Transport_Mechanisms.TLS;
   use PolyORB.Transport.Connected.Sockets.TLS;

   use Socket_Name_Lists;

   function Create_Empty_Component return Tagged_Component_Access;

   procedure Initialize;

   function To_Tagged_Component
     (TM : PolyORB.Security.Transport_Mechanisms.
       Target_Transport_Mechanism_Access)
     return Tagged_Component_Access;

   function To_Security_Transport_Mechanism
     (TC : access Tagged_Component'Class)
      return
       PolyORB.Security.Transport_Mechanisms.Client_Transport_Mechanism_Access;

   ----------------------------
   -- Create_Empty_Component --
   ----------------------------

   function Create_Empty_Component return Tagged_Component_Access is
   begin
      return new TC_TLS_Sec_Trans;
   end Create_Empty_Component;

   ---------------
   -- Duplicate --
   ---------------

   overriding function Duplicate
     (C : TC_TLS_Sec_Trans)
     return Tagged_Component_Access
   is
      TC     : constant Tagged_Component_Access := new TC_TLS_Sec_Trans;
      Result : TC_TLS_Sec_Trans renames TC_TLS_Sec_Trans (TC.all);
      Iter   : Socket_Name_Lists.Iterator := First (C.Addresses);
   begin
      Result.Target_Supports := C.Target_Supports;
      Result.Target_Requires := C.Target_Requires;

      while not Last (Iter) loop
         Append (Result.Addresses, new Socket_Name'(Value (Iter).all.all));
         Next (Iter);
      end loop;

      return TC;
   end Duplicate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Register tagged component

      Register (Tag_TLS_Sec_Trans, Create_Empty_Component'Access, null);

      --  Register Tagged Component <=> Security Transport Mechanism convertors

      PolyORB.GIOP_P.Tagged_Components.CSI_Sec_Mech_List.Register
        (Tag_TLS_Sec_Trans,
         To_Tagged_Component'Access,
         To_Security_Transport_Mechanism'Access);
   end Initialize;

   -----------------------------
   -- Marshall_Component_Data --
   -----------------------------

   overriding procedure Marshall_Component_Data
     (C      : access TC_TLS_Sec_Trans;
      Buffer : access Buffer_Type)
   is
      Temp_Buf : Buffer_Access            := new Buffer_Type;
      Iter     : Socket_Name_Lists.Iterator := First (C.Addresses);

   begin
      Start_Encapsulation (Temp_Buf);

      Marshall (Temp_Buf, Types.Unsigned_Short (C.Target_Supports));
      Marshall (Temp_Buf, Types.Unsigned_Short (C.Target_Requires));
      Marshall (Temp_Buf, Types.Unsigned_Long (Length (C.Addresses)));

      while not Last (Iter) loop
         Marshall_Socket (Temp_Buf, Value (Iter).all.all);
         Next (Iter);
      end loop;

      Marshall (Buffer, Encapsulate (Temp_Buf));
      Release (Temp_Buf);
   end Marshall_Component_Data;

   ----------------------
   -- Release_Contents --
   ----------------------

   overriding procedure Release_Contents (C : access TC_TLS_Sec_Trans) is
      Iter : Socket_Name_Lists.Iterator := First (C.Addresses);
   begin
      while not Last (Iter) loop
         Free (Value (Iter).all);
         Next (Iter);
      end loop;
      Deallocate (C.Addresses);
   end Release_Contents;

   -------------------------------------
   -- To_Security_Transport_Mechanism --
   -------------------------------------

   function To_Security_Transport_Mechanism
     (TC : access Tagged_Component'Class)
      return
       PolyORB.Security.Transport_Mechanisms.Client_Transport_Mechanism_Access
   is
      Result : constant Client_Transport_Mechanism_Access :=
                 new Client_TLS_Transport_Mechanism;
   begin
      Client_TLS_Transport_Mechanism (Result.all).Target_Supports :=
        TC_TLS_Sec_Trans (TC.all).Target_Supports;
      Client_TLS_Transport_Mechanism (Result.all).Target_Requires :=
        TC_TLS_Sec_Trans (TC.all).Target_Requires;

      --  XXX  Set up addresses (if will be needed)

      return Result;
   end To_Security_Transport_Mechanism;

   -------------------------
   -- To_Tagged_Component --
   -------------------------

   function To_Tagged_Component
     (TM : PolyORB.Security.Transport_Mechanisms.
       Target_Transport_Mechanism_Access)
     return Tagged_Component_Access
   is
      use PolyORB.Security.Types;

      Result : constant Tagged_Component_Access := new TC_TLS_Sec_Trans;
      Iter   : TAP_Lists.Iterator := TAP_Lists.First (TM.TAP);

   begin
      TC_TLS_Sec_Trans (Result.all).Target_Supports := Target_Supports (TM);
      TC_TLS_Sec_Trans (Result.all).Target_Requires := Target_Requires (TM);

      while not TAP_Lists.Last (Iter) loop
         Append
           (TC_TLS_Sec_Trans (Result.all).Addresses,
            new Socket_Name'
              (TLS_Access_Point (TAP_Lists.Value (Iter).all.all).
                 Socket_AP_Publish_Name));
         TAP_Lists.Next (Iter);
      end loop;

      return Result;
   end To_Tagged_Component;

   -------------------------------
   -- Unmarshall_Component_Data --
   -------------------------------

   overriding procedure Unmarshall_Component_Data
     (C      : access TC_TLS_Sec_Trans;
      Buffer : access Buffer_Type;
      Error  :    out PolyORB.Errors.Error_Container)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use PolyORB.Errors;

      Tag_Body : aliased Encapsulation := Unmarshall (Buffer);
      Temp_Buf : Buffer_Access         := new Buffer_Type;
      Length   : Types.Unsigned_Long;

   begin
      Decapsulate (Tag_Body'Access, Temp_Buf);

      C.Target_Supports :=
        PolyORB.Security.Types.Association_Options
        (Types.Unsigned_Short'(Unmarshall (Temp_Buf)));

      C.Target_Requires :=
        PolyORB.Security.Types.Association_Options
        (Types.Unsigned_Short'(Unmarshall (Temp_Buf)));

      Length := Unmarshall (Temp_Buf);

      for J in 1 .. Length loop
         Append (C.Addresses, new Socket_Name'(Unmarshall_Socket (Temp_Buf)));
      end loop;

      pragma Assert (Remaining (Temp_Buf) = 0);
      Release (Temp_Buf);

   exception
      when others =>
         Release (Temp_Buf);
         Throw
           (Error,
            Bad_Param_E,
            System_Exception_Members'(10, Completed_No));
   end Unmarshall_Component_Data;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"tagged_components.tls_sec_trans",
          Conflicts => PolyORB.Initialization.String_Lists.Empty,
          Depends   => +"giop_p.tagged_components.csi_sec_mech_list",
          Provides  => PolyORB.Initialization.String_Lists.Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.GIOP_P.Tagged_Components.TLS_Sec_Trans;
