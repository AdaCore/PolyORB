------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.GIOP_P.TAGGED_COMPONENTS.TLS_SEC_TRANS               --
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

with PolyORB.GIOP_P.Tagged_Components.CSI_Sec_Mech_List;
with PolyORB.Initialization;
with PolyORB.Representations.CDR.Common;
with PolyORB.Security.Transport_Mechanisms.TLS;
with PolyORB.Transport.Connected.Sockets.TLS;
with PolyORB.Utils.Sockets;
with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Tagged_Components.TLS_Sec_Trans is

   use PolyORB.Representations.CDR.Common;
   use PolyORB.Security.Transport_Mechanisms;
   use PolyORB.Security.Transport_Mechanisms.TLS;
   use PolyORB.Transport.Connected.Sockets.TLS;
   use PolyORB.Utils.Sockets;
   use Sock_Addr_Lists;

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

   function Duplicate (C : TC_TLS_Sec_Trans) return Tagged_Component_Access is
      TC     : constant Tagged_Component_Access := new TC_TLS_Sec_Trans;
      Result : TC_TLS_Sec_Trans renames TC_TLS_Sec_Trans (TC.all);

   begin
      Result.Target_Supports := C.Target_Supports;
      Result.Target_Requires := C.Target_Requires;
      Result.Addresses       := Sock_Addr_Lists.Duplicate (C.Addresses);

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

   procedure Marshall_Component_Data
     (C      : access TC_TLS_Sec_Trans;
      Buffer : access Buffer_Type)
   is
      Temp_Buf : Buffer_Access            := new Buffer_Type;
      Iter     : Sock_Addr_Lists.Iterator := First (C.Addresses);

   begin
      Start_Encapsulation (Temp_Buf);

      Marshall (Temp_Buf, Types.Unsigned_Short (C.Target_Supports));
      Marshall (Temp_Buf, Types.Unsigned_Short (C.Target_Requires));
      Marshall (Temp_Buf, Types.Unsigned_Long (Length (C.Addresses)));

      while not Last (Iter) loop
         Marshall_Socket (Temp_Buf, Value (Iter).all);
         Next (Iter);
      end loop;

      Marshall (Buffer, Encapsulate (Temp_Buf));
      Release (Temp_Buf);
   end Marshall_Component_Data;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (C : access TC_TLS_Sec_Trans) is
   begin
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
      Result : constant Client_Transport_Mechanism_Access
        := new Client_TLS_Transport_Mechanism;

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
            Address_Of (TLS_Access_Point (TAP_Lists.Value (Iter).all.all)));
         TAP_Lists.Next (Iter);
      end loop;

      return Result;
   end To_Tagged_Component;

   -------------------------------
   -- Unmarshall_Component_Data --
   -------------------------------

   procedure Unmarshall_Component_Data
     (C      : access TC_TLS_Sec_Trans;
      Buffer : access Buffer_Type;
      Error  :    out PolyORB.Errors.Error_Container)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use PolyORB.Errors;

      Tag_Body : aliased Encapsulation := Unmarshall (Buffer);
      Temp_Buf : Buffer_Access         := new Buffer_Type;
      Length   : Types.Unsigned_Long;
      Sock     : PolyORB.Sockets.Sock_Addr_Type;

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
         Unmarshall_Socket (Temp_Buf, Sock);

         Append (C.Addresses, Sock);
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
