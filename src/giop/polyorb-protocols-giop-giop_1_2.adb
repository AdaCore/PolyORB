------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . P R O T O C O L S . G I O P . G I O P _ 1 _ 2       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.Any;
with PolyORB.Binding_Data;        use PolyORB.Binding_Data;
with PolyORB.Binding_Data.IIOP;
with PolyORB.Log;
with PolyORB.Protocols;           use PolyORB.Protocols;
with PolyORB.Representations.CDR; use PolyORB.Representations.CDR;

with PolyORB.Types;

package body PolyORB.Protocols.GIOP.GIOP_1_2 is

   use PolyORB.Buffers;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.References;
   use PolyORB.References.IOR;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.protocols.giop.giop_1_2");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   ---------------
   -- Constants --
   ---------------

   Major_Version : constant Types.Octet
     := 1;
   Minor_Version : constant Types.Octet
     := 2;

   -------------------------
   -- Marshalling helpers --
   -------------------------

   procedure Marshall
     (Buffer  : access Buffers.Buffer_Type;
      Addr    : Addressing_Disposition);

   function Unmarshall
     (Buffer  : access Buffers.Buffer_Type)
     return Addressing_Disposition;

   procedure Marshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type);
   --  XXX Dummy marshalling for Service Context List.

   procedure Unmarshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type);
   --  XXX Dummy unmarshalling for Service Context List.

   --------------------------
   -- Marshall_GIOP_Header --
   --------------------------

   procedure Marshall_GIOP_Header
     (Buffer        : access Buffer_Type;
      Message_Type  : in Msg_Type;
      Message_Size  : in Stream_Element_Offset;
      Fragment_Next : in Boolean)
   is
      use Representations.CDR;
      Flags : Types.Octet := 0;

   begin

      --  1.2.1 The message header.

      --  Magic
      for I in Magic'Range loop
         Marshall (Buffer, Types.Octet (Magic (I)));
      end loop;

      --  Version
      Marshall (Buffer, Major_Version);
      Marshall (Buffer, Minor_Version);

      Set (Flags, Endianness_Bit, Endianness (Buffer.all) = Little_Endian);
      Set (Flags, Fragment_Bit, Fragment_Next);

      Marshall (Buffer, Flags);

      --  Message type
      Marshall (Buffer, Message_Type);

      --  Message size
      Marshall (Buffer,
                Types.Unsigned_Long (Message_Size - Message_Header_Size));

   end Marshall_GIOP_Header;

   -----------------------------------
   -- Marshall_Service_Context_List --
   -----------------------------------

   procedure Marshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type) is
   begin
      pragma Debug (O ("Enter: Marshall_Service_Context_List"));

      --  Marshall a dummy Service_Context_List built after the one
      --  sent by omniorb.
      Marshall (Buffer, Types.Unsigned_Long (0));
      --  One element in the sequence

--        Marshall (Buffer, Types.Unsigned_Long (1));
      --  Id = 0x01

--        Marshall (Buffer, Types.Unsigned_Long (12));
--        Marshall (Buffer, Types.Octet (1));
--        Marshall (Buffer, Types.Octet (0));
--        Marshall (Buffer, Types.Octet (0));
--        Marshall (Buffer, Types.Octet (0));
--        Marshall (Buffer, Types.Octet (1));
--        Marshall (Buffer, Types.Octet (0));
--        Marshall (Buffer, Types.Octet (1));
--        Marshall (Buffer, Types.Octet (0));
--        Marshall (Buffer, Types.Octet (9));
--        Marshall (Buffer, Types.Octet (1));
--        Marshall (Buffer, Types.Octet (1));
--        Marshall (Buffer, Types.Octet (0));

      pragma Debug (O ("Leave: Marshall_Service_Context_List"));
   end Marshall_Service_Context_List;

   -------------------------------------
   -- Unmarshall_Service_Context_List --
   -------------------------------------

   procedure Unmarshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type)
   is
      Nb : PolyORB.Types.Unsigned_Long;
      --  Get the size.

      Garbage_Context_Id : PolyORB.Types.Unsigned_Long;
   begin
      pragma Debug (O ("Enter: Unmarshall_Service_Context_List"));
      Nb := Unmarshall (Buffer);

      pragma Debug (O ("List size: "
                       & PolyORB.Types.Unsigned_Long'Image (Nb)));

      for I in 1 .. Nb loop
         Garbage_Context_Id := Unmarshall (Buffer);
         --  Unmarshall the Context_Id.

         --  Unmarshall the Context_Data.
         declare
            Size : constant PolyORB.Types.Unsigned_Long := Unmarshall (Buffer);
            Garbage_Data : PolyORB.Types.Octet;
         begin
            for I in 1 .. Size loop
               Garbage_Data := Unmarshall (Buffer);
            end loop;
         end;
      end loop;

      pragma Debug (O ("Leave: Unmarshall_Service_Context_List"));
   end Unmarshall_Service_Context_List;

   -------------------------------------
   -- Marshaling of GIOP 1.2 messages --
   -------------------------------------

   ------------------------------
   -- Marshall_Request_Message --
   ------------------------------

   procedure Marshall_Request_Message
     (Buffer     : access Buffers.Buffer_Type;
      Request_Id : in     Types.Unsigned_Long;
      Target_Ref : in     Target_Address;
      Sync_Type  : in     Sync_Scope;
      Operation  : in     String)

   is
      use Representations.CDR;
      use Binding_Data.IIOP;
      use PolyORB.References.IOR;
      Reserved : constant Types.Octet := 0;

   begin

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Response flags (see Chap 15.4.2.1, p15-35, CORBA 2.6)
      case Sync_Type is
         when NONE | WITH_TRANSPORT =>
            Marshall (Buffer, Types.Octet (0));

         when WITH_SERVER =>
            Marshall (Buffer, Types.Octet (1));

         when WITH_TARGET =>
            Marshall (Buffer, Types.Octet (3));
      end case;
      --  XXX inconsistency: is this Response_Flags or Sync_Type?

      --  Reserved
      for I in 1 .. 3 loop
         Marshall (Buffer, Reserved);
      end loop;

      --  Marshalling Target Reference
      Marshall (Buffer, Target_Ref.Address_Type);

      case Target_Ref.Address_Type is
         when Key_Addr =>
            pragma Assert (Target_Ref.Object_Key /= null);
            Marshall
              (Buffer, Stream_Element_Array
               (Target_Ref.Object_Key.all));

         when Profile_Addr  =>
            Marshall_IIOP_Profile_Body (Buffer, Target_Ref.Profile);

         when Reference_Addr  =>
            Marshall (Buffer, Target_Ref.Ref.Selected_Profile_Index);
            References.IOR.Marshall_IOR (Buffer, Target_Ref.Ref.IOR);
      end case;

      --  Operation
      Marshall (Buffer, Operation);

      --  Service context
      Marshall_Service_Context_List (Buffer);

   end Marshall_Request_Message;

   ----------------------------
   --- Marshall_No_Exception --
   ----------------------------

   procedure Marshall_No_Exception
    (Buffer      : access Buffer_Type;
     Request_Id  : in Types.Unsigned_Long)

   is
      use  Representations.CDR;
   begin

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Reply Status
      Marshall (Buffer, GIOP.No_Exception);

      --  Service context
      Marshall_Service_Context_List (Buffer);

   end Marshall_No_Exception;

   ------------------------
   -- Marshall_Exception --
   ------------------------

   procedure Marshall_Exception
     (Buffer     : access Buffers.Buffer_Type;
      Request_Id :        Types.Unsigned_Long;
      Reply_Type : in     Reply_Status_Type;
      Occurrence : in     Any.Any)
   is
      use  Representations.CDR;
   begin
      pragma Assert (Reply_Type in User_Exception .. System_Exception);

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Reply Status
      Marshall (Buffer, Reply_Type);

      --  Service context
      Marshall_Service_Context_List (Buffer);

      --  Occurrence
      Marshall (Buffer, Any.TypeCode.Id (Any.Get_Type (Occurrence)));
      Marshall_From_Any (Buffer, Occurrence);
   end  Marshall_Exception;

   -------------------------------
   -- Marshall_Location_Forward --
   -------------------------------

   procedure Marshall_Location_Forward
    (Buffer        :   access Buffers.Buffer_Type;
     Request_Id    :   in  Types.Unsigned_Long;
     Reply_Type    :   in  Reply_Status_Type;
     Target_Ref    :   in  PolyORB.References.IOR.IOR_Type)
   is
      use Representations.CDR;
      use References.IOR;
   begin

      pragma Assert (Reply_Type in Location_Forward .. Location_Forward_Perm);

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Reply Status
      Marshall (Buffer, Reply_Type);

      --  Service context
      Marshall_Service_Context_List (Buffer);

      --  Reference
      References.IOR.Marshall_IOR (Buffer, Target_Ref);

   end Marshall_Location_Forward;

   ------------------------------------
   -- Marshall_Needs_Addressing_Mode --
   ------------------------------------

   procedure Marshall_Needs_Addressing_Mode
    (Buffer              : access Buffers.Buffer_Type;
     Request_Id          : in Types.Unsigned_Long;
     Address_Type        : in GIOP.Addressing_Disposition)
   is
      use  Representations.CDR;
   begin

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Reply Status
      Marshall (Buffer, GIOP.Needs_Addressing_Mode);

      --  Service context
      Marshall_Service_Context_List (Buffer);

      --  Address Disposition
      Marshall (Buffer, Address_Type);

   end  Marshall_Needs_Addressing_Mode;

   ------------------------------
   --- Marshall_Locate_Request --
   ------------------------------

   procedure Marshall_Locate_Request
    (Buffer            : access Buffer_Type;
     Request_Id        : in Types.Unsigned_Long;
     Target_Ref        : in Target_Address)

   is
      use Representations.CDR;
      use Binding_Data.IIOP;
   begin

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Marshalling the Target Address
      Marshall (Buffer, Target_Ref.Address_Type);

      case Target_Ref.Address_Type is
         when Key_Addr =>
            Marshall
              (Buffer,
               Stream_Element_Array
               (Target_Ref.Object_Key.all));

         when Profile_Addr =>
            Marshall_IIOP_Profile_Body
              (Buffer, Target_Ref.Profile);

         when Reference_Addr =>
            Marshall (Buffer, Target_Ref.Ref.Selected_Profile_Index);
            References.IOR.Marshall_IOR (Buffer, Target_Ref.Ref.IOR);
      end case;

   end Marshall_Locate_Request;

   -----------------------
   -- Marshall_Fragment --
   -----------------------

   procedure Marshall_Fragment
    (Buffer       : access Buffers.Buffer_Type;
     Request_Id   : in Types.Unsigned_Long)
   is
      use  Representations.CDR;
   begin

      --  Request id
      Marshall (Buffer, Request_Id);

   end Marshall_Fragment;

   ---------------------------------------
   -- Unmarshaling of GIOP 1.2 messages --
   ---------------------------------------

   --------------------------------
   -- Unmarshall_Request_Message --
   --------------------------------

   procedure Unmarshall_Request_Message
     (Buffer            : access Buffer_Type;
      Request_Id        :    out Types.Unsigned_Long;
      Response_Expected :    out Boolean;
      Sync_Type         :    out Sync_Scope;
      Target_Ref        :    out Target_Address_Access;
      Operation         :    out Types.String)
   is
      use  Representations.CDR;

      Reserved             : Types.Octet;
      Received_Flags       : Types.Octet;
      Temp_Octet           : Addressing_Disposition;

   begin

      --  Request id
      Request_Id := Unmarshall (Buffer);

      --  Response flags (see Chap 15.4.2.1, p15-35, CORBA 2.6)
      --  Dual of Marshall_Request_Message.
      Received_Flags := Unmarshall (Buffer);

      case Received_Flags is
         when 0 =>
            Response_Expected := False;
            Sync_Type := WITH_TRANSPORT;
            --  XXX or NONE, equivalent at this stage
            --  use WITH_TRANSPORT for tests done later

         when 1 =>
            Response_Expected := False;
            Sync_Type := WITH_SERVER;

         when 3 =>
            Response_Expected := True;
            Sync_Type := WITH_TARGET;

         when others =>
            raise Program_Error;
            --  XXX should raise an exception ?
      end case;

      --  Reserved
      for I in 1 .. 3 loop
         Reserved := Unmarshall (Buffer);
      end loop;

      --  Target Ref
      Temp_Octet := Unmarshall (Buffer);

      case  Temp_Octet is
         when Key_Addr  =>

            declare
               Obj : constant Stream_Element_Array :=  Unmarshall (Buffer);
            begin
               Target_Ref := new Target_Address'
                 (Address_Type => Key_Addr,
                  Object_Key => new Object_Id'(Object_Id (Obj)));
            end;

         when Profile_Addr  =>
            Target_Ref := new Target_Address'
              (Address_Type => Profile_Addr,
               Profile  =>  Binding_Data.IIOP.
               Unmarshall_IIOP_Profile_Body (Buffer));

         when Reference_Addr  =>
            declare
               Temp_Ref : constant IOR_Addressing_Info_Access
                 := new IOR_Addressing_Info;

            begin
               Temp_Ref.Selected_Profile_Index := Unmarshall (Buffer);
               Temp_Ref.IOR := Representations.CDR.Unmarshall (Buffer);

               Target_Ref := new Target_Address'
                 (Address_Type => Reference_Addr,
                  Ref  => Temp_Ref);
            end;

         when others =>
            pragma Debug (O ("Incorrect address type in request:"
                             & Temp_Octet'Img));
            raise GIOP_Error;

      end case;

      --  Operation
      Operation :=  Unmarshall (Buffer);

      --  Service context
      Unmarshall_Service_Context_List (Buffer);

   end Unmarshall_Request_Message;

   ------------------------------
   -- Unmarshall_Reply_Message --
   ------------------------------

   procedure Unmarshall_Reply_Message
      (Buffer       : access Buffer_Type;
       Request_Id   : out Types.Unsigned_Long;
       Reply_Status : out Reply_Status_Type)
   is
      use  Representations.CDR;

   begin

      --  Request id
      Request_Id := Unmarshall (Buffer);

      --  Reply Status
      Reply_Status := Unmarshall (Buffer);

      --  Service context
      Unmarshall_Service_Context_List (Buffer);

   end Unmarshall_Reply_Message;

   -------------------------------
   -- Unmarshall_Locate_Request --
   -------------------------------

   procedure Unmarshall_Locate_Request
     (Buffer     : access Buffer_Type;
      Request_Id :    out Types.Unsigned_Long;
      Target_Ref :    out Target_Address)

   is
      Temp_Octet : Addressing_Disposition;
   begin

      --  Request Id
      Request_Id := Unmarshall (Buffer);

      --  Target Ref
      Temp_Octet := Unmarshall (Buffer);

      case  Temp_Octet  is
         when Key_Addr  =>
            declare
               Obj : constant Stream_Element_Array := Unmarshall (Buffer);
            begin
               Target_Ref := Target_Address'
                 (Address_Type => Key_Addr,
                  Object_Key => new Object_Id'(Object_Id (Obj)));
            end;

         when Profile_Addr =>
            Target_Ref := Target_Address'
              (Address_Type => Profile_Addr,
               Profile      => Binding_Data.IIOP.Unmarshall_IIOP_Profile_Body
               (Buffer));

         when Reference_Addr  =>
            declare
                  Temp_Ref : IOR_Addressing_Info_Access
                    := new IOR_Addressing_Info;
            begin
                  Temp_Ref.Selected_Profile_Index := Unmarshall (Buffer);
                  Temp_Ref.IOR := Representations.CDR.Unmarshall (Buffer);
                  Target_Ref := Target_Address'
                    (Address_Type => Reference_Addr,
                     Ref  => Temp_Ref);
            end;

         when others =>
            raise GIOP_Error;

      end case;

   end Unmarshall_Locate_Request;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer  : access Buffers.Buffer_Type;
      Addr    : Addressing_Disposition)
   is
   begin
      Marshall (Buffer, Types.Short (Addressing_Disposition'Pos (Addr)));
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer  : access Buffers.Buffer_Type)
     return Addressing_Disposition
   is
      Value : constant Types.Short := Unmarshall (Buffer);
   begin
      return Addressing_Disposition'Val (Value);
   end Unmarshall;

end PolyORB.Protocols.GIOP.GIOP_1_2;
