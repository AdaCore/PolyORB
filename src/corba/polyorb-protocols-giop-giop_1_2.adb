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

with PolyORB.Binding_Data;        use PolyORB.Binding_Data;
with PolyORB.Binding_Data.IIOP;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
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
      Marshall
        (Buffer,
         Types.Unsigned_Long (Message_Size - Message_Header_Size));
   end Marshall_GIOP_Header;


   ------------------------------------------------
   --  Marshaling of the  GIOP 1.2 messages
   ------------------------------------------------
   -- Marshalling of the request Message ----------
   ------------------------------------------------


   procedure Marshall_Request_Message
     (Buffer            : access Buffers.Buffer_Type;
      Request_Id        : in Types.Unsigned_Long;
      Target_Ref        : in Target_Address;
      Sync_Type         : in Sync_Scope;
      Operation         : in Requests.Operation_Id)

   is
      use Representations.CDR;
      use Binding_Data.IIOP;
      use PolyORB.References.IOR;
      Reserved : constant Types.Octet := 0;

   begin

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Response_Flags
      Marshall (Buffer, Types.Octet (Sync_Scope'Pos (Sync_Type)));
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
            References.IOR.Marshall (Buffer, Target_Ref.Ref.IOR);
      end case;

      --  Operation
      Marshall (Buffer, Operation);

      --  Service context
      for I in Service_Context_List_1_2'Range loop
         Marshall
           (Buffer,
            Types.Octet (ServiceId'Pos (Service_Context_List_1_2 (I))));
      end loop;

   end Marshall_Request_Message;



   -----------------------------
   --  Marshalling of Reply messages
   --
   --  Marshalling of Reply with
   ----------------------------------

   -----------------------------
   --- No Exception Reply
   ------------------------------

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
      for I in Service_Context_List_1_2'Range loop
         Marshall
           (Buffer,
            Types.Octet (ServiceId'Pos (Service_Context_List_1_2 (I))));
      end loop;


   end Marshall_No_Exception;



   -------------------------------------
   --  Exception Marshall
   -------------------------------------

   procedure Marshall_Exception
    (Buffer      : access Buffers.Buffer_Type;
     Request_Id  : Types.Unsigned_Long;
     Reply_Type  : in Reply_Status_Type;
     Occurence   : in CORBA.Exception_Occurrence)

   is
      use  Representations.CDR;
   begin

      pragma Assert (Reply_Type in User_Exception .. System_Exception);

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Reply Status
      Marshall (Buffer, Reply_Type);


      --  Service context
      for I in Service_Context_List_1_2'Range loop
         Marshall
           (Buffer,
            Types.Octet (ServiceId'Pos (Service_Context_List_1_2 (I))));
      end loop;

      --  Occurrence
      Marshall (Buffer, Occurence);
      --  XXX WRONG! This procedure can only marshall
      --  SYSTEM exceptions, not USER exceptions!
   end  Marshall_Exception;



   -------------------------------------
   -- Location Forward Reply Marshall --
   -------------------------------------

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
      for I in Service_Context_List_1_2'Range loop
         Marshall
           (Buffer,
            Types.Octet (ServiceId'Pos (Service_Context_List_1_2 (I))));
      end loop;

      --  Reference
      References.IOR.Marshall (Buffer, Target_Ref);

   end Marshall_Location_Forward;

   ------------------------------------
   --  Needs Addessing Mode Marshall
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
      for I in Service_Context_List_1_2'Range loop
         Marshall (Buffer, Types.Octet (ServiceId'Pos
                 (Service_Context_List_1_2 (I))));
      end loop;

      --  Address Disposition
      Marshall (Buffer, Address_Type);

   end  Marshall_Needs_Addressing_Mode;


   ------------------------------------------------
   ---  Locate Request Message Marshall
   -----------------------------------------------


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
            References.IOR.Marshall (Buffer, Target_Ref.Ref.IOR);
      end case;

   end Marshall_Locate_Request;

   -------------------------------
   -- Fragment Message Marshall --
   -------------------------------

   procedure Marshall_Fragment
    (Buffer       : access Buffers.Buffer_Type;
     Request_Id   : in Types.Unsigned_Long)
   is
      use  Representations.CDR;
   begin

      --  Request id
      Marshall (Buffer, Request_Id);

   end Marshall_Fragment;


   ------------------------
   -- Request Unmarshall --
   ------------------------

   procedure Unmarshall_Request_Message
     (Buffer            : access Buffer_Type;
      Request_Id        :    out Types.Unsigned_Long;
      Response_Expected :    out Boolean;
      Target_Ref        :    out Target_Address_Access;
      Operation         :    out Types.String)
   is
      use  Representations.CDR;

      Service_Context      : array (0 .. 9) of Types.Octet;
      Reserved             : Types.Octet;
      Received_Flags       : Types.Octet;
      Temp_Octet           : Addressing_Disposition;

   begin

      --  Request id
      Request_Id := Unmarshall (Buffer);

      --  Response expected
      Received_Flags := Unmarshall (Buffer);

      if Received_Flags = 3 then
         --  XXX Dubious.
         --  I seem to remember that Response_Expected
         --  is the LSBit of Flags.
         Response_Expected := True;
      else
         Response_Expected := False;
      end if;

      --  Reserved
      for I in 1 .. 3 loop
         Reserved := Unmarshall (Buffer);
      end loop;

      --  Target Ref
      Temp_Octet := Unmarshall (Buffer);

      case  Temp_Octet is
         when Key_Addr  =>

            declare
               Obj : Stream_Element_Array :=  Unmarshall (Buffer);
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
               Temp_Ref.IOR := Unmarshall (Buffer);

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
      for I in Service_Context'Range loop
         Service_Context (I) := Unmarshall (Buffer);
      end loop;

      for I in Service_Context'Range loop
         if Service_Context (I) /= ServiceId'Pos
            (Service_Context_List_1_2 (I)) then
            pragma Debug
              (O ("Request_Message_Unmarshall: incorrect context"));
            raise GIOP_Error;
         end if;
      end loop;

   end Unmarshall_Request_Message;

   ------------------------------
   -- Reply Message Unmarshall --
   ------------------------------

   procedure Unmarshall_Reply_Message
      (Buffer       : access Buffer_Type;
       Request_Id   : out Types.Unsigned_Long;
       Reply_Status : out Reply_Status_Type)
   is
      use  Representations.CDR;
      Service_Context   : array (0 .. 9) of Types.Octet;
   begin


      --  Request id
      Request_Id := Unmarshall (Buffer);

      --  Reply Status
      Reply_Status := Unmarshall (Buffer);

      --  Service context
      for I in Service_Context'Range loop
         Service_Context (I) := Unmarshall (Buffer);
      end loop;

      for I in Service_Context'Range loop
         if Service_Context (I) /= ServiceId'Pos
            (Service_Context_List_1_2 (I)) then
            pragma Debug (O
             (" Request_Message_Unmarshall : incorrect context"));
            raise GIOP_Error;
         end if;
      end loop;

   end Unmarshall_Reply_Message;

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
               Obj : Stream_Element_Array := Unmarshall (Buffer);
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
                  Temp_Ref.IOR := Unmarshall (Buffer);
                  Target_Ref := Target_Address'
                    (Address_Type => Reference_Addr,
                     Ref  => Temp_Ref);
            end;

         when others =>
            raise GIOP_Error;

      end case;

   end Unmarshall_Locate_Request;

   procedure Marshall
     (Buffer  : access Buffers.Buffer_Type;
      Addr    : Addressing_Disposition)
   is
   begin
      Marshall (Buffer, Types.Short (Addressing_Disposition'Pos (Addr)));
   end Marshall;

   function Unmarshall
     (Buffer  : access Buffers.Buffer_Type)
     return Addressing_Disposition is
      Value : Types.Short := Unmarshall (Buffer);
   begin
      return Addressing_Disposition'Val (Value);
   end Unmarshall;

end PolyORB.Protocols.GIOP.GIOP_1_2;
