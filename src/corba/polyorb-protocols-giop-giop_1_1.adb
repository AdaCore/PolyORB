------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . P R O T O C O L S . G I O P . G I O P _ 1 _ 1       --
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

with Ada.Streams; use Ada.Streams;
with Ada.Strings;
with Ada.Strings.Unbounded;

with CORBA;

with PolyORB.Buffers;             use PolyORB.Buffers;
with PolyORB.Binding_Data;        use PolyORB.Binding_Data;
with PolyORB.Binding_Data.IIOP;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Protocols;           use PolyORB.Protocols;
with PolyORB.References;          use PolyORB.References;
with PolyORB.Representations.CDR; use PolyORB.Representations.CDR;
with PolyORB.Types;

package body PolyORB.Protocols.GIOP.GIOP_1_1 is

   use PolyORB.Log;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.protocols.giop.giop_1_1");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Nobody_Principal : constant Ada.Strings.Unbounded.Unbounded_String
      := Ada.Strings.Unbounded.To_Unbounded_String ("nobody");


   --------------------------
   -- Marshall_GIOP_Header --
   --------------------------

   --  This is different from GIOP 1.0 and must be redefined.

   procedure Marshall_GIOP_Header
     (Buffer        : access Buffer_Type;
      Message_Type  : in Msg_Type;
      Message_Size  : in Stream_Element_Offset;
      Fragment_Next : in Boolean)

   is

      use Representations.CDR;
      use PolyORB.Buffers;
      Flags : Types.Octet := 0;

   begin

      --  Magic
      for I in Magic'Range loop
         Marshall (Buffer, Types.Octet (Magic (I)));
      end loop;

      --  Version
      Marshall (Buffer, Major_Version);
      Marshall (Buffer, Minor_Version);

      --  Flags
      Set (Flags, Endianness_Bit, Endianness (Buffer.all) = Little_Endian);
      Set (Flags, Fragment_Bit, Fragment_Next);

      Marshall (Buffer, Flags);

      --  Message type
      Marshall (Buffer, Message_Type);

      --  Message size
      Marshall
        (Buffer,
         Types.Unsigned_Long (Message_Size - Message_Header_Size));
   end  Marshall_GIOP_Header;


   ------------------------------------------------
   --  Marshaling of the  GIOP 1.1 messages
   ------------------------------------------------
   -- Marshalling of the request Message ----------
   ------------------------------------------------

   procedure Marshall_Request_Message
     (Buffer             : access Buffers.Buffer_Type;
      Request_Id         : in Types.Unsigned_Long;
      Target_Profile     : in Binding_Data.Profile_Access;
      Response_Expected  : in Boolean;
      Operation          : in Requests.Operation_Id)

   is
      use Representations.CDR;
      Reserved : constant Types.Octet := 0;

   begin

      --  Service context
      Marshall (Buffer, Types.Octet (ServiceId'Pos
               (Service_Context_List_1_1 (0))));
      Marshall (Buffer, Types.Octet (ServiceId'Pos
               (Service_Context_List_1_1 (1))));

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Response expected
      Marshall (Buffer, Response_Expected);

      --  Reserved
      for I in 1 .. 3 loop
         Marshall (Buffer, Reserved);
      end loop;

      --  Object key
      Marshall (Buffer, Stream_Element_Array
            (Binding_Data.IIOP.Get_Object_Key (IIOP.IIOP_Profile_Type
            (Target_Profile.all)).all));


      --  Operation
      Marshall (Buffer, Operation);


      --  Principal
      Marshall (Buffer,
               Types.String (Nobody_Principal));

   end Marshall_Request_Message;


   -----------------------------
   ---  No Exception Reply
   ------------------------------

   procedure Marshall_No_Exception
    (Buffer      : access Buffer_Type;
     Request_Id  : in Types.Unsigned_Long)

   is
      use Representations.CDR;
   begin


      --  Service context
      Marshall (Buffer, Types.Octet (ServiceId'Pos
                   (Service_Context_List_1_1 (0))));
      Marshall (Buffer, Types.Octet (ServiceId'Pos
                   (Service_Context_List_1_1 (1))));

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Reply Status
      Marshall (Buffer, GIOP.No_Exception);

   end Marshall_No_Exception;



   -------------------------------------
   --  System Exception Marshall
   -------------------------------------

   procedure Marshall_Exception
    (Buffer           : access Buffer_Type;
     Request_Id       : in Types.Unsigned_Long;
     Exception_Type   : in Reply_Status_Type;
     Occurence        : in CORBA.Exception_Occurrence)
   is
      use Representations.CDR;
   begin

      pragma Assert (Exception_Type in User_Exception .. System_Exception);

      --  Service context
      Marshall (Buffer, Types.Octet (ServiceId'Pos
                (Service_Context_List_1_1 (0))));
      Marshall (Buffer, Types.Octet (ServiceId'Pos
                (Service_Context_List_1_1 (1))));

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Reply Status
      Marshall (Buffer, Exception_Type);

      --  Occurrence
      Marshall (Buffer, Occurence);
      --  XXX WRONG! This procedure can only marshall
      --  SYSTEM exceptions, not USER exceptions!
   end Marshall_Exception;

   -------------------------------------
   -- Location Forward Reply Marshall --
   -------------------------------------

   procedure Marshall_Location_Forward
    (Buffer           : access Buffer_Type;
     Request_Id       : in  Types.Unsigned_Long;
     Forward_Ref      : in  PolyORB.References.IOR.IOR_Type)
   is
      use References.IOR;
   begin

      --  Service context
      Marshall (Buffer, Types.Octet (ServiceId'Pos
               (Service_Context_List_1_1 (0))));
      Marshall (Buffer, Types.Octet (ServiceId'Pos
               (Service_Context_List_1_1 (1))));

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Reply Status
      Marshall (Buffer, GIOP.Location_Forward);

      --  Object reference
      References.IOR.Marshall (Buffer, Forward_Ref);

   end Marshall_Location_Forward;



   ---------------------------------------
   ----- Fragment Message Marshall
   ----------------------------------------


   procedure Marshall_Fragment
    (Buffer      : access Buffer_Type;
     Request_Id  : in Types.Unsigned_Long)

   is
      use Representations.CDR;
   begin

      --   Request id
      Marshall (Buffer, Request_Id);

   end Marshall_Fragment;

   ----------------------------------
   --- Request Unmarshalling
   -----------------------------------------

   procedure Unmarshall_Request_Message
     (Buffer            : access Buffer_Type;
      Request_Id        : out Types.Unsigned_Long;
      Response_Expected : out Boolean;
      Object_Key        : out Objects.Object_Id_Access;
      Operation         : out Types.String)
   is
      use PolyORB.Objects;
      Service_Context1  : Types.Unsigned_Long := Unmarshall (Buffer);
      Service_Context2  : Types.Unsigned_Long := Unmarshall (Buffer);
      Reserved          : Types.Octet;
      Principal         : Types.String;
   begin

      --  Service context

      if Service_Context1 /= ServiceId'Pos (Service_Context_List_1_1 (0)) or
          Service_Context2 /= ServiceId'Pos (Service_Context_List_1_1 (1))
      then
         pragma Debug (O (" Request_Message_Unmarshall : incorrect context"));
         raise GIOP_Error;
      end if;

      --  Request id
      Request_Id := Unmarshall (Buffer);

      --  Response expected
      Response_Expected := Unmarshall (Buffer);

      --  Reserved
      for I in 1 .. 3 loop
         Reserved := Unmarshall (Buffer);
      end loop;

      --  Object Key
      declare
         Obj : Stream_Element_Array := Unmarshall (Buffer);
      begin
         Object_Key := new Object_Id'(Object_Id (Obj));
      end;

      --  Operation
      Operation :=  Unmarshall (Buffer);

      --  Principal
      Principal :=  Unmarshall (Buffer);

   end Unmarshall_Request_Message;


   -----------------------------------------
   --- Reply  Unmarshalling
   -----------------------------------------

   procedure Unmarshall_Reply_Message
      (Buffer       : access Buffer_Type;
       Request_Id   : out Types.Unsigned_Long;
       Reply_Status : out Reply_Status_Type)

   is
      Service_Context1  : Types.Unsigned_Long := Unmarshall (Buffer);
      Service_Context2  : Types.Unsigned_Long := Unmarshall (Buffer);
   begin

      --  Service context
      if Service_Context1 /= ServiceId'Pos (Service_Context_List_1_1 (0)) or
          Service_Context2 /= ServiceId'Pos (Service_Context_List_1_1 (1)) then

         pragma Debug (O (" Request_Message_Unmarshall : incorrect context"));
         raise GIOP_Error;
      end if;

      --  Request id
      Request_Id := Unmarshall (Buffer);

      --  Reply Status
      Reply_Status := Unmarshall (Buffer);

   end Unmarshall_Reply_Message;

end PolyORB.Protocols.GIOP.GIOP_1_1;
