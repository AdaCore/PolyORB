------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        M O M A . M E S S A G E S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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

with MOMA.Destinations;
with MOMA.Messages.MAnys;
with MOMA.Messages.MBytes;
with MOMA.Messages.MExecutes;
with MOMA.Messages.MMaps;
with MOMA.Messages.MTexts;
with MOMA.Types;

with PolyORB.Initialization;
--  with PolyORB.Log;
with PolyORB.Types;
with PolyORB.Utils.Strings;

package body MOMA.Messages is

   use MOMA.Destinations;
   use MOMA.Types;

   use PolyORB.Any;
   --  use PolyORB.Log;

   --  package L is new PolyORB.Log.Facility_Log ("moma.messages");
   --  procedure O (Message : in Standard.String; Level : Log_Level := Debug)
   --    renames L.Output;

   -----------------
   -- Acknowledge --
   -----------------

   procedure Acknowledge is
   begin
      null;
   end Acknowledge;

   ----------------
   -- Clear_Body --
   ----------------

   procedure Clear_Body is
   begin
      null;
   end Clear_Body;

   --------------
   -- From_Any --
   --------------

   function From_Any (Self : PolyORB.Any.Any) return Message'Class
   is
      use MOMA.Messages.MAnys;
      use MOMA.Messages.MBytes;
      use MOMA.Messages.MExecutes;
      use MOMA.Messages.MMaps;
      use MOMA.Messages.MTexts;

      Pos : MOMA.Types.Short;

      Type_Of_Message : Message_Type;
      Message_Id      : MOMA.Types.String;
      Correlation_Id  : MOMA.Types.String;
      Destination     : MOMA.Destinations.Destination;
      Reply_To        : MOMA.Destinations.Destination;
      Is_Persistent   : MOMA.Types.Boolean;
      Is_Redelivered  : MOMA.Types.Boolean;
      Payload         : Any;
   begin
      Pos := From_Any
        (Get_Aggregate_Element (Self,
                                TypeCode.TC_Short,
                                Unsigned_Long (0)));
      Type_Of_Message := Message_Type'Val (Pos);

      Message_Id := From_Any
        (Get_Aggregate_Element (Self,
                                TypeCode.TC_String,
                                Unsigned_Long (1)));

      Correlation_Id := From_Any
        (Get_Aggregate_Element (Self,
                                TypeCode.TC_String,
                                Unsigned_Long (2)));

      Destination := From_Any
        (Get_Aggregate_Element (Self,
                                TC_MOMA_Destination,
                                Unsigned_Long (3)));

      Reply_To := From_Any
        (Get_Aggregate_Element (Self,
                                TC_MOMA_Destination,
                                Unsigned_Long (4)));

      Is_Persistent := From_Any
        (Get_Aggregate_Element (Self,
                                TypeCode.TC_Boolean,
                                Unsigned_Long (5)));

      Is_Redelivered := From_Any
        (Get_Aggregate_Element (Self,
                                TypeCode.TC_Boolean,
                                Unsigned_Long (6)));

      Payload := From_Any
        (Get_Aggregate_Element (Self,
                                TypeCode.TC_Any,
                                Unsigned_Long (7)));

      if Type_Of_Message = Any_M then
         declare
            Rcvd_Message : MOMA.Messages.MAnys.MAny := Create_Any_Message;
         begin
            Set_Message_Id (Rcvd_Message, Message_Id);
            Set_Correlation_Id (Rcvd_Message, Correlation_Id);
            Set_Destination (Rcvd_Message, Destination);
            Set_Reply_To (Rcvd_Message, Reply_To);
            Set_Persistent (Rcvd_Message, Is_Persistent);
            Set_Redelivered (Rcvd_Message, Is_Redelivered);
            Set_Payload (Rcvd_Message, Payload);
            return Rcvd_Message;
         end;

      elsif Type_Of_Message = Byte_M then
         declare
            Rcvd_Message : MOMA.Messages.MBytes.MByte := Create_Byte_Message;
         begin
            Set_Message_Id (Rcvd_Message, Message_Id);
            Set_Correlation_Id (Rcvd_Message, Correlation_Id);
            Set_Destination (Rcvd_Message, Destination);
            Set_Reply_To (Rcvd_Message, Reply_To);
            Set_Persistent (Rcvd_Message, Is_Persistent);
            Set_Redelivered (Rcvd_Message, Is_Redelivered);
            Set_Payload (Rcvd_Message, Payload);
            return Rcvd_Message;
         end;

      elsif Type_Of_Message = Map_M then
         declare
            Rcvd_Message : MOMA.Messages.MMaps.MMap := Create_Map_Message;
         begin
            Set_Message_Id (Rcvd_Message, Message_Id);
            Set_Correlation_Id (Rcvd_Message, Correlation_Id);
            Set_Destination (Rcvd_Message, Destination);
            Set_Reply_To (Rcvd_Message, Reply_To);
            Set_Persistent (Rcvd_Message, Is_Persistent);
            Set_Redelivered (Rcvd_Message, Is_Redelivered);
            Set_Payload (Rcvd_Message, Payload);
            return Rcvd_Message;
         end;

      elsif Type_Of_Message = Text_M then
         declare
            Rcvd_Message : MOMA.Messages.MTexts.MText := Create_Text_Message;
         begin
            Set_Message_Id (Rcvd_Message, Message_Id);
            Set_Correlation_Id (Rcvd_Message, Correlation_Id);
            Set_Destination (Rcvd_Message, Destination);
            Set_Reply_To (Rcvd_Message, Reply_To);
            Set_Persistent (Rcvd_Message, Is_Persistent);
            Set_Redelivered (Rcvd_Message, Is_Redelivered);
            Set_Payload (Rcvd_Message, Payload);
            return Rcvd_Message;
         end;

      elsif Type_Of_Message = Execute_M then
         declare
            Rcvd_Message : MOMA.Messages.MExecutes.MExecute
              := Create_Execute_Message;
         begin
            Set_Message_Id (Rcvd_Message, Message_Id);
            Set_Correlation_Id (Rcvd_Message, Correlation_Id);
            Set_Destination (Rcvd_Message, Destination);
            Set_Reply_To (Rcvd_Message, Reply_To);
            Set_Persistent (Rcvd_Message, Is_Persistent);
            Set_Redelivered (Rcvd_Message, Is_Redelivered);
            Set_Payload (Rcvd_Message, Payload);
            return Rcvd_Message;
         end;

      end if;
      raise Program_Error;
      --  Should not come to this point.

   end From_Any;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property (Name : MOMA.Types.String)
                          return MOMA.Types.Property_Type is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Get_Property (Name);
      pragma Warnings (On);
   end Get_Property;

   ------------------------
   -- Get_Correlation_Id --
   ------------------------

   function Get_Correlation_Id (Self : Message)
                                return MOMA.Types.String is
   begin
      return Self.Correlation_Id;
   end Get_Correlation_Id;

   --------------------
   -- Get_Persistent --
   --------------------

   function Get_Persistent (Self : Message)
                            return MOMA.Types.Boolean is
   begin
      return Self.Is_Persistent;
   end Get_Persistent;

   ---------------------
   -- Get_Destination --
   ---------------------

   function Get_Destination (Self : Message)
                             return MOMA.Destinations.Destination is
   begin
      return Self.Destination;
   end Get_Destination;

   --------------------
   -- Get_Expiration --
   --------------------

   function Get_Expiration (Self : Message)
                            return Ada.Real_Time.Time is
   begin
      return Self.Expiration;
   end Get_Expiration;

   --------------------
   -- Get_Message_Id --
   --------------------

   function Get_Message_Id (Self : Message)
                            return MOMA.Types.String is
   begin
      return Self.Message_Id;
   end Get_Message_Id;

   -----------------
   -- Get_Payload --
   -----------------

   function Get_Payload (Self : Message)
                         return PolyORB.Any.Any is
   begin
      return Self.Payload;
   end Get_Payload;

   -------------------
   --  Get_Priority --
   -------------------

   function Get_Priority (Self : Message)
                          return MOMA.Types.Priority is
   begin
      return Self.Priority;
   end Get_Priority;

   ---------------------
   -- Get_Redelivered --
   ---------------------

   function Get_Redelivered (Self : Message)
                             return MOMA.Types.Boolean is
   begin
      return Self.Is_Redelivered;
   end Get_Redelivered;

   ------------------
   -- Get_Reply_To --
   ------------------

   function Get_Reply_To (Self : Message)
                          return MOMA.Destinations.Destination is
   begin
      return Self.Reply_To;
   end Get_Reply_To;

   -------------------
   -- Get_Timestamp --
   -------------------

   function Get_Timestamp (Self : Message)
                           return Ada.Real_Time.Time is
   begin
      return Self.Timestamp;
   end Get_Timestamp;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Self : Message)
                      return MOMA.Types.Message_Type is
   begin
      return Self.Type_Of_Message;
   end Get_Type;

   ------------------------
   -- Get_Property_Names --
   ------------------------
   --  ???
   function Get_Property_Names return Integer is
   begin
      return 0;
   end Get_Property_Names;

   ---------------------
   -- Property_Exists --
   ---------------------

   function Property_Exists (Name : MOMA.Types.String)
                             return MOMA.Types.Boolean is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Name);
      pragma Warnings (On);
      return False;
      --  XXX Not Implemented
   end Property_Exists;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property (Name : MOMA.Types.String;
                           Value : MOMA.Types.Property_Type) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Name);
      pragma Unreferenced (Value);
      pragma Warnings (On);
      null;
      --  XXX Not Implemented
   end Set_Property;

   ------------------------
   -- Set_Correlation_Id --
   ------------------------

   procedure Set_Correlation_Id (Self : in out Message;
                                 Correlation_Id : MOMA.Types.String) is
   begin
      Self.Correlation_Id := Correlation_Id;
   end Set_Correlation_Id;

   --------------------
   -- Set_Persistent --
   --------------------

   procedure Set_Persistent (Self : in out Message;
                             Is_Persistent : MOMA.Types.Boolean) is
   begin
      Self.Is_Persistent := Is_Persistent;
   end Set_Persistent;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination (Self : in out Message;
                              Destination : MOMA.Destinations.Destination) is
   begin
      Self.Destination := Destination;
   end Set_Destination;

   --------------------
   -- Set_Expiration --
   --------------------

   procedure Set_Expiration (Self : in out Message;
                             Expiration : Ada.Real_Time.Time) is
   begin
      Self.Expiration := Expiration;
   end Set_Expiration;

   -----------------
   -- Set_Payload --
   -----------------

   procedure Set_Payload (Self : in out Message;
                          Payload : PolyORB.Any.Any) is
   begin
      Self.Payload := Payload;
   end Set_Payload;

   -----------
   -- Image --
   -----------

   function Image (Self : Message)
                   return String is
   begin
      return "<Message_Id: "
             & To_Standard_String (Self.Message_Id)
             & ",Correlation_Id: "
             & To_Standard_String (Self.Correlation_Id)
             & ",Destination: "
             & Image (Self.Destination)
             & ",Reply_To: "
             & Image (Self.Reply_To)
             & ",Is_Persistent: "
             & Boolean'Image (Self.Is_Persistent)
             & ",Is_Redelivered: "
             & Boolean'Image (Self.Is_Redelivered)
             & ",Content : "
             & Image (Self.Payload) & ">";
   end Image;

   --------------------------------
   -- Set_Default_Message_Header --
   --------------------------------

   procedure Set_Default_Message_Header (Self : in out Message) is
   begin
      Set_Message_Id     (Self, To_MOMA_String ("moma"));
      Set_Correlation_Id (Self, To_MOMA_String ("moma"));
      Set_Destination    (Self, Create);
      Set_Reply_To       (Self, Create);
      --  Set_Priority       (Self, Priority);
      --  Set_Timestamp      (Self, Timestamp);
      --  Set_Expiration     (Self, Expiration);
      Set_Persistent     (Self, True);
      Set_Redelivered    (Self, True);

   end Set_Default_Message_Header;

   ------------------------
   -- Set_Message_Header --
   ------------------------

   procedure Set_Message_Header
     (Self            : in out Message;
      Message_Id      : MOMA.Types.String;
      Correlation_Id  : MOMA.Types.String;
      Destination     : MOMA.Destinations.Destination;
      Reply_To        : MOMA.Destinations.Destination;
      Priority        : MOMA.Types.Priority;
      Timestamp       : Ada.Real_Time.Time;
      Expiration      : Ada.Real_Time.Time;
      Is_Persistent   : MOMA.Types.Boolean;
      Is_Redelivered  : MOMA.Types.Boolean) is
   begin
      Set_Message_Id     (Self, Message_Id);
      Set_Correlation_Id (Self, Correlation_Id);
      Set_Destination    (Self, Destination);
      Set_Reply_To       (Self, Reply_To);
      Set_Priority       (Self, Priority);
      Set_Timestamp      (Self, Timestamp);
      Set_Expiration     (Self, Expiration);
      Set_Persistent     (Self, Is_Persistent);
      Set_Redelivered    (Self, Is_Redelivered);
   end Set_Message_Header;

   --------------------
   -- Set_Message_Id --
   --------------------

   procedure Set_Message_Id (Self : in out Message;
                             Id : MOMA.Types.String) is
   begin
      Self.Message_Id := Id;
   end Set_Message_Id;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority (Self : in out Message;
                           Priority : MOMA.Types.Priority) is
   begin
      Self.Priority := Priority;
   end Set_Priority;

   ---------------------
   -- Set_Redelivered --
   ---------------------

   procedure Set_Redelivered (Self : in out Message;
                              Redelivered : MOMA.Types.Boolean) is
   begin
      Self.Is_Redelivered := Redelivered;
   end Set_Redelivered;

   ------------------
   -- Set_Reply_To --
   ------------------

   procedure Set_Reply_To (Self : in out Message;
                           Reply_To : MOMA.Destinations.Destination) is
   begin
      Self.Reply_To := Reply_To;
   end Set_Reply_To;

   -------------------
   -- Set_Timestamp --
   -------------------

   procedure Set_Timestamp (Self : in out Message;
                            Timestamp : Ada.Real_Time.Time) is
   begin
      Self.Timestamp := Timestamp;
   end Set_Timestamp;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type (Self : in out Message;
                       Type_Of_Message : MOMA.Types.Message_Type) is
   begin
      Self.Type_Of_Message := Type_Of_Message;
   end Set_Type;

   ------------
   -- To_Any --
   ------------

   function To_Any (Self : Message)
                    return PolyORB.Any.Any
   is
      Result : Any := Get_Empty_Any_Aggregate (TC_MOMA_Message);

   begin
      Add_Aggregate_Element (Result, To_Any
                             (Short
                              (Message_Type'Pos (Self.Type_Of_Message))));

      Add_Aggregate_Element (Result, To_Any (Self.Message_Id));
      Add_Aggregate_Element (Result, To_Any (Self.Correlation_Id));
      Add_Aggregate_Element (Result, To_Any (Self.Destination));
      Add_Aggregate_Element (Result, To_Any (Self.Reply_To));
      Add_Aggregate_Element (Result, To_Any (Self.Is_Persistent));
      Add_Aggregate_Element (Result, To_Any (Self.Is_Redelivered));
      Add_Aggregate_Element (Result, To_Any (Self.Payload));

      return Result;
   end To_Any;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize
   is
      use PolyORB.Utils.Strings;
      use PolyORB.Types;
   begin
      TypeCode.Add_Parameter (TC_MOMA_Message,
                              To_Any (To_PolyORB_String ("moma_message")));
      TypeCode.Add_Parameter (TC_MOMA_Message,
                              To_Any (To_PolyORB_String
                                      ("MOMA:messages/moma_message:1.0")));

      TypeCode.Add_Parameter (TC_MOMA_Message, To_Any (TC_Short));
      TypeCode.Add_Parameter (TC_MOMA_Message,
                              To_Any (To_PolyORB_String ("type")));

      TypeCode.Add_Parameter (TC_MOMA_Message, To_Any (TC_String));
      TypeCode.Add_Parameter (TC_MOMA_Message,
                              To_Any (To_PolyORB_String ("message_id")));

      TypeCode.Add_Parameter (TC_MOMA_Message, To_Any (TC_String));
      TypeCode.Add_Parameter (TC_MOMA_Message,
                              To_Any (To_PolyORB_String ("correlation_id")));

      TypeCode.Add_Parameter (TC_MOMA_Message, To_Any (TC_MOMA_Destination));
      TypeCode.Add_Parameter (TC_MOMA_Message,
                              To_Any (To_PolyORB_String ("destination")));

      TypeCode.Add_Parameter (TC_MOMA_Message, To_Any (TC_MOMA_Destination));
      TypeCode.Add_Parameter (TC_MOMA_Message,
                              To_Any (To_PolyORB_String ("reply_to")));

      TypeCode.Add_Parameter (TC_MOMA_Message, To_Any (TC_Boolean));
      TypeCode.Add_Parameter (TC_MOMA_Message,
                              To_Any (To_PolyORB_String ("is_persistent")));

      TypeCode.Add_Parameter (TC_MOMA_Message, To_Any (TC_Boolean));
      TypeCode.Add_Parameter (TC_MOMA_Message,
                              To_Any (To_PolyORB_String ("is_redelivered")));

      TypeCode.Add_Parameter (TC_MOMA_Message, To_Any (TC_Any));
      TypeCode.Add_Parameter (TC_MOMA_Message,
                              To_Any (To_PolyORB_String ("payload")));
   end Initialize;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"MOMA.Messages",
          Conflicts => Empty,
          Depends   => +"MOMA.Destinations",
          Provides  => Empty,
          Init      => Initialize'Access));
   end;

end MOMA.Messages;
