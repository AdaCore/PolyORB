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

package body MOMA.Messages is

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

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property (Name : MOMA.Types.String)
                          return MOMA.Types.Property_Type is
   begin
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
                            return Ada.Calendar.Time is
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
                           return Ada.Calendar.Time is
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
                             Expiration : Ada.Calendar.Time) is
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
                            Timestamp : Ada.Calendar.Time) is
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

   function To_Any (Self : Message) return PolyORB.Any.Any is
   begin
      return Self.Message_To_Any (Self);
   end To_Any;

   ------------------------
   -- Set_Message_to_Any --
   ------------------------

   procedure Set_Message_To_Any (Self : in out Message;
                                 Message_To_Any : Message_To_Any_T) is
   begin
      Self.Message_To_Any := Message_To_Any;
   end Set_Message_To_Any;

   ------------------------
   -- Simple_Marshalling --
   ------------------------

   function Simple_Marshalling (Self : Message) return PolyORB.Any.Any is
   begin
      return PolyORB.Any.To_Any (Self.Payload);
   end Simple_Marshalling;

end MOMA.Messages;
