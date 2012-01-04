------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        M O M A . M E S S A G E S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  Root of all MOMA message types.

with Ada.Real_Time;
with MOMA.Destinations;
with MOMA.Types;
with PolyORB.Any;

package MOMA.Messages is

   use PolyORB.Any;

   type Message is tagged private;

   procedure Acknowledge;
   --  Acknowledge message.
   --  XXX not implemented. need to define acknowledgment process.

   procedure Clear_Body;
   --  Clear message payload.
   --  XXX not implemented.

   function Image (Self : Message) return String;
   --  Image function for message type.

   procedure Set_Default_Message_Header (Self : in out Message);
   --  Set Message header to its default value.
   --  XXX define this 'default value'.

   procedure Set_Message_Header
     (Self            : in out Message;
      Message_Id      :        MOMA.Types.String;
      Correlation_Id  :        MOMA.Types.String;
      Destination     :        MOMA.Destinations.Destination;
      Reply_To        :        MOMA.Destinations.Destination;
      Priority        :        MOMA.Types.Priority;
      Timestamp       :        Ada.Real_Time.Time;
      Expiration      :        Ada.Real_Time.Time;
      Is_Persistent   :        MOMA.Types.Boolean;
      Is_Redelivered  :        MOMA.Types.Boolean);
   --  Set the Message's header.

   --  Accessor to Message internal data.

   function Get_Correlation_Id
     (Self : Message)
     return MOMA.Types.String;

   function Get_Persistent
     (Self : Message)
     return MOMA.Types.Boolean;

   function Get_Destination
     (Self : Message)
     return MOMA.Destinations.Destination;

   function Get_Expiration
     (Self : Message)
     return Ada.Real_Time.Time;

   function Get_Message_Id
     (Self : Message)
     return MOMA.Types.String;

   function Get_Payload
     (Self : Message)
     return MOMA.Types.Any;

   function Get_Priority
     (Self : Message)
     return MOMA.Types.Priority;

   function Get_Redelivered
     (Self : Message)
     return MOMA.Types.Boolean;

   function Get_Reply_To
     (Self : Message)
     return MOMA.Destinations.Destination;

   function Get_Timestamp
     (Self : Message)
     return Ada.Real_Time.Time;

   function Get_Type
     (Self : Message)
     return MOMA.Types.Message_Type;

   procedure Set_Correlation_Id
     (Self           : in out Message;
      Correlation_Id :        MOMA.Types.String);

   procedure Set_Persistent
     (Self          : in out Message;
      Is_Persistent :        MOMA.Types.Boolean);

   procedure Set_Destination
     (Self        : in out Message;
      Destination :        MOMA.Destinations.Destination);

   procedure Set_Expiration
     (Self       : in out Message;
      Expiration :        Ada.Real_Time.Time);

   procedure Set_Message_Id
     (Self : in out Message;
      Id   :        MOMA.Types.String);

   procedure Set_Payload
     (Self    : in out Message;
      Payload :        MOMA.Types.Any);

   procedure Set_Priority
     (Self     : in out Message;
      Priority :        MOMA.Types.Priority);

   procedure Set_Redelivered
     (Self        : in out Message;
      Redelivered :        MOMA.Types.Boolean);

   procedure Set_Reply_To
     (Self     : in out Message;
      Reply_To :        MOMA.Destinations.Destination);

   procedure Set_Timestamp
     (Self      : in out Message;
      Timestamp :        Ada.Real_Time.Time);

   procedure Set_Type
     (Self            : in out Message;
      Type_Of_Message :        MOMA.Types.Message_Type);

   --  XXX Are the following functions junk ?

   function Get_Property_Names
     return Integer;

   function Property_Exists
     (Name : MOMA.Types.String)
     return MOMA.Types.Boolean;

   procedure Set_Property
     (Name  : MOMA.Types.String;
      Value : MOMA.Types.Property_Type);

   function Get_Property
     (Name : MOMA.Types.String)
     return MOMA.Types.Property_Type;
   pragma Inline (Get_Property);

   --  Marshalling support for Message type.

   TC_MOMA_Message : TypeCode.Local_Ref;

   function To_Any (Self : Message) return MOMA.Types.Any;
   function From_Any (Self : MOMA.Types.Any) return Message'Class;

private

   type Message is tagged record
      Type_Of_Message : MOMA.Types.Message_Type;
      Message_Id      : MOMA.Types.String;
      Correlation_Id  : MOMA.Types.String;
      Destination     : MOMA.Destinations.Destination;
      Reply_To        : MOMA.Destinations.Destination;
      Priority        : MOMA.Types.Priority := MOMA.Types.Invalid_Priority;
      Timestamp       : Ada.Real_Time.Time;
      Expiration      : Ada.Real_Time.Time;
      Is_Persistent   : MOMA.Types.Boolean;
      Is_Redelivered  : MOMA.Types.Boolean;
      Payload         : MOMA.Types.Any;
   end record;

   pragma Inline (Get_Correlation_Id);
   pragma Inline (Get_Persistent);
   pragma Inline (Get_Destination);
   pragma Inline (Get_Expiration);
   pragma Inline (Get_Message_Id);
   pragma Inline (Get_Payload);
   pragma Inline (Get_Priority);
   pragma Inline (Get_Redelivered);
   pragma Inline (Get_Reply_To);
   pragma Inline (Get_Timestamp);
   pragma Inline (Get_Type);

   pragma Inline (Set_Correlation_Id);
   pragma Inline (Set_Persistent);
   pragma Inline (Set_Destination);
   pragma Inline (Set_Expiration);
   pragma Inline (Set_Message_Id);
   pragma Inline (Set_Payload);
   pragma Inline (Set_Priority);
   pragma Inline (Set_Redelivered);
   pragma Inline (Set_Reply_To);
   pragma Inline (Set_Timestamp);
   pragma Inline (Set_Type);

end MOMA.Messages;
