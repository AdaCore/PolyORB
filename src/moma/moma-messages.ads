------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        M O M A . M E S S A G E S                         --
--                                                                          --
--                                 S p e c                                  --
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

--  Root of all MOMA message types.

--  $Id$

with Ada.Real_Time;
with MOMA.Destinations;
with MOMA.Types;
with PolyORB.Any;

package MOMA.Messages is

   use PolyORB.Any;

   type Message is tagged private;

   TC_MOMA_Message : TypeCode.Object := TypeCode.TC_Struct;

   procedure Acknowledge;

   procedure Clear_Body;

   function Image (Self : Message) return String;

   procedure Set_Default_Message_Header (Self : in out Message);

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
      Is_Redelivered  : MOMA.Types.Boolean);

   --
   --  Accessor functions to message internals
   --

   function Get_Correlation_Id (Self : Message)
                                return MOMA.Types.String;

   function Get_Persistent (Self : Message)
                            return MOMA.Types.Boolean;

   function Get_Destination (Self : Message)
                             return MOMA.Destinations.Destination;

   function Get_Expiration (Self : Message)
                            return Ada.Real_Time.Time;

   function Get_Message_Id (Self : Message)
                            return MOMA.Types.String;

   function Get_Payload (Self : Message)
                         return PolyORB.Any.Any;

   function Get_Priority (Self : Message)
                          return MOMA.Types.Priority;

   function Get_Redelivered (Self : Message)
                             return MOMA.Types.Boolean;

   function Get_Reply_To (Self : Message)
                          return MOMA.Destinations.Destination;

   function Get_Timestamp (Self : Message)
                           return Ada.Real_Time.Time;

   function Get_Type (Self : Message)
                      return MOMA.Types.Message_Type;

   procedure Set_Correlation_Id (Self : in out Message;
                                 Correlation_Id : MOMA.Types.String);

   procedure Set_Persistent (Self : in out Message;
                             Is_Persistent : MOMA.Types.Boolean);

   procedure Set_Destination (Self : in out Message;
                              Destination : MOMA.Destinations.Destination);

   procedure Set_Expiration (Self : in out Message;
                             Expiration : Ada.Real_Time.Time);

   procedure Set_Message_Id (Self : in out Message;
                             Id : MOMA.Types.String);

   procedure Set_Payload (Self : in out Message;
                          Payload : PolyORB.Any.Any);

   procedure Set_Priority (Self : in out Message;
                           Priority : MOMA.Types.Priority);

   procedure Set_Redelivered (Self : in out Message;
                              Redelivered : MOMA.Types.Boolean);

   procedure Set_Reply_To (Self : in out Message;
                           Reply_To : MOMA.Destinations.Destination);

   procedure Set_Timestamp (Self : in out Message;
                            Timestamp : Ada.Real_Time.Time);

   procedure Set_Type (Self : in out Message;
                       Type_Of_Message : MOMA.Types.Message_Type);

   --  XXX Are the following functions junk ?
   --  ??? return
   function Get_Property_Names
     return Integer;

   function Property_Exists (Name : MOMA.Types.String)
                             return MOMA.Types.Boolean;

   procedure Set_Property (Name : MOMA.Types.String;
                           Value : MOMA.Types.Property_Type);

   function Get_Property (Name : MOMA.Types.String)
                          return MOMA.Types.Property_Type;
   pragma Inline (Get_Property);

   --
   --  Message Marshalling/Unmarshalling functions
   --

   function To_Any (Self : Message) return PolyORB.Any.Any;

   function From_Any (Self : PolyORB.Any.Any) return Message'Class;

private

   type Message is tagged record
      Type_Of_Message : MOMA.Types.Message_Type;
      Message_Id      : MOMA.Types.String;
      Correlation_Id  : MOMA.Types.String;
      Destination     : MOMA.Destinations.Destination;
      Reply_To        : MOMA.Destinations.Destination;
      Priority        : MOMA.Types.Priority;
      Timestamp       : Ada.Real_Time.Time;
      Expiration      : Ada.Real_Time.Time;
      Is_Persistent   : MOMA.Types.Boolean;
      Is_Redelivered  : MOMA.Types.Boolean;
      Payload         : PolyORB.Any.Any;
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
