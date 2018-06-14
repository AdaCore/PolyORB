------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               M O M A . M E S S A G E _ P R O D U C E R S                --
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

pragma Ada_2012;

--  A Message_Producer object is the client view of the message sending
--  process. It is the facade to all communication carried out with
--  a message pool to send messages; it contains the stub to access
--  Message_Producer servants (see MOMA.Provider for more details).

--  NOTE: A MOMA client must use only this package to send messages to a
--  message pool.

with Ada.Real_Time;

with MOMA.Destinations;
with MOMA.Messages;
with MOMA.Sessions;
with MOMA.Types;

with PolyORB.Annotations;
with PolyORB.Call_Back;

package MOMA.Message_Producers is

   use Ada.Real_Time;

   type Message_Producer is private;
   --  Priority_Level : priority of the message producer.
   --  Persistent     : default persistent status for sent messages.
   --  TTL            : default time to live for sent messages.
   --  Destination    : destination of sent messages.
   --  Type_Id_Of     : XXX to be defined.
   --  Ref            : reference to the provider servant.
   --  CBH            : call back handler associated to the producer.

   type CBH_Note is new PolyORB.Annotations.Note with record
      Dest : MOMA.Types.Ref;
   end record;

   function Create_Producer
     (Session  : MOMA.Sessions.Session;
      Dest     : MOMA.Destinations.Destination)
     return Message_Producer;
   --  Create a message producer whose destination is a MOM object.

   function Create_Producer
     (ORB_Object : MOMA.Types.String;
      Mesg_Pool  : MOMA.Types.String)
     return Message_Producer;
   --  Create a message producer whose destination is an ORB object.

   procedure Close;
   --  XXX not implemented. Rename it to Destroy ?

   procedure Send
     (Self    :        Message_Producer;
      Message : in out MOMA.Messages.Message'Class);
   --  Send a Message using the producer Self.
   --  XXX should send asynchronous message !!!

   procedure Send
     (Self           : Message_Producer;
      Message        : MOMA.Messages.Message'Class;
      Persistent     : Boolean;
      Priority_Value : MOMA.Types.Priority;
      TTL            : Time);
   --  Same as above, overriding default producer's values.
   --  XXX not implemented.

   --  Accessors to Message_Producer internal data.

   function Get_Persistent (Self : Message_Producer) return Boolean;

   procedure Set_Persistent
     (Self       : in out Message_Producer;
      Persistent :        Boolean);

   function Get_Priority
     (Self : Message_Producer)
     return MOMA.Types.Priority;

   procedure Set_Priority
     (Self  : in out Message_Producer;
      Value :        MOMA.Types.Priority);

   function Get_Time_To_Live
     (Self : Message_Producer)
     return Time;

   procedure Set_Time_To_Live
     (Self : in out Message_Producer;
      TTL  :        Time);

   function Get_Ref
     (Self : Message_Producer)
     return MOMA.Types.Ref;

   procedure Set_Ref
     (Self : in out Message_Producer;
      Ref  :        MOMA.Types.Ref);

   function Get_Type_Id_Of
     (Self : Message_Producer)
     return MOMA.Types.String;

   procedure Set_Type_Id_Of
     (Self       : in out Message_Producer;
      Type_Id_Of :        MOMA.Types.String);

   function Get_CBH
     (Self : Message_Producer)
     return PolyORB.Call_Back.CBH_Access;

   procedure Set_CBH
     (Self : in out Message_Producer;
      CBH  :        PolyORB.Call_Back.CBH_Access);

private

   type Message_Producer is record
      Priority_Level : MOMA.Types.Priority;
      Persistent     : Boolean;
      TTL            : Time;
      Destination    : MOMA.Destinations.Destination;
      Type_Id_Of     : MOMA.Types.String;
      Ref            : MOMA.Types.Ref;
      CBH            : PolyORB.Call_Back.CBH_Access;
   end record;

   --  Private accessors to Message_Producer internal data.

   function Get_Destination
     (Self : Message_Producer)
     return MOMA.Destinations.Destination;

   procedure Set_Destination
     (Self : in out Message_Producer;
      Dest :        MOMA.Destinations.Destination);

   pragma Inline (Get_Persistent);
   pragma Inline (Set_Persistent);
   pragma Inline (Get_Priority);
   pragma Inline (Set_Priority);
   pragma Inline (Get_Time_To_Live);
   pragma Inline (Set_Time_To_Live);
   pragma Inline (Get_Ref);
   pragma Inline (Set_Ref);
   pragma Inline (Get_Destination);
   pragma Inline (Set_Destination);
   pragma Inline (Get_Type_Id_Of);
   pragma Inline (Set_Type_Id_Of);
   pragma Inline (Get_CBH);
   pragma Inline (Set_CBH);

end MOMA.Message_Producers;
