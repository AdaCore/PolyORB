------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               M O M A . M E S S A G E _ P R O D U C E R S                --
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

--  A Message_Producer object is the client view of the message sending
--  process. It is the facade to all communication carried out with
--  a message pool to send messages; it contains the stub to access
--  'Message_Producer' servants (see MOMA.Provider and child packages
--  for more details).

--  NOTE: A MOMA client must use only this package, and its child packages to
--  send message to a message pool.

--  $Id$

with Ada.Real_Time;

with MOMA.Destinations;
with MOMA.Types;

with PolyORB.Call_Back;
with PolyORB.References;

package MOMA.Message_Producers is

   use Ada.Real_Time;

   type Message_Producer is abstract tagged private;
   --  Priority_Level : priority of the message producer
   --  Persistent     : default persistent status for sent messages
   --  TTL            : default time to live for sent messages.
   --  Destination    : destination of sent messages.
   --  Type_Id_Of     : XXX to be defined
   --  Ref            : reference (XXX to be defined).
   --  CBH            : call back handler associated to the producer.

   procedure Close;
   --  XXX not implemented. rename it to Destroy ?

   --  Accessors to Message_Producer internal data.

   function Get_Persistent (Self : Message_Producer) return Boolean;

   procedure Set_Persistent (Self : in out Message_Producer;
                             Persistent : Boolean);

   function Get_Priority (Self : Message_Producer) return MOMA.Types.Priority;

   procedure Set_Priority (Self : in out Message_Producer;
                           Value : MOMA.Types.Priority);

   function Get_Time_To_Live (Self : Message_Producer) return Time;

   procedure Set_Time_To_Live (Self : in out Message_Producer;
                               TTL : Time);

   function Get_Ref (Self : Message_Producer) return PolyORB.References.Ref;

   procedure Set_Ref (Self : in out Message_Producer;
                      Ref  : PolyORB.References.Ref);

   function Get_Destination (Self : Message_Producer)
                             return MOMA.Destinations.Destination;

   procedure Set_Destination (Self : in out Message_Producer'Class;
                              Dest : MOMA.Destinations.Destination);

   --  XXX These two functions should not be called from the client
   --  Need to hide them ...

   function Get_Type_Id_Of (Self : Message_Producer)
                            return MOMA.Types.String;

   procedure Set_Type_Id_Of (Self : in out Message_Producer;
                             Type_Id_Of : MOMA.Types.String);

   function Get_CBH (Self : Message_Producer)
                    return PolyORB.Call_Back.CBH_Access;

   procedure Set_CBH (Self : in out Message_Producer;
                      CBH  : PolyORB.Call_Back.CBH_Access);
private

   type Message_Producer is abstract tagged record
      Priority_Level : MOMA.Types.Priority;
      Persistent     : Boolean;
      TTL            : Time;
      Destination    : MOMA.Destinations.Destination;
      Type_Id_Of     : MOMA.Types.String;
      Ref            : PolyORB.References.Ref;
      CBH            : PolyORB.Call_Back.CBH_Access;
   end record;

   pragma Inline (Get_Persistent,
                    Set_Persistent,
                    Get_Priority,
                    Set_Priority,
                    Get_Time_To_Live,
                    Set_Time_To_Live,
                    Get_Ref,
                    Set_Ref,
                    Get_Destination,
                    Set_Destination,
                    Get_Type_Id_Of,
                    Set_Type_Id_Of,
                    Get_CBH,
                    Set_CBH);

end MOMA.Message_Producers;
