------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          C L I E N T _ C A L L _ B A C K _ P R O C E D U R E S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2004 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Procedures for call_back test client

with MOMA.Messages;
with MOMA.Message_Producers;
with MOMA.Message_Consumers;
with MOMA.Message_Handlers;
with MOMA.Types;

with PolyORB.Annotations;

package Client_Call_Back_Procedures is

   use MOMA.Message_Producers;
   use MOMA.Message_Consumers;
   use MOMA.Message_Handlers;

   type Byte_Test_Note is new PolyORB.Annotations.Note
   with record
      Byte_Value : MOMA.Types.Byte;
      Proceed    : Boolean;
   end record;

   function Get_Byte_Value
     (Message : MOMA.Messages.Message'Class)
     return MOMA.Types.Byte;

   --  The following procedures also set the Proceed Boolean to True
   --  to allow the client to proceed with next test.

   procedure Handle_Then_Notify
     (Handler : access Message_Handler;
      Message :        MOMA.Messages.Message'Class);
   --  Handle the message, compare its Id with current Byte_Value in
   --  Call_Back_Byte_test and set the behavior to Notify.

   procedure Notify_And_Receive (Handler : access Message_Handler);
   --  Receive notified message, compare its Id with current Byte_Value in
   --  Call_Back_Byte_test. Does not change behavior.

   procedure Notify_Then_Handle (Handler : access Message_Handler);
   --  Change the behavior to Handle.

   function Receive_MByte
     (MOMA_Consumer : Message_Consumer)
     return MOMA.Types.Byte;

   procedure Send_MByte
     (MOMA_Producer : Message_Producer;
      Id            : MOMA.Types.Byte);

   procedure Set_Byte_Test_Note
     (Handler    : access Message_Handler;
      Proceed    :        Boolean;
      Byte_Value : MOMA.Types.Byte);

end Client_Call_Back_Procedures;
