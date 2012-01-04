------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          C L I E N T _ C A L L _ B A C K _ P R O C E D U R E S           --
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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
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
