------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          C L I E N T _ C A L L _ B A C K _ P R O C E D U R E S           --
--                                                                          --
--                                 B o d y                                  --
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

with MOMA.Messages.MBytes;

with PolyORB.Utils.Report;

package body Client_Call_Back_Procedures is

   use MOMA.Messages;
   use MOMA.Messages.MBytes;
   use MOMA.Types;

   use PolyORB.Annotations;
   use PolyORB.Utils.Report;

   --------------------
   -- Get_Byte_Value --
   --------------------

   function Get_Byte_Value
     (Message : MOMA.Messages.Message'Class)
     return MOMA.Types.Byte
   is
      MByte_Message_Rcvd : MOMA.Messages.MBytes.MByte;

   begin
      if Message in MOMA.Messages.MBytes.MByte then
         MByte_Message_Rcvd
           := MOMA.Messages.MBytes.MByte (Message);

      else
         raise Program_Error;
      end if;

      return Get_Byte (MByte_Message_Rcvd);
   end Get_Byte_Value;

   ------------------------
   -- Handle_Then_Notify --
   ------------------------

   procedure Handle_Then_Notify
     (Handler : access Message_Handler;
      Message :        MOMA.Messages.Message'Class)
   is
      Data : Byte_Test_Note;
      Id : constant Byte := Get_Byte_Value (Message);
      Ok : Boolean := False;

   begin
      Output ("Handling message ", True);
      Get_Call_Back_Data (Handler, Data);
      Ok := Id = Data.Byte_Value;
      Data.Proceed := True;
      Set_Call_Back_Data (Handler, Data);
      Output ("Retrieved message " & MOMA.Types.Byte'Image (Id), Ok);
      Set_Behavior (Handler, Notify);
   end Handle_Then_Notify;

   ------------------------
   -- Notify_And_Receive --
   ------------------------

   procedure Notify_And_Receive
     (Handler : access Message_Handler)
   is
      Data : Byte_Test_Note;
      Id : constant Byte := Receive_MByte (Get_Consumer (Handler));
      Ok : Boolean := False;

   begin
      Output ("Notified", True);
      Get_Call_Back_Data (Handler, Data);
      Ok := Id = Data.Byte_Value;
      Data.Proceed := True;
      Set_Call_Back_Data (Handler, Data);
      Output ("Retrieved message " & MOMA.Types.Byte'Image (Id), Ok);
   end Notify_And_Receive;

   ------------------------
   -- Notify_Then_Handle --
   ------------------------

   procedure Notify_Then_Handle (Handler : access Message_Handler) is
      Data : Byte_Test_Note;

   begin
      Output ("Notified", True);
      Get_Call_Back_Data (Handler, Data);
      Data.Proceed := True;
      Set_Call_Back_Data (Handler, Data);
      Set_Behavior (Handler, Handle);
   end Notify_Then_Handle;

   -------------------
   -- Receive_MByte --
   -------------------

   function Receive_MByte (MOMA_Consumer : Message_Consumer)
      return MOMA.Types.Byte
   is
      MOMA_Message_Temp : constant MOMA.Messages.Message'Class :=
                            Receive (MOMA_Consumer);
   begin
      return Get_Byte_Value (MOMA_Message_Temp);
   end Receive_MByte;

   ----------------
   -- Send_MByte --
   ----------------

   procedure Send_MByte
     (MOMA_Producer : Message_Producer;
      Id : MOMA.Types.Byte)
   is
      MByte_Message_Sent : MOMA.Messages.MBytes.MByte := Create_Byte_Message;

   begin
      Set_Byte (MByte_Message_Sent, Id);
      Send (MOMA_Producer, MByte_Message_Sent);
      Output ("Send message #" & Id'Img, True);
   end Send_MByte;

   ------------------------
   -- Set_Byte_Test_Note --
   ------------------------

   procedure Set_Byte_Test_Note
     (Handler : access Message_Handler;
      Proceed : Boolean;
      Byte_Value : MOMA.Types.Byte) is
   begin
      Set_Call_Back_Data
        (Handler,
         Byte_Test_Note'(Note with
            Byte_Value => Byte_Value,
            Proceed => Proceed));
   end Set_Byte_Test_Note;

end Client_Call_Back_Procedures;
