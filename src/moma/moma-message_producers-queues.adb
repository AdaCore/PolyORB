------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        M O M A . M E S S A G E _ P R O D U C E R S . Q U E U E S         --
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

with MOMA.Messages;
with MOMA.Messages.MBytes;
with MOMA.Messages.MTexts;

with PolyORB.Types;
with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Log;
with PolyORB.Requests;

package body MOMA.Message_Producers.Queues is

   use MOMA.Messages;
   use MOMA.Messages.MBytes;
   use MOMA.Messages.MTexts;
   use MOMA.Connections.Queues;

   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.Types;

   package L is
     new PolyORB.Log.Facility_Log ("moma.message_producers.queues");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ---------------
   -- Get_Queue --
   ---------------

   function Get_Queue return MOMA.Destinations.Queues.Queue is
   begin
      pragma Warnings (Off);
      return Get_Queue;
      pragma Warnings (On);
   end Get_Queue;

   ----------
   -- Send --
   ----------

   procedure Send (Self    : Queue;
                   Message : MOMA.Messages.Message'Class)
   is
      Argument_Mesg : PolyORB.Any.Any := MOMA.Messages.To_Any (Message);
      Request       : PolyORB.Requests.Request_Access;
      Arg_List      : PolyORB.Any.NVList.Ref;
      Result        : PolyORB.Any.NamedValue;

   begin
      pragma Debug (O ("Sending : " & PolyORB.Any.Image (Argument_Mesg)));

      PolyORB.Any.NVList.Create (Arg_List);

      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   To_PolyORB_String ("Message"),
                                   Argument_Mesg,
                                   PolyORB.Any.ARG_IN);

      Result := (Name      => To_PolyORB_String ("Result"),
                 Argument  => PolyORB.Any.Get_Empty_Any (PolyORB.Any.TC_Void),
                 Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => Get_Ref (Self),
         Operation => "Publish",
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);

      PolyORB.Requests.Destroy_Request (Request);

   end Send;

   ------------------
   -- Send_Receive --
   ------------------

   function Send_Receive (Self : MOMA.Connections.Queues.Queue;
                          Operation_Name : String;
                          Message : MOMA.Messages.Message'Class)
                          return MOMA.Messages.Message'Class
   is
      Argument_Mesg : PolyORB.Any.Any := Get_Payload (Message);

      Request       : PolyORB.Requests.Request_Access;
      Arg_List      : PolyORB.Any.NVList.Ref;
      Result        : PolyORB.Any.NamedValue;
      Result_Name   : PolyORB.Types.String := To_PolyORB_String ("Result");
      Result_Any    : PolyORB.Any.Any;
      TypeCode_Kind : PolyORB.Any.TCKind;

   begin
      PolyORB.Any.NVList.Create (Arg_List);

      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   To_PolyORB_String ("Message"),
                                   Argument_Mesg,
                                   PolyORB.Any.ARG_IN);

      Result := (Name      => PolyORB.Types.Identifier (Result_Name),
                 Argument  => Get_Empty_Any (Get_Type (Argument_Mesg)),
                 Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => Get_Ref (Self),
         Operation => Operation_Name,
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);

      PolyORB.Requests.Destroy_Request (Request);

      Result_Any := Result.Argument;
      pragma Debug (O ("Received " & PolyORB.Any.Image (Result_Any)));

      TypeCode_Kind := TypeCode.Kind (Get_Type (Result_Any));
      if TypeCode_Kind = Tk_String then
         declare
            Rcvd_Message : MOMA.Messages.MTexts.MText := Create_Text_Message;
         begin
            Set_Payload (Rcvd_Message, Result_Any);
            return Rcvd_Message;
         end;

      elsif TypeCode_Kind = Tk_Boolean or
        TypeCode_Kind = Tk_Octet or
        TypeCode_Kind = Tk_Char or
        TypeCode_Kind = Tk_Double or
        TypeCode_Kind = Tk_Float or
        TypeCode_Kind = Tk_Long or
        TypeCode_Kind = Tk_Short or
        TypeCode_Kind = Tk_Ulong or
        TypeCode_Kind = Tk_Ushort
      then
         declare
            Rcvd_Message : MOMA.Messages.MBytes.MByte := Create_Byte_Message;
         begin
            Set_Payload (Rcvd_Message, Result_Any);
            return Rcvd_Message;
         end;
      end if;

      raise Program_Error;
      --  Should not come to this point.

   end Send_Receive;


   ----------
   -- Send --
   ----------

   procedure Send
     (Message        : MOMA.Messages.Message'Class;
      Persistent     : Boolean;
      Priority_Value : MOMA.Types.Priority;
      TTL            : Time)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Message);
      pragma Unreferenced (Persistent);
      pragma Unreferenced (Priority_Value);
      pragma Unreferenced (TTL);
      pragma Warnings (On);
      null;
   end Send;

end MOMA.Message_Producers.Queues;

