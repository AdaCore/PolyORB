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
with MOMA.Messages.MExecutes;
with MOMA.Types;

with PolyORB.Annotations;
with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Call_Back;
with PolyORB.Log;
with PolyORB.Requests;
with PolyORB.References;
with PolyORB.Types;

package body MOMA.Message_Producers.Queues is

   use MOMA.Messages;
   use MOMA.Messages.MExecutes;
   use MOMA.Types;

   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.Types;

   package L is
     new PolyORB.Log.Facility_Log ("moma.message_producers.queues");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Send_To_MOM (Servant : PolyORB.References.Ref;
                          Message : MOMA.Messages.Message'Class;
                          Kind    : MOMA.Types.Destination_Type := Unknown);
   --  Send Message to a MOM object.

   procedure Send_To_ORB (Self    : Queue;
                          Message : MOMA.Messages.Message'Class);
   --  Send Message to an ORB object, see MOMA.Messages.MExecutes
   --  specifications for more details.

   ---------------
   -- Get_Queue --
   ---------------

   --  function Get_Queue return MOMA.Destinations.Queues.Queue is
   --  begin
   --     pragma Warnings (Off);
   --     return Get_Queue;
   --     pragma Warnings (On);
   --  end Get_Queue;

   ----------
   -- Send --
   ----------

   procedure Send (Self    : Queue;
                   Message : MOMA.Messages.Message'Class)
   is
      use MOMA.Destinations;
      Type_Id_S     : constant MOMA.Types.String := Get_Type_Id_Of (Self);
   begin
      if Type_Id_S = MOMA.Types.MOMA_Type_Id then
         Send_To_MOM (Get_Ref (Self),
                      Message,
                      Get_Kind (Get_Destination (Self)));
      else
         Send_To_ORB (Self, Message);
      end if;
   end Send;

   -----------------
   -- Send_To_MOM --
   -----------------

   procedure Send_To_MOM (Servant : PolyORB.References.Ref;
                          Message : MOMA.Messages.Message'Class;
                          Kind    : MOMA.Types.Destination_Type := Unknown)
   is
      Argument_Mesg : PolyORB.Any.Any := MOMA.Messages.To_Any (Message);
      Request       : PolyORB.Requests.Request_Access;
      Arg_List      : PolyORB.Any.NVList.Ref;
      Result        : PolyORB.Any.NamedValue;
--      Argument_Mesg2 : PolyORB.Any.Any := PolyORB.Any.To_Any
--        (To_PolyORB_String (""));
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Kind);
      pragma Warnings (On);

      pragma Debug (O ("Sending to MOM object : "
                       & PolyORB.Any.Image (Argument_Mesg)));
      PolyORB.Any.NVList.Create (Arg_List);

      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   To_PolyORB_String ("Message"),
                                   Argument_Mesg,
                                   PolyORB.Any.ARG_IN);

--      if Kind = Topic then
--         PolyORB.Any.NVList.Add_Item (Arg_List,
--                                      To_PolyORB_String ("Topic_Id"),
--                                      Argument_Mesg2,
--                                      PolyORB.Any.ARG_IN);
      --  XXX The destination should not be always Test ! ;)
--      end if;
--  XXX To uncomment.

      Result := (Name      => To_PolyORB_String ("Result"),
                 Argument  => PolyORB.Any.Get_Empty_Any (PolyORB.Any.TC_Void),
                 Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => Servant,
         Operation => "Publish",
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);

      PolyORB.Requests.Destroy_Request (Request);

   end Send_To_MOM;

   -----------------
   -- Send_To_ORB --
   -----------------

   procedure Send_To_ORB (Self    : Queue;
                          Message : MOMA.Messages.Message'Class)
   is
      use PolyORB.Any.TypeCode;
      use PolyORB.Call_Back;

      Request       : PolyORB.Requests.Request_Access;
      Arg_List      : PolyORB.Any.NVList.Ref;
      Result        : PolyORB.Any.NamedValue;
      Parameter_Map : Map;

   begin
      pragma Debug (O ("Sending message to ORB object."));

      if Message not in MExecute then
         raise Program_Error;
      else
         Parameter_Map := Get_Parameter (MExecute (Message));
      end if;

      declare
         Method_Name : constant String
           := MOMA.Types.To_Standard_String
           (Get_String (Element_Of (Parameter_Map, 1)));

         Result_TypeCode  : constant PolyORB.Any.TypeCode.Object
           := Get_Type (Element_Of (Parameter_Map, 2).Value);
      begin
         pragma Debug (O ("Method name : " & Method_Name));

         PolyORB.Any.NVList.Create (Arg_List);

         for J in 3 .. Length (Parameter_Map)  loop
            pragma Debug (O ("Argument : " & MOMA.Types.To_Standard_String
                             (From_Any (Element_Of
                                        (Parameter_Map, J).Value))));

            PolyORB.Any.NVList.Add_Item (Arg_List,
                                         To_PolyORB_String ("Message"),
                                         Element_Of (Parameter_Map, J).Value,
                                         PolyORB.Any.ARG_IN);
         end loop;

         Result := (Name      => To_PolyORB_String ("Result"),
                    Argument  => PolyORB.Any.Get_Empty_Any (Result_TypeCode),
                    Arg_Modes => 0);

         PolyORB.Requests.Create_Request
           (Target    => Get_Ref (Self),
            Operation => Method_Name,
            Arg_List  => Arg_List,
            Result    => Result,
            Req       => Request,
            Req_Flags => PolyORB.Requests.Sync_Call_Back);

         if Result_TypeCode /= TypeCode.TC_Void then
            pragma Debug (O ("Non void return parameter."));
            Attach_Request_To_CB (Request, Self.CBH);
         end if;

         pragma Debug (O ("Invoking : "
                          & PolyORB.Requests.Image (Request.all)));

         PolyORB.Requests.Invoke (Request);

         if Result_TypeCode = TypeCode.TC_Void then
            PolyORB.Requests.Destroy_Request (Request);
         end if;
         --  Note : in the other case, the request is destroyed when sending
         --  see polyorb-protocols.adb for more details.

      end;

   end Send_To_ORB;

   ----------
   -- Send --
   ----------

   procedure Send
     (Self           : Queue;
      Message        : MOMA.Messages.Message'Class;
      Persistent     : Boolean;
      Priority_Value : MOMA.Types.Priority;
      TTL            : Time)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (Message);
      pragma Unreferenced (Persistent);
      pragma Unreferenced (Priority_Value);
      pragma Unreferenced (TTL);
      pragma Warnings (On);
      null;
      --  XXX Not Implemented
   end Send;

   ----------------------
   -- Response_Handler --
   ----------------------

   procedure Response_Handler
     (Req : PolyORB.Requests.Request;
      CBH : access PolyORB.Call_Back.Call_Back_Handler)
   is
      use PolyORB.Annotations;
      use PolyORB.Any;
      use PolyORB.Call_Back;

      Message : MExecute := Create_Execute_Message;
   begin
      pragma Debug (O ("Got : " & PolyORB.Requests.Image (Req)));
      pragma Debug (O ("return value : "
                       & PolyORB.Any.Image (Req.Result.Argument)));
      declare
         Method_Name   : Map_Element;
         Return_1      : Map_Element;
         Parameter_Map : Map;
         Note          : CBH_Note;
      begin
         Method_Name := (Name  => To_MOMA_String ("method"),
                         Value => To_Any
                         (PolyORB.Types.String (Req.Operation)));

         Return_1 := (Name  => To_MOMA_String ("return_1"),
                      Value => Req.Result.Argument);
         Append (Parameter_Map, Method_Name);
         Append (Parameter_Map, Return_1);
         Set_Parameter (Message, Parameter_Map);

         Get_Note (Notepad_Of (CBH).all, Note);
         Send_To_MOM (Note.Dest, Message);
      end;
   end Response_Handler;

end MOMA.Message_Producers.Queues;
