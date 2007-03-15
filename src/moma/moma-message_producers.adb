------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               M O M A . M E S S A G E _ P R O D U C E R S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with MOMA.Messages.MExecutes;
with MOMA.Runtime;

with PolyORB.MOMA_P.Exceptions;
with PolyORB.MOMA_P.Provider.Message_Producer;

with PolyORB.Any.NVList;
with PolyORB.Errors;
with PolyORB.Log;
with PolyORB.Minimal_Servant.Tools;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Request_QoS;
with PolyORB.QoS.Priority;
with PolyORB.Tasking.Priorities;
with PolyORB.Types;

package body MOMA.Message_Producers is

   use MOMA.Messages;
   use MOMA.Messages.MExecutes;
   use MOMA.Types;

   use PolyORB.MOMA_P.Provider.Message_Producer;

   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.Minimal_Servant.Tools;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("moma.message_producers");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   procedure Response_Handler
     (Req :        PolyORB.Requests.Request;
      CBH : access PolyORB.Call_Back.Call_Back_Handler);
   --  Call back handler attached to a MOM producer interacting with
   --  an ORB node.

   procedure Send_To_MOM
     (Servant : MOMA.Types.Ref;
      Message : MOMA.Messages.Message'Class);
   --  Send Message to a MOM object.

   procedure Send_To_ORB
     (Self    : Message_Producer;
      Message : MOMA.Messages.Message'Class);
   --  Send Message to an ORB object, see MOMA.Messages.MExecutes
   --  specifications for more details.

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      null;
   end Close;

   ---------------------
   -- Create_Producer --
   ---------------------

   function Create_Producer
     (Session  : MOMA.Sessions.Session;
      Dest     : MOMA.Destinations.Destination)
     return Message_Producer
   is
      pragma Warnings (Off);
      pragma Unreferenced (Session);
      pragma Warnings (On);
      --  XXX Session is to be used to 'place' the receiver
      --  using session position in the POA

      use PolyORB.Errors;
      use PolyORB.References;

      MOMA_Obj : constant PolyORB.MOMA_P.Provider.Message_Producer.Object_Acc
        := new PolyORB.MOMA_P.Provider.Message_Producer.Object;

      MOMA_Ref : PolyORB.References.Ref;
      Producer : MOMA.Message_Producers.Message_Producer;
      Type_Id_S : MOMA.Types.String :=
        To_MOMA_String (Type_Id_Of (MOMA.Destinations.Get_Ref (Dest)));

      Error : Error_Container;
   begin
      Initiate_Servant
        (MOMA_Obj,
         MOMA.Runtime.MOMA_OA,
         PolyORB.Types.String (MOMA.Types.MOMA_Type_Id),
         MOMA_Ref,
         Error);

      if Found (Error) then
         PolyORB.MOMA_P.Exceptions.Raise_From_Error (Error);
      end if;

      Set_Remote_Ref (MOMA_Obj.all, MOMA.Destinations.Get_Ref (Dest));
      Set_Destination (Producer, Dest);
      Set_Ref (Producer, MOMA_Ref);
      Set_Type_Id_Of (Producer, Type_Id_S);
      --  XXX Is it really useful to have the Ref to the remote destination in
      --  the Message_Producer itself ? By construction, this ref is
      --  encapsulated in the PolyORB.MOMA_P.Provider.Message_Producer.Object
      return Producer;
   end Create_Producer;

   function Create_Producer
     (ORB_Object : MOMA.Types.String;
      Mesg_Pool  : MOMA.Types.String)
     return Message_Producer
   is
      use PolyORB.Annotations;
      use PolyORB.Call_Back;
      use PolyORB.References;

      Producer : MOMA.Message_Producers.Message_Producer;

      ORB_Object_IOR      : PolyORB.References.Ref;
      Dest_Ref_Object_IOR : PolyORB.References.Ref;

   begin
      String_To_Object
        (MOMA.Types.To_Standard_String (ORB_Object), ORB_Object_IOR);
      String_To_Object
        (MOMA.Types.To_Standard_String (Mesg_Pool), Dest_Ref_Object_IOR);

      declare
         Type_Id_S : constant String := Type_Id_Of (ORB_Object_IOR);
      begin

         if Type_Id_S = MOMA_Type_Id then
            raise Program_Error;
         end if;

         Set_Ref (Producer, ORB_Object_IOR);
         Set_Type_Id_Of (Producer, To_PolyORB_String (Type_Id_S));
         Set_CBH (Producer, new PolyORB.Call_Back.Call_Back_Handler);
         --  XXX should free this memory sometime, somewhere ...
      end;

      Attach_Handler_To_CB
        (Call_Back_Handler (Get_CBH (Producer).all),
         MOMA.Message_Producers.Response_Handler'Access);

      Set_Note
         (Notepad_Of (Get_CBH (Producer)).all,
          CBH_Note'(Note with Dest => Dest_Ref_Object_IOR));

      return Producer;
   end Create_Producer;

   -------------
   -- Get_CBH --
   -------------

   function Get_CBH
     (Self : Message_Producer)
     return PolyORB.Call_Back.CBH_Access is
   begin
      return Self.CBH;
   end Get_CBH;

   ---------------------
   -- Get_Destination --
   ---------------------

   function Get_Destination
     (Self : Message_Producer)
     return MOMA.Destinations.Destination is
   begin
      return Self.Destination;
   end Get_Destination;

   --------------------
   -- Get_Persistent --
   --------------------

   function Get_Persistent
     (Self : Message_Producer)
     return Boolean is
   begin
      return Self.Persistent;
   end Get_Persistent;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority
     (Self : Message_Producer)
     return MOMA.Types.Priority is
   begin
      return Self.Priority_Level;
   end Get_Priority;

   -------------
   -- Get_Ref --
   -------------

   function Get_Ref
     (Self : Message_Producer)
     return MOMA.Types.Ref is
   begin
      return Self.Ref;
   end Get_Ref;

   ----------------------
   -- Get_Time_To_Live --
   ----------------------

   function Get_Time_To_Live
     (Self : Message_Producer)
     return Time is
   begin
      return Self.TTL;
   end Get_Time_To_Live;

   --------------------
   -- Get_Type_Id_Of --
   --------------------

   function Get_Type_Id_Of
     (Self : Message_Producer)
     return MOMA.Types.String is
   begin
      return Self.Type_Id_Of;
   end Get_Type_Id_Of;

   ----------------------
   -- Response_Handler --
   ----------------------

   procedure Response_Handler
     (Req :        PolyORB.Requests.Request;
      CBH : access PolyORB.Call_Back.Call_Back_Handler)
   is
      use PolyORB.Annotations;
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
         Method_Name
           := (Name  => To_MOMA_String ("method"),
               Value => PolyORB.Any.To_Any
               (PolyORB.Types.To_PolyORB_String (Req.Operation.all)));

         Return_1 := (Name  => To_MOMA_String ("return_1"),
                      Value => Req.Result.Argument);
         Append (Parameter_Map, Method_Name);
         Append (Parameter_Map, Return_1);
         Set_Parameter (Message, Parameter_Map);

         Get_Note (Notepad_Of (CBH).all, Note);
         Send_To_MOM (Note.Dest, Message);
      end;
   end Response_Handler;

   ----------
   -- Send --
   ----------

   procedure Send
     (Self    :        Message_Producer;
      Message : in out MOMA.Messages.Message'Class)
   is
      use MOMA.Destinations;
      Type_Id_S     : constant MOMA.Types.String := Get_Type_Id_Of (Self);
   begin
      MOMA.Messages.Set_Destination (Message,
                                     Get_Destination (Self));
      if Type_Id_S = MOMA.Types.MOMA_Type_Id then
         Send_To_MOM (Get_Ref (Self),
                      Message);
      else
         Send_To_ORB (Self, Message);
      end if;
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send
     (Self           : Message_Producer;
      Message        : MOMA.Messages.Message'Class;
      Persistent     : Boolean;
      Priority_Value : MOMA.Types.Priority;
      TTL            : Time)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (Message);
      pragma Unreferenced (Persistent);
      pragma Unreferenced (Priority_Value);
      pragma Unreferenced (TTL);
      pragma Warnings (On);
   begin

      null;
      --  XXX Not Implemented
   end Send;

   -----------------
   -- Send_To_MOM --
   -----------------

   procedure Send_To_MOM
     (Servant : MOMA.Types.Ref;
      Message : MOMA.Messages.Message'Class)
   is
      use type PolyORB.Tasking.Priorities.External_Priority;

      Argument_Mesg : PolyORB.Any.Any := MOMA.Messages.To_Any (Message);
      Request       : PolyORB.Requests.Request_Access;
      Arg_List      : PolyORB.Any.NVList.Ref;
      Result        : PolyORB.Any.NamedValue;
   begin
      pragma Debug (O ("Sending to MOM object : "
                       & PolyORB.Any.Image (Argument_Mesg)));
      PolyORB.Any.NVList.Create (Arg_List);

      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   To_PolyORB_String ("Message"),
                                   Argument_Mesg,
                                   PolyORB.Any.ARG_IN);

      Result := (Name      => To_PolyORB_String ("Result"),
                 Argument  => PolyORB.Any.Get_Empty_Any (PolyORB.Any.TC_Void),
                 Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => Servant,
         Operation => "Publish",
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);

      if MOMA.Messages.Get_Priority (Message) /= Invalid_Priority then
         declare
            Prio_QoS : PolyORB.QoS.QoS_Parameter_Access;

         begin
            Prio_QoS := new PolyORB.QoS.Priority.QoS_Static_Priority;
            PolyORB.QoS.Priority.QoS_Static_Priority (Prio_QoS.all).EP
              := MOMA.Messages.Get_Priority (Message);

            PolyORB.Request_QoS.Add_Request_QoS
              (Request,
               PolyORB.QoS.Static_Priority,
               Prio_QoS);
         end;
      end if;

      PolyORB.Requests.Invoke (Request);

      PolyORB.Requests.Destroy_Request (Request);
   end Send_To_MOM;

   -----------------
   -- Send_To_ORB --
   -----------------

   procedure Send_To_ORB
     (Self    : Message_Producer;
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
           (Get_String (Get_Element (Parameter_Map, 1)));

         Result_TypeCode  : constant PolyORB.Any.TypeCode.Object
           := Get_Type (Get_Element (Parameter_Map, 2).Value);
      begin
         pragma Debug (O ("Method name : " & Method_Name));

         PolyORB.Any.NVList.Create (Arg_List);

         for J in 3 .. Length (Parameter_Map)  loop
            pragma Debug (O ("Argument: " & PolyORB.Types.To_Standard_String
                             (PolyORB.Any.From_Any
                              (Get_Element (Parameter_Map, J).Value))));

            PolyORB.Any.NVList.Add_Item (Arg_List,
                                         To_PolyORB_String ("Message"),
                                         Get_Element (Parameter_Map, J).Value,
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

   -------------
   -- Set_CBH --
   -------------

   procedure Set_CBH
     (Self : in out Message_Producer;
      CBH  :        PolyORB.Call_Back.CBH_Access) is
   begin
      Self.CBH := CBH;
   end Set_CBH;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Self : in out Message_Producer;
      Dest :        MOMA.Destinations.Destination) is
   begin
      Self.Destination := Dest;
   end Set_Destination;

   --------------------
   -- Set_Persistent --
   --------------------

   procedure Set_Persistent
     (Self       : in out Message_Producer;
      Persistent :        Boolean) is
   begin
      Self.Persistent := Persistent;
   end Set_Persistent;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (Self  : in out Message_Producer;
      Value :        MOMA.Types.Priority) is
   begin
      Self.Priority_Level := Value;
   end Set_Priority;

   -------------
   -- Set_Ref --
   -------------

   procedure Set_Ref
     (Self : in out Message_Producer;
      Ref  :        MOMA.Types.Ref) is
   begin
      Self.Ref := Ref;
   end Set_Ref;

   ----------------------
   -- Set_Time_To_Live --
   ----------------------

   procedure Set_Time_To_Live
     (Self : in out Message_Producer;
      TTL  :        Time) is
   begin
      Self.TTL := TTL;
   end Set_Time_To_Live;

   --------------------
   -- Set_Type_Id_Of --
   --------------------

   procedure Set_Type_Id_Of
     (Self        : in out Message_Producer;
      Type_Id_Of  :        MOMA.Types.String) is
   begin
      Self.Type_Id_Of := Type_Id_Of;
   end Set_Type_Id_Of;

end MOMA.Message_Producers;
