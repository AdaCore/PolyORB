------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . P R O T O C O L S . S O A P                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

with Ada.Unchecked_Deallocation;
with Ada.Streams; use Ada.Streams;

with PolyORB.Representations.SOAP;
with PolyORB.Representations.SOAP.Any;
--   with PolyORB.Representations.SOAP.Name_Spaces;

with PolyORB.Any.NVList;
with PolyORB.Types;
with PolyORB.Requests;
with PolyORB.ORB;
with PolyORB.ORB.Interface;
with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.Binding_Data.Local;
with PolyORB.Binding_Data.SOAP;
with PolyORB.References;
with PolyORB.Buffers;
with PolyORB.Opaque;

with PolyORB.Components;
with PolyORB.Filters.Interface;

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

with PolyORB.Protocols;
with PolyORB.Protocols.HTTP;
with PolyORB.Utils.HTTP;
with PolyORB.Utils.HTTP_Messages;

package body PolyORB.Protocols.SOAP  is

   use PolyORB.Any;
   use PolyORB.Any.NVList;
   use PolyORB.Buffers;
   use PolyORB.Log;
   use PolyORB.Protocols;
   use PolyORB.Protocols.HTTP;
   use PolyORB.Representations.SOAP.Any;
   use PolyORB.Types;
   use PolyORB.Utils.HTTP_Messages;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols.soap");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Request_To_SOAP_Method
     (Operation : PolyORB.Types.Identifier;
      Args  : PolyORB.Any.NVList.Ref;
      Uri : XML_String;
      Method : out XML_Component_Access)
   is
      use Internals;
      use Internals.NV_Sequence;
      List : NV_Sequence_Access;
      Current_Arg  : NamedValue;
      XML_Comp  : XML_Component_Access;

   begin
      Method := new XML_Component;
      Initialize (Method,  Method_Tag_Reference & ":"
         & XML_String (Operation), XML_Null_String,
         Xsd_Struct);

      Add_Attributes (Method, "xmlns:" & Method_Tag_Reference,
                      """" & Uri & """");

      List :=  List_Of (Args);
      for I in 1 ..  Get_Count (Args) loop
         Current_Arg := NV_Sequence.Element_Of (List.all, Positive (I));
         Any_To_XML_Components (Current_Arg.Name,
            Current_Arg.Argument, XML_Comp);
         Add_Child (Method, XML_Comp);
         Set_Parent (XML_Comp, Method);
      end loop;
   end Request_To_SOAP_Method;


   procedure Response_To_SOAP_Method
     (Operation : PolyORB.Types.Identifier;
      Result    : PolyORB.Any.NamedValue;
      Uri : XML_String;
      Method : out XML_Component_Access)
   is
      XML_Comp  : XML_Component_Access;

   begin
      Method := new XML_Component;
      Initialize (Method,  Method_Tag_Reference & ":"
         & XML_String (Operation), XML_Null_String,
         Xsd_Struct);

      Add_Attributes (Method, "xmlns:" & Method_Tag_Reference,
                      """" & Uri & """");

      Any_To_XML_Components (Result.Name,
          Result.Argument, XML_Comp);

      Add_Child (Method, XML_Comp);
      Set_Parent (XML_Comp, Method);
   end Response_To_SOAP_Method;


   procedure Set_Envelope
      (Mess : access SOAP_Message)
   is
      XML_Comp : XML_Component_Access := new XML_Component;
   begin
      Initialize (XML_Comp, SOAP_Tag & Separator_Tag & Envelope_Tag,
            XML_Null_String, Xsd_Struct);
      Add_Attributes (XML_Comp, SOAP_XMLNS_Tag, SOAP_URI);
      Add_Attributes (XML_Comp, SOAP_Tag & Separator_Tag
            & Encoding_Style_Tag, Encoding_Style_URI);

      Mess.Envelope_Field := XML_Comp;
   end Set_Envelope;


   procedure Set_Method
      (Mess    : access SOAP_Message;
       Req     : Requests.Request_Access;
       Role    : Role_Type := Client)
   is
      XML_Comp_Body     : XML_Component_Access := new XML_Component;
      XML_Comp_Method : XML_Component_Access;
   begin

      Initialize (XML_Comp_Body, SOAP_Tag & Separator_Tag
          & Body_Tag, XML_Null_String, Xsd_Struct);
      if Role = Client then
         Request_To_SOAP_Method (Req.Operation, Req.Args,
          Mess.Method_NS, XML_Comp_Method);
      else
         Response_To_SOAP_Method (Req.Operation, Req.Result,
          Mess.Method_NS, XML_Comp_Method);
      end if;

      Add_Child (XML_Comp_Method, XML_Comp_Body);
      Set_Parent (XML_Comp_Body, XML_Comp_Method);

      if Mess.Envelope_Field /= null then
         Add_Child (Mess.Envelope_Field, XML_Comp_Body);
         Set_Parent (XML_Comp_Body, Mess.Envelope_Field);
      end if;

      Mess.Body_Field := XML_Comp_Body; -- XML_Comp_Body;
   end Set_Method;

   procedure Set_Fault
     (Mess          : access SOAP_Message;
      Faultcode   : Integer;
      Runcode     : XML_String;
      Faultstring : XML_String := XML_Null_String;
      Detail          : XML_String := XML_Null_String)
   is
      XML_Comp_Body : XML_Component_Access := new XML_Component;
      XML_Comp_Fault : XML_Component_Access := new XML_Component;
      XML_Comp_Faultcode : XML_Component_Access := new XML_Component;
      XML_Comp_Faultruncode : XML_Component_Access := new XML_Component;
      XML_Comp_Faultstring : XML_Component_Access := new XML_Component;
      XML_Comp_Faultdetail : XML_Component_Access := new XML_Component;
   begin
      Initialize (XML_Comp_Body, SOAP_Tag & Separator_Tag & Body_Tag,
          XML_Null_String, Xsd_Struct);
      Initialize (XML_Comp_Fault, SOAP_Tag & Separator_Tag & Fault_Tag,
          XML_Null_String, Xsd_Struct);

      Add_Child (XML_Comp_Body, XML_Comp_Fault);
      Set_Parent (XML_Comp_Fault, XML_Comp_Body);

      Initialize (XML_Comp_Faultcode, Faultcode_Tag,
         To_PolyORB_String (Integer'Image (Faultcode)), Xsd_Simple);
      Add_Child (XML_Comp_Fault, XML_Comp_Faultcode);
      Set_Parent (XML_Comp_Faultcode, XML_Comp_Fault);

      Initialize (XML_Comp_Faultruncode, Runcode_Tag, Runcode, Xsd_Simple);
      Add_Child (XML_Comp_Fault, XML_Comp_Faultruncode);
      Set_Parent (XML_Comp_Faultruncode, XML_Comp_Fault);

      Initialize (XML_Comp_Faultstring, Faultstring_Tag,
           Faultstring, Xsd_Simple);
      Add_Child (XML_Comp_Fault, XML_Comp_Faultstring);
      Set_Parent (XML_Comp_Faultstring, XML_Comp_Fault);

      Initialize (XML_Comp_Faultdetail, Detail_Tag,
           Detail, Xsd_Simple);
      Add_Child (XML_Comp_Fault, XML_Comp_Faultdetail);
      Set_Parent (XML_Comp_Faultdetail, XML_Comp_Fault);

      Mess.Body_Field := XML_Comp_Body;
   end Set_Fault;


   function Method
     (Item : XML_Component_Access)
     return XML_Component_Access
   is
      Result : XML_Component_Access;
   begin
      if Item.Childs /= null then
         Result := First_Element (Item);
      end if;
      return Result;
   end Method;


   procedure Build_SOAP_Message
     (Root : XML_Component_Access;
      Mess : out SOAP_Message_Access)
   is
      Envelope_Path : constant XML_String := SOAP_Tag
             & Separator_Tag & Envelope_Tag;

      Header_Path : constant XML_String := Envelope_Path & '.' & SOAP_Tag &
                     Separator_Tag & Header_Tag;

      Body_Path : constant XML_String := Envelope_Path & '.'
                   & SOAP_Tag & Separator_Tag & Body_Tag;
      Item : XML_Component_Access;

   begin
      Mess := new SOAP_Message;
      Mess.Envelope_Field := Get (Root, Envelope_Path);
      Mess.Header_Field := Get (Root, Header_Path);
      Mess.Body_Field := Get (Root, Body_Path);
      if Mess.Body_Field /= null then
         Item  := Get (Mess.Body_Field, SOAP_Tag &
                       Separator_Tag & '.' & Fault_Tag);
         if Item = null then
            Mess.Method_NS := Attributes (Method (Mess.Body_Field),
                      SOAP_XMLNS_Tag &
                      Separator_Tag & Method_Tag_Reference);
         end if;
      else
         raise SOAP_Syntax_Fault;
      end if;
   end Build_SOAP_Message;

   procedure Release
     (Mess : in out SOAP_Message_Access)
   is
      procedure Free is
         new Ada.Unchecked_Deallocation
        (SOAP_Message, SOAP_Message_Access);
   begin
      Release (Mess.Header_Field);
      Release (Mess.Body_Field);
      --    Release (Mess.Fault_Field);
      Release (Mess.Envelope_Field);
      Free (Mess);
   end Release;

   procedure Release
      (Ses : in out HTTP_Session_Access)
   is
      procedure Free is
         new Ada.Unchecked_Deallocation
        (HTTP_Session, HTTP_Session_Access);
   begin
      --  Release (Ses.Connection);
      --  Release (Ses.Response);
      --  Release (Ses.Request);
      Free (Ses);
   end Release;


   -----------------------------
   --  Handling incoming data
   -----------------------------


   -----------------------------
   --  Invoked on the server side
   -----------------------------

   procedure Perform_Request (S : access SOAP_Session)
   is
      use PolyORB.ORB;
      use PolyORB.ORB.Interface;
      use Internals;
      use Internals.NV_Sequence;
      use PolyORB.Objects;
      use PolyORB.Utils.HTTP;
      use PolyORB.Obj_Adapters;
      use PolyORB.Binding_Data.Local;
      use PolyORB.References;
      use PolyORB.Components;
      use PolyORB.Filters.Interface;
      Fault_Code  : Integer    := 0;
      Fault_RC  : XML_String := XML_Null_String;
      Fault_String  : XML_String := XML_Null_String;
      Method_Comp  : XML_Component_Access;
      Req : SOAP_Message_Access renames S.Request;
   begin
      if Attributes (Req.Envelope_Field, SOAP_XMLNS_Tag
          & Separator_Tag & SOAP_Tag) = SOAP_URI and then
         Attributes (Req.Envelope_Field, SOAP_Tag
         & Separator_Tag & Encoding_Style_Tag)
                     = Encoding_Style_URI then
         Method_Comp  := Method (S.Request.Body_Field);
         if Method_Comp /= null then
            declare
               Method_Name : XML_String := Last_Name (Tag (Method_Comp));
            begin
               if First_Name (Tag (Method_Comp)) =
                   Method_Tag_Reference and then
                    Req.Method_NS /= XML_Null_String then
                  declare
                           ORB : constant ORB_Access := ORB_Access (S.Server);
                           Args   : Any.NVList.Ref;
                           Result : Any.NamedValue;
                           Temp_Arg  : Any.NamedValue;
                           List     : NV_Sequence_Access;
                           Obj_Id : Object_Id := Object_Id (Base64_Decode
                                   (To_Standard_String (Request_URI
                                   (S.HTTP_Session.Request.all))));
                           Error : Boolean := False;
                           Current_Comp : XML_Component_Access;
                  begin
                           S.Response.Method_NS := Req.Method_NS;
                           --   remettre la requete à PolyORB
                           Args := Obj_Adapters.Get_Empty_Arg_List
                                   (Object_Adapter (ORB),
                                   Obj_Id,
                                   To_Standard_String (Method_Name));
                           List :=  List_Of (Args);
                           for I in 1 .. Get_Count (Args) loop
                              Temp_Arg :=  NV_Sequence.Element_Of
                                 (List.all, Positive (I));
                              Current_Comp := Nieme_Child (Method_Comp,
                                          Integer (I));
                              if Tag (Current_Comp) = XML_String
                                  (Temp_Arg.Name) then
                                 if False
                                   or else Temp_Arg.Arg_Modes = ARG_IN
                                   or else Temp_Arg.Arg_Modes = ARG_INOUT
                                 then
                                    XML_Component_To_Any (Current_Comp,
                                     Temp_Arg.Argument);
                                 end if;
                                 NV_Sequence.Replace_Element
                                 (List.all, Positive (I), Temp_Arg);
                              else
                                 Error := True;
                                 exit;
                              end if;
                           end loop;

                           if Error then
                              Fault_Code  := Invalid_Request;
                              Fault_RC := RC_No;
                              Fault_String := Method_Name &
                                   ":invalid Parameters";
                           else
                              declare
                                       Prof : Binding_Data.Profile_Access
                                            := new Local_Profile_Type;
                                       Target : References.Ref;
                                       R    : Request_Access;
                              begin
                                       Result :=
                                          (Name     => To_PolyORB_String
                                             ("Result"),
                                           Argument => Obj_Adapters.
                                             Get_Empty_Result
                                             (Object_Adapter (ORB),
                                              Obj_Id,
                                              To_Standard_String
                                               (Method_Name)),
                                              Arg_Modes => 0);
                                       Create_Local_Profile
                                           (Obj_Id, Local_Profile_Type
                                             (Prof.all));
                                       Create_Reference ((1 => Prof), "",
                                        Target);

                                       Create_Request
                                           (Target    => Target,
                                            Operation => To_Standard_String
                                               (Method_Name),
                                            Arg_List  => Args,
                                            Result    => Result,
                                            Req       => R);

                                       S.Pending_Req := R;

                                       Emit_No_Reply
                                           (Component_Access (ORB),
                                            Queue_Request'
                                            (Request => R,
                                            Requestor => Component_Access
                                                   (S)));
                                       return;
                              end;
                           end if;
                  end;
               else
                  Fault_Code  := Invalid_Request;
                  Fault_RC := RC_No;
                  Fault_String := Method_Name;
               end if;
            end;
         else
            Fault_Code  := Invalid_Request;
            Fault_RC := RC_No;
            Fault_String := XML_Null_String;
         end if;


      else
         Fault_Code := Version_Mismatch;
         Fault_RC := RC_No;
         Fault_String := XML_Null_String;
      end if;

      declare
         Message : Types.String;
      begin
         Set_Envelope (S.Response);
         Set_Fault (S.Response, Fault_Code, Fault_RC, Fault_String);
         Answer_Client (S500,  Types.String (To_XML_String
             (S.Response.Envelope_Field)), Message);
         S.Buffer := To_Buffer (To_Standard_String (Message));
         Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => S.Buffer));
      end;
   end Perform_Request;


   -----------------------------
   ---  Invoked on the client side
   ------------------------------

   procedure Receive_Response
     (S : access SOAP_Session)

   is
      use PolyORB.ORB;
      use PolyORB.ORB.Interface;
      use PolyORB.Objects;
      use PolyORB.Utils.HTTP;
      use PolyORB.Obj_Adapters;
      use PolyORB.References;
      use PolyORB.Components;
      use PolyORB.Filters.Interface;
      Current_Comp : XML_Component_Access;
      Method_Comp : XML_Component_Access;
      Method_Name : XML_String;
      Resp : SOAP_Message_Access renames S.Response;

   begin
      if Attributes (Resp.Envelope_Field, SOAP_XMLNS_Tag
         & Separator_Tag & SOAP_Tag) = SOAP_URI and then
          Attributes (Resp.Envelope_Field, SOAP_Tag
          & Separator_Tag & Encoding_Style_Tag)
                     = Encoding_Style_URI then
         Method_Comp  := Method (Resp.Body_Field);
         if Tag (Method_Comp) /=   SOAP_Tag &
              Separator_Tag & Fault_Tag then
            Method_Name  := Last_Name (Tag (Method_Comp));
            if Method_Comp /= null and then
                Tag (Method_Comp) = Method_Tag_Reference & Separator_Tag
                 & XML_String (S.Pending_Req.Operation) then
               if Resp.Method_NS = S.Request.Method_NS then
                  declare
                           ORB : constant ORB_Access := ORB_Access (S.Server);
                           Obj_Id : Object_Id := Object_Id (Base64_Decode
                                  (To_Standard_String (Get_Host_URL
                                  (S.HTTP_Session.Connection.all).URI)));
                  begin
                           Current_Comp := First_Element (Method_Comp);
                           S.Pending_Req.Result :=
                               (Name     => To_PolyORB_String ("Result"),
                                 Argument => Obj_Adapters.Get_Empty_Result
                                  (Object_Adapter (ORB),
                                   Obj_Id,
                                   To_Standard_String
                                     (S.Pending_Req.Operation)),
                                   Arg_Modes => Any.ARG_OUT);
                           S.Pending_Req.Result.Name  :=
                               Types.Identifier (Tag (Current_Comp));
                           XML_Component_To_Any (Current_Comp,
                             S.Pending_Req.Result.Argument);
                           Emit_No_Reply
                                (Component_Access (ORB),
                                 Queue_Request'(Request   => S.Pending_Req,
                                 Requestor => Component_Access (S)));
                  end;
               else
                  pragma Debug
                     (O ("Invalid URI in Response's Method"));
                  return;
               end if;
            else
               pragma Debug
                   (O ("Invalid Method Field in Response"));
               return;
            end if;
         else
            pragma Debug
                   (O ("Receiving a SOAP Messahe with a Fault Field "));
            return;
         end if;
      else
         pragma Debug
                (O ("Response Version Mismatch"));
         return;
      end if;

   end Receive_Response;



   -----------------------------------
   ---  PolyORB primitives
   -----------------------------------

   procedure Create
     (Proto   : access SOAP_Protocol;
      Session : out Filter_Access)
   is
      Result : constant Filter_Access
        := new SOAP_Session;
   begin
      pragma Debug (O ("creating SOAP session."));
      SOAP_Session (Result.all).Buffer  := new Buffers.Buffer_Type;
      Session := Result;
      --  something to add?
   end Create;

   -------------------
   -- Store Profile --
   -------------------

   procedure Store_Profile
     (Ses      :  access SOAP_Session;
      Profile  :  Profile_Access)
   is
   begin
      Ses.Target  := Profile;
   end Store_Profile;

   procedure Store_Connection
    (Ses      :  access SOAP_Session;
     C        :  HTTP_Connection_Access)
   is
   begin
      Ses.HTTP_Session.Connection := C;
   end Store_Connection;


   procedure Invoke_Request
     (S   : access SOAP_Session;
      R   : Requests.Request_Access)
   is
      use PolyORB.Protocols.HTTP;
      use PolyORB.Binding_Data.SOAP;
      use PolyORB.Components;
      use PolyORB.Filters.Interface;
      Mess : XML_String;
      Message : Types.String;
   begin
      if S.Role /= Client then
         raise SOAP_Error;
      end if;

      S.Pending_Req := R;
      S.Request := new SOAP_Message;

      Set_Envelope (S.Request);
      Set_Method (S.Request, R, Client);

      Mess := To_XML_String (S.Request.Envelope_Field);

      Post (S.HTTP_Session.Connection.all, Types.String (Mess),
            Get_URI (Get_URL (SOAP_Profile_Type (S.Target.all))),
            Message);

      S.Buffer := To_Buffer (To_Standard_String (Message));

      Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => S.Buffer));
   end Invoke_Request;


   procedure Abort_Request (S : access SOAP_Session;
                            R : Requests.Request_Access)
   is
   begin
      return;
   end Abort_Request;

   procedure Send_Reply
      (S : access SOAP_Session;
       R : Requests.Request_Access)
   is
      use PolyORB.Protocols.HTTP;
      use PolyORB.Components;
      use PolyORB.Filters.Interface;
      Mess : XML_String;
   begin
      if S.Role  /= Server then
         raise SOAP_Error;
      end if;

      S.Response := new SOAP_Message;

      Set_Envelope (S.Response);
      Set_Method (S.Response, R, Server);

      Mess := To_XML_String (S.Request.Envelope_Field);

      Answer_Client (S200,  Types.String (To_XML_String
             (S.Response.Envelope_Field)), Types.String (Mess));
      S.Buffer := To_Buffer (To_Standard_String (Mess));
      Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => S.Buffer));

      S.Pending_Req := null;
   end Send_Reply;


   procedure Handle_Data_Indication
      (S : access SOAP_Session)
   is
      use PolyORB.Buffers;
   begin
      pragma Assert (S.HTTP_Session /= null);

      if S.Expect_Header = True then
         pragma Debug (O ("SOAP Data_Indication: header"));
         declare
            Octets_List : constant Stream_Element_Array
              := To_Stream_Element_Array (S.Buffer);
            Header : Types.String := To_PolyORB_String ("");
            Resp : HTTP_Response_Access
              renames S.HTTP_Session.Response;
            Req  : HTTP_Request_Access
              renames S.HTTP_Session.Request;
            Succ : Boolean;
         begin
            Release_Contents (S.Buffer.all);
            for I in  Octets_List'Range loop
               Append (Header, Character'Val (Natural (Octets_List (I))));
            end loop;
            pragma Debug (O ("Received header: "
                             & To_Standard_String (Header)));

            pragma Debug (O ("My role: " & S.Role'Img));

            if S.Role = Client then
               Resp := new HTTP_Response;
               Response_Parse_Header (Header, Resp);
               if Response_Status (Resp.all) = S200
                 or else Response_Status (Resp.all) = S500
               then
                  if Response_CT (Resp.all) = Text_XML
                    and then Response_CL (Resp.all) > 0
                    and then Response_TE (Resp.all) /= "chunked"
                  then
                     Expect_Data
                       (S, S.Buffer, Stream_Element_Offset
                        (Response_CL (Resp.all)));
                     S.Expect_Header := False;
                  else
                     pragma Debug (O
                                   ("Invalid HTTP response in SOAP Context"));
                     Expect_Data (S, S.Buffer, 0);
                  end if;
               else
                  pragma Debug
                    (O ("Invalid HTTP Response in SOAP Context"));
                  Expect_Data (S, S.Buffer, 0);
               end if;

            else
               Req := new HTTP_Request;
               Request_Parse_Header (Header, S.HTTP_Session.Request, Succ);
               if Succ
                 and then Request_Mtd (Req.all) = POST
                 and then Request_Version (Req.all) = HTTP_Version
                 and then Request_CT (Req.all) = Text_XML
                 and then Request_CL (Req.all) > 0
               then
                  Expect_Data
                    (S, S.Buffer,
                     Stream_Element_Offset (Request_CL (Req.all)));
                  S.Expect_Header := False;
               else
                  pragma Debug (O ("Invalid HTTP Request in SOAP Context"));
                  null;
               end if;
            end if;

         end;
      else
         pragma Debug (O ("SOAP Data_Indication: body"));
         declare
            Octets_List : PolyORB.Opaque.Zone_Access
              := To_Stream_Element_Array (S.Buffer);
            Message : Types.String := To_PolyORB_String ("");
            XML_Comp : XML_Component_Access := new XML_Component;
         begin
            for I in  Octets_List'Range loop
               Append (Message, Character'Val
                       (Natural (Octets_List (I))));
            end loop;
            PolyORB.Opaque.Free (Octets_List);
            S.HTTP_Session.Message_Body := Message;
            XML_Parse (XML_Comp, new Stream_Char'
                       (Current_Pos => 0,
                        Chars => XML_String (Message)));
            if S.Role = Client then
               Build_SOAP_Message (XML_Comp, S.Response);
               --  handling the reponse
            else
               Build_SOAP_Message (XML_Comp, S.Request);
               Perform_Request (S);
            end if;
            Expect_Data (S, S.Buffer, 0);
         end;
      end if;
   end Handle_Data_Indication;

   procedure Handle_Connect_Indication
     (S : access SOAP_Session)
   is
   begin
      S.Role := Server;
      S.Expect_Header := True;
      S.HTTP_Session := new HTTP_Session;
      Expect_Data (S, S.Buffer, 0);
   end Handle_Connect_Indication;

   procedure Handle_Connect_Confirmation
     (S : access SOAP_Session)
   is
   begin
      S.Role := Client;
      S.Expect_Header := True;
      Expect_Data (S, S.Buffer, 0);
   end Handle_Connect_Confirmation;

   procedure Handle_Disconnect
     (S : access SOAP_Session)
   is
   begin
      Release (S.Request);
      Release (S.Response);
      Destroy_Profile (S.Target);
      Release (S.Buffer);
      Release (S.HTTP_Session);
   end Handle_Disconnect;

end PolyORB.Protocols.SOAP;
