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

with AWS.Response;

with SOAP.Message;
with SOAP.Message.XML;
with SOAP.Message.Payload;
with SOAP.Message.Response;
with SOAP.Parameters;

with PolyORB.Annotations;
with PolyORB.Binding_Data;
with PolyORB.Binding_Data.Local;
with PolyORB.Filters.Interface;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Objects;
with PolyORB.References;
with PolyORB.Utils.Text_Buffers;

package body PolyORB.Protocols.SOAP_Pr is

   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.ORB;
   use Standard.SOAP;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols.soap_pr");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   type Request_Note is new PolyORB.Annotations.Note with record
      SOAP_Req : SOAP.Message.Payload.Object;
   end record;

   procedure Create
     (Proto   : access SOAP_Protocol;
      Session : out Filter_Access)
   is
      Result : constant Filter_Access
        := new SOAP_Session;
   begin
      SOAP_Session (Result.all).In_Buf
        := new PolyORB.Buffers.Buffer_Type;
      Session := Result;
   end Create;

   procedure Invoke_Request
     (S   : access SOAP_Session;
      R   : Requests.Request_Access)
   is
   begin
      raise PolyORB.Not_Implemented;
   end Invoke_Request;

   procedure Abort_Request
     (S : access SOAP_Session;
      R : Requests.Request_Access)
   is
   begin
      raise PolyORB.Not_Implemented;
   end Abort_Request;

   procedure Send_Reply
      (S : access SOAP_Session;
       R : Requests.Request_Access)
   is
      N : Request_Note;
   begin
      Annotations.Get_Note (R.Notepad, N);
      declare
         use PolyORB.Any;
         use PolyORB.Any.NVList;
         use PolyORB.Any.NVList.Internals;
         use PolyORB.Any.NVList.Internals.NV_Sequence;

         use SOAP.Parameters;

         RO : SOAP.Message.Response.Object
           := SOAP.Message.Response.From (N.SOAP_Req);
         RP : SOAP.Parameters.List;

         Args : constant NV_Sequence_Access
           := List_Of (R.Args);
         A : Any.NamedValue;
      begin
         RP := +R.Result;
         for I in 1 .. Get_Count (R.Args) loop
            A :=  NV_Sequence.Element_Of (Args.all, Positive (I));
            if False
              or else A.Arg_Modes = ARG_INOUT
              or else A.Arg_Modes = ARG_OUT
            then
               RP := RP & A;
            end if;
         end loop;

         SOAP.Message.Set_Parameters (RO, RP);
         declare
            RD : AWS.Response.Data
              := SOAP.Message.Response.Build (RO);
            --  XXX possible abstraction violation detected!
            --  Here we construct an /AWS/ response object.
            --  Does this mean we assume that this SOAP engine
            --  is bound to an HTTP engine? (esp. considering that
            --  AWS.Response.Data contains an /HTTP/ status code. :( ).
            --  (but that also means that AWS suffers from the same
            --  abstraction violation).
            pragma Warnings (Off, RD);
            --  XXX not referenced yet
         begin
            --  XXX send out RD.
            raise Not_Implemented;
         end;
      end;
   end Send_Reply;

   function URI_To_Oid (URI : PolyORB.Types.String)
     return Objects.Object_Id;

   function URI_To_Oid (URI : PolyORB.Types.String)
     return Objects.Object_Id
   is
      S : constant String := PolyORB.Types.To_Standard_String (URI);
   begin
      return PolyORB.Objects.To_Oid (S (S'First + 1 .. S'Last));
      --  For now use /<hexdigits> as URI.
   end URI_To_Oid;

   procedure Handle_Data_Indication
     (S : access SOAP_Session;
      Data_Amount : Ada.Streams.Stream_Element_Count)
   is
      Entity : String (1 .. Integer (Data_Amount));
   begin
      PolyORB.Utils.Text_Buffers.Unmarshall_String
        (S.In_Buf, Entity);
      pragma Debug (O ("SOAP entity received: " & Entity));
      if S.Role = Server then
         declare
            use Ada.Streams;
            use PolyORB.Binding_Data.Local;
            use PolyORB.Types;

            M : constant Message.Payload.Object
              := Message.XML.Load_Payload (Entity);
            Req : Request_Access;

            Result : Any.NamedValue;
            --  Dummy NamedValue for Create_Request; the actual Result
            --  is set by the called method.

            ORB : constant ORB_Access := ORB_Access (S.Server);

            Target : PolyORB.References.Ref;
            Target_Profile : Binding_Data.Profile_Access
              := new Local_Profile_Type;
            --  Should be free'd when Target is finalized.

         begin
            Create_Local_Profile
              (URI_To_Oid (S.Target), Local_Profile_Type (Target_Profile.all));
            PolyORB.References.Create_Reference
              ((1 => Target_Profile),
               Type_Id => "", R => Target);
            --  Create a temporary, typeless reference for this object.

            Create_Request
              (Target    => Target,
               Operation => Standard.SOAP.Message.Payload.Procedure_Name (M),
               Arg_List  => PolyORB.Any.NVList.Ref
               (Standard.SOAP.Message.Parameters (M)),
               Result    => Result,
               Deferred_Arguments_Session => null,
               Req       => Req);
            S.Target := PolyORB.Types.To_PolyORB_String ("");

            Annotations.Set_Note
              (Req.Notepad,
               Request_Note'(Annotations.Note
                             with SOAP_Req => M));

            PolyORB.ORB.Queue_Request_To_Handler
              (ORB.Tasking_Policy, ORB,
               PolyORB.ORB.Interface.Queue_Request'
               (Request => Req,
                Requestor => PolyORB.Components.Component_Access (S)));
         end;
      else
         declare
            M : constant Standard.SOAP.Message.Response.Object'Class
              := Standard.SOAP.Message.XML.Load_Response (Entity);
         begin
            pragma Warnings (Off, M);
            --  XXX not referenced.
            --  Process_Reply (To_Reply (M));
            raise Not_Implemented;
         end;
      end if;
   end Handle_Data_Indication;

   procedure Handle_Connect_Indication
     (S : access SOAP_Session)
   is
   begin
      S.Role := Server;
      Expect_Data (S, S.In_Buf, 0);
   end Handle_Connect_Indication;

   procedure Handle_Connect_Confirmation
     (S : access SOAP_Session)
   is
   begin
      S.Role := Client;
   end Handle_Connect_Confirmation;

   procedure Handle_Disconnect
     (S : access SOAP_Session)
   is
   begin
      raise PolyORB.Not_Implemented;
   end Handle_Disconnect;

   function Handle_Message
     (Sess : access SOAP_Session;
      S : Components.Message'Class)
     return Components.Message'Class
   is
      Result : Components.Null_Message;
   begin
      if S in Set_Target_Object then
         Sess.Target := Set_Target_Object (S).Target;
         return Result;
      else
         return PolyORB.Protocols.Handle_Message
           (PolyORB.Protocols.Session (Sess.all)'Access, S);
      end if;
   end Handle_Message;

end PolyORB.Protocols.SOAP_Pr;
