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

with SOAP.Message;
with SOAP.Message.XML;
with SOAP.Message.Payload;
with SOAP.Message.Response;

with PolyORB.Binding_Data;
with PolyORB.Binding_Data.Local;
with PolyORB.Filters.Interface;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Objects;
with PolyORB.References;
with PolyORB.Utils.Text_Buffers;

package body PolyORB.Protocols.SOAP  is

   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.ORB;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols.soap");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

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
   begin
      raise PolyORB.Not_Implemented;
   end Send_Reply;


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

            M : constant Standard.SOAP.Message.Payload.Object
              := Standard.SOAP.Message.XML.Load_Payload (Entity);
            Req : Request_Access;

            Result : Any.NamedValue;
            --  Dummy NamedValue for Create_Request; the actual Result
            --  is set by the called method.

            ORB : constant ORB_Access := ORB_Access (S.Server);

            Target_URI : constant String
              := To_Standard_String (S.Target);
            Target_Oid : Objects.Object_Id (1 .. Target_URI'Length);
            --  XXX URLdecode?????

            Target : PolyORB.References.Ref;
            Target_Profile : Binding_Data.Profile_Access
              := new Local_Profile_Type;
            --  Should be free'd when Target is finalized.

         begin
            for I in Target_URI'Range loop
               Target_Oid (Stream_Element_Offset (I))
                 := Character'Pos (Target_URI (I));
            end loop;

            Create_Local_Profile
              (Target_Oid, Local_Profile_Type (Target_Profile.all));
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

end PolyORB.Protocols.SOAP;
