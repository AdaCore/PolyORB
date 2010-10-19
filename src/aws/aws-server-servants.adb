------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  A W S . S E R V E R . S E R V A N T S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2010, Free Software Foundation, Inc.          --
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

with Ada.Streams;

with AWS.Status;
with AWS.Status.Set;
with AWS.Parameters;
with AWS.Parameters.Set;
with AWS.Resources;
with SOAP.Types;
with SOAP.Message.Payload;
with SOAP.Parameters;

with PolyORB.Errors.Helper;
with PolyORB.Any.NVList;
with PolyORB.Requests;
with PolyORB.Objects;
with PolyORB.Obj_Adapters;
with PolyORB.ORB;
with PolyORB.Types;
with PolyORB.Setup;
with PolyORB.Log;
with PolyORB.References;
with PolyORB.Binding_Data;

package body AWS.Server.Servants is

   use PolyORB.Log;
   package L is
     new PolyORB.Log.Facility_Log ("aws.server");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   --  the polyorb logging facility

   procedure Request_Handler
     (PolyORB_Servant : access HTTP'Class;
      PolyORB_Request : PolyORB.Requests.Request_Access);
   --  handles the requests made to the servants.  Replaces protocol_handler

   ---------------------
   -- Request_Handler --
   ---------------------

   procedure Request_Handler
     (PolyORB_Servant : access AWS.Server.HTTP'Class;
      PolyORB_Request : PolyORB.Requests.Request_Access)
   is
      use PolyORB.Errors;
      use PolyORB.Errors.Helper;

      HTTP_10      : constant String := "HTTP/1.0";

      AWS_Request  : AWS.Status.Data;
      AWS_Response : AWS.Response.Data;

      Error : Error_Container;

      procedure Extract_Context;

      procedure Extract_Data;

      procedure Call_Callback;

      procedure Integrate_Context;

      procedure Integrate_Data;

      ---------------------
      -- Extract_Context --
      ---------------------

      procedure Extract_Context
      is
         use AWS.Status.Set;
      begin
         --  we do nothing for now, as contexts are not implemented
         Reset (AWS_Request);
      end Extract_Context;

      ------------------
      -- Extract_Data --
      ------------------

      procedure Extract_Data
      is
         use AWS.Parameters;
         use AWS.Parameters.Set;
         use AWS.Status;
         use AWS.Status.Set;
         use PolyORB.Types;
         use PolyORB.Any.NVList;
         use PolyORB.Any.NVList.Internals;
         use PolyORB.Any.NVList.Internals.NV_Lists;
         use PolyORB.Requests;
         use PolyORB.Objects;

         P_List : AWS.Parameters.List;
         Args : PolyORB.Any.NVList.Ref;
         Nth_Arg : PolyORB.Any.NVList.Internals.NV_Lists.Iterator;

         HTTP_Method : Request_Method;
         Number_Of_Args : Long;
         URI_Path : PolyORB.Types.String;

         The_Oid : PolyORB.Objects.Object_Id_Access;
--         The_Reference : PolyORB.References.Ref;
      begin

         pragma Debug (C, O (PolyORB.References.Image
                          (PolyORB_Request.Target)));

         declare
            Profiles : constant PolyORB.References.Profile_Array :=
              PolyORB.References.Profiles_Of (PolyORB_Request.Target);
         begin
            for Prof_Index in Profiles'Range loop
               if PolyORB.Binding_Data.Get_Object_Key
                 (Profiles (Prof_Index).all) /= null
               then
                  The_Oid := PolyORB.Binding_Data.Get_Object_Key
                    (Profiles (Prof_Index).all);
                  exit;
               end if;
            end loop;
         end;

         Obj_Adapters.Oid_To_Rel_URI
           (PolyORB.ORB.Object_Adapter (Setup.The_ORB),
            The_Oid,
            URI_Path,
            Error);

         if Found (Error) then
            Catch (Error);
            return;
         end if;

         declare
            URI : constant String := To_String (URI_Path);
         begin
            Parameters.Set.Reset (P_List);
            pragma Debug
              (C, O ("Extract_Data: structures have been initialized"));
            Create (Args);
            Arguments (PolyORB_Request, Args, Error, Can_Extend => True);

            if Found (Error) then
               return;
            end if;

            Number_Of_Args := Get_Count (Args);
            pragma Debug (C, O ("Extract_Data: parameters have been fetched"));
            pragma Debug (C, O ("Extract_Data: " & Long'Image (Number_Of_Args)
                             & " parameters"));

            if PolyORB_Servant.all in Web_Servant'Class then
               pragma Debug (C, O ("Extract_Data: got a Web request"));

               if PolyORB_Request.Operation.all = "GET" then
                  HTTP_Method := GET;
               elsif PolyORB_Request.Operation.all = "HEAD" then
                  HTTP_Method := HEAD;
               elsif PolyORB_Request.Operation.all = "POST" then
                  HTTP_Method := POST;
               elsif PolyORB_Request.Operation.all = "PUT" then
                  HTTP_Method := PUT;
               else
                  raise Program_Error;
                  --  if none of the recognized method names is
                  --  provided, then we raise an error
               end if;

               Nth_Arg := First (List_Of (Args).all);

               while not Last (Nth_Arg) loop
                  AWS.Parameters.Set.Add
                    (P_List,
                     To_Standard_String (Value (Nth_Arg).Name),
                     To_Standard_String (PolyORB.Any.From_Any
                                         (Value (Nth_Arg).Argument)));
                  Next (Nth_Arg);
               end loop;

               AWS.Status.Set.Request
                 (AWS_Request,
                  HTTP_Method,
                  URI,
                  HTTP_10);
               --  we fill in the basic parameters of the request :
               --  the method name and the URI

               AWS.Status.Set.Parameters (AWS_Request, P_List);
               --  then we set the parameter list

            elsif PolyORB_Servant.all in SOAP_Servant'Class then
               pragma Debug (C, O ("Extract_Data: got a SOAP request named "
                                & PolyORB_Request.Operation.all));

               AWS.Status.Set.Request
                 (AWS_Request,
                  POST,
                  URI,
                  HTTP_10);

               --  We fill in the basic parameters of the request:
               --  the method name and the URI.
               --
               --  SOAP relies on the POST method, according to w3c.

               declare
                  SOAP_Params : SOAP.Parameters.List;
                  SOAP_Object : SOAP.Message.Payload.Object;
               begin
                  Nth_Arg := First (List_Of (Args).all);

                  while not Last (Nth_Arg) loop
                     SOAP_Params := SOAP.Parameters."&"
                       (SOAP_Params,
                        SOAP.Types.From_NamedValue (Value (Nth_Arg).all));

                     Next (Nth_Arg);
                  end loop;
                  SOAP.Message.Set_Parameters (SOAP_Object, SOAP_Params);
                  SOAP.Message.Set_Wrapper_Name
                    (SOAP_Object, PolyORB_Request.Operation.all);
                  AWS.Status.Set.Payload (AWS_Request, SOAP_Object);
               end;

            else
               raise Program_Error;
               --  there is only two kinds of AWS servants: Web and SOAP. We
               --  are not supposed to handle anything else
            end if;

            PolyORB_Request.Arguments_Called := True;
         end;
      end Extract_Data;

      -------------------
      -- Call_Callback --
      -------------------

      procedure Call_Callback
      is
      begin
         AWS_Response := Dispatchers.Dispatch
           (PolyORB_Servant.Dispatcher.all, AWS_Request);
      end Call_Callback;

      -----------------------
      -- Integrate_Context --
      -----------------------

      procedure Integrate_Context
      is
      begin
         --  we do nothing for now, as contexts are not implemented
         null;
      end Integrate_Context;

      --------------------
      -- Integrate_Data --
      --------------------

      procedure Integrate_Data
      is
         use PolyORB.Any;
         use PolyORB.Any.NVList;
         use PolyORB.Types;
         use PolyORB.Requests;
         use AWS.Response;
      begin
         if Mode (AWS_Response) = Message then
            pragma Debug (C, O ("Integrate_Data: classical Web response"));

            Set_Result (PolyORB_Request,
                        To_Any (To_PolyORB_String
                                (Message_Body (AWS_Response))));

         elsif Mode (AWS_Response) = Header
           or else  Mode (AWS_Response) = No_Data
         then
            pragma Debug (C, O ("Integrate_Data: Header or No_Data response"));
            Set_Result (PolyORB_Request,
                        Get_Empty_Any (TC_Null));

         elsif Mode (AWS_Response) = File then
            pragma Debug (C, O ("Integrate_Data:"
                             & " byte sequence response (file)"));
            declare
               Sq_Type : constant PolyORB.Any.TypeCode.Local_Ref
                 := PolyORB.Any.TypeCode.TC_Sequence;
            begin
               PolyORB.Any.TypeCode.Add_Parameter
                 (Sq_Type,
                  PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Long (0)));

               --  We cannot guess the size of the whole stream, so we
               --  create an unbounded sequence.

               PolyORB.Any.TypeCode.Add_Parameter
                 (Sq_Type, PolyORB.Any.To_Any
                  (PolyORB.Any.TypeCode.TC_Octet));

               declare
                  use Ada.Streams;
                  Sq : PolyORB.Any.Any :=
                    PolyORB.Any.Get_Empty_Any_Aggregate
                    (Sq_Type);
                  Last : Stream_Element_Offset;
                  Buffer_Size : constant := 4 * 1_1024;
                  --  from aws.server.protocol_handler

                  File : AWS.Resources.File_Type;
                  Buffer : Stream_Element_Array (1 .. Buffer_Size);
               begin
                  AWS.Response.Create_Resource (File, AWS_Response);

                  loop
                     AWS.Resources.Read (File, Buffer, Last);
                     exit  when Last < Buffer'First;

                     for K in Buffer'First .. Last loop
                        PolyORB.Any.Add_Aggregate_Element
                          (Sq, PolyORB.Any.To_Any
                           (PolyORB.Types.Octet (Buffer (K))));
                     end loop;
                  end loop;
                  Set_Result (PolyORB_Request, Sq);
                  AWS.Resources.Close (File);
               end;
            end;

         elsif AWS.Response.Mode (AWS_Response) = SOAP_Message then
            pragma Debug (C, O ("Integrate_Data: SOAP response"));
            Set_Result (PolyORB_Request,
                        SOAP.Types.To_Any
                        (SOAP.Parameters.Argument
                         (SOAP.Message.Parameters
                          (SOAP_Message (AWS_Response)), 1)));
            --  note that we transmit only the first soap object of
            --  the parameters list, as a function is not supposed to
            --  return more than one element
         end if;

         Create (PolyORB_Request.Out_Args);
         AWS.Status.Set.Free (AWS_Request);
      end Integrate_Data;

      use PolyORB.Requests;

   begin
      pragma Debug (C, O ("Request_Handler: received a request"));
      Extract_Context;

      if Found (Error) then
         Set_Result (PolyORB_Request, Error_To_Any (Error));
         return;
      end if;

      Extract_Data;

      if Found (Error) then
         Set_Result (PolyORB_Request, Error_To_Any (Error));
         return;
      end if;

      Call_Callback;

      if Found (Error) then
         Set_Result (PolyORB_Request, Error_To_Any (Error));
         return;
      end if;

      Integrate_Context;

      if Found (Error) then
         Set_Result (PolyORB_Request, Error_To_Any (Error));
         return;
      end if;

      Integrate_Data;

      if Found (Error) then
         Set_Result (PolyORB_Request, Error_To_Any (Error));
         return;
      end if;

   end Request_Handler;

   ---------------------
   -- Execute_Servant --
   ---------------------

   function Execute_Servant
     (S   : not null access Web_Servant;
      Req : Requests.Request_Access) return Boolean
   is
      use PolyORB.Requests;
      use PolyORB.Errors;

      R : constant Request_Access := Req;
      Error : Error_Container;
   begin
      pragma Debug (C, O ("Execute_Servant: processing a Web request"));
      Request_Handler (S, R);

      pragma Debug (C, O ("Execute_Servant:"
                       & " executed, setting out args"));
      Set_Out_Args (R, Error);

      if Found (Error) then
         raise Program_Error;
      end if;

      pragma Debug (C, O ("Execute_Servant: leave"));
      return True;
   end Execute_Servant;

   function Execute_Servant
     (S   : not null access SOAP_Servant;
      Req : Requests.Request_Access) return Boolean
   is
      use PolyORB.Requests;
      use PolyORB.Errors;

      R : constant Request_Access := Req;
      Error : Error_Container;
   begin
      pragma Debug (C, O ("Execute_Servant: processing a SOAP request"));
      Request_Handler (S, R);

      pragma Debug (C, O ("Execute_Servant:"
                       & " executed, setting out args"));
      Set_Out_Args (R, Error);

      if Found (Error) then
         raise Program_Error;
      end if;

      pragma Debug (C, O ("Execute_Servant: leave"));
      return True;
   end Execute_Servant;

end AWS.Server.Servants;
