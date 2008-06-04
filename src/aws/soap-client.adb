------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          S O A P . C L I E N T                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2006, Free Software Foundation, Inc.          --
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

with SOAP.Parameters;
with SOAP.Types;

with AWS.URL;

with PolyORB.Any.NVList;
with PolyORB.Any;
with PolyORB.Types;
with PolyORB.References.URI;
with PolyORB.Requests;
with PolyORB.Log;

package body SOAP.Client is

   use PolyORB.Log;
   package L is
      new PolyORB.Log.Facility_Log ("aws.soap_client");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug
   --  the polyorb logging facility

--   use Ada.Strings.Unbounded;

   function Handle_Request
     (Connection : access AWS.Client.HTTP_Connection;
      P          : SOAP.Message.Payload.Object;
      SOAPAction : String)
     return SOAP.Message.Response.Object'Class;
   --  sends the soap payload to the host specified in Connection,
   --  calling the method called SOAPAction

   function Handle_Request
     (Connection : access AWS.Client.HTTP_Connection;
      P          : SOAP.Message.Payload.Object;
      SOAPAction : String)
     return SOAP.Message.Response.Object'Class
   is
      --  we read the method name stored in P, as we are sure it is
      --  set (cf. soap.message.payload.build)

      use PolyORB.Any.NVList;
      use PolyORB.Any;
      use PolyORB.Types;
      use PolyORB.References.URI;
      use PolyORB.Requests;
      use AWS.URL;
      use AWS.Client;
      use SOAP.Message.Payload;

      Args : PolyORB.Any.NVList.Ref;
      PolyORB_Request : PolyORB.Requests.Request_Access;
      PolyORB_Response : PolyORB.Any.NamedValue;

      SOAP_Params : SOAP.Parameters.List := SOAP.Message.Parameters (P);
      Reference : constant String :=
        Protocol (Host_URL (Connection.all))
        & "://"
        & Host (Host_URL (Connection.all))
        & ":" & Port (Host_URL ((Connection.all)))
        & Path (Host_URL (Connection.all))
        & File (Host_URL (Connection.all));
   begin
      pragma Debug (O ("Handle_Request: building a request named "
                       & SOAPAction));
      pragma Debug (O ("Handle_Request: Reference is " & Reference));
      Create (Args);

      for Index in 1 .. SOAP.Parameters.Argument_Count (SOAP_Params) loop
         Add_Item (Args, SOAP.Types.To_NamedValue
                   (Parameters.Argument (SOAP_Params, Positive (Index))));
      end loop;

      Create_Request (Target => PolyORB.References.URI.String_To_Object
                      (Reference),
                      Operation => SOAPAction,
                      Arg_List => Args,
                      Result => PolyORB_Response,
                      Req => PolyORB_Request);

      Invoke (PolyORB_Request);

      pragma Debug (O ("Type of response is " & Image
                       (Get_Unwound_Type
                        (PolyORB_Request.Result.Argument))));

      declare
         use SOAP.Parameters;
         use SOAP.Message;
         SOAP_Response : SOAP.Message.Response.Object;
      begin
         Set_Parameters (SOAP_Response,
                         +(SOAP.Types.From_NamedValue
                           (PolyORB_Request.Result)));
         Set_Wrapper_Name (SOAP_Response,
                           PolyORB.Types.To_String (PolyORB_Response.Name));
         return SOAP_Response;
      end;

   end Handle_Request;

   ----------
   -- Call --
   ----------

   function Call
     (URL        : String;
      P          : SOAP.Message.Payload.Object;
      SOAPAction : String         := Not_Specified)
      return SOAP.Message.Response.Object'Class
   is
      use AWS.Client;
      use SOAP.Message.Payload;
--      procedure RPC_Call;
      --  Does the actual RPC over HTTP call.

--      Message_Body : Unbounded_String;

      --------------
      -- RPC_Call --
      --------------

--       procedure RPC_Call is
--       begin
--          if SOAPAction = Not_Specified then
--             declare
--                URL_Object : constant AWS.URL.Object := AWS.URL.Parse (URL);
--             begin
--                Response := AWS.Client.SOAP_Post
--                  (URL,
--                   To_String (Message_Body),
--                   AWS.URL.URL (URL_Object)
--                     & '#' & SOAP.Message.Payload.Procedure_Name (P));
--             end;

--          else
--             Response := AWS.Client.SOAP_Post
--               (URL,
--                To_String (Message_Body),
--                SOAPAction);
--          end if;
--       end RPC_Call;

      Connection : aliased HTTP_Connection;

   begin
--      Message_Body := SOAP.Message.XML.Image (P);
--      RPC_Call;
--     return Message.XML.Load_Response (AWS.Response.Message_Body (Response));

      if SOAPAction = Not_Specified then
         declare
            The_SOAPAction : constant String := Procedure_Name (P);

            --  If no SOAP Action was specified, we retrieve the one
            --  stored in the SOAP Object
         begin
            Create (Connection, Host => URL, SOAPAction => The_SOAPAction);
            return Handle_Request (Connection'Access, P, The_SOAPAction);
         end;
      else
         Create (Connection, Host => URL, SOAPAction => SOAPAction);
         return Handle_Request (Connection'Access, P, SOAPAction);
      end if;

   end Call;

   ----------
   -- Call --
   ----------

   function Call
     (Connection : access AWS.Client.HTTP_Connection;
      P          : SOAP.Message.Payload.Object)
      return SOAP.Message.Response.Object'Class
   is
      use SOAP.Message.Payload;

--      Message_Body : Unbounded_String;

   begin
--      Message_Body := SOAP.Message.XML.Image (P);
--    Response := AWS.Client.SOAP_Post (Connection, To_String (Message_Body));
--     return Message.XML.Load_Response (AWS.Response.Message_Body (Response));

      pragma Debug (O ("Call: processing a request named "
                        & Procedure_Name (P)));
      return Handle_Request (Connection, P, Procedure_Name (P));

   end Call;

end SOAP.Client;
