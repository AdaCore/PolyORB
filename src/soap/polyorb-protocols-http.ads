------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . P R O T O C O L S . H T T P                --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Streams; use Ada.Streams;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with PolyORB.Types; use PolyORB.Types;
--  with PolyORB.Sockets;
with PolyORB.Buffers; use PolyORB.Buffers;
--  with PolyORB.Transport; use PolyORB.Transport;

with PolyORB.Utils.HTTP_Messages;

--  with Avl_Tree_Generic;

package PolyORB.Protocols.HTTP is

   type Key_Value_Data is record
      Key, Value : Types.String;
   end record;

   function Key_For (Node : in Key_Value_Data) return String;

   --  package Tree is new Avl_Tree_Generic (String, Key_Value_Data, Key_For);

   --  type Set is new Tree.Avl_Tree;

   type Data_Mode is (Header, Message, File);

   type Stream_Element_Array_Access is access Stream_Element_Array;

   Default_Port : constant Positive := 80;

   type URL_Object is record
      Server_Name : Types.String;
      Port        : Positive := Default_Port;
      Security    : Boolean := False;
      URI         : Types.String;
   end record;

   Retry_Default : constant Positive;

   No_Data  : constant String := " ";

   Null_Str : constant Types.String := To_PolyORB_String (" ");

   HTTP_Version   : constant String := "HTTP/1.1";

   Text_XML         :  constant String := "text/html";

   type Request_Method is (GET, HEAD, POST, PUT);
   type Request_Range is (POST, NON_POST, EMPTY);


   type HTTP_Connection is private;
   type HTTP_Connection_Access is access all HTTP_Connection;

   type HTTP_Response is private;
   type HTTP_Response_Access is access all HTTP_Response;

   type HTTP_Request is private;
   type HTTP_Request_Access is access all HTTP_Request;

   HTTP_Error : exception;
   URL_Error : exception;
   Internal_Error : exception;
   Connection_Error : exception;


   --  function Is_Match (Str, Pattern : in String) return Boolean;

   function Image (N : Natural) return String;

   function To_Buffer (S : String)
      return Buffer_Access;

   function URL_Parse (URL : in String) return URL_Object;

   function Get_URI
      (URL : URL_Object)
     return Types.String;

   function Create_Connection
     (Host       : in String;
      User       : in String   := No_Data;
      Pwd        : in String   := No_Data;
      Proxy      : in String   := No_Data;
      Proxy_User : in String   := No_Data;
      Proxy_Pwd  : in String   := No_Data;
      Retry      : in Positive := Retry_Default;
      Persistent : in Boolean  := True)
     return HTTP_Connection;


   procedure Post
     (Connection : in out HTTP_Connection;
      Data       : in  Types.String;
      URI        : in  Types.String := Null_Str;
      Message    : out Types.String);

   procedure Answer_Client
    (Status   : Utils.HTTP_Messages.Status_Code;
     Data     : Types.String;
     Message  : out Types.String);





   procedure Response_Parse_Header
     (Mess  : in  Types.String;
      Param : HTTP_Response_Access);

   procedure Request_Parse_Header
     (Mess    : in Types.String;
      Status  : HTTP_Request_Access;
      Success : out Boolean);

   procedure Request_Parse_Line
      (Command : String;
       Status  : HTTP_Request_Access;
       Success : out Boolean);

   function Get_Host_URL
      (C : HTTP_Connection)
     return URL_Object;



   function Response_Status (Resp : HTTP_Response)
      return Utils.HTTP_Messages.Status_Code;

   function Response_Body (Resp : HTTP_Response)
     return Types.String;

   function Response_CT (Resp : HTTP_Response)
     return Types.String;

   function Response_TE (Resp : HTTP_Response)
     return Types.String;

   --------------------------------

   function Request_Mtd (Req : HTTP_Request)
     return Request_Method;

   function Request_Version (Req : HTTP_Request)
     return Types.String;

   function Request_CT (Req : HTTP_Request)
     return Types.String;

   function Request_URI (Req : HTTP_Request)
     return Types.String;

private

   type HTTP_Connection is record
      Connect_URL : URL_Object;
      Host        : Types.String;
      Host_URL    : URL_Object;
      User        : Types.String;
      Pwd         : Types.String;
      Proxy       : Types.String;
      Proxy_URL   : URL_Object;
      Proxy_User  : Types.String;
      Proxy_Pwd   : Types.String;
      Opened      : Boolean;
      Persistent  : Boolean;
      Retry       : Positive;
   end record;

   type HTTP_Request is record
      Connection        : Types.String;
      Host              : Types.String;
      Peername          : Types.String;
      Method            : Request_Method     := GET;
      URI               : Types.String;
      Message_Body      : Types.String;
      HTTP_Version      : Types.String;
      Content_Type      : Types.String;
      Boundary          : Types.String;
      Content_Length    : Natural            := 0;
      If_Modified_Since : Types.String;
      File_Up_To_Date   : Boolean            := False;
      Auth_Name         : Types.String;
      Auth_Password     : Types.String;
      Session_ID        : Types.String;
   end record;

   type HTTP_Response is record
      Mode              : Data_Mode;
      Code              : Utils.HTTP_Messages.Status_Code;
      Content_Length    : Natural;
      Content_Type      : Types.String := Null_Str;
      Message_Body      : Types.String := Null_Str;
      Transfer_Encoding : Types.String := Null_Str;
      Location          : Types.String := Null_Str;
      Connection        : Types.String := Null_Str;
      Realm             : Types.String := Null_Str;
      Elements          : Stream_Element_Array_Access;
   end record;

   Retry_Default : constant Positive  := 1;

   CRLF : constant String := CR & LF;

end  PolyORB.Protocols.HTTP;
