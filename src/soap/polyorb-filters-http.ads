------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . F I L T E R S . H T T P                  --
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

--  HTTP protocol implementation (as a filter so higher-level protocol
--  engines can be plugged on it.)

with Ada.Streams;

with PolyORB.Buffers;
with PolyORB.HTTP_Methods;
with PolyORB.ORB;
with PolyORB.Types;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Utils.Strings;

package PolyORB.Filters.HTTP is

   type HTTP_Filter_Factory is new Factory with private;

   procedure Create
     (Fact   : access HTTP_Filter_Factory;
      Filt   : out Filter_Access);

   type HTTP_Status_Code is
     (S_Unknown,

      S_100_Continue,
      S_101_Switching_Protocols,
      S_1xx_Other_Informational,

      S_200_OK,
      S_201_Created,
      S_202_Accepted,
      S_203_Non_Authoritative_Information,
      S_204_No_Content,
      S_205_Reset_Content,
      S_206_Partial_Content,
      S_2xx_Other_Successful,

      S_300_Multiple_Choices,
      S_301_Moved_Permanently,
      S_302_Found,
      S_303_See_Other,
      S_304_Not_Modified,
      S_305_Use_Proxy,
      S_306_Unused,
      S_307_Temporary_Redirect,
      S_3xx_Other_Redirection,

      S_400_Bad_Request,
      S_401_Unauthorized,
      S_402_Payment_Required,
      S_403_Forbidden,
      S_404_Not_Found,
      S_405_Method_Not_Allowed,
      S_406_Not_Acceptable,
      S_407_Proxy_Authentication_Required,
      S_408_Request_Timeout,
      S_409_Conflict,
      S_410_Gone,
      S_411_Length_Required,
      S_412_Precondition_Failed,
      S_413_Request_Entity_Too_Large,
      S_414_Request_URI_Too_Long,
      S_415_Unsupported_Media_Type,
      S_416_Request_Range_Not_Satisfiable,
      S_417_Expectation_Failed,
      S_4xx_Other_Client_Error,

      S_500_Internal_Server_Error,
      S_501_Not_Implemented,
      S_502_Bad_Gateway,
      S_503_Service_Unavailable,
      S_504_Gateway_Timeout,
      S_505_HTTP_Version_Not_Supprted,
      S_5xx_Other_Server_Error);

   for HTTP_Status_Code'Size use Integer'Size;
   for HTTP_Status_Code use
      (S_Unknown => 0,

       S_100_Continue => 100,
       S_101_Switching_Protocols => 101,
       S_1xx_Other_Informational => 199,

       S_200_OK => 200,
       S_201_Created => 201,
       S_202_Accepted => 202,
       S_203_Non_Authoritative_Information => 203,
       S_204_No_Content => 204,
       S_205_Reset_Content => 205,
       S_206_Partial_Content => 206,
       S_2xx_Other_Successful => 299,

       S_300_Multiple_Choices => 300,
       S_301_Moved_Permanently => 301,
       S_302_Found => 302,
       S_303_See_Other => 303,
       S_304_Not_Modified => 304,
       S_305_Use_Proxy => 305,
       S_306_Unused => 306,
       S_307_Temporary_Redirect => 307,
       S_3xx_Other_Redirection => 399,

       S_400_Bad_Request => 400,
       S_401_Unauthorized => 401,
       S_402_Payment_Required => 402,
       S_403_Forbidden => 403,
       S_404_Not_Found => 404,
       S_405_Method_Not_Allowed => 405,
       S_406_Not_Acceptable => 406,
       S_407_Proxy_Authentication_Required => 407,
       S_408_Request_Timeout => 408,
       S_409_Conflict => 409,
       S_410_Gone => 410,
       S_411_Length_Required => 411,
       S_412_Precondition_Failed => 412,
       S_413_Request_Entity_Too_Large => 413,
       S_414_Request_URI_Too_Long => 414,
       S_415_Unsupported_Media_Type => 415,
       S_416_Request_Range_Not_Satisfiable => 416,
       S_417_Expectation_Failed => 417,
       S_4xx_Other_Client_Error => 499,

       S_500_Internal_Server_Error => 500,
       S_501_Not_Implemented => 501,
       S_502_Bad_Gateway => 502,
       S_503_Service_Unavailable => 503,
       S_504_Gateway_Timeout => 504,
       S_505_HTTP_Version_Not_Supprted => 505,
       S_5xx_Other_Server_Error => 599);

   subtype Informational_Status_Code is HTTP_Status_Code
     range S_100_Continue .. S_1xx_Other_Informational;
   subtype Successful_Status_Code is HTTP_Status_Code
     range S_200_OK .. S_2xx_Other_Successful;
   subtype Redirection_Status_Code is HTTP_Status_Code
     range S_300_Multiple_Choices .. S_3xx_Other_Redirection;
   subtype Client_Error_Status_Code is HTTP_Status_Code
     range S_400_Bad_Request .. S_4xx_Other_Client_Error;
   subtype Server_Error_Status_Code is HTTP_Status_Code
     range S_500_Internal_Server_Error .. S_5xx_Other_Server_Error;

private

   type HTTP_Version is record
     Major : Natural;
     Minor : Natural;
   end record;

   -------------------------------------
   -- Terminals of the message syntax --
   -------------------------------------

   CRLF             : constant String := ASCII.CR & ASCII.LF;

   HTTP_Slash       : constant String := "HTTP/";
   --  in HTTP-Version

   Encoding_Chunked  : constant String := "chunked";
   Encoding_Identity : constant String := "identity";
   --  in transfer-coding

   Default_HTTP_Version : constant HTTP_Version
     := (Major => 1, Minor => 1);

   -----------
   -- Types --
   -----------

   type HTTP_Filter_Factory is new Factory with null record;

   type HTTP_State is
     (Start_Line,
      Header,
      Chunk_Size,
      Entity,
      Trailer);
   --  An HTTP session is either expecting a (request or response)
   --  message start line, a generic message header, the start of
   --  a data chunk (in chunked transfer encoding), entity data
   --  or a trailer of entity-headers following the last chunk
   --  of chunked data.

   subtype Line_By_Line is HTTP_State range Start_Line .. Chunk_Size;
   --  In these states, message data is processed by entire lines
   --  terminated by CRLF.

   function Image (V : HTTP_Version) return String;

   package String_Lists renames PolyORB.Utils.Strings.Lists;

   type HTTP_Filter is new Filter with record
      Role : PolyORB.ORB.Endpoint_Role;
      --  The role associated with this protocol engine.

      State  : HTTP_State;
      --  Current state of the HTTP session.

      CR_Seen : Boolean := False;
      --  In Start_Line or Header state, True iff the last character
      --  seen is a CR.

      In_Buf : PolyORB.Buffers.Buffer_Access;
      Data_Received : Ada.Streams.Stream_Element_Count;
      --  Data received in In_Buf and not processed yet
      --  (reset when changing states).

      Message_Buf : PolyORB.Buffers.Buffer_Access;
      --  This buffer is used for communication of complete
      --  received message bodies to the upper layer.

      Out_Buf : PolyORB.Buffers.Buffer_Access;
      --  Buffer used to prepare outbound messages.
      --  XXX could this not be the same as In_Buf?

      ----------------------------------------------------------
      -- Parameters concerning the HTTP message               --
      -- currently being processed.                           --
      -- Whenever a member is added here, its                 --
      -- initialization must be added to Clear_Message_State. --
      ----------------------------------------------------------

      Version : HTTP_Version;
      Status  : HTTP_Status_Code;

      Request_Method : PolyORB.HTTP_Methods.Method;
      Request_URI    : Utils.Strings.String_Ptr;

      Content_Length    : Ada.Streams.Stream_Element_Offset;
      Transfer_Encoding : String_Lists.List;
      --  Values of the corresponding HTTP headers.

      Chunked : Boolean;
      --  Applied transfer encodings, in REVERSE order
      --  (consequence: if Length (Transfer_Encoding) > 0 then
      --    First (Tranfer_Encoding) MUST have the value "chunked"
      --  (RFC 2616 3.6), and in this case Chunked is True).

      Transfer_Length : Ada.Streams.Stream_Element_Offset;
      --  The size of the currently expected chunk of data.
      --  -1 means expect data of unspecified length;
      --  0 means that all the expected data for this message
      --  has been received, and can now be signalled to the
      --  upper layer.

      Entity : PolyORB.Types.String;
      --  The contents of the entity, as transferred by the
      --  peer, according to the encoding specified in
      --  Transfer_Encoding.

      SOAP_Action : PolyORB.Types.String;
      --  The contents of a received SOAPAction HTTP header
      --  (server-side only, optional).
   end record;

   procedure Clear_Message_State (F : in out HTTP_Filter);
   --  Reset all message state members in F to their
   --  initialization values.

   procedure Initialize (F : in out HTTP_Filter);
   procedure Finalize (F : in out HTTP_Filter);

   function Handle_Message
     (F : access HTTP_Filter;
      S : Components.Message'Class)
     return Components.Message'Class;

end PolyORB.Filters.HTTP;
