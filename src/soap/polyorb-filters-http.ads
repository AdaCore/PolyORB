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
with Ada.Unchecked_Deallocation;

with PolyORB.Buffers;
with PolyORB.Utils.Chained_Lists;
pragma Elaborate_All (PolyORB.Utils.Chained_Lists);
with PolyORB.HTTP_Methods;
with PolyORB.ORB;
with PolyORB.Types;

package PolyORB.Filters.HTTP is

   pragma Elaborate_Body;

   type HTTP_Filter_Factory is new Factory with private;

   procedure Create
     (Fact   : access HTTP_Filter_Factory;
      Filt   : out Filter_Access);

   Protocol_Error : exception;

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

   type String_Ptr is access all Standard.String;
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Standard.String, String_Ptr);
   package String_Lists is new PolyORB.Utils.Chained_Lists (String);

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

      ----------------------------------------------------------
      -- Parameters concerning the HTTP message               --
      -- currently being processed.                           --
      -- Whenever a member is added here, its                 --
      -- initialization must be added to Clear_Message_State. --
      ----------------------------------------------------------

      Version : HTTP_Version;
      Status  : Integer;

      Request_Method : PolyORB.HTTP_Methods.Method;
      Request_URI    : String_Ptr;

      Content_Length    : Integer;
      Transfer_Encoding : String_Lists.List;
      --  Values of the corresponding HTTP headers.

      Chunked : Boolean;
      --  Applied transfer encodings, in REVERSE order
      --  (consequence: if Length (Transfer_Encoding) > 0 then
      --    First (Tranfer_Encoding) MUST have the value "chunked"
      --  (RFC 2616 3.6), and in this case Chunked is True).

      Transfer_Length : Integer;
      --  The size of the currently expected chunk of data.
      --  -1 means expect data of unspecified length;
      --  0 means that all the expected data for this message
      --  has been received, and can now be signalled to the
      --  upper layer.

      Entity : PolyORB.Types.String;
      --  The contents of the entity, as transferred by the
      --  peer, according to the encoding specified in
      --  Transfer_Encoding.
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
