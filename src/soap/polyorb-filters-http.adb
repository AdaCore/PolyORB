------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . F I L T E R S . H T T P                  --
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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Unchecked_Conversion;

with AWS.MIME;
with AWS.Response;

with PolyORB.Components;
with PolyORB.Filters.AWS_Interface;
with PolyORB.Filters.Interface;
with PolyORB.HTTP_Headers;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Opaque;
with PolyORB.Protocols;
--  For exception Protocol_Error.
with PolyORB.Utils;
with PolyORB.Utils.Text_Buffers;

package body PolyORB.Filters.HTTP is

   use Ada.Streams;

   use PolyORB.Filters.AWS_Interface;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.ORB;
   use PolyORB.Utils;

   use String_Lists;

   package L is new PolyORB.Log.Facility_Log ("polyorb.filters.http");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Protocol_Error : exception renames Protocols.Protocol_Error;

   -----------------------------------------
   -- Declaration of internal subprograms --
   -----------------------------------------

   procedure Handle_Data_Indication
     (F : access HTTP_Filter;
      S : Filters.Interface.Data_Indication);
   --  Process a Data_Indication message from lower layers.

   procedure Process_Line
     (F : access HTTP_Filter;
      Line_Length : Stream_Element_Count);
   --  Process one start-line or header line in an HTTP message,
   --  starting at the current input buffer position of F,
   --  and spanning Line_Length characters.

   procedure Message_Complete (F : access HTTP_Filter);
   --  A message has been completely received and processed by F:
   --  send the upper layer a Data_Indication.

   --------------------------------------
   -- Preparation of outgoing messages --
   --------------------------------------

   function Image (I : Integer) return String
     renames PolyORB.Utils.Trimmed_Image;

   procedure Prepare_Request
     (F  : access HTTP_Filter;
      RO : PolyORB.Filters.AWS_Interface.AWS_Request_Out);

   procedure Prepare_Header_Only
     (F  : access HTTP_Filter;
      RD : AWS.Response.Data);

   procedure Prepare_General_Header
     (F  : access HTTP_Filter;
      RD : AWS.Response.Data);

   procedure Prepare_Message
     (F  : access HTTP_Filter;
      RD : AWS.Response.Data);

   procedure Error
     (F      : access HTTP_Filter;
      Status : HTTP_Status_Code);
   --  Send an error message to F.
   --  XXX should a message body be included with the error status?

   --  General filter management

   procedure Create
     (Fact   : access HTTP_Filter_Factory;
      Filt   : out Filter_Access)
   is
      Res : constant Filter_Access := new HTTP_Filter;
   begin
      Filt := Res;
   end Create;

   procedure Clear_Message_State (F : in out HTTP_Filter) is
   begin
      F.Version := Default_HTTP_Version;
      F.Status  := S_Unknown;

      F.Request_Method := PolyORB.HTTP_Methods.Extension_Method;
      Utils.Strings.Free (F.Request_URI);

      F.Content_Length := -1;
      Deallocate (F.Transfer_Encoding);
      F.Chunked := False;
      F.Transfer_Length := -1;
      F.Entity := PolyORB.Types.To_PolyORB_String ("");
   end Clear_Message_State;

   procedure Initialize (F : in out HTTP_Filter) is
   begin
      Clear_Message_State (F);
   end Initialize;

   procedure Finalize (F : in out HTTP_Filter) is
   begin
      Clear_Message_State (F);
      Finalize (Filter (F));
   end Finalize;

   Buffer_Size : constant := 1024;

   --  Main filter message processing

   function Handle_Message
     (F : access HTTP_Filter;
      S : Components.Message'Class)
     return Components.Message'Class
   is
      use PolyORB.Buffers;

      Res : Components.Null_Message;
   begin
      if False
        or else S in Connect_Indication
        or else S in Connect_Confirmation
      then
         if S in Connect_Indication then
            F.Role := Server;
         else
            F.Role := Client;
         end if;

         F.State := Start_Line;
         F.Data_Received := 0;
         F.In_Buf := new PolyORB.Buffers.Buffer_Type;
         F.Out_Buf := new PolyORB.Buffers.Buffer_Type;
         --  HTTP has its own buffer for protocol stuff;
         --  the upper layer provides another buffer for
         --  message payload.

         Expect_Data (F, F.In_Buf, Buffer_Size);
         --  Wait for first line of message.

         Emit_No_Reply (F.Upper, S);
      elsif S in Data_Expected then
         F.Message_Buf := Data_Expected (S).In_Buf;

      elsif S in Data_Indication then
         Handle_Data_Indication (F, Data_Indication (S));

      elsif S in AWS_Request_Out then
         Release_Contents (F.Out_Buf.all);
         Prepare_Request (F, AWS_Request_Out (S));
         Emit_No_Reply (Lower (F), Data_Out'(Out_Buf => F.Out_Buf));

      elsif S in AWS_Response_Out then

         declare
            RD : constant AWS.Response.Data
              := AWS_Response_Out (S).Data;
         begin
            Release_Contents (F.Out_Buf.all);
            case AWS.Response.Mode (RD) is
               when AWS.Response.Header =>
                  Prepare_Header_Only (F, RD);
               when AWS.Response.Message =>
                  Prepare_Message (F, RD);
            end case;
         end;
         Emit_No_Reply (Lower (F), Data_Out'(Out_Buf => F.Out_Buf));

      elsif S in Set_Server then
         Emit_No_Reply (F.Upper, S);
      elsif S in Disconnect_Request then
         return Emit (F.Lower, S);
      else
         raise PolyORB.Components.Unhandled_Message;
      end if;

      return Res;
   end Handle_Message;

   -----------------------------
   -- HTTP message processing --
   -----------------------------

   function Is_LWS (C : Character) return Boolean;
   pragma Inline (Is_LWS);
   --  True iff C is a linear whitespace character (space or tab).

   procedure Skip_LWS (S : String; First : in out Integer);
   --  Increment First until it points to a non-LWS position in S
   --  or equals S'Last + 1.

   procedure Trim_LWS (S : String; Last : in out Integer);
   --  Decrement Last until it points to a non-LWS position in S
   --  or equals S'First - 1.

   procedure Parse_CSL_Item
     (S     : String;
      Pos   : in out Integer;
      First : out Integer;
      Last  : out Integer);
   --  Get one item from a comma-separated list, starting at Pos.
   --  On return, First and Last are the indices of the start and
   --  end of the parsed token (possibly empty), and Pos is
   --  the position at which parsing should proceed for the next
   --  token. If Pos > S'Last on return then the string has been
   --  completely parsed.

   function Parse_Hex (S : String) return Natural;
   --  chunk-size ::= 1*HEX

   function Parse_HTTP_Version (S : String) return HTTP_Version;
   --  HTTP-Version

   function To_HTTP_Status_Code (Status : Integer) return HTTP_Status_Code;
   --  HTTP status codes.
   --  Integers corresponding to known codes are translated to the
   --  corresponding enum value. Integers corresponding to unknown
   --  codes in a valid class are translated to the 'other' code for
   --  that class. For other values of Status, Constraint_Error is raised.

   procedure Parse_Request_Line (F : access HTTP_Filter; S : String);
   --  Request-Line

   procedure Parse_Status_Line (F : access HTTP_Filter; S : String);
   --  Status-Line

   procedure Parse_Header_Line (F : access HTTP_Filter; S : String);
   --  {general,request,response,entity}-header

   procedure Parse_Chunk_Size (F : access HTTP_Filter; S : String);
   --  chunk-size

   ---------------------
   -- Implementations --
   ---------------------

   procedure Handle_Data_Indication
     (F : access HTTP_Filter;
      S : Filters.Interface.Data_Indication)
   is
      use PolyORB.Buffers;

      Data_Received : Stream_Element_Count
        := Stream_Element_Count (S.Data_Amount);

      New_Data : PolyORB.Opaque.Opaque_Pointer;
      New_Data_Position : Stream_Element_Offset
        := Length (F.In_Buf) - Data_Received;
   begin

      ---------------------------
      -- Process received data --
      ---------------------------

      <<Process_Received_Data>>

      if F.State in Line_By_Line then

         ------------------------------------------
         -- Processing data in line-by-line mode --
         -- (header or chunk size line).         --
         ------------------------------------------

         Extract_Data
           (F.In_Buf, New_Data, Data_Received,
            Use_Current => False,
            At_Position => New_Data_Position);
         --  Peek at the newly-received data.

         Scan_Line :
         for I in New_Data.Offset
           .. New_Data.Offset + Data_Received - 1
         loop
            case New_Data.Zone (I) is
               when Character'Pos (ASCII.CR) =>
                  if F.CR_Seen then
                     raise Protocol_Error;
                     --  Two consecutive CRs.
                  end if;

                  F.CR_Seen := True;

               when Character'Pos (ASCII.LF) =>
                  if not F.CR_Seen then
                     raise Protocol_Error;
                     --  LF not preceded with CR.
                  end if;
                  F.CR_Seen := False;

                  begin
                     Process_Line
                       (F, Line_Length =>
                          New_Data_Position - CDR_Position (F.In_Buf)
                        + I - New_Data.Offset + 1);
                  exception
                     when E : others =>
                        O ("Received exception in "
                           & F.State'Img & " state:", Error);
                        O (Ada.Exceptions.Exception_Information (E),
                           Error);
                        Clear_Message_State (F.all);
                        if F.Role = Server then
                           Error (F, S_400_Bad_Request);
                        else
                           --  XXX what to do on client side?
                           raise;
                        end if;
                        --  XXX close???
                  end;

                  --  Calculation of the length of the current line:
                  --  New_Data_Position - CDR_Position = amount of data
                  --    received  but not yet processed
                  --    (the beginning of this line)
                  --  I - I'First + 1 = amount of data now appended
                  --    (the end of this line).

                  New_Data_Position := CDR_Position (F.In_Buf);
                  Data_Received := Length (F.In_Buf) - New_Data_Position;
                  if Data_Received > 0 then
                     pragma Debug (O ("Restarting HTTP processing"));
                     pragma Debug
                       (O ("Transfer length:" & F.Transfer_Length'Img));
                     pragma Debug
                       (O ("Pending data:" & Data_Received'Img));
                     goto Process_Received_Data;
                  end if;
                  --  Update state, and restart data processing if
                  --  necessary (Process_Line may have changed F.State,
                  --  so we cannot simply continue running Scan_Line).

               when others =>
                  F.CR_Seen := False;
            end case;
         end loop Scan_Line;

         if CDR_Position (F.In_Buf) = Length (F.In_Buf) then
            --  All data currently in F.In_Buf has been processed,
            --  so release it (NB: in the HTTP filter, the initial
            --  position in the buffer is always 0.)
            Release_Contents (F.In_Buf.all);
         end if;

      else

         ------------------------------------------------------
         -- Not in a line-by-line state: transferring entity --
         ------------------------------------------------------

         declare
            use PolyORB.Types;

            Data : PolyORB.Opaque.Opaque_Pointer;
            Data_Processed : Stream_Element_Count := Data_Received;
         begin
            if Data_Processed > F.Transfer_Length then
               Data_Processed := F.Transfer_Length;
            end if;

            PolyORB.Buffers.Extract_Data
              (F.In_Buf, Data, Data_Processed, Use_Current => True);

            declare
               S : String (1 .. Integer (Data_Processed));
               --  XXX BAD BAD do not allocate that on the stack!
            begin
               for I in S'Range loop
                  S (I) := Character'Val
                    (Data.Zone
                     (Data.Offset
                      + Stream_Element_Offset (I - S'First)));
               end loop;

               Append (F.Entity, S);
            end;

            F.Transfer_Length := F.Transfer_Length - Data_Processed;

            if F.Transfer_Length = 0 then
               if F.Chunked then
                  --  Got a complete chunk
                  declare
                     L : constant Integer := Length (F.Entity);
                  begin
                     if Slice (F.Entity, L - 1, L) /= CRLF then
                        raise Protocol_Error;
                        --  XXX chunk data not terminated by CRLF;
                     end if;

                     Delete (F.Entity, L - 1, L);
                  end;

                  --  End of chunk data, wait for next.
                  F.State := Chunk_Size;
                  F.Transfer_Length := -1;
               else
                  Message_Complete (F);
               end if;
            end if;

            New_Data_Position := CDR_Position (F.In_Buf);
            Data_Received := Data_Received - Data_Processed;
            if Data_Received > 0 then
               pragma Debug (O ("Restarting HTTP processing"));
               goto Process_Received_Data;
            end if;
         end;
      end if;

      ----------------------------------------
      -- Prepare for further data reception --
      ----------------------------------------

      --  At this point, all the received data have
      --  been processed. Further data must now be Expected
      --  according to the current state information (which
      --  may have been modified by the above processing.

      case F.Transfer_Length is
         when -1 =>
            --  Either state is Start_Line, Header, Chunk_Size
            --  or (state is entity
            --      and transfer length is unknown).
            Expect_Data (F, F.In_Buf, Buffer_Size);

         when 0 =>
            --  All expected data was received, upper layer
            --  has been notified (see calls to Message_Complete
            --  above).
            null;

         when others =>
            Expect_Data
              (F, F.In_Buf, F.Transfer_Length);

      end case;
   end Handle_Data_Indication;

   procedure Process_Line
     (F : access HTTP_Filter;
      Line_Length : Stream_Element_Count)
   is
      Data : PolyORB.Opaque.Opaque_Pointer;
      S : String (1 .. Integer (Line_Length) - 2);
      --  Ignore last 2 characters (CR/LF).
   begin
      pragma Debug
        (O ("Processing line at position "
            & Stream_Element_Offset'Image
            (PolyORB.Buffers.CDR_Position (F.In_Buf))));
      PolyORB.Buffers.Extract_Data
        (F.In_Buf, Data, Line_Length, Use_Current => True);
      for I in S'Range loop
         S (I) := Character'Val
           (Data.Zone
            (Data.Offset + Stream_Element_Offset (I - S'First)));
      end loop;

      pragma Debug (O ("HTTP line received: " & S));

      case F.State is
         when Start_Line =>
            if F.Role = Server then
               Parse_Request_Line (F, S);
            else
               Parse_Status_Line (F, S);
            end if;

         when Chunk_Size =>
            Parse_Chunk_Size (F, S);

         when Header | Trailer =>
            if S'Length > 2 then
               Parse_Header_Line (F, S);
            else
               --  End of headers (an empty line).
               pragma Debug (O ("Headers complete."));

               --  Determine the message body transfer length
               --  (RFC 2616 4.4)

               --  if Is_Response_Without_Body (F) then
               --  XXX implement predicate Is_Resp_WO_Body
               if False then
                  F.Transfer_Length := 0;
                  --  Response received complete, does not (and
                  --  MUST not) contain a body.

                  --  XXX now we must send Data_Indication to
                  --  the upper layer, and switch to Start_Line
                  --  state.

               elsif Length (F.Transfer_Encoding) > 0 then
                  if Value (First (F.Transfer_Encoding)).all
                    = Encoding_Chunked
                  then
                     F.Chunked := True;
                     F.State := Chunk_Size;
                  end if;
               elsif F.Content_Length > 0 then
                  F.Transfer_Length := F.Content_Length;
                  --  Expect content-length octets, NO trailing CRLF.
                  F.State := Entity;
--             elsif Media-Type is multipart/byteranges
--                ... use that to determine the transfer-length
               else
                  if F.Role = Server then
                     --  XXX 400 Bad request: the client cannot
                     --  indicate the transfer length by closing
                     --  the connection at the end of the message,
                     --  because then there would be no channel
                     --  for sending a response.
                     raise Protocol_Error;
                  end if;

                  --  We are on the client side, and the
                  --  transfer-length will be indicated by the
                  --  server closing the connection.
                  F.Transfer_Length := -1;
                  F.State := Entity;
               end if;
            end if;

         when others =>
            raise Program_Error;
      end case;

   end Process_Line;

   --  Linear white space

   function Is_LWS (C : Character) return Boolean is
   begin
      return C = ' ' or else C = ASCII.HT;
   end Is_LWS;

   procedure Skip_LWS (S : String; First : in out Integer) is
   begin
      while First in S'Range and then Is_LWS (S (First)) loop
         First := First + 1;
      end loop;
   end Skip_LWS;

   procedure Trim_LWS (S : String; Last : in out Integer) is
   begin
      while Last in S'Range and then Is_LWS (S (Last)) loop
         Last := Last - 1;
      end loop;
   end Trim_LWS;

   procedure Parse_CSL_Item
     (S     : String;
      Pos   : in out Integer;
      First : out Integer;
      Last  : out Integer)
   is
      Item_First : Integer := Pos;
      Item_Last : Integer;
      --  Start and end of current item.

      Separator : Integer;
      --  Position of separator after current token.

   begin
      pragma Assert (Pos in S'Range);

      --  Skip initial linear white space

      while Item_First <= S'Last and then Is_LWS (S (Item_First))
      loop
         Item_First := Item_First + 1;
      end loop;

      First := Item_First;

      if Item_First > S'Last then
         --  There was only LWS from Pos to the end of the string.
         Pos := S'Last + 1;
         return;
      end if;

      Separator := Item_First;
      loop
         exit when Separator > S'Last or else S (Separator) = ',';
         Separator := Separator + 1;
      end loop;
      Item_Last := Separator - 1;
      Pos := Separator + 1;

      while Item_Last > Item_First and then Is_LWS (S (Item_Last))
      loop
         Item_Last := Item_Last - 1;
      end loop;
      Last := Item_Last;
      Pos := Separator + 1;
   end Parse_CSL_Item;

   function Image (V : HTTP_Version) return String is
   begin
      return HTTP_Slash & Image (V.Major) & "." & Image (V.Minor);
   end Image;

   function Parse_Hex (S : String) return Natural is
      Res : Natural := 0;
   begin
      for I in S'Range loop
         Res := Res * 16#10# + PolyORB.Utils.Hex_Value (S (I));
      end loop;
      return Res;
   end Parse_Hex;

   function Parse_HTTP_Version (S : String) return HTTP_Version
   is
      Version : constant Integer := S'First + HTTP_Slash'Length;
      Dot : Integer;
      Result : HTTP_Version;
   begin
      if S (S'First .. Version - 1) /= HTTP_Slash then
         raise Protocol_Error;
      end if;
      Dot := Find (S, Version, '.');
      if Dot >= S'Last then
         raise Protocol_Error;
      end if;
      Result.Major := Natural'Value (S (Version .. Dot - 1));
      Result.Minor := Natural'Value (S (Dot + 1 .. S'Last));
      return Result;
   end Parse_HTTP_Version;

   procedure Parse_Request_Line
     (F : access HTTP_Filter;
      S : String)
   is
      Space   : Integer;
      URI     : Integer;
      Version : Integer;
   begin
      --  Ignore empty lines received instead of Request_Line.
      if S'Length = 0 then
         return;
      end if;

      Space := Find_Whitespace (S, S'First);

      F.Request_Method
        := PolyORB.HTTP_Methods.In_Word_Set
        (S (S'First .. Space - 1));

      URI     := Skip_Whitespace (S, Space);
      Space   := Find_Whitespace (S, URI);
      Version := Skip_Whitespace (S, Space);

      if Version > S'Last then
         --  XXX bad request
         raise Protocol_Error;
      end if;

      Utils.Strings.Free (F.Request_URI);
      F.Request_URI := new String'(S (URI .. Space - 1));
      F.Version     := Parse_HTTP_Version (S (Version .. S'Last));
      F.State := Header;

      pragma Debug (O ("Parsed request-line:"));
      pragma Debug
        (O (F.Request_Method'Img & " " & F.Request_URI.all
            & " " & Image (F.Version)));
   end Parse_Request_Line;

   procedure Parse_Status_Line
     (F : access HTTP_Filter;
      S : String)
   is
      Space : Integer;
      Status_Pos : Integer;
   begin
      Space := Find_Whitespace (S, S'First);
      if Space > S'Last then
         raise Protocol_Error;
      end if;
      F.Version := Parse_HTTP_Version (S (S'First .. Space - 1));
      Status_Pos := Skip_Whitespace (S, Space);
      Space := Find_Whitespace (S, Status_Pos);
      if Space > S'Last or else Space - Status_Pos /= 3 then
         raise Protocol_Error;
      end if;

      F.Status := To_HTTP_Status_Code
        (Natural'Value (S (Status_Pos .. Space - 1)));
      --  The remainder of the line is the response-phrase
      --  and is ignored.

      F.State := Header;

      pragma Debug (O ("Parsed status-line:"));
      pragma Debug (O (Image (F.Version) & " " & F.Status'Img
                       & S (Space .. S'Last)));
   end Parse_Status_Line;

   procedure Parse_Header_Line
     (F : access HTTP_Filter;
      S : String)
   is
      use PolyORB.HTTP_Headers;

      Colon : constant Integer := Find (S, S'First, ':');
      Header_Kind : PolyORB.HTTP_Headers.Header;
      Pos : Integer;
      Tok_First, Tok_Last : Integer;
   begin
      if Colon > S'Last then
         raise Protocol_Error;
      end if;
      Header_Kind := PolyORB.HTTP_Headers.In_Word_Set
        (S (S'First .. Colon - 1));
      if (F.Role = Client and then Header_Kind in Request_Header)
        or else (F.Role = Server and then Header_Kind in Response_Header)
      then
         raise Protocol_Error;
      end if;

      Pos := Colon + 1;
      Skip_LWS (S, Pos);

      case Header_Kind is
         when H_Content_Length =>
            Tok_Last := S'Last;
            Trim_LWS (S, Tok_Last);
            if Pos > Tok_Last then
               raise Protocol_Error;
            end if;
            F.Content_Length := Stream_Element_Count'Value
              (S (Pos .. Tok_Last));

         when H_Transfer_Encoding =>
            Pos := Colon + 1;

            if Length (F.Transfer_Encoding) /= 0 then
               raise Protocol_Error;
               --  XXX duplicate Transfer-Encoding header.
            end if;

            while Pos <= S'Last loop
               Parse_CSL_Item (S, Pos, Tok_First, Tok_Last);
               String_Lists.Prepend
                 (F.Transfer_Encoding,
                  Ada.Characters.Handling.To_Lower
                  (S (Tok_First .. Tok_Last)));
            end loop;

            declare
               Nb_Encodings : constant Natural
                 := Length (F.Transfer_Encoding);
            begin
               if Nb_Encodings = 0 then
                  raise Protocol_Error;
                  --  XXX at least one token is required.
               end if;

               if Value (First (F.Transfer_Encoding)).all
                 /= Encoding_Chunked
               then
                  raise Protocol_Error;
                  --  XXX RFC 2616 3.6 When one or more
                  --  are specified, "chunked" must be specified
                  --  exactly once and must be the last specified.
                  --  XXX Respond 501 not implemented per RFC 2616 3.6?
               end if;
            end;
         when others =>
            pragma Debug
              (O ("Ignoring HTTP header " & Header_Kind'Img & ":"));
            pragma Debug (O (S));
            null;
            --  Ignore non-recognised headers.
      end case;
   end Parse_Header_Line;

   procedure Parse_Chunk_Size
     (F : access HTTP_Filter;
      S : String)
   is
      Chunk_Size : Stream_Element_Count;
      Semicolon : constant Integer
        := Find (S, S'First, ';');
      --  Optional: chunk-extensions
   begin
      Chunk_Size := Stream_Element_Count
        (Parse_Hex (S (S'First .. Semicolon - 1)));

      if Chunk_Size > 0 then
         F.Transfer_Length := Chunk_Size + 2;
         --  Expect chunk-data + CRLF

         F.State := Entity;
      else

         --  Last chunk received, go to trailer state.
         --  When in trailer state and headers are completed,
         --  finally call Message_Complete.

         F.Transfer_Length := -1;
         F.State := Trailer;
      end if;

   end Parse_Chunk_Size;

   procedure Message_Complete (F : access HTTP_Filter)
   is
      use PolyORB.Buffers;
      use PolyORB.Types;
      use type PolyORB.Utils.Strings.String_Ptr;
   begin
      pragma Debug (O ("Message_Complete: enter"));

      --  Check validity of message body buffer now.
      if F.Message_Buf = null then
         raise Program_Error;
      end if;
      Release_Contents (F.Message_Buf.all);

      declare
         S : constant String := To_Standard_String (F.Entity);
         --  XXX BAD BAD do not allocate that on the stack!
      begin
         PolyORB.Utils.Text_Buffers.Marshall_String
           (F.Message_Buf, S);
         Rewind (F.Message_Buf);
         if F.Request_URI /= null then
            Emit_No_Reply
              (F.Upper, Set_Target_Object'
               (Target => To_PolyORB_String (F.Request_URI.all)));
         end if;
         Emit_No_Reply
           (F.Upper, Data_Indication'(Data_Amount => S'Length));
         Release_Contents (F.In_Buf.all);
      end;
      --  XXX it is unfortunate that we:
      --    1. receive entity data in In_Buf;
      --    2. copy it to Unbounded_String entity;
      --    3. copy it back to buffer Message_Buf.
      --  Alternative solutions:
      --    * receive directly in Message_Buf (BUT we may
      --      have to copy the first few bytes of the entity
      --      body from In_Buf) (or insert them as a foreign chunk!)
      --      (but then we do not guarantee the contiguousness of
      --      Message_Buf)
      --    * no Message_Buf, send a Data_Indication containing
      --      he Entity unbounded-string (for efficiency's sake,
      --      check that unbounded strings are copy-on-write.)

      Clear_Message_State (F.all);
      F.State := Start_Line;
   end Message_Complete;

   function To_HTTP_Status_Code
     (Status : Integer)
     return HTTP_Status_Code
   is
      function Cvt is new Ada.Unchecked_Conversion
        (Integer, HTTP_Status_Code);
      Res : HTTP_Status_Code := Cvt (Status);

      Unknown_Codes : constant array (Integer range <>) of HTTP_Status_Code
        := (1 => S_1xx_Other_Informational,
            2 => S_2xx_Other_Successful,
            3 => S_3xx_Other_Redirection,
            4 => S_4xx_Other_Client_Error,
            5 => S_5xx_Other_Server_Error);
   begin
      if Res'Valid then
         return Res;
      else
         declare
            Class : constant Integer := Status / 100;
         begin
            if Class in Unknown_Codes'Range then
               return Unknown_Codes (Class);
            else
               raise Constraint_Error;
            end if;
         end;
      end if;
   end To_HTTP_Status_Code;

   function To_Integer is new Ada.Unchecked_Conversion
     (HTTP_Status_Code, Integer);

   --------------------------------------
   -- Preparation of outgoing messages --
   --------------------------------------

   procedure Put (F : access HTTP_Filter; S : String);
   procedure New_Line (F : access HTTP_Filter);
   procedure Put_Line (F : access HTTP_Filter; S : String);

   function Header
     (H : PolyORB.HTTP_Headers.Header; Value : String)
     return String;

   procedure Put_Status_Line
     (F : access HTTP_Filter; Status : HTTP_Status_Code);

   function Header
     (H : PolyORB.HTTP_Headers.Header; Value : String)
     return String is
   begin
      return PolyORB.HTTP_Headers.To_String (H) & ": " & Value;
   end Header;

   procedure Put_Status_Line
     (F : access HTTP_Filter; Status : HTTP_Status_Code)
   is
   begin
      Put_Line
        (F,
         Image (F.Version)
         --  XXX Should we reply with that version?
         & Integer'Image (To_Integer (Status)) & " "
         & Status'Img);
   end Put_Status_Line;

   procedure Error (F : access HTTP_Filter; Status : HTTP_Status_Code) is
   begin
      Put_Status_Line (F, Status);
      Clear_Message_State (F.all);
      Emit_No_Reply (Lower (F), Data_Out'(Out_Buf => F.Out_Buf));
   end Error;

   procedure Put (F : access HTTP_Filter; S : String) is
   begin
      PolyORB.Utils.Text_Buffers.Marshall_String (F.Out_Buf, S);
   end Put;

   procedure New_Line (F : access HTTP_Filter)
   is
      use PolyORB.Utils.Text_Buffers;
   begin
      Marshall_Char (F.Out_Buf, ASCII.CR);
      Marshall_Char (F.Out_Buf, ASCII.LF);
   end New_Line;

   procedure Put_Line (F : access HTTP_Filter; S : String) is
   begin
      Put (F, S);
      New_Line (F);
   end Put_Line;

   use PolyORB.HTTP_Headers;

   --  The procedures Prepare_* are adapted from code in
   --  AWS.Server.Protocol_Handler and AWS.Client.

   procedure Prepare_Request
     (F  : access HTTP_Filter;
      RO : PolyORB.Filters.AWS_Interface.AWS_Request_Out)
   is
      use PolyORB.HTTP_Methods;
      use PolyORB.Types;

      SOAP_Action : constant String
        := To_Standard_String (RO.SOAP_Action);
   begin
      Put_Line
        (F, To_String (RO.Request_Method)
         & " " & Types.To_Standard_String (RO.Relative_URI)
         & " " & Image (F.Version));

      --  Put_Line (F, H_Host (Host_Address));
      --  XXX When binding an HTTP profile, the Host
      --  info (from the URL or a Tag_SOAP or a Tag_HTTP
      --  profile) should be propagated down the protocol
      --  stack so we can generate a proper Host: header.

      --  XXX Cookie??

      --  Put_Line (F, H_Accept_Type ("text/html, */*"));
      Put_Line (F, Header (H_Accept_Language, "fr, us"));
      Put_Line (F, Header (H_User_Agent, "PolyORB"));
      --  XXX BAD BAD too much hardcoded stuff.

      if False then
         Put_Line (F, Header (H_Connection, "Keep-Alive"));
         --  XXX should provide keepalive mechanism!
         --  (it should even be the default).
      else
         Put_Line (F, Header (H_Connection, "Close"));
      end if;

      --  XXX Authentication??

      if SOAP_Action'Length /= 0 then
         Put_Line (F, Header (H_SOAPAction, SOAP_Action));
      end if;

      case RO.Request_Method is
         when GET =>
            New_Line (F);

         when POST =>
            if SOAP_Action'Length /= 0 then
               Put_Line (F, Header (H_Content_Type, AWS.MIME.Text_XML));
            else
               Put_Line (F, Header (H_Content_Type, AWS.MIME.Appl_Form_Data));
            end if;
            Put_Line
              (F, Header (H_Content_Length, Image (Length (RO.Data))));
            New_Line (F);
            Put (F, To_Standard_String (RO.Data));
            --  XXX bad bad passing complete SOAP request
            --  on the stack!! Would be better off inserting
            --  it directly as a chunk!! (Marshall-by-address
            --  for Types.String!)

         when others =>
            raise PolyORB.Not_Implemented;
      end case;

   end Prepare_Request;

   procedure Prepare_General_Header
     (F : access HTTP_Filter;
      RD : AWS.Response.Data)
   is
   begin
      --  Put_Line (F, Header (H_Date, To_HTTP_Date (OS_Lib.GMT_Clock)));
      Put_Line (F, Header (H_Server, "PolyORB"));

      --  Connection

--       if Will_Close then
--          --  If there is no connection received we assume a non
--          --  Keep-Alive connection.

--          Put_Line (F, Header (H_Connection, "close"));
--       else
--       Put_Line (F, Messages.Connection
--         (AWS.Status.Connection (C_Stat)));
--       end if;
      --  XXX What should we truly do here?

   end Prepare_General_Header;

   procedure Prepare_Header_Only
     (F : access HTTP_Filter;
      RD : AWS.Response.Data)
   is
      Status : constant HTTP_Status_Code
        := AWS.Response.Status_Code (RD);
   begin
      Put_Status_Line (F, Status);
      Prepare_General_Header (F, RD);

      --  There is no content
      Put_Line (F, Header (H_Content_Length, "0"));

      if Status = S_401_Unauthorized then
         Put_Line
           (F, Header (H_WWW_Authenticate,
                       "Basic realm="""
                       & AWS.Response.Realm (RD) & """"));
      end if;

      --  End of header
      New_Line (F);
   end Prepare_Header_Only;

   procedure Prepare_Message
     (F : access HTTP_Filter;
      RD : AWS.Response.Data)
   is
      Status : constant HTTP_Status_Code
        := AWS.Response.Status_Code (RD);
   begin
      Put_Status_Line (F, Status);
      if Status = S_301_Moved_Permanently then
         Put_Line
           (F, Header (H_Location, AWS.Response.Location (RD)));
      end if;
      Prepare_General_Header (F, RD);

      Put_Line (F, Header
                  (H_Content_Length,
                   Image (AWS.Response.Content_Length (RD))));

      Put_Line (F, Header
                  (H_Content_Type,
                   AWS.Response.Content_Type (RD)));

      if Status = S_401_Unauthorized then
         Put_Line
           (F, Header (H_WWW_Authenticate,
                       "Basic realm="""
                       & AWS.Response.Realm (RD) & """"));
      end if;

      --  End of headers.

      New_Line (F);

      Put (F, AWS.Response.Message_Body (RD));
      --  XXX could be more clever and send it chunked...
   end Prepare_Message;

end PolyORB.Filters.HTTP;
