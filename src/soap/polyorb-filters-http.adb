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

with PolyORB.Components;
with PolyORB.Filters.Interface;
with PolyORB.HTTP_Headers;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Opaque;
with PolyORB.Utils;

package body PolyORB.Filters.HTTP is

   use Ada.Streams;

   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.ORB;

   use String_Lists;

   package L is new PolyORB.Log.Facility_Log ("polyorb.filters.http");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   -----------------------------------------
   -- Declaration of internal subprograms --
   -----------------------------------------

   function Find_Skip
     (S     : String;
      Start : Integer;
      What  : Character;
      Skip  : Boolean)
     return Integer;
   --  If Skip is False, return the index of the
   --  first occurrence of What in S.
   --  If Skip is True, return the index of the
   --  first occurrence of any character OTHER THAN What.
   --  If no such character exists, S'Last + 1 is returned.

   --  Shorthands for commonly-used forms of Find_Skip

   function Find
     (S     : String;
      Start : Integer;
      What  : Character;
      Skip  : Boolean := False)
     return Integer
     renames Find_Skip;

   function Find_Whitespace
     (S     : String;
      Start : Integer;
      What  : Character := ' ';
      Skip  : Boolean := False)
     return Integer
     renames Find_Skip;

   function Skip_Whitespace
     (S     : String;
      Start : Integer;
      What  : Character := ' ';
      Skip  : Boolean := True)
     return Integer
     renames Find_Skip;

   procedure Process_Line
     (F : access HTTP_Filter;
      Line_Length : Stream_Element_Count);
   --  Process one start-line or header line in an HTTP message,
   --  starting at the current input buffer position of F,
   --  and spanning Line_Length characters.

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
      F.Status  := -1;

      F.Request_Method := PolyORB.HTTP_Methods.Extension_Method;
      Deallocate (F.Request_URI);

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
         --  HTTP has its own buffer.

         Expect_Data (F, F.In_Buf, Buffer_Size);
         --  Wait for first line of message.

      elsif S in Data_Indication then

         declare
            Data_Received : Stream_Element_Count
              := Stream_Element_Count
              (Data_Indication (S).Data_Amount);

            New_Data : PolyORB.Opaque.Opaque_Pointer;
            New_Data_Position : Stream_Element_Offset
              := Length (F.In_Buf) - Data_Received;
         begin

            ---------------------------
            -- Process received data --
            ---------------------------

            <<Process_Received_Data>>

            if F.State in Line_By_Line then

               --  Processing data in line-by-line mode.

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

                        Process_Line
                          (F, Line_Length =>
                             New_Data_Position - CDR_Position (F.In_Buf)
                           + I - New_Data.Offset + 1);

                        --  Calculation of the length of the current line:
                        --  New_Data_Position - CDR_Position = amount of data
                        --    received  but not yet processed
                        --    (the beginning of this line)
                        --  I - I'First + 1 = amount of data now appended
                        --    (the end of this line).

                        New_Data_Position := CDR_Position (F.In_Buf);
                        Data_Received := Length (F.In_Buf) - New_Data_Position;
                        if Data_Received > 0 then
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
               --  Not in a line-by-line state: transferring entity.

               pragma Assert
                 (Data_Received <= Stream_Element_Count (F.Transfer_Length));

               declare
                  use PolyORB.Types;
                  Data : PolyORB.Opaque.Opaque_Pointer;
               begin
                  PolyORB.Buffers.Extract_Data
                    (F.In_Buf, Data, Data_Received, Use_Current => True);

                  declare
                     S : String (1 .. Integer (Data_Received));
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

                  F.Transfer_Length
                    := F.Transfer_Length - Integer (Data_Received);

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
                        --  XXX Entity_Complete (F);
                        raise Not_Implemented;
                     end if;
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
                  --  All expected data was received, signal upper.
                  --  XXX
                  raise PolyORB.Not_Implemented;

               when others =>
                  Expect_Data
                    (F, F.In_Buf, Stream_Element_Offset (F.Transfer_Length));

            end case;
         end;

      elsif S in Set_Server then
         PolyORB.Components.Emit_No_Reply (F.Upper, S);
      else
         raise PolyORB.Components.Unhandled_Message;
      end if;

      return Res;
   end Handle_Message;

   function Find_Skip
     (S     : String;
      Start : Integer;
      What  : Character;
      Skip  : Boolean)
     return Integer
   is
      I : Integer := Start;
   begin
      while I < S'Last loop
         I := I + 1;
         exit when I > S'Last or else (S (I) = What xor Skip);
      end loop;

      return I;
   end Find_Skip;

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

      pragma Debug (O ("HTTP line received:" & S));

      case F.State is
         when Start_Line =>
            if F.Role = Server then
               Parse_Request_Line (F, S);
            else
               Parse_Status_Line (F, S);
            end if;

         when Chunk_Size =>
            Parse_Chunk_Size (F, S);

         when Header =>
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
      Major_Image : constant String := V.Major'Img;
      Minor_Image : constant String := V.Minor'Img;
   begin
      return HTTP_Slash
        & Major_Image (Major_Image'First + 1 .. Major_Image'Last)
        & "."
        & Minor_Image (Minor_Image'First + 1 .. Minor_Image'Last);
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

      Deallocate (F.Request_URI);
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
      Status : Natural;
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

      Status := Natural'Value (S (Status_Pos .. Space - 1));
      if Status < 100 then
         raise Protocol_Error;
      end if;
      F.Status := Status;

      --  The remainder of the line is the response-phrase
      --  and is ignored.

      pragma Debug (O ("Parsed status-line:"));
      pragma Debug (O (Image (F.Version) & Status'Img
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
            F.Content_Length := Natural'Value (S (Pos .. Tok_Last));

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
      Chunk_Size : Integer;
      Semicolon : constant Integer
        := Find (S, S'First, ';');
      --  Optional: chunk-extensions
   begin
      Chunk_Size := Parse_Hex (S (S'First .. Semicolon - 1));

      if Chunk_Size > 0 then
         F.Transfer_Length := Chunk_Size + 2;
         --  Expect chunk-data + CRLF
         F.State := Entity;
      else
         --  Last chunk received, go to trailer (= entity-header)
         --  state. When in trailer mode and headers are completed,
         --  finally call Entity_Complete.
         raise PolyORB.Not_Implemented;
         --  XXX
      end if;

   end Parse_Chunk_Size;

end PolyORB.Filters.HTTP;
