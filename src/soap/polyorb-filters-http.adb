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

with PolyORB.Components;
with PolyORB.Filters.Interface;
with PolyORB.HTTP_Headers;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Opaque;

package body PolyORB.Filters.HTTP is

   use Ada.Streams;

   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.ORB;

   package L is new PolyORB.Log.Facility_Log ("polyorb.filters.http");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Buffer_Size : constant := 4096;

   procedure Process_Line
     (F : access HTTP_Filter;
      Line_Length : Stream_Element_Count);

   procedure Create
     (Fact   : access HTTP_Filter_Factory;
      Filt   : out Filter_Access)
   is
      Res : constant Filter_Access := new HTTP_Filter;
   begin
      Filt := Res;
   end Create;

   procedure Finalize (F : in out HTTP_Filter) is
   begin
      Finalize (Filter (F));
      Deallocate (F.Request_URI);
      String_Lists.Deallocate (F.Transfer_Encoding);
   end Finalize;

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
            CDRP : constant Stream_Element_Offset
              := CDR_Position (F.In_Buf);

            Data_Received : constant Stream_Element_Count
              := Stream_Element_Count
              (Data_Indication (S).Data_Amount);
            New_Data : PolyORB.Opaque.Opaque_Pointer;
            New_Data_Position : constant Stream_Element_Offset
              := CDRP + Length (F.In_Buf) - Data_Received;
         begin
            if F.State = Start_Line or else F.State = Header then
               Extract_Data
                 (F.In_Buf, New_Data, Data_Received,
                  Use_Current => False,
                  At_Position => New_Data_Position);

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
                             New_Data_Position - CDRP
                           + I - New_Data.Offset + 1);

                        --  Calculation of the length of the current line:
                        --  New_Data_Position - CDRP = amount of data received
                        --    in previous DI messages but not yet processed
                        --    (constituting the beginning of this line)
                        --  I - I'First + 1 = amount of data received in this
                        --    DI, constituting the end of this line.

                     when others =>
                        F.CR_Seen := False;
                  end case;
               end loop;
            end if;

            --  XXX if all the available data have been processed,
            --  should exit here.

            case F.State is
               when Start_Line =>
                  raise Program_Error;
               when Header =>
                  --  Check for EOL-EOL in received data.
                  raise Program_Error;
               when Entity =>
                  --  Accumulate until end of entity.
                  raise Program_Error;
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

   --  HTTP-Version strings.
   HTTP_Slash : constant String := "HTTP/";

   function Image (V : HTTP_Version) return String is
      Major_Image : constant String := V.Major'Img;
      Minor_Image : constant String := V.Minor'Img;
   begin
      return HTTP_Slash
        & Major_Image (Major_Image'First + 1 .. Major_Image'Last)
        & "."
        & Minor_Image (Minor_Image'First + 1 .. Minor_Image'Last);
   end Image;

   -----------------------------------------
   -- HTTP protocol non-terminals parsing --
   -----------------------------------------

   function Parse_HTTP_Version (S : String) return HTTP_Version;
   --  HTTP-Version

   procedure Parse_Request_Line (F : access HTTP_Filter; S : String);
   --  Request-Line

   procedure Parse_Status_Line (F : access HTTP_Filter; S : String);
   --  Status-Line

   procedure Parse_Header_Line (F : access HTTP_Filter; S : String);
   --  {general,request,response,entity}-header

   function Is_LWS (C : Character) return Boolean;
   pragma Inline (Is_LWS);
   --  True iff C is a linear whitespace character (space or tab).

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

   function Is_LWS (C : Character) return Boolean is
   begin
      return C = ' ' or else C = ASCII.HT;
   end Is_LWS;

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
      Result.Major := Integer'Value (S (Version .. Dot - 1));
      Result.Minor := Integer'Value (S (Dot + 1 .. S'Last));
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
      F.Version     := Parse_HTTP_Version (S (Version .. S'Last - 2));
      --  Last two characters are CR/LF.

      pragma Debug (O ("Parsed request-line:"));
      pragma Debug
        (O (F.Request_Method'Img & " " & F.Request_URI.all
            & " " & Image (F.Version)));
   end Parse_Request_Line;

   procedure Parse_Header_Line
     (F : access HTTP_Filter;
      S : String)
   is
      use PolyORB.HTTP_Headers;

      Colon : constant Integer := Find (S, S'First, ':');
      Header_Kind : PolyORB.HTTP_Headers.Header;
      Pos : Integer;
      First, Last : Integer;
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

      case Header_Kind is
         when H_Transfer_Encoding =>
            Pos := Colon + 1;
            String_Lists.Deallocate (F.Transfer_Encoding);
            while Pos <= S'Last loop
               Parse_CSL_Item (S, Pos, First, Last);
               String_Lists.Append
                 (F.Transfer_Encoding, S (First .. Last));
            end loop;

         when others =>
            null;
            --  Ignore non-recognised headers.
      end case;
   end Parse_Header_Line;

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

      Status := Integer'Value (S (Status_Pos .. Space - 1));
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

   procedure Process_Line
     (F : access HTTP_Filter;
      Line_Length : Stream_Element_Count)
   is
      Data : PolyORB.Opaque.Opaque_Pointer;
      S : String (1 .. Integer (Line_Length));
   begin
      PolyORB.Buffers.Extract_Data
        (F.In_Buf, Data, Line_Length, Use_Current => True);
      for I in S'Range loop
         S (I) := Character'Val
           (Data.Zone
            (Data.Offset + Stream_Element_Offset (I - S'First)));
      end loop;

      pragma Assert (S (S'Last - 1 .. S'Last) = ASCII.CR & ASCII.LF);
      pragma Debug (O ("HTTP line received:"));
      pragma Debug (O (S (S'First .. S'Last - 2)));

      case F.State is
         when Start_Line =>
            if F.Role = Server then
               Parse_Request_Line (F, S);
            else
               Parse_Status_Line (F, S);
            end if;

         when Header =>
            Parse_Header_Line (F, S);

         when others =>
            raise Program_Error;
      end case;

   end Process_Line;

end PolyORB.Filters.HTTP;
