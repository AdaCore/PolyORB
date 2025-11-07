------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         A W S . R E S P O N S E                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with PolyORB.Utils.Unchecked_Deallocation;

with AWS.Headers.Set;
with AWS.Headers.Values;
with AWS.Resources.Embedded;
with AWS.Response.Set;
with AWS.Translator;

package body AWS.Response is

   use Streams;

   -----------------
   -- Acknowledge --
   -----------------

   function Acknowledge
     (Status_Code  : Messages.Status_Code;
      Message_Body : String := "";
      Content_Type : String := MIME.Text_HTML)
      return Data
   is
      Result : Data;
   begin
      Set.Status_Code (Result, Status_Code);

      if Message_Body = "" then
         Set.Mode (Result, Header);
      else
         Set.Message_Body (Result, Message_Body);
         Set.Content_Type (Result, Content_Type);
      end if;

      return Result;
   end Acknowledge;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Data) is
   begin
      Object.Ref_Counter.all := Object.Ref_Counter.all + 1;
   end Adjust;

   ------------------
   -- Authenticate --
   ------------------

   function Authenticate
     (Realm : String;
      Mode  : Authentication_Mode := Basic;
      Stale : Boolean             := False)
      return Data
   is
      Result : Data;
      CRLF : constant String := ASCII.CR & ASCII.LF;

      Auth_Mess : constant String
        := "<HTML><HEAD>" & CRLF
        & "<TITLE>401 Authorization Required</TITLE>" & CRLF
        & "</HEAD><BODY>" & CRLF
        & "<H1>Authorization Required</H1>" & CRLF
        & "This server could not verify that you" & CRLF
        & "are authorized to access the document you" & CRLF
        & "requested.  Either you supplied the wrong" & CRLF
        & "credentials (e.g., bad password), or your" & CRLF
        & "browser doesn't understand how to supply" & CRLF
        & "the credentials required.<P>" & CRLF
        & "</BODY></HTML>" & CRLF;
   begin
      Set.Authentication (Result, Realm, Mode, Stale);
      Set.Content_Type   (Result, AWS.MIME.Text_HTML);
      Set.Message_Body   (Result, Auth_Mess);
      return Result;
   end Authenticate;

   --------------------
   -- Authentication --
   --------------------

   function Authentication (D : Data) return Authentication_Mode is
      use AWS.Headers;
      Auth_Values : constant VString_Array
        := Get_Values (D.Header, Messages.WWW_Authenticate_Token);
   begin
      if Auth_Values'Length = 1 then
         return Authentication_Mode'Value
            (Values.Get_Unnamed_Value (To_String (Auth_Values (1)), 1));
      else
         return Any;
      end if;
   end Authentication;

   --------------------------
   -- Authentication_Stale --
   --------------------------

   function Authentication_Stale (D : Data) return Boolean is
      use AWS.Headers;
      Auth_Values : constant VString_Array
        := Get_Values (D.Header, Messages.WWW_Authenticate_Token);
   begin
      for J in Auth_Values'Range loop
         declare
            Stale_Image : constant String :=
              Values.Search (To_String (Auth_Values (J)), "stale", False);
         begin
            if Stale_Image /= "" then
               return Boolean'Value (Stale_Image);
            end if;
         end;
      end loop;
      return False;
   end Authentication_Stale;

   -----------
   -- Build --
   -----------

   function Build
     (Content_Type  : String;
      Message_Body  : String;
      Status_Code   : Messages.Status_Code  := Messages.S200;
      Cache_Control : Messages.Cache_Option := Messages.Unspecified)
      return Data
   is
      Result : Data;
   begin
      Set.Status_Code   (Result, Status_Code);
      Set.Content_Type  (Result, Content_Type);
      Set.Message_Body  (Result, Message_Body);
      Set.Cache_Control (Result, Cache_Control);
      return Result;
   end Build;

   function Build
     (Content_Type    : String;
      UString_Message : Strings.Unbounded.Unbounded_String;
      Status_Code     : Messages.Status_Code  := Messages.S200;
      Cache_Control   : Messages.Cache_Option := Messages.Unspecified)
      return Data
   is
      Result : Data;
   begin
      Set.Status_Code   (Result, Status_Code);
      Set.Content_Type  (Result, Content_Type);
      Set.Message_Body  (Result, UString_Message);
      Set.Cache_Control (Result, Cache_Control);
      return Result;
   end Build;

   function Build
     (Content_Type  : String;
      Message_Body  : Streams.Stream_Element_Array;
      Status_Code   : Messages.Status_Code         := Messages.S200;
      Cache_Control : Messages.Cache_Option        := Messages.Unspecified)
      return Data
   is
      Result : Data;
   begin
      Set.Status_Code   (Result, Status_Code);
      Set.Content_Type  (Result, Content_Type);
      Set.Message_Body  (Result, Message_Body);
      Set.Cache_Control (Result, Cache_Control);
      return Result;
   end Build;

   function Build
     (SOAP_Body  : SOAP.Message.Response.Object)
      return Data
   is
      Result : Data;
   begin
      Set.Status_Code   (Result, Messages.S200);
      Set.Content_Type  (Result, AWS.MIME.Text_XML);
      Set.Message_Body  (Result, SOAP_Body);
      Set.Cache_Control (Result, Messages.Unspecified);
      return Result;
   end Build;

   -------------------
   -- Cache_Control --
   -------------------

   function Cache_Control (D : Data) return Messages.Cache_Option is
   begin
      return Messages.Cache_Option
        (Headers.Get (D.Header, Messages.Cache_Control_Token));
   end Cache_Control;

   --------------------
   -- Content_Length --
   --------------------

   function Content_Length (D : Data) return Content_Length_Type is
   begin
      return D.Content_Length;
   end Content_Length;

   ------------------
   -- Content_Type --
   ------------------

   function Content_Type (D : Data) return String is
   begin
      return Headers.Get (D.Header, Messages.Content_Type_Token);
   end Content_Type;

   ----------------------
   -- Create_Resource --
   ----------------------

   procedure Create_Resource
     (File :    out AWS.Resources.File_Type;
      D    : Data)
   is
      use AWS.Resources;
   begin
      case D.Mode is
         when Response.File =>
            Open (File, Filename (D), "shared=no");

         when Response.Stream =>
            Resources.Streams.Create (File, D.Stream);

         when Response.Message =>
            Embedded.Create (File, Embedded.Buffer_Access (D.Message_Body));

         when others =>
            --  Should not be called for others response modes.
            raise Constraint_Error;
      end case;
   end Create_Resource;

   -----------
   -- Empty --
   -----------

   function Empty return Data is
      Result : Data;
   begin
      Set.Status_Code  (Result, Messages.S204);
      return Result;
   end Empty;

   ----------
   -- File --
   ----------

   function File
     (Content_Type : String;
      Filename     : String;
      Status_Code  : Messages.Status_Code := Messages.S200)
      return Data
   is
      Result : Data;
   begin
      Set.Status_Code  (Result, Status_Code);
      Set.Content_Type (Result, Content_Type);
      Set.Filename     (Result, Filename);
      return Result;
   exception
      when Resources.Resource_Error =>
         return Acknowledge (Messages.S404, "<p> " & Filename & " not found");
   end File;

   --------------
   -- Filename --
   --------------

   function Filename (D : Data) return String is
   begin
      return To_String (D.Filename);
   end Filename;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Data) is

      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => Natural,
         Name => Natural_Access);

   begin
      Object.Ref_Counter.all := Object.Ref_Counter.all - 1;

      if Object.Ref_Counter.all = 0 then
         Free (Object.Ref_Counter);
         Utils.Free (Object.Message_Body);

         AWS.Headers.Set.Free (Object.Header);
      end if;
   end Finalize;

   ------------
   -- Header --
   ------------

   function Header
     (D    : Data;
      Name : String;
      N    : Positive)
      return String is
   begin
      return Headers.Get (D.Header, Name, N);
   end Header;

   function Header
     (D    : Data;
      Name : String)
      return String is
   begin
      return Headers.Get_Values (D.Header, Name);
   end Header;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Data) is
   begin
      Object.Ref_Counter := new Natural'(1);
      AWS.Headers.Set.Reset (Object.Header);
   end Initialize;

   --------------
   -- Location --
   --------------

   function Location (D : Data) return String is
   begin
      return Headers.Get (D.Header, Messages.Location_Token);
   end Location;

   ------------------
   -- Message_Body --
   ------------------

   function Message_Body (D : Data) return String is
      use type Utils.Stream_Element_Array_Access;
   begin
      if D.Message_Body = null then
         return "";
      else
         return Translator.To_String (D.Message_Body.all);
      end if;
   end Message_Body;

   function Message_Body (D : Data) return Unbounded_String is
      use type Utils.Stream_Element_Array_Access;
   begin
      if D.Message_Body = null then
         return Null_Unbounded_String;
      else
         return Translator.To_Unbounded_String (D.Message_Body.all);
      end if;
   end Message_Body;

   function Message_Body (D : Data) return Streams.Stream_Element_Array is
      use type Utils.Stream_Element_Array_Access;
      No_Data : constant Streams.Stream_Element_Array := (1 .. 0 => 0);
   begin
      if D.Message_Body = null then
         return No_Data;
      else
         return D.Message_Body.all;
      end if;
   end Message_Body;

   ------------------
   -- SOAP_Message --
   ------------------

   function SOAP_Message (D : Data) return SOAP.Message.Response.Object
   is
   begin
      return D.SOAP_Message;
   end SOAP_Message;

   ----------
   -- Mode --
   ----------

   function Mode (D : Data) return Data_Mode is
   begin
      return D.Mode;
   end Mode;

   -----------
   -- Moved --
   -----------

   function Moved
     (Location : String;
      Message  : String := Default_Moved_Message)
      return Data
   is
      use Ada.Strings;

      Result : Data;

      function Build_Message_Body return String;
      --  Returns proper message body using Message template. It replaces _@_
      --  in Message by Location.

      ------------------------
      -- Build_Message_Body --
      ------------------------

      function Build_Message_Body return String is
         Start : constant Natural := Fixed.Index (Message, "_@_");
      begin
         if Start = 0 then
            return Message;
         else
            return Fixed.Replace_Slice (Message, Start, Start + 2, Location);
         end if;
      end Build_Message_Body;

      Message_Body : constant String := Build_Message_Body;

   begin
      Set.Location     (Result, Location);
      Set.Status_Code  (Result, Messages.S301);
      Set.Message_Body (Result, Message_Body);
      Set.Content_Type (Result, AWS.MIME.Text_HTML);
      return Result;
   end Moved;

   -----------
   -- Realm --
   -----------

   function Realm (D : Data) return String is
      use Headers;
   begin
      return Values.Search
        (Header_Value   => Get (D.Header, Messages.WWW_Authenticate_Token),
         Name           => "realm",
         Case_Sensitive => False);
   end Realm;

   -----------------
   -- Send_Header --
   -----------------

--   procedure Send_Header (Socket : Net.Socket_Type'Class; D : Data) is
--     begin
--        Headers.Send_Header (Socket, D.Header);
--     end Send_Header;

   ------------------
   -- Socket_Taken --
   ------------------

   function Socket_Taken return Data is
      Result : Data;
   begin
      Set.Mode (Result, Socket_Taken);
      return Result;
   end Socket_Taken;

   -----------------
   -- Status_Code --
   -----------------

   function Status_Code (D : Data) return Messages.Status_Code is
   begin
      return D.Status_Code;
   end Status_Code;

   ------------
   -- Stream --
   ------------

   function Stream
     (Content_Type  : String;
      Stream_Handle : Resources.Streams.Stream_Access;
      Stream_Size   : Content_Length_Type   := Undefined_Length;
      Status_Code   : Messages.Status_Code  := Messages.S200;
      Cache_Control : Messages.Cache_Option := Messages.No_Cache)
      return Data
   is
      Result : Data;
   begin
      Set.Stream        (Result, Stream_Handle, Stream_Size);
      Set.Status_Code   (Result, Status_Code);
      Set.Content_Type  (Result, Content_Type);
      Set.Cache_Control (Result, Cache_Control);
      return Result;
   end Stream;

   ---------
   -- URL --
   ---------

   function URL (Location : String) return Data is
      Result : Data;
   begin
      Set.Status_Code (Result, Messages.S301);
      Set.Location    (Result, Location);
      Set.Mode (Result, Header);
      return Result;
   end URL;

end AWS.Response;
