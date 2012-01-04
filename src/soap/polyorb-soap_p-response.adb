------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . S O A P _ P . R E S P O N S E               --
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

--  taken from aws.response

with Ada.Strings.Fixed;

--  with AWS.OS_Lib;

package body PolyORB.SOAP_P.Response is

   -----------------
   -- Acknowledge --
   -----------------

   function Acknowledge
     (Status_Code  : HTTP_Status_Code;
      Message_Body : String := "";
      Content_Type : String := PolyORB.Web.MIME.Text_HTML)
     return Data is
   begin
      if Message_Body = "" then
         return Data'(Header,
                      Status_Code,
                      0,
                      Null_Unbounded_String,
                      Null_Unbounded_String,
                      Null_Unbounded_String,
                      Null_Unbounded_String,
                      null);
      else
         return Data'(Message,
                      Status_Code,
                      Message_Body'Length,
                      To_Unbounded_String (Content_Type),
                      To_Unbounded_String (Message_Body),
                      Null_Unbounded_String,
                      Null_Unbounded_String,
                      null);
      end if;
   end Acknowledge;

   ------------------
   -- Authenticate --
   ------------------

   function Authenticate (Realm : String) return Data is

      CRLF : constant String := ASCII.CR & ASCII.LF;

      Auth_Mess : constant String :=
        "<HTML><HEAD>" & CRLF
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
      return Data'(Message,
                   S_401_Unauthorized,
                   Auth_Mess'Length,
                   To_Unbounded_String (PolyORB.Web.MIME.Text_HTML),
                   To_Unbounded_String (Auth_Mess),
                   Null_Unbounded_String,
                   To_Unbounded_String (Realm),
                   null);
   end Authenticate;

   ------------
   -- Binary --
   ------------

   function Binary (D : Data) return Streams.Stream_Element_Array is
      No_Data : constant Streams.Stream_Element_Array := (1 .. 0 => 0);
   begin
      if D.Elements = null then
         return No_Data;
      else
         return D.Elements.all;
      end if;
   end Binary;

   -----------
   -- Build --
   -----------

   function Build
     (Content_Type : String;
      Message_Body : String;
      Status_Code  : HTTP_Status_Code := S_200_OK)
     return Data is
   begin
      return Data'(Message,
                   Status_Code,
                   Message_Body'Length,
                   To_Unbounded_String (Content_Type),
                   To_Unbounded_String (Message_Body),
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   null);
   end Build;

   function Build
     (Content_Type    : String;
      UString_Message : Strings.Unbounded.Unbounded_String;
      Status_Code     : HTTP_Status_Code := S_200_OK)
     return Data is
   begin
      return Data'(Message,
                   Status_Code,
                   Length (UString_Message),
                   To_Unbounded_String (Content_Type),
                   UString_Message,
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   null);
   end Build;

   function Build
     (Content_Type : String;
      Message_Body : Streams.Stream_Element_Array;
      Status_Code  : HTTP_Status_Code := S_200_OK)
     return Data is
   begin
      return Data'(Message,
                   Status_Code,
                   Message_Body'Length,
                   To_Unbounded_String (Content_Type),
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   new Streams.Stream_Element_Array'(Message_Body));
   end Build;

   --------------------
   -- Content_Length --
   --------------------

   function Content_Length (D : Data) return Natural is
   begin
      return D.Content_Length;
   end Content_Length;

   ------------------
   -- Content_Type --
   ------------------

   function Content_Type (D : Data) return String is
   begin
      return To_String (D.Content_Type);
   end Content_Type;

   ----------
   -- File --
   ----------

--    function File
--      (Content_Type : String;
--       Filename     : String) return Data is
--    begin
--       return Data'(File,
--                    Messages.S200,
--                    Integer (OS_Lib.File_Size (Filename)),
--                    To_Unbounded_String (Content_Type),
--                    To_Unbounded_String (Filename),
--                    Null_Unbounded_String,
--                    Null_Unbounded_String,
--                    null);
--    end File;

   --------------
   -- Location --
   --------------

   function Location (D : Data) return String is
   begin
      return To_String (D.Location);
   end Location;

   ------------------
   -- Message_Body --
   ------------------

   function Message_Body (D : Data) return String is
   begin
      return To_String (D.Message_Body);
   end Message_Body;

   function Message_Body (D : Data) return Unbounded_String is
   begin
      return D.Message_Body;
   end Message_Body;

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
     (Location     : String;
      Message      : String := Default_Moved_Message)
     return Data
   is
      use Ada.Strings;

      function Build_Message_Body return String;
      --  Return proper message body using Message template. It replaces _@_
      --  in Message by Location.

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
      return Data'(Response.Message,
                   S_301_Moved_Permanently,
                   Message_Body'Length,
                   To_Unbounded_String (PolyORB.Web.MIME.Text_HTML),
                   To_Unbounded_String (Message_Body),
                   To_Unbounded_String (Location),
                   Null_Unbounded_String,
                   null);
   end Moved;

   -----------
   -- Realm --
   -----------

   function Realm (D : Data) return String is
   begin
      return To_String (D.Realm);
   end Realm;

   ------------------
   -- Socket_Taken --
   ------------------

--    function Socket_Taken return Data is
--    begin
--       return Data'(Response.Socket_Taken,
--                    Messages.S200,
--                    0,
--                    Null_Unbounded_String,
--                    Null_Unbounded_String,
--                    Null_Unbounded_String,
--                    Null_Unbounded_String,
--                    null);
--    end Socket_Taken;

   -----------------
   -- Status_Code --
   -----------------

   function Status_Code (D : Data) return HTTP_Status_Code is
   begin
      return D.Status_Code;
   end Status_Code;

   ---------
   -- URL --
   ---------

   function URL (Location : String)
     return Data is
   begin
      return Data'(Response.Message,
                   S_301_Moved_Permanently,
                   0,
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   To_Unbounded_String (Location),
                   Null_Unbounded_String,
                   null);
   end URL;

end PolyORB.SOAP_P.Response;
