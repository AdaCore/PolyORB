------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       A W S . S T A T U S . S E T                        --
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

with AWS.Parameters.Set;

package body AWS.Status.Set is

   use Ada.Strings;

--   procedure Authorization (D : in out Data);
   --  Parse the Authorization parameters from the Authorization header value.

--   procedure Update_Data_From_Header (D : in out Data);
   --  Update some Data fields from the internal Data header container.
   --  The Update_Data_From_Header should be called after the complete
   --  header parsing.

   -------------------
   -- Authorization --
   -------------------

--     procedure Authorization (D : in out Data) is

--        Header_Value : constant String
--          := AWS.Headers.Get (D.Header, Messages.Authorization_Token);

--     procedure Named_Value (Name, Value : String; Quit : in out Boolean);

--        procedure Value (Item : String; Quit : in out Boolean);

--        -----------------
--        -- Named_Value --
--        -----------------

--        procedure Named_Value
--          (Name, Value : String;
--           Quit        : in out Boolean)
--        is

--           type Digest_Attribute
--              is (Username, Realm, Nonce, NC, CNonce,
--                  QOP, URI, Response, Algorithm);
--           --  The enumeration type is using to be able to
--           --  use the name in the case statement.
--           --  The case statement has usially faster implementation.

--           Attribute : Digest_Attribute;

--           function "+"
--             (Item : String)
--              return Unbounded_String
--             renames To_Unbounded_String;

--        begin
--           begin
--              Attribute := Digest_Attribute'Value (Name);
--           exception
--              when Constraint_Error =>
--                 --  Ignoring unrecognized attribute
--                 return;
--           end;

--         --  Check if the attributes is for the Digest authenticatio schema.
--        --  AWS does not support othe authentication schemas with attributes
--           --  now.
--           if D.Auth_Mode /= Digest then
--              Quit := True;
--           end if;

--           case Attribute is
--              when Username  => D.Auth_Name     := +Value;
--              when Realm     => D.Auth_Realm    := +Value;
--              when NC        => D.Auth_NC       := +Value;
--              when CNonce    => D.Auth_CNonce   := +Value;
--              when QOP       => D.Auth_QOP      := +Value;
--              when Nonce     => D.Auth_Nonce    := +Value;
--              when Response  => D.Auth_Response := +Value;
--              when URI       => D.URI
--                := URL.Parse (Value, False, False);
--              when Algorithm =>
--                 if Value /= "MD5" then
--                    Ada.Exceptions.Raise_Exception
--                       (Constraint_Error'Identity,
--                        "Only MD5 algorithm is supported.");
--                 end if;
--           end case;
--        end Named_Value;

--        -----------
--        -- Value --
--        -----------

--        procedure Value (Item : String; Quit : in out Boolean) is
--           Upper_Item : constant String
--             := Ada.Characters.Handling.To_Upper (Item);
--        begin
--           if Upper_Item = "BASIC" then

--              D.Auth_Mode := Basic;

--              Quit := True;

--              --  We could not continue to parse Basic authentication
--              --  by the regular way, becouse next value is Base64
--              --  encoded username:password, it is possibe to be
--              --  symbol '=' there, our parser could
--              --  think that it is name/value delimiter.
--              declare
--                 use Ada.Streams;

--                 Auth_Str : constant String
--                   := Translator.To_String (Translator.Base64_Decode
--                     (Header_Value (Item'Length + 2 .. Header_Value'Last)));

--                 Delimit  : constant Natural := Fixed.Index (Auth_Str, ":");
--              begin
--                 if Delimit = 0 then
--                    D.Auth_Name := To_Unbounded_String (Auth_Str);

--                 else
--                    D.Auth_Name
--                      := To_Unbounded_String (Auth_Str (1 .. Delimit - 1));
--                    D.Auth_Password
--                      := To_Unbounded_String
--                           (Auth_Str (Delimit + 1 .. Auth_Str'Last));
--                 end if;
--              end;

--           elsif Upper_Item = "DIGEST" then
--              D.Auth_Mode := Digest;

--           end if;
--        end Value;

--        procedure Parse is new Headers.Values.Parse (Value, Named_Value);

--     begin
--        Parse (Header_Value);
--     end Authorization;

   ------------
   -- Binary --
   ------------

   procedure Binary
     (D         : in out Data;
      Parameter : Stream_Element_Array) is
   begin
      D.Binary_Data := new Stream_Element_Array'(Parameter);
   end Binary;

   ----------
   -- Free --
   ----------

   procedure Free (D : in out Data) is
   begin
      Utils.Free (D.Binary_Data);

      AWS.Parameters.Set.Free (D.Parameters);
--      AWS.Headers.Set.Free (D.Header);
   end Free;

   ----------------
   -- Keep_Alive --
   ----------------

   procedure Keep_Alive
     (D    : in out Data;
      Flag : Boolean) is
   begin
      D.Keep_Alive := Flag;
   end Keep_Alive;

   ----------------
   -- Parameters --
   ----------------

   procedure Parameters (D : in out Data; Set : AWS.Parameters.List) is
   begin
      D.Parameters := Set;
   end Parameters;

   -------------
   -- Payload --
   -------------

   procedure Payload
     (D       : in out Data;
      Payload : SOAP.Message.Payload.Object) is
   begin
      D.SOAP_Payload := Payload;
      D.SOAP_Action := True;
   end Payload;

   --------------
   -- Peername --
   --------------

   procedure Peername
     (D        : in out Data;
      Peername : String) is
   begin
      D.Peername := To_Unbounded_String (Peername);
   end Peername;

   -------------
   -- Request --
   -------------

   procedure Request
     (D            : in out Data;
      Method       : Request_Method;
      URI          : String;
      HTTP_Version : String) is
   begin
      D.Method       := Method;
      D.URI          := URL.Parse (URI, False, False);
      D.HTTP_Version := To_Unbounded_String (HTTP_Version);
   end Request;

   -----------
   -- Reset --
   -----------

   procedure Reset (D : in out Data) is
   begin
      Utils.Free (D.Binary_Data);

      D.Method            := GET;
      D.HTTP_Version      := Null_Unbounded_String;
      D.Content_Length    := 0;
      D.Auth_Mode         := None;
      D.Auth_Name         := Null_Unbounded_String;
      D.Auth_Password     := Null_Unbounded_String;
      D.Auth_Realm        := Null_Unbounded_String;
      D.Auth_Nonce        := Null_Unbounded_String;
      D.Auth_NC           := Null_Unbounded_String;
      D.Auth_CNonce       := Null_Unbounded_String;
      D.Auth_QOP          := Null_Unbounded_String;
      D.Auth_Response     := Null_Unbounded_String;
      D.Session_ID        := AWS.Session.No_Session;
      D.Session_Created   := False;

      AWS.Parameters.Set.Reset (D.Parameters);
--      AWS.Headers.Set.Reset (D.Header);
   end Reset;

   -------------
   -- Session --
   -------------

   procedure Session
     (D : in out Data) is
   begin
      D.Session_ID      := AWS.Session.Create;
      D.Session_Created := True;
   end Session;

end AWS.Status.Set;
