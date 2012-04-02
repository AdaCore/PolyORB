------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.SECURITY.AUTHENTICATION_MECHANISMS.GSSUP_TARGET          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

with Ada.Strings.Fixed;
with Ada.Text_IO;

with PolyORB.Buffers;
with PolyORB.Errors;
with PolyORB.Initialization;
with PolyORB.Parameters;
with PolyORB.Representations.CDR.Common;
with PolyORB.Security.Identities.Principal_Name;
with PolyORB.Security.Exported_Names.GSSUP;
with PolyORB.Utils.Strings;

package body PolyORB.Security.Authentication_Mechanisms.GSSUP_Target is

   use Ada.Streams;
   use PolyORB.Buffers;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Security.Exported_Names;
   use PolyORB.Security.Exported_Names.GSSUP;

   procedure Initialize;

   function Create_Mechanism
     (Section_Name : String)
      return Target_Authentication_Mechanism_Access;

   type Error_Code is new PolyORB.Types.Unsigned_Long;

   GSS_UP_S_G_UNSPECIFIED  : constant Error_Code := 1;
   GSS_UP_S_G_NOUSER       : constant Error_Code := 2;
   GSS_UP_S_G_BAD_PASSWORD : constant Error_Code := 3;
   GSS_UP_S_G_BAD_TARGET   : constant Error_Code := 4;

   Encoded_Mechanism_OID : PolyORB.Security.Types.Stream_Element_Array_Access;

   -----------------------------
   -- Accept_Security_Context --
   -----------------------------

   overriding procedure Accept_Security_Context
     (Mechanism    : access GSSUP_Target_Authentication_Mechanism;
      Token        :        PolyORB.Security.Types.Stream_Element_Array_Access;
      Success      : out    Boolean;
      Return_Token : out
        PolyORB.Security.Types.Stream_Element_Array_Access;
      Identity     : out    PolyORB.Security.Identities.Identity_Access)
   is
      use PolyORB.Security.Types;
      use PolyORB.Types;

      procedure Create_Error_Token (Code : Error_Code);

      ------------------------
      -- Create_Error_Token --
      ------------------------

      procedure Create_Error_Token (Code : Error_Code) is
         Buffer : Buffer_Access := new Buffer_Type;

      begin
         Start_Encapsulation (Buffer);

         Marshall (Buffer, Unsigned_Long (Code));

         Return_Token := new Stream_Element_Array'(Encapsulate (Buffer));

         Release (Buffer);
      end Create_Error_Token;

      User_Name   : PolyORB.Types.String;
      Password    : PolyORB.Types.String;
      Target_Name : Exported_Name_Access;
      Buffer      : aliased Buffer_Type;
      First       : Stream_Element_Offset;
      Last        : Stream_Element_Offset;

   begin
      --  Extract mechanism OID

      if Token (Token'First + 1) < 128 then
         First := Token'First + 2;

      else
         raise Program_Error;
      end if;

      if Token (First + 1) < 128 then
         Last := First + Stream_Element_Offset (Token (First + 1)) + 2 - 1;

      else
         raise Program_Error;
      end if;

      --  Check is mechanism OID is valid

      if Token (First .. Last) /= Encoded_Mechanism_OID.all then
         Success := False;
         Create_Error_Token (GSS_UP_S_G_UNSPECIFIED);

         return;
      end if;

      --  Unmarshall Initial Token

      declare
         Data : aliased Encapsulation := Token (Last + 1 .. Token'Last);

      begin
         Decapsulate (Data'Access, Buffer'Access);

         --  User name and password are encoded as sequence<octet> in the
         --  token, but stored as strings internally.

         declare
            Binary : constant Stream_Element_Array
              := Unmarshall (Buffer'Access);
            Image  : String (1 .. Binary'Length);
            for Image'Address use Binary'Address;
            pragma Import (Ada, Image);

         begin
            User_Name := To_PolyORB_String (Image);
         end;

         declare
            Binary : constant Stream_Element_Array
              := Unmarshall (Buffer'Access);
            Image  : String (1 .. Binary'Length);
            for Image'Address use Binary'Address;
            pragma Import (Ada, Image);

         begin
            Password := To_PolyORB_String (Image);
         end;

         declare
            use PolyORB.Errors;

            Encoded_Name : constant Stream_Element_Array
              := Unmarshall (Buffer'Access);
            Error        : Error_Container;

         begin
            Decode (Encoded_Name, Target_Name, Error);

            if Found (Error) then
               raise Program_Error;
            end if;
         end;
      end;

      --  Check Target Name

      if not Is_Equivalent (Target_Name, Mechanism.Target_Name) then
         Success := False;
         Create_Error_Token (GSS_UP_S_G_BAD_TARGET);
         Release_Contents (Target_Name);

         return;
      end if;

      Release_Contents (Target_Name);

      --  Check password

      declare
         use Ada.Strings.Fixed;
         use Ada.Text_IO;
         use PolyORB.Security.Identities.Principal_Name;

         File      : File_Type;
         Buffer    : String (1 .. 1024);
         Last      : Natural;
         Delimiter : Natural;

      begin
         Open (File, In_File, To_Standard_String (Mechanism.Passwd_File));

         while not End_Of_File (File) loop
            Get_Line (File, Buffer, Last);

            Delimiter := Index (Buffer (1 .. Last), ":");

            if Delimiter /= 0 then
               if Buffer (1 .. Delimiter - 1) = User_Name then
                  if Buffer (Delimiter + 1 .. Last) = Password then
                     Success := True;
                     Close (File);

                     Identity :=
                       Create_Principal_Name_Identity
                       (Create_GSSUP_Exported_Name
                        (To_Standard_String (User_Name)));

                     return;

                  else
                     Success := False;
                     Create_Error_Token (GSS_UP_S_G_BAD_PASSWORD);
                     Close (File);

                     return;
                  end if;
               end if;
            end if;
         end loop;

         Close (File);
      end;

      Success := False;
      Create_Error_Token (GSS_UP_S_G_NOUSER);
   end Accept_Security_Context;

   ----------------------
   -- Create_Mechanism --
   ----------------------

   function Create_Mechanism
     (Section_Name : String)
      return Target_Authentication_Mechanism_Access
   is
      use PolyORB.Parameters;
      use PolyORB.Types;

      Target_Name : constant String
        := Get_Conf (Section_Name, "gssup.target_name", "");
      Passwd_File : constant String
        := Get_Conf (Section_Name, "gssup.passwd_file", "");

   begin
      if Target_Name = ""
        or else Passwd_File = ""
      then
         raise Program_Error;
      end if;

      return
        new GSSUP_Target_Authentication_Mechanism'
        (Mechanism_OID     =>
           PolyORB.ASN1.To_Object_Identifier
           (PolyORB.Security.Types.GSSUPMechOID),
         Target_Name       => Create_GSSUP_Exported_Name (Target_Name),
         Identity_Types    => PolyORB.Security.Types.ITT_Principal_Name,
         Naming_Mechanisms =>
           PolyORB.Security.Types.OID_Lists."+"
           (PolyORB.ASN1.To_Object_Identifier
            (PolyORB.Security.Types.GSSUPMechOID)),
         Passwd_File       => To_PolyORB_String (Passwd_File));
   end Create_Mechanism;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Encoded_Mechanism_OID :=
        new Ada.Streams.Stream_Element_Array'
        (PolyORB.ASN1.Encode
         (PolyORB.ASN1.To_Object_Identifier
          (PolyORB.Security.Types.GSSUPMechOID)));

      Register ("gssup", Create_Mechanism'Access);
   end Initialize;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      =>
          +"polyorb.security.authentication_mechanisms.gssup_target",
          Conflicts => PolyORB.Initialization.String_Lists.Empty,
          Depends   => PolyORB.Initialization.String_Lists.Empty,
          Provides  => PolyORB.Initialization.String_Lists.Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.Security.Authentication_Mechanisms.GSSUP_Target;
