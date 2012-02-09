------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.SECURITY.AUTHENTICATION_MECHANISMS.GSSUP_CLIENT          --
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

with PolyORB.Buffers;
with PolyORB.Initialization;
with PolyORB.Representations.CDR.Common;
with PolyORB.Security.Credentials.Compound;
with PolyORB.Security.Credentials.GSSUP;
with PolyORB.Security.Exported_Names.GSSUP;
with PolyORB.Utils.Strings;

package body PolyORB.Security.Authentication_Mechanisms.GSSUP_Client is

   use Ada.Streams;
   use PolyORB.Buffers;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Security.Credentials;
   use PolyORB.Security.Credentials.Compound;
   use PolyORB.Security.Credentials.GSSUP;
   use PolyORB.Security.Exported_Names;
   use PolyORB.Security.Exported_Names.GSSUP;

   procedure Initialize;

   function Extract_GSSUP_Credentials
     (Credentials : Credentials_Ref)
      return GSSUP_Credentials_Access;
   --  Extract GSSUP specific credentials from compound credentials

   function Encode_Length (Length : Natural) return Stream_Element_Array;
   --  Encode item length in ASN.1 DER format

   function Create_Mechanism
     (Target_Name : PolyORB.Security.Exported_Names.Exported_Name_Access)
      return Client_Authentication_Mechanism_Access;

   Mechanism_OID         : PolyORB.ASN1.Object_Identifier;
   Encoded_Mechanism_OID : PolyORB.Security.Types.Stream_Element_Array_Access;

   ----------------------
   -- Create_Mechanism --
   ----------------------

   function Create_Mechanism
     (Target_Name : PolyORB.Security.Exported_Names.Exported_Name_Access)
      return Client_Authentication_Mechanism_Access
   is
   begin
      if Target_Name.all not in GSSUP_Exported_Name_Type then
         raise Program_Error;
      end if;

      return
        new GSSUP_Client_Authentication_Mechanism'
        (Target_Name       => Target_Name);
   end Create_Mechanism;

   -------------------
   -- Encode_Length --
   -------------------

   function Encode_Length (Length : Natural) return Stream_Element_Array is
   begin
      if Length >= 128 then
         raise Program_Error;
         --  XXX Only length < 128 bytes supported for now

      else
         return Stream_Element_Array'(1 .. 1 => Stream_Element (Length));
      end if;
   end Encode_Length;

   -------------------------------
   -- Extract_GSSUP_Credentials --
   -------------------------------

   function Extract_GSSUP_Credentials
     (Credentials : Credentials_Ref)
      return GSSUP_Credentials_Access
   is
      Creds : Credentials_Access
        := Credentials_Access (Entity_Of (Credentials));

   begin
      if Creds /= null then
         Creds :=
           Credentials_Access
           (Entity_Of
            (Get_Authentication_Credentials
             (Compound_Credentials_Access (Creds))));

         if Creds /= null
           and then Creds.all in GSSUP_Credentials'Class
         then
            return GSSUP_Credentials_Access (Creds);
         end if;

      end if;

      return null;
   end Extract_GSSUP_Credentials;

   ---------------------------
   -- Init_Security_Context --
   ---------------------------

   overriding function Init_Security_Context
     (Mechanism   : access GSSUP_Client_Authentication_Mechanism;
      Credentials :        PolyORB.Security.Credentials.Credentials_Ref)
      return Ada.Streams.Stream_Element_Array
   is
      pragma Unreferenced (Mechanism);

      Buffer : Buffer_Access := new Buffer_Type;
      Creds  : constant GSSUP_Credentials_Access
        := Extract_GSSUP_Credentials (Credentials);

   begin
      Start_Encapsulation (Buffer);

      Marshall_Latin_1_String (Buffer, Get_User_Name (Creds));
      Marshall_Latin_1_String (Buffer, Get_Password (Creds));
      Marshall (Buffer, Encode (Get_Target_Name (Creds)));

      declare
         Aux : constant Stream_Element_Array := Encapsulate (Buffer);

      begin
         Release (Buffer);

         return
           16#60#
           & Encode_Length (Encoded_Mechanism_OID'Length + Aux'Length)
           & Encoded_Mechanism_OID.all
           & Aux;
      end;
   end Init_Security_Context;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Mechanism_OID :=
        PolyORB.ASN1.To_Object_Identifier
        (PolyORB.Security.Types.GSSUPMechOID);
      Encoded_Mechanism_OID :=
        new Ada.Streams.Stream_Element_Array'
        (PolyORB.ASN1.Encode (Mechanism_OID));

      Register (Mechanism_OID, Create_Mechanism'Access);
   end Initialize;

   -----------------
   -- Is_Supports --
   -----------------

   overriding function Is_Supports
     (Mechanism   : access GSSUP_Client_Authentication_Mechanism;
      Credentials :        PolyORB.Security.Credentials.Credentials_Ref)
      return Boolean
   is
      use PolyORB.Security.Types;

      Creds  : constant GSSUP_Credentials_Access
        := Extract_GSSUP_Credentials (Credentials);

   begin
      return
        Creds /= null
        and then Is_Equivalent
        (Get_Target_Name (Creds), Mechanism.Target_Name);
   end Is_Supports;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      =>
          +"polyorb.security.authentication_mechanisms.gssup_client",
          Conflicts => PolyORB.Initialization.String_Lists.Empty,
          Depends   => PolyORB.Initialization.String_Lists.Empty,
          Provides  => PolyORB.Initialization.String_Lists.Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.Security.Authentication_Mechanisms.GSSUP_Client;
