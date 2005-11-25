------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . S S L                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Interfaces.C.Strings;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.SSL is

   use type Interfaces.C.int;

   package Thin is

      type SSL_Error_Code is new Interfaces.C.unsigned_long;
      type SSL_Verify_Mode is new Interfaces.C.unsigned_long;

      SSL_Verify_None                 : constant SSL_Verify_Mode := 0;
      SSL_Verify_Peer                 : constant SSL_Verify_Mode := 1;
      SSL_Verify_Fail_If_No_Peer_Cert : constant SSL_Verify_Mode := 2;
      SSL_Verify_Client_Once          : constant SSL_Verify_Mode := 4;

      type SSL_Method is private;

      type X509_STORE_CTX is private;

      type Stack_Of_X509_NAME is private;
      No_Stack_Of_X509_NAME : constant Stack_Of_X509_NAME;

      type SSL_Verify_Callback is
        access function
        (Preverify : in Interfaces.C.int;
         Ctx       : in X509_STORE_CTX)
         return Interfaces.C.int;
      pragma Convention (C, SSL_Verify_Callback);

      type Stack_Of_SSL_Cipher is private;
      No_Stack_Of_SSL_Cipher : constant Stack_Of_SSL_Cipher;

      --  General initialization subprograms

      procedure SSL_library_init;

      procedure SSL_load_error_strings;

      --  Context subprograms

      function SSL_CTX_new (Method : in SSL_Method) return SSL_Context_Type;

      procedure SSL_CTX_free (Context : in SSL_Context_Type);

      function SSL_CTX_use_certificate_file
        (Ctx  : in SSL_Context_Type;
         File : in Interfaces.C.char_array;
         T    : in Interfaces.C.int)
         return Interfaces.C.int;

      function SSL_CTX_use_PrivateKey_file
        (Ctx  : in SSL_Context_Type;
         File : in Interfaces.C.char_array;
         T    : in Interfaces.C.int)
         return Interfaces.C.int;

      function SSL_CTX_check_private_key
        (Ctx : in SSL_Context_Type)
         return Interfaces.C.int;

      procedure SSL_CTX_set_verify
        (Ctx      : in SSL_Context_Type;
         Mode     : in SSL_Verify_Mode;
         Callback : in SSL_Verify_Callback := null);

      procedure SSL_CTX_set_client_CA_list
        (Ctx  : in SSL_Context_Type;
         List : in Stack_Of_X509_NAME);

      function SSL_CTX_load_verify_locations
        (Ctx    : in SSL_Context_Type;
         CAFile : in Interfaces.C.Strings.chars_ptr;
         CAPath : in Interfaces.C.Strings.chars_ptr)
         return Interfaces.C.int;

      function SSL_CTX_set_default_verify_paths
        (Ctx : in SSL_Context_Type)
         return Interfaces.C.int;

      function SSL_CTX_get_verify_mode
        (Ctx : in SSL_Context_Type)
         return SSL_Verify_Mode;

      --  Methods

      function SSLv2_method return SSL_Method;
      function SSLv2_client_method return SSL_Method;
      function SSLv2_server_method return SSL_Method;

      function SSLv23_method return SSL_Method;
      function SSLv23_client_method return SSL_Method;
      function SSLv23_server_method return SSL_Method;

      function SSLv3_method return SSL_Method;
      function SSLv3_client_method return SSL_Method;
      function SSLv3_server_method return SSL_Method;

      function TLSv1_method return SSL_Method;
      function TLSv1_client_method return SSL_Method;
      function TLSv1_server_method return SSL_Method;

      --  Sockets subprograms

      function SSL_new (Ctx : in SSL_Context_Type) return SSL_Socket_Type;

      procedure SSL_free (Context : in SSL_Socket_Type);

      function SSL_set_fd
        (SSL : in SSL_Socket_Type;
         FD  : in Sockets.Socket_Type)
         return Interfaces.C.int;

      function SSL_get_fd
        (SSL : in SSL_Socket_Type)
         return Sockets.Socket_Type;

      function SSL_accept (SSL : in SSL_Socket_Type) return Interfaces.C.int;

      function SSL_connect (SSL : in SSL_Socket_Type) return Interfaces.C.int;

      function SSL_pending (SSL : in SSL_Socket_Type) return Interfaces.C.int;

      function SSL_shutdown (SSL : in SSL_Socket_Type) return Interfaces.C.int;

      function SSL_read
        (SSL    : in SSL_Socket_Type;
         Buffer : in Sockets.Stream_Element_Reference;
         Length : in Interfaces.C.int)
         return Interfaces.C.int;

      function SSL_write
        (SSL    : in SSL_Socket_Type;
         Buffer : in Sockets.Stream_Element_Reference;
         Length : in Interfaces.C.int)
         return Interfaces.C.int;

      function SSL_get_ciphers
        (SSL    : in SSL_Socket_Type)
         return Stack_Of_SSL_Cipher;

      --  Error handling subprograms

      function ERR_get_error return SSL_Error_Code;

      function ERR_error_string (Error_Code : in SSL_Error_Code) return String;

      --  Others subprograms

      function SSL_load_client_CA_file (File : in Interfaces.C.char_array)
         return Stack_Of_X509_NAME;

      --  Ciphers subprograms

      function SSL_CIPHER_description (Cipher : in SSL_Cipher_Type)
        return String;

      --  Stack subprograms

      function sk_SSL_CIPHER_num
        (Stack : in Stack_Of_SSL_Cipher)
         return Interfaces.C.int;

      function sk_SSL_CIPHER_value
        (Stack : in Stack_Of_SSL_Cipher;
         Index : in Interfaces.C.int)
         return SSL_Cipher_Type;

   private

      pragma Linker_Options ("-lssl");
      pragma Linker_Options ("-lcrypto");

      type SSL_Method_Record is null record;
      pragma Convention (C, SSL_Method_Record);

      type SSL_Method is access all SSL_Method_Record;

      type X509_STORE_CTX_Record is null record;
      pragma Convention (C, X509_STORE_CTX_Record);

      type X509_STORE_CTX is access all X509_STORE_CTX_Record;

      type Stack_Of_X509_NAME_Record is null record;

      type Stack_Of_X509_NAME is access all Stack_Of_X509_NAME_Record;
      No_Stack_Of_X509_NAME : constant Stack_Of_X509_NAME := null;

      type Stack_Of_SSL_Cipher_Record is null record;
      pragma Convention (C, Stack_Of_SSL_Cipher_Record);

      type Stack_Of_SSL_Cipher is access all Stack_Of_SSL_Cipher_Record;
      No_Stack_Of_SSL_Cipher : constant Stack_Of_SSL_Cipher := null;

      pragma Import (C, ERR_get_error, "ERR_get_error");
      pragma Import (C, SSL_CTX_check_private_key,
                       "SSL_CTX_check_private_key");
      pragma Import (C, SSL_CTX_free, "SSL_CTX_free");
      pragma Import (C, SSL_CTX_get_verify_mode, "SSL_CTX_get_verify_mode");
      pragma Import (C, SSL_CTX_load_verify_locations,
                       "SSL_CTX_load_verify_locations");
      pragma Import (C, SSL_CTX_new, "SSL_CTX_new");
      pragma Import (C, SSL_CTX_set_client_CA_list,
                       "SSL_CTX_set_client_CA_list");
      pragma Import (C, SSL_CTX_set_default_verify_paths,
                       "SSL_CTX_set_default_verify_paths");
      pragma Import (C, SSL_CTX_set_verify, "SSL_CTX_set_verify");
      pragma Import (C, SSL_CTX_use_certificate_file,
                       "SSL_CTX_use_certificate_file");
      pragma Import (C, SSL_CTX_use_PrivateKey_file,
                       "SSL_CTX_use_PrivateKey_file");

      pragma Import (C, SSL_accept, "SSL_accept");
      pragma Import (C, SSL_connect, "SSL_connect");
      pragma Import (C, SSL_free, "SSL_free");
      pragma Import (C, SSL_get_fd, "SSL_get_fd");
      pragma Import (C, SSL_get_ciphers, "SSL_get_ciphers");
      pragma Import (C, SSL_library_init, "SSL_library_init");
      pragma Import (C, SSL_load_error_strings, "SSL_load_error_strings");
      pragma Import (C, SSL_load_client_CA_file, "SSL_load_client_CA_file");
      pragma Import (C, SSL_new, "SSL_new");
      pragma Import (C, SSL_pending, "SSL_pending");
      pragma Import (C, SSL_read, "SSL_read");
      pragma Import (C, SSL_set_fd, "SSL_set_fd");
      pragma Import (C, SSL_shutdown, "SSL_shutdown");
      pragma Import (C, SSL_write, "SSL_write");

      pragma Import (C, SSLv2_method, "SSLv2_method");
      pragma Import (C, SSLv2_client_method, "SSLv2_client_method");
      pragma Import (C, SSLv2_server_method, "SSLv2_server_method");
      pragma Import (C, SSLv23_method, "SSLv23_method");
      pragma Import (C, SSLv23_client_method, "SSLv23_client_method");
      pragma Import (C, SSLv23_server_method, "SSLv23_server_method");
      pragma Import (C, SSLv3_method, "SSLv3_method");
      pragma Import (C, SSLv3_client_method, "SSLv3_client_method");
      pragma Import (C, SSLv3_server_method, "SSLv3_server_method");
      pragma Import (C, TLSv1_method, "TLSv1_method");
      pragma Import (C, TLSv1_client_method, "TLSv1_client_method");
      pragma Import (C, TLSv1_server_method, "TLSv1_server_method");

      pragma Import (C, sk_SSL_CIPHER_num, "__PolyORB_sk_SSL_CIPHER_num");
      pragma Import (C, sk_SSL_CIPHER_value, "__PolyORB_sk_SSL_CIPHER_value");

   end Thin;

   --  Library initialization

   procedure Initialize;
   --  Initialize must be called before using any other SSL socket routines

   function To_SSL_Verify_Mode
     (Value : SSL_Verification_Mode) return Thin.SSL_Verify_Mode;
   --  Convert user friendly SSL_Verification_Mode structure into SSL internal
   --  representation.

   function To_SSL_Verification_Mode
     (Value : Thin.SSL_Verify_Mode) return SSL_Verification_Mode;
   --  Convert SSL internal representation of SSL_Verify_Mode into
   --  SSL_Verification_Mode

   -------------------
   -- Accept_Socket --
   -------------------

   procedure Accept_Socket
     (Server  : Sockets.Socket_Type;
      Context : SSL_Context_Type;
      Socket  : out SSL_Socket_Type;
      Address : out Sockets.Sock_Addr_Type)
   is
      Sock : Sockets.Socket_Type;
   begin
      Sockets.Accept_Socket (Server, Sock, Address);

      Socket := Thin.SSL_new (Context);
      if Socket = null then
         Ada.Exceptions.Raise_Exception
           (SSL_Error'Identity, Get_Errors_String);
      end if;

      if Thin.SSL_set_fd (Socket, Sock) /= 1 then
         Thin.SSL_free (Socket);
         Ada.Exceptions.Raise_Exception
           (SSL_Error'Identity, Get_Errors_String);
      end if;

      if Thin.SSL_accept (Socket) /= 1 then
         Thin.SSL_free (Socket);
         Ada.Exceptions.Raise_Exception
           (SSL_Error'Identity, Get_Errors_String);
      end if;
   end Accept_Socket;

   ----------------
   -- Ciphers_Of --
   ----------------

   function Ciphers_Of
    (Context : in SSL_Context_Type)
     return SSL_Cipher_Array
   is
      Socket : constant SSL_Socket_Type := Thin.SSL_new (Context);
      Result : constant SSL_Cipher_Array := Ciphers_Of (Socket);

   begin
      Thin.SSL_free (Socket);
      return Result;
   end Ciphers_Of;

   function Ciphers_Of (Socket : in SSL_Socket_Type) return SSL_Cipher_Array is
      use type Thin.Stack_Of_SSL_Cipher;

      Stack : constant Thin.Stack_Of_SSL_Cipher
        := Thin.SSL_get_ciphers (Socket);

   begin
      if Stack = Thin.No_Stack_Of_SSL_Cipher then
         return SSL_Cipher_Array'(1 .. 0 => No_SSL_Cipher);
      end if;

      declare
         Length : constant Interfaces.C.int := Thin.sk_SSL_CIPHER_num (Stack);
         Result : SSL_Cipher_Array (1 .. Integer (Length));
      begin
         for J in 0 .. Length - 1 loop
            Result (Natural (J + 1)) := Thin.sk_SSL_CIPHER_value (Stack, J);
         end loop;

         return Result;
      end;
   end Ciphers_Of;

   ------------------
   -- Close_Socket --
   ------------------

   procedure Close_Socket (Socket : in SSL_Socket_Type) is
      Status : Interfaces.C.int;

   begin
      --  Shutdown procedure may not complete in one call, thus call it
      --  again until it return complete or error status

      loop
         Status := Thin.SSL_shutdown (Socket);

         exit when Status = 1;

         if Status /= 0 then
            Ada.Exceptions.Raise_Exception
              (SSL_Error'Identity, Get_Errors_String);
         end if;
      end loop;

      Sockets.Close_Socket (Socket_Of (Socket));
      Thin.SSL_free (Socket);
   end Close_Socket;

   --------------------
   -- Connect_Socket --
   --------------------

   procedure Connect_Socket
     (Sock    : in     Sockets.Socket_Type;
      Context : in     SSL_Context_Type;
      Socket  :    out SSL_Socket_Type;
      Address : in out Sockets.Sock_Addr_Type)
   is
   begin
      Sockets.Connect_Socket (Sock, Address);

      Socket := Thin.SSL_new (Context);
      if Socket = null then
         Ada.Exceptions.Raise_Exception
           (SSL_Error'Identity, Get_Errors_String);
      end if;

      if Thin.SSL_set_fd (Socket, Sock) /= 1 then
         Thin.SSL_free (Socket);
         Ada.Exceptions.Raise_Exception
           (SSL_Error'Identity, Get_Errors_String);
      end if;

      if Thin.SSL_connect (Socket) /= 1 then
         Thin.SSL_free (Socket);
         Ada.Exceptions.Raise_Exception
           (SSL_Error'Identity, Get_Errors_String);
      end if;
   end Connect_Socket;

   --------------------
   -- Create_Context --
   --------------------

   procedure Create_Context
     (Context                    :    out SSL_Context_Type;
      Method                     : in     SSL_Method_Type;
      Private_Key_File           : in     String;
      Certificate_File           : in     String;
      CA_File                    : in     String := "";
      CA_Path                    : in     String := "";
      Verification_Mode          : in     SSL_Verification_Mode
        := (others => False))
   is
      M : Thin.SSL_Method;

   begin
      case Method is
         when SSL_2 =>
            M := Thin.SSLv2_method;

         when SSL_2_Client =>
            M := Thin.SSLv2_client_method;

         when SSL_2_Server =>
            M := Thin.SSLv2_server_method;

         when SSL_3 =>
            M := Thin.SSLv3_method;

         when SSL_3_Client =>
            M := Thin.SSLv3_client_method;

         when SSL_3_Server =>
            M := Thin.SSLv3_server_method;

         when TLS_1 =>
            M := Thin.TLSv1_method;

         when TLS_1_Client =>
            M := Thin.TLSv1_client_method;

         when TLS_1_Server =>
            M := Thin.TLSv1_server_method;

         when Any =>
            M := Thin.SSLv23_method;

         when Any_Client =>
            M := Thin.SSLv23_client_method;

         when Any_Server =>
            M := Thin.SSLv23_server_method;

      end case;

      Context := Thin.SSL_CTX_new (M);

      if Context = null then
         Ada.Exceptions.Raise_Exception
           (SSL_Error'Identity, Get_Errors_String);
      end if;

      --  Set certificate verification level

      Thin.SSL_CTX_set_verify
        (Context, To_SSL_Verify_Mode (Verification_Mode));

      --  Setting up locations for trusted CA certificates

      if CA_File /= "" or else CA_Path /= "" then
         declare
            File   : Interfaces.C.Strings.chars_ptr
              := Interfaces.C.Strings.Null_Ptr;
            Path   : Interfaces.C.Strings.chars_ptr
              := Interfaces.C.Strings.Null_Ptr;
            Status : Interfaces.C.int;
         begin
            if CA_File /= "" then
               File := Interfaces.C.Strings.New_String (CA_File);
            end if;

            if CA_Path /= "" then
               Path := Interfaces.C.Strings.New_String (CA_Path);
            end if;

            Status :=
              Thin.SSL_CTX_load_verify_locations (Context, File, Path);

            Interfaces.C.Strings.Free (File);
            Interfaces.C.Strings.Free (Path);

            if Status /= 1 then
               Ada.Exceptions.Raise_Exception
                 (SSL_Error'Identity, Get_Errors_String);
            end if;
         end;

      else
         if Thin.SSL_CTX_set_default_verify_paths (Context) /= 1 then
            Ada.Exceptions.Raise_Exception
              (SSL_Error'Identity, Get_Errors_String);
         end if;
      end if;

      --  Loading Certificate and Private Key files only if both are specified

      if Certificate_File = "" or Private_Key_File = "" then
         return;
      end if;

      --  Loading Certificate file

      if Thin.SSL_CTX_use_certificate_file
          (Context, Interfaces.C.To_C (Certificate_File), 1) /= 1
      then
         Thin.SSL_CTX_free (Context);
         Ada.Exceptions.Raise_Exception
           (SSL_Error'Identity, Get_Errors_String);
      end if;

      --  Loading Private Key file

      if Thin.SSL_CTX_use_PrivateKey_file
          (Context, Interfaces.C.To_C (Private_Key_File), 1) /= 1
      then
         Thin.SSL_CTX_free (Context);
         Ada.Exceptions.Raise_Exception
           (SSL_Error'Identity, Get_Errors_String);
      end if;

      --  Check consistency of Certificate and Private Key

      if Thin.SSL_CTX_check_private_key (Context) /= 1 then
         Thin.SSL_CTX_free (Context);
         Ada.Exceptions.Raise_Exception
           (SSL_Error'Identity, Get_Errors_String);
      end if;
   end Create_Context;

   --------------------
   -- Description_Of --
   --------------------

   function Description_Of (Cipher : in SSL_Cipher_Type) return String
     renames Thin.SSL_CIPHER_description;

   ---------------------
   -- Destroy_Context --
   ---------------------

   procedure Destroy_Context (Context : in SSL_Context_Type) is
   begin
      Thin.SSL_CTX_free (Context);
   end Destroy_Context;

   -----------------------
   -- Get_Errors_String --
   -----------------------

   function Get_Errors_String return String is
      use type Thin.SSL_Error_Code;

      Error : constant Thin.SSL_Error_Code := Thin.ERR_get_error;

   begin
      if Error /= 0 then
         return Get_Errors_String & Thin.ERR_error_string (Error);
      else
         return "";
      end if;
   end Get_Errors_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Thin.SSL_load_error_strings;
      Thin.SSL_library_init;
      --  XXX actions_to_seed_PRNG
   end Initialize;

   --------------------
   -- Load_Client_CA --
   --------------------

   procedure Load_Client_CA
     (Context : in SSL_Context_Type;
      CA_File : in String)
   is
      use type Thin.Stack_Of_X509_NAME;

      List : constant Thin.Stack_Of_X509_NAME
        := Thin.SSL_load_client_CA_file (Interfaces.C.To_C (CA_File));

   begin
      if List = Thin.No_Stack_Of_X509_NAME then
         Ada.Exceptions.Raise_Exception
           (SSL_Error'Identity, Get_Errors_String);
      end if;

      Thin.SSL_CTX_set_client_CA_list (Context, List);
   end Load_Client_CA;

   --------------------
   -- Pending_Length --
   --------------------

   function Pending_Length (Socket : in SSL_Socket_Type) return Natural is
   begin
      return Natural (Thin.SSL_pending (Socket));
   end Pending_Length;

   --------------------
   -- Receive_Vector --
   --------------------

   procedure Receive_Vector
     (Socket : in     SSL_Socket_Type;
      Vector : in     Sockets.Vector_Type;
      Count  :    out Ada.Streams.Stream_Element_Count)
   is
      use type Ada.Streams.Stream_Element_Count;

      Bytes_Readed : Interfaces.C.int;

   begin
      Count := 0;

      for J in Vector'Range loop
         Bytes_Readed :=
           Thin.SSL_read
           (Socket, Vector (J).Base, Interfaces.C.int (Vector (J).Length));

         if Bytes_Readed <= 0 then
            Ada.Exceptions.Raise_Exception
              (SSL_Error'Identity, Get_Errors_String);
         end if;

         Count := Count + Ada.Streams.Stream_Element_Count (Bytes_Readed);

         if Bytes_Readed < Interfaces.C.int (Vector (J).Length) then
            --  Where are no more data for reading. Exiting.
            return;
         end if;
      end loop;
   end Receive_Vector;

   -----------------
   -- Send_Vector --
   -----------------

   procedure Send_Vector
     (Socket : in     SSL_Socket_Type;
      Vector : in     Sockets.Vector_Type;
      Count  :    out Ada.Streams.Stream_Element_Count)
   is
      use type Ada.Streams.Stream_Element_Count;

      Bytes_Written : Interfaces.C.int;

   begin
      Count := 0;

      for J in Vector'Range loop
         Bytes_Written :=
           Thin.SSL_write
           (Socket, Vector (J).Base, Interfaces.C.int (Vector (J).Length));

         if Bytes_Written <= 0 then
            Ada.Exceptions.Raise_Exception
              (SSL_Error'Identity, Get_Errors_String);
         end if;

         Count := Count + Ada.Streams.Stream_Element_Count (Bytes_Written);

         if Bytes_Written < Interfaces.C.int (Vector (J).Length) then
            --  The actually written number of bytes differ from requested
            --  number. The operation was successful, but incomplete for some
            --  reasons. Report this to caller.
            return;
         end if;
      end loop;
   end Send_Vector;

   ---------------
   -- Socket_Of --
   ---------------

   function Socket_Of (Socket : in SSL_Socket_Type)
     return Sockets.Socket_Type
   is
   begin
      return Thin.SSL_get_fd (Socket);
   end Socket_Of;

   ----------
   -- Thin --
   ----------

   package body Thin is

      ----------------------
      -- ERR_error_string --
      ----------------------

      function ERR_error_string
        (Error_Code : in SSL_Error_Code)
         return String
      is
         procedure ERR_error_string_n
           (Error_Code : in SSL_Error_Code;
            Buf        : in Interfaces.C.char_array;
            Len        : in Interfaces.C.size_t);
         pragma Import (C, ERR_error_string_n, "ERR_error_string_n");

         Buffer : Interfaces.C.char_array (1 .. 1024);
         pragma Warnings (Off, Buffer);
         --  Buffer not needed to be initialized and modified, because
         --  of side effect of C function ERR_error_string_n

      begin
         ERR_error_string_n (Error_Code, Buffer, Buffer'Length);
         return Interfaces.C.To_Ada (Buffer);
      end ERR_error_string;

      ----------------------------
      -- SSL_CIPHER_description --
      ----------------------------

      function SSL_CIPHER_description (Cipher : in SSL_Cipher_Type)
        return String
      is
         procedure SSL_CIPHER_description
           (Cipher : in SSL_Cipher_Type;
            Buf    : in Interfaces.C.char_array;
            Size   : in Interfaces.C.int);
         pragma Import (C, SSL_CIPHER_description, "SSL_CIPHER_description");

         Buffer : Interfaces.C.char_array (1 .. 512);
         pragma Warnings (Off, Buffer);
         --  Buffer not needed to be initialized and modified, because
         --  of side effect of C function SSL_CIPHER_description

      begin
         SSL_CIPHER_description (Cipher, Buffer, Buffer'Length);
         return Interfaces.C.To_Ada (Buffer);
      end SSL_CIPHER_description;

   end Thin;

   ------------------------
   -- To_SSL_Verify_Mode --
   ------------------------

   function To_SSL_Verify_Mode
     (Value : in SSL_Verification_Mode)
      return Thin.SSL_Verify_Mode
   is
      use type Thin.SSL_Verify_Mode;

      Result : Thin.SSL_Verify_Mode := Thin.SSL_Verify_None;

   begin
      if Value (Peer) then
         Result := Thin.SSL_Verify_Peer;

         if Value (Fail_If_No_Peer_Certificate) then
            Result := Result or Thin.SSL_Verify_Fail_If_No_Peer_Cert;
         end if;

         if Value (Client_Once) then
            Result := Result or Thin.SSL_Verify_Client_Once;
         end if;
      end if;

      return Result;
   end To_SSL_Verify_Mode;

   ------------------------------
   -- To_SSL_Verification_Mode --
   ------------------------------

   function To_SSL_Verification_Mode
     (Value : in Thin.SSL_Verify_Mode)
      return SSL_Verification_Mode
   is
      use type Thin.SSL_Verify_Mode;

      Result : SSL_Verification_Mode := (others => False);

   begin
      if (Value and Thin.SSL_Verify_Peer) = Thin.SSL_Verify_Peer then
         Result (Peer) := True;

         if (Value and Thin.SSL_Verify_Fail_If_No_Peer_Cert)
           = Thin.SSL_Verify_Fail_If_No_Peer_Cert
         then
            Result (Fail_If_No_Peer_Certificate) := True;
         end if;

         if (Value and Thin.SSL_Verify_Client_Once)
           = Thin.SSL_Verify_Client_Once
         then
            Result (Client_Once) := True;
         end if;
      end if;

      return Result;
   end To_SSL_Verification_Mode;

   --------------------------
   -- Verification_Mode_Of --
   --------------------------

   function Verification_Mode_Of
     (Context : in SSL_Context_Type)
      return SSL_Verification_Mode
   is
   begin
      return To_SSL_Verification_Mode (Thin.SSL_CTX_get_verify_mode (Context));
   end Verification_Mode_Of;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"ssl",
          Conflicts => Empty,
          Depends   => +"sockets",
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access));
   end;
end PolyORB.SSL;
