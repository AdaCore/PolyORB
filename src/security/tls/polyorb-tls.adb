------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . T L S                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2009, Free Software Foundation, Inc.          --
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

with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Interfaces.C.Strings;

with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Utils.Strings;

with PolyORB.Platform.SSL_Linker_Options;
pragma Warnings (Off, PolyORB.Platform.SSL_Linker_Options);
--  No entity referenced

package body PolyORB.TLS is

   use PolyORB.Log;
   use type Interfaces.C.int;
   use type Interfaces.C.Strings.chars_ptr;

   package L is new PolyORB.Log.Facility_Log ("polyorb.tls");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   package Thin is

      type SSL_Method is private;

      type SSL_Error_Code is new Interfaces.C.unsigned_long;

      type SSL_Verify_Mode is new Interfaces.C.unsigned_long;

      SSL_Verify_None                 : constant SSL_Verify_Mode := 0;
      SSL_Verify_Peer                 : constant SSL_Verify_Mode := 1;
      SSL_Verify_Fail_If_No_Peer_Cert : constant SSL_Verify_Mode := 2;
      SSL_Verify_Client_Once          : constant SSL_Verify_Mode := 4;

      type SSL_File_Type is private;

      PEM  : constant SSL_File_Type;
--      ASN1 : constant SSL_File_Type;

      type Stack_Of_SSL_Cipher is private;
      No_Stack_Of_SSL_Cipher : constant Stack_Of_SSL_Cipher;

      --  Callbacks

      type SSL_Verify_Callback is
        access function
--        (Preverify : Interfaces.C.int;
--         Ctx       : X509_STORE_CTX)
         return Interfaces.C.int;
      pragma Convention (C, SSL_Verify_Callback);

      --  Context subprograms

      function SSL_CTX_new (Method : SSL_Method) return TLS_Context_Type;
      procedure SSL_CTX_free (Context : TLS_Context_Type);

      function SSL_CTX_use_certificate
        (Context     : TLS_Context_Type;
         Certificate : PolyORB.X509.Certificate) return Interfaces.C.int;

      function SSL_CTX_use_certificate_file
        (Context     : TLS_Context_Type;
         File        : Interfaces.C.char_array;
         Format_Type : SSL_File_Type) return Interfaces.C.int;

      function SSL_CTX_use_certificate_chain_file
        (Context : TLS_Context_Type;
         File    : Interfaces.C.char_array) return Interfaces.C.int;

      function SSL_CTX_use_PrivateKey_file
        (Context     : TLS_Context_Type;
         File        : Interfaces.C.char_array;
         Format_Type : SSL_File_Type) return Interfaces.C.int;

      function SSL_CTX_check_private_key
        (Context : TLS_Context_Type) return Interfaces.C.int;

      function SSL_CTX_load_verify_locations
        (Context : TLS_Context_Type;
         CA_File : Interfaces.C.char_array;
         CA_Path : Interfaces.C.char_array) return Interfaces.C.int;

      function SSL_CTX_load_verify_locations
        (Context : TLS_Context_Type;
         CA_File : Interfaces.C.Strings.chars_ptr;
         CA_Path : Interfaces.C.char_array) return Interfaces.C.int;

      function SSL_CTX_load_verify_locations
        (Context : TLS_Context_Type;
         CA_File : Interfaces.C.char_array;
         CA_Path : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;

      function SSL_CTX_load_verify_locations
        (Context : TLS_Context_Type;
         CA_File : Interfaces.C.Strings.chars_ptr;
         CA_Path : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;

      procedure SSL_CTX_set_verify
        (Ctx      : TLS_Context_Type;
         Mode     : SSL_Verify_Mode;
         Callback : SSL_Verify_Callback);

      function SSL_CTX_set_cipher_list
        (Context : TLS_Context_Type;
         Ciphers : Interfaces.C.char_array) return Interfaces.C.int;

      --  Cipher subprograms

      function SSL_CIPHER_description (Cipher : TLS_Cipher_Type) return String;

      --  Cipher stack subprogram

      function sk_SSL_CIPHER_num
        (Stack : Stack_Of_SSL_Cipher) return Interfaces.C.int;

      function sk_SSL_CIPHER_value
        (Stack : Stack_Of_SSL_Cipher;
         Index : Interfaces.C.int) return TLS_Cipher_Type;

      --  Socket subprograms

      function SSL_new (Context : TLS_Context_Type) return TLS_Socket_Type;

      procedure SSL_free (Socket : TLS_Socket_Type);

      function SSL_get_cipher_list
        (Socket   : TLS_Socket_Type;
         Priority : Interfaces.C.int)
         return Interfaces.C.Strings.chars_ptr;

      function SSL_get_ciphers
        (Socket : TLS_Socket_Type) return Stack_Of_SSL_Cipher;

      function SSL_get_peer_certificate
        (Socket : TLS_Socket_Type) return PolyORB.X509.Certificate;

      function SSL_get_fd
        (SSL : TLS_Socket_Type)
         return Sockets.Socket_Type;

      function SSL_set_fd
        (Socket : TLS_Socket_Type;
         FD     : Sockets.Socket_Type) return Interfaces.C.int;

      function SSL_connect (Socket : TLS_Socket_Type) return Interfaces.C.int;

      function SSL_accept (Socket : TLS_Socket_Type) return Interfaces.C.int;

      function SSL_pending (SSL : TLS_Socket_Type) return Interfaces.C.int;

      function SSL_read
        (Socket : TLS_Socket_Type;
         Buffer : Sockets.Stream_Element_Reference;
         Length : Interfaces.C.int)
         return Interfaces.C.int;

      function SSL_write
        (Socket : TLS_Socket_Type;
         Buffer : Sockets.Stream_Element_Reference;
         Length : Interfaces.C.int)
         return Interfaces.C.int;

      function SSL_shutdown (SSL : TLS_Socket_Type) return Interfaces.C.int;

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

      --  Error handling

      function ERR_get_error return SSL_Error_Code;

      function ERR_error_string (Error_Code : SSL_Error_Code) return String;

      --  Library initialization

      procedure SSL_library_init;

      procedure SSL_load_error_strings;

      --  PolyORB extensions

   private

      type SSL_Method_Record is null record;
      pragma Convention (C, SSL_Method_Record);

      type SSL_Method is access all SSL_Method_Record;

      type SSL_File_Type is new Interfaces.C.int;

      PEM  : constant SSL_File_Type := 1;
--      ASN1 : constant SSL_File_Type := 2;

      type Stack_Of_SSL_Cipher_Record is null record;
      pragma Convention (C, Stack_Of_SSL_Cipher_Record);

      type Stack_Of_SSL_Cipher is access all Stack_Of_SSL_Cipher_Record;

      No_Stack_Of_SSL_Cipher : constant Stack_Of_SSL_Cipher := null;

      pragma Import (C, ERR_get_error,            "ERR_get_error");
      pragma Import (C, SSL_CTX_check_private_key,
                     "SSL_CTX_check_private_key");
      pragma Import (C, SSL_CTX_free,             "SSL_CTX_free");
      pragma Import (C, SSL_CTX_load_verify_locations,
                     "SSL_CTX_load_verify_locations");
      pragma Import (C, SSL_CTX_new,              "SSL_CTX_new");
      pragma Import (C, SSL_CTX_set_cipher_list,  "SSL_CTX_set_cipher_list");
      pragma Import (C, SSL_CTX_set_verify,       "SSL_CTX_set_verify");
      pragma Import (C, SSL_CTX_use_PrivateKey_file,
                     "SSL_CTX_use_PrivateKey_file");
      pragma Import (C, SSL_CTX_use_certificate,  "SSL_CTX_use_certificate");
      pragma Import (C, SSL_CTX_use_certificate_chain_file,
                     "SSL_CTX_use_certificate_chain_file");
      pragma Import (C, SSL_CTX_use_certificate_file,
                     "SSL_CTX_use_certificate_file");
      pragma Import (C, SSL_accept,               "SSL_accept");
      pragma Import (C, SSL_connect,              "SSL_connect");
      pragma Import (C, SSL_free,                 "SSL_free");
      pragma Import (C, SSL_get_cipher_list,      "SSL_get_cipher_list");
      pragma Import (C, SSL_get_ciphers,          "SSL_get_ciphers");
      pragma Import (C, SSL_get_fd,               "SSL_get_fd");
      pragma Import (C, SSL_get_peer_certificate, "SSL_get_peer_certificate");
      pragma Import (C, SSL_library_init,         "SSL_library_init");
      pragma Import (C, SSL_load_error_strings,   "SSL_load_error_strings");
      pragma Import (C, SSL_new,                  "SSL_new");
      pragma Import (C, SSL_pending,              "SSL_pending");
      pragma Import (C, SSL_read,                 "SSL_read");
      pragma Import (C, SSL_set_fd,               "SSL_set_fd");
      pragma Import (C, SSL_shutdown,             "SSL_shutdown");
      pragma Import (C, SSL_write,                "SSL_write");
      pragma Import (C, SSLv2_client_method,      "SSLv2_client_method");
      pragma Import (C, SSLv2_method,             "SSLv2_method");
      pragma Import (C, SSLv2_server_method,      "SSLv2_server_method");
      pragma Import (C, SSLv3_client_method,      "SSLv3_client_method");
      pragma Import (C, SSLv3_method,             "SSLv3_method");
      pragma Import (C, SSLv3_server_method,      "SSLv3_server_method");
      pragma Import (C, SSLv23_client_method,     "SSLv23_client_method");
      pragma Import (C, SSLv23_method,            "SSLv23_method");
      pragma Import (C, SSLv23_server_method,     "SSLv23_server_method");
      pragma Import (C, TLSv1_client_method,      "TLSv1_client_method");
      pragma Import (C, TLSv1_method,             "TLSv1_method");
      pragma Import (C, TLSv1_server_method,      "TLSv1_server_method");
      pragma Import (C, sk_SSL_CIPHER_num,
                     "__PolyORB_sk_SSL_CIPHER_num");
      pragma Import (C, sk_SSL_CIPHER_value,
                     "__PolyORB_sk_SSL_CIPHER_value");
   end Thin;

   procedure Raise_TLS_Error;
   --  Raise TLS_Error with error description from OpenSSL library

   procedure Initialize;
   --  Initialize OpenSSL library

   function To_SSL_Verify_Mode
     (Value : TLS_Verification_Mode) return Thin.SSL_Verify_Mode;
   --  Convert user friendly TLS_Verification_Mode structure into SSL internal
   --  representation.

--   function To_TLS_Verification_Mode
--     (Value : Thin.SSL_Verify_Mode) return TLS_Verification_Mode;
--   --  Convert SSL internal representation of SSL_Verify_Mode into
--   --  TLS_Verification_Mode

   -------------------
   -- Accept_Socket --
   -------------------

   procedure Accept_Socket (Socket : TLS_Socket_Type) is
   begin
      if Thin.SSL_accept (Socket) /= 1 then
         Raise_TLS_Error;
      end if;
   end Accept_Socket;

   -----------------------
   -- Check_Private_Key --
   -----------------------

   procedure Check_Private_Key (Context : TLS_Context_Type) is
   begin
      if Thin.SSL_CTX_check_private_key (Context) /= 1 then
         Raise_TLS_Error;
      end if;
   end Check_Private_Key;

   ---------------
   -- Cipher_Of --
   ---------------

   function Cipher_Of
     (Socket   : TLS_Socket_Type;
      Priority : Natural) return String
   is
      Result : Interfaces.C.Strings.chars_ptr;

   begin
      Result := Thin.SSL_get_cipher_list (Socket, Interfaces.C.int (Priority));

      if Result = Interfaces.C.Strings.Null_Ptr then
         return "";

      else
         return Interfaces.C.Strings.Value (Result);
      end if;
   end Cipher_Of;

   ----------------
   -- Ciphers_Of --
   ----------------

   function Ciphers_Of
    (Context : TLS_Context_Type)
     return TLS_Cipher_List
   is
      Socket : constant TLS_Socket_Type := Thin.SSL_new (Context);
      Result : constant TLS_Cipher_List := Ciphers_Of (Socket);

   begin
      Thin.SSL_free (Socket);
      return Result;
   end Ciphers_Of;

   function Ciphers_Of (Socket  : TLS_Socket_Type) return TLS_Cipher_List is
      use type Thin.Stack_Of_SSL_Cipher;

      Stack : constant Thin.Stack_Of_SSL_Cipher
        := Thin.SSL_get_ciphers (Socket);

   begin
      if Stack = Thin.No_Stack_Of_SSL_Cipher then
         return TLS_Cipher_List'(1 .. 0 => No_TLS_Cipher);
      end if;

      declare
         Length : constant Interfaces.C.int := Thin.sk_SSL_CIPHER_num (Stack);
         Result : TLS_Cipher_List (1 .. Integer (Length));
      begin
         for J in 1 .. Length loop
            Result (Positive (J)) := Thin.sk_SSL_CIPHER_value (Stack, J - 1);
         end loop;

         return Result;
      end;
   end Ciphers_Of;

   ------------------
   -- Close_Socket --
   ------------------

   procedure Close_Socket (Socket : in out TLS_Socket_Type) is
      Status : Interfaces.C.int;

   begin
--      --  Shutdown procedure may not complete in one call, thus call it
--      --  again until it return complete or error status
--
--      loop
--         Status := Thin.SSL_shutdown (Socket);
--         pragma Debug
--          (C, O ("SSL_shutdown:" & Interfaces.C.int'Image (Status)));
--
--         exit when Status = 1;
--
--         if Status /= 0 then
--            Raise_TLS_Error;
--         end if;
--      end loop;

      --  XXX Original code may go into forever loop if connection closed
      --  as result of peer termination.

      Status := Thin.SSL_shutdown (Socket);

      if Status = 0 then
         Status := Thin.SSL_shutdown (Socket);
      end if;

      if Status not in 0 .. 1 then
         Raise_TLS_Error;
      end if;

      Sockets.Close_Socket (Socket_Of (Socket));
      Thin.SSL_free (Socket);

      Socket := No_TLS_Socket;
   end Close_Socket;

   --------------------
   -- Connect_Socket --
   --------------------

   procedure Connect_Socket (Socket : TLS_Socket_Type) is
   begin
      if Thin.SSL_connect (Socket) /= 1 then
         Raise_TLS_Error;
      end if;
   end Connect_Socket;

   ------------
   -- Create --
   ------------

   function Create (Context : TLS_Context_Type) return TLS_Socket_Type is
      Result : TLS_Socket_Type;

   begin
      Result := Thin.SSL_new (Context);

      if Result = null then
         Raise_TLS_Error;
      end if;

      return Result;
   end Create;

   function Create (Method : TLS_Method_Type) return TLS_Context_Type is
      M      : Thin.SSL_Method;
      Result : TLS_Context_Type;

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

      Result := Thin.SSL_CTX_new (M);

      if Result = null then
         Raise_TLS_Error;
      end if;

      return Result;
   end Create;

   --------------------
   -- Description_Of --
   --------------------

   function Description_Of (Cipher : TLS_Cipher_Type) return String
     renames Thin.SSL_CIPHER_description;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out TLS_Context_Type) is
   begin
      if Context /= null then
         Thin.SSL_CTX_free (Context);
         Context := null;
      end if;
   end Destroy;

   procedure Destroy (Socket : in out TLS_Socket_Type) is
   begin
      if Socket /= null then
         Thin.SSL_free (Socket);
         Socket := null;
      end if;
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Thin.SSL_load_error_strings;
      Thin.SSL_library_init;
      --  XXX actions_to_seed_PRNG
   end Initialize;

   ---------------------------
   -- Load_Verify_Locations --
   ---------------------------

   procedure Load_Verify_Locations
     (Context                    : TLS_Context_Type;
      Certificate_Authority_File : String;
      Certificate_Authority_Path : String)
   is
   begin
      if Certificate_Authority_File /= ""
        and then Certificate_Authority_Path /= ""
      then
         if Thin.SSL_CTX_load_verify_locations
           (Context,
            Interfaces.C.To_C (Certificate_Authority_File),
            Interfaces.C.To_C (Certificate_Authority_Path)) /= 1
         then
            Raise_TLS_Error;
         end if;

      elsif Certificate_Authority_File /= "" then
         if Thin.SSL_CTX_load_verify_locations
           (Context,
            Interfaces.C.To_C (Certificate_Authority_File),
            Interfaces.C.Strings.Null_Ptr) /= 1
         then
            Raise_TLS_Error;
         end if;

      elsif Certificate_Authority_Path /= "" then
         if Thin.SSL_CTX_load_verify_locations
           (Context,
            Interfaces.C.Strings.Null_Ptr,
            Interfaces.C.To_C (Certificate_Authority_Path)) /= 1
         then
            Raise_TLS_Error;
         end if;

      else
         if Thin.SSL_CTX_load_verify_locations
           (Context,
            Interfaces.C.Strings.Null_Ptr,
            Interfaces.C.Strings.Null_Ptr) /= 1
         then
            Raise_TLS_Error;
         end if;
      end if;
   end Load_Verify_Locations;

   -------------------------
   -- Peer_Certificate_Of --
   -------------------------

   function Peer_Certificate_Of
     (Socket : TLS_Socket_Type) return PolyORB.X509.Certificate
   is
   begin
      return Thin.SSL_get_peer_certificate (Socket);
   end Peer_Certificate_Of;

   --------------------
   -- Pending_Length --
   --------------------

   function Pending_Length (Socket : TLS_Socket_Type) return Natural is
   begin
      return Natural (Thin.SSL_pending (Socket));
   end Pending_Length;

   ---------------------
   -- Raise_TLS_Error --
   ---------------------

   procedure Raise_TLS_Error is

      function Get_Errors_String return String;

      -----------------------
      -- Get_Errors_String --
      -----------------------

      function Get_Errors_String return String is
         use type Thin.SSL_Error_Code;

         Error : constant Thin.SSL_Error_Code := Thin.ERR_get_error;

      begin
         if Error /= 0 then
            return Get_Errors_String
            & Ada.Characters.Latin_1.LF
            & Thin.ERR_error_string (Error);

         else
            return "";
         end if;
      end Get_Errors_String;

      X : constant String := Get_Errors_String;

   begin
      pragma Debug (C, O ("TLS ERROR:" & X));

      Ada.Exceptions.Raise_Exception (TLS_Error'Identity, X);
   end Raise_TLS_Error;

   --------------------
   -- Receive_Vector --
   --------------------

   procedure Receive_Vector
     (Socket :     TLS_Socket_Type;
      Vector :     Sockets.Vector_Type;
      Count  : out Ada.Streams.Stream_Element_Count)
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
            Raise_TLS_Error;
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
     (Socket :     TLS_Socket_Type;
      Vector :     Sockets.Vector_Type;
      Count  : out Ada.Streams.Stream_Element_Count)
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
            Raise_TLS_Error;
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

   ---------------------
   -- Set_Cipher_List --
   ---------------------

   procedure Set_Cipher_List
     (Context : TLS_Context_Type;
      Ciphers : String)
   is
   begin
      if Thin.SSL_CTX_set_cipher_list
          (Context, Interfaces.C.To_C (Ciphers)) /= 1
      then
         Raise_TLS_Error;
      end if;
   end Set_Cipher_List;

   ----------------
   -- Set_Socket --
   ----------------

   procedure Set_Socket
     (Socket : TLS_Socket_Type;
      Sock   : Sockets.Socket_Type)
   is
   begin
      if Thin.SSL_set_fd (Socket, Sock) /= 1 then
         Raise_TLS_Error;
      end if;
   end Set_Socket;

   ---------------------
   -- Set_Verify_Mode --
   ---------------------

   procedure Set_Verify_Mode
     (Context : TLS_Context_Type;
      Mode    : TLS_Verification_Mode)
   is
   begin
      Thin.SSL_CTX_set_verify (Context, To_SSL_Verify_Mode (Mode), null);
   end Set_Verify_Mode;

   ---------------
   -- Socket_Of --
   ---------------

   function Socket_Of (Socket : TLS_Socket_Type)
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
        (Error_Code : SSL_Error_Code)
         return String
      is
         procedure ERR_error_string_n
           (Error_Code : SSL_Error_Code;
            Buf        : Interfaces.C.char_array;
            Len        : Interfaces.C.size_t);
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

      function SSL_CIPHER_description
        (Cipher : TLS_Cipher_Type) return String
      is
         procedure SSL_CIPHER_description
           (Cipher : TLS_Cipher_Type;
            Buf    : Interfaces.C.char_array;
            Size   : Interfaces.C.int);
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
     (Value : TLS_Verification_Mode)
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

   ---------------------
   -- Use_Certificate --
   ---------------------

   procedure Use_Certificate
     (Context   : TLS_Context_Type;
      File_Name : String)
   is
   begin
      if Thin.SSL_CTX_use_certificate_file
          (Context, Interfaces.C.To_C (File_Name), Thin.PEM) /= 1
      then
         Raise_TLS_Error;
      end if;
   end Use_Certificate;

   procedure Use_Certificate
     (Context     : TLS_Context_Type;
      Certificate : PolyORB.X509.Certificate)
   is
   begin
      if Thin.SSL_CTX_use_certificate (Context, Certificate) /= 1 then
         Raise_TLS_Error;
      end if;
   end Use_Certificate;

   ---------------------------
   -- Use_Certificate_Chain --
   ---------------------------

   procedure Use_Certificate_Chain
     (Context   : TLS_Context_Type;
      File_Name : String)
   is
   begin
      if Thin.SSL_CTX_use_certificate_chain_file
          (Context, Interfaces.C.To_C (File_Name)) /= 1
      then
         Raise_TLS_Error;
      end if;
   end Use_Certificate_Chain;

   ---------------------
   -- Use_Private_Key --
   ---------------------

   procedure Use_Private_Key
     (Context   : TLS_Context_Type;
      File_Name : String)
   is
   begin
      if Thin.SSL_CTX_use_PrivateKey_file
          (Context, Interfaces.C.To_C (File_Name), Thin.PEM) /= 1
      then
         Raise_TLS_Error;
      end if;
   end Use_Private_Key;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"tls",
          Conflicts => Empty,
          Depends   => +"sockets" & "x509",
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.TLS;
