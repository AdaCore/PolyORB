------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         P O L Y O R B . X 5 0 9                          --
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

with Ada.Characters.Latin_1;
with Ada.Exceptions;

with Interfaces.C.Strings;
with Interfaces.C.Pointers;

with System.Storage_Elements;

with PolyORB.Initialization;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Threads;
with PolyORB.Utils.Strings;

package body PolyORB.X509 is

   use Interfaces;
   use type C.int;

   package PTM renames PolyORB.Tasking.Mutexes;
   package PTT renames PolyORB.Tasking.Threads;

--   type X509_Lookup_Method_Record is null record;
--   pragma Convention (C, X509_Lookup_Method_Record);
--
--   type X509_Lookup_Method is access all X509_Lookup_Method_Record;

   package Stream_Element_Pointers is
     new Interfaces.C.Pointers
     (Ada.Streams.Stream_Element_Offset,
      Ada.Streams.Stream_Element,
      Ada.Streams.Stream_Element_Array,
      0);
   use Stream_Element_Pointers;

   ------------------
   -- Crypto locks --
   ------------------

   --  OpenSSL relies on the user to provide thread identification and a
   --  mutual exclusion facility. In the case of PolyORB, we rely on
   --  PolyORB.Tasking to provide both facilities.

   type Lock_Array is array (C.int range <>) of PTM.Mutex_Access;
   type Lock_Array_Access is access Lock_Array;

   Crypto_Locks : Lock_Array_Access;
   --  A set of n locks, where n is the value returned by OpenSSL function
   --  CRYPTO_num_locks. These are used to implement the OpenSSL locking
   --  callback.

   procedure PolyORB_Locking_Function
     (Mode : C.unsigned;
      N    : C.int;
      File : C.Strings.chars_ptr;
      Line : C.int);
   pragma Convention (C, PolyORB_Locking_Function);
   --  Callback for CRYPTO_set_locking_callback:
   --  Lock or unlock (depending on whether (Mode and CRYPTO_LOCK) is nonzero)
   --  the N'th lock in Crypto_Locks.
   --  File and Line denote a source location in the OpenSSL library, and are
   --  provided for debugging purposes.

   function PolyORB_Id_Function return C.unsigned_long;
   pragma Convention (C, PolyORB_Id_Function);
   --  Callback for CRYPTO_set_id_callback:
   --  Return an unsigned long value identifying the current thread.

   procedure Initialize;

   procedure Raise_X509_Error;

   package Thin is

      type SSL_Error_Code is new Interfaces.C.unsigned_long;

      -------------------------------
      -- Generic CRYPTO operations --
      -------------------------------

      function CRYPTO_num_locks return C.int;
      --  Number of locks to be allocated for locking callback

      function Get_CRYPTO_LOCK return C.unsigned;
      --  Function returning the value of C macro CRYPTO_LOCK (used for
      --  Mode parameter in locking callback).

      CRYPTO_LOCK : constant C.unsigned;
      --  Constant used to cache the value of C macro CRYPTO_LOCK

      ---------------------------
      -- X.509 Name operations --
      ---------------------------

      procedure X509_NAME_free (Item : Name);

      function X509_NAME_dup (Item : Name) return Name;

      function d2i_X509_NAME
        (Buffer : Ada.Streams.Stream_Element_Array;
         Length : C.int) return Name;

      procedure i2d_X509_NAME
        (Item   :     Name;
         Buffer : out Stream_Element_Pointers.Pointer;
         Length : out C.int);

      function X509_NAME_oneline (The_Name : Name) return String;

      ----------------------------------
      -- X.509 Certificate operations --
      ----------------------------------

      procedure X509_free (The_Certificate : Certificate);

      function X509_get_subject_name
        (The_Certificate : Certificate) return PolyORB.X509.Name;

      ----------------------------------------
      -- X.509 Certificate Chain operations --
      ----------------------------------------

      function d2i_X509_CHAIN
        (Buffer : Ada.Streams.Stream_Element_Array;
         Length : C.int) return Certificate_Chain;

      procedure i2d_X509_CHAIN
        (Item   :     Certificate_Chain;
         Buffer : out Stream_Element_Pointers.Pointer;
         Length : out C.int);

      --------------------------------
      -- Error handling subprograms --
      --------------------------------

      function ERR_get_error return SSL_Error_Code;
      function ERR_error_string (Error_Code : SSL_Error_Code) return String;

      -----------------------
      -- Memory management --
      -----------------------

      procedure OPENSSL_free (Item : Stream_Element_Pointers.Pointer);

      ------------------------
      -- PolyORB extensions --
      ------------------------

      procedure ERR_load_PolyORB_strings;

      procedure Initialize
        (Locking_Function : System.Address;
         Id_Function      : System.Address);
      --  Perform any required initialization at the C level.
      --  Locking_Function is passed to CRYPTO_set_locking_callback.
      --  Id_Function is passed to CRYPTO_set_id_callback.

   private

      pragma Import (C, CRYPTO_num_locks,         "CRYPTO_num_locks");
      pragma Import (C, Get_CRYPTO_LOCK,          "__PolyORB_Get_CRYPTO_LOCK");
      pragma Import (C, ERR_get_error,            "ERR_get_error");
      pragma Import (C, ERR_load_PolyORB_strings, "ERR_load_PolyORB_strings");
      pragma Import (C, OPENSSL_free,             "__PolyORB_OPENSSL_free");
      pragma Import (C, X509_NAME_dup,            "X509_NAME_dup");
      pragma Import (C, X509_NAME_free,           "X509_NAME_free");
      pragma Import (C, X509_free,                "X509_free");
      pragma Import (C, X509_get_subject_name,    "X509_get_subject_name");
      pragma Import (C, d2i_X509_CHAIN,           "__PolyORB_d2i_X509_CHAIN");
      pragma Import (C, d2i_X509_NAME,            "__PolyORB_d2i_X509_NAME");
      pragma Import (C, i2d_X509_CHAIN,           "__PolyORB_i2d_X509_CHAIN");
      pragma Import (C, i2d_X509_NAME,            "__PolyORB_i2d_X509_NAME");
      pragma Import (C, Initialize,               "__PolyORB_X509_Intialize");

      CRYPTO_LOCK : constant C.unsigned := Get_CRYPTO_LOCK;

   end Thin;

--   ------------------------------------
--   -- Add_Certificate_Authority_File --
--   ------------------------------------
--
--   procedure Add_Certificate_Authority_File
--     (Store                      : X509_Store;
--      Certificate_Authority_File : String)
--   is
--
--      function X509_STORE_load_locations
--        (Store : X509_Store;
--         File  : Interfaces.C.char_array;
--         Path  : Interfaces.C.Strings.chars_ptr)
--         return C.int;
--      pragma Import
--        (C, X509_STORE_load_locations, "X509_STORE_load_locations");
--
--   begin
--      if X509_STORE_load_locations
--          (Store,
--           Interfaces.C.To_C (Certificate_Authority_File),
--           Interfaces.C.Strings.Null_Ptr) /= 1
--      then
--         Raise_X509_Error;
--      end if;
--   end Add_Certificate_Authority_File;
--
--   ------------------------------------
--   -- Add_Certificate_Authority_Path --
--   ------------------------------------
--
--   procedure Add_Certificate_Authority_Path
--     (Store                      : X509_Store;
--      Certificate_Authority_Path : String)
--   is
--
--      function X509_STORE_load_locations
--        (Store : X509_Store;
--         Path  : Interfaces.C.Strings.chars_ptr;
--         File  : Interfaces.C.char_array)
--         return C.int;
--      pragma Import
--        (C, X509_STORE_load_locations, "X509_STORE_load_locations");
--
--   begin
--      if X509_STORE_load_locations
--          (Store,
--           Interfaces.C.Strings.Null_Ptr,
--           Interfaces.C.To_C (Certificate_Authority_Path)) /= 1
--      then
--         Raise_X509_Error;
--      end if;
--   end Add_Certificate_Authority_Path;
--
--   ------------------------------------------
--   -- Add_Certificate_Revocation_List_File --
--   ------------------------------------------
--
--   procedure Add_Certificate_Revocation_List_File
--     (Store                            : X509_Store;
--      Certificate_Revocation_List_File : String)
--   is
--
--      function X509_LOOKUP_file return X509_Lookup_Method;
--      pragma Import (C, X509_LOOKUP_file, "X509_LOOKUP_file");
--
--      function X509_STORE_add_lookup
--        (Store  : X509_Store;
--         Method : X509_Lookup_Method)
--         return X509_Lookup;
--      pragma Import (C, X509_STORE_add_lookup, "X509_STORE_add_lookup");
--
--      function X509_load_crl_file
--        (Lookup    : X509_Lookup;
--         File_Name : Interfaces.C.char_array;
--         File_Type : C.int)
--         return C.int;
--      pragma Import (C, X509_load_crl_file, "X509_load_crl_file");
--
--      Lookup : constant X509_Lookup
--        := X509_STORE_add_lookup (Store, X509_LOOKUP_file);
--
--   begin
--      if Lookup = null then
--         Raise_X509_Error;
--      end if;
--
--      if X509_load_crl_file
--          (Lookup,
--           Interfaces.C.To_C (Certificate_Revocation_List_File),
--           1) /= 1
--      then
--         Raise_X509_Error;
--      end if;
--   end Add_Certificate_Revocation_List_File;
--
--   --------------------------------------
--   -- Add_System_Certificate_Authority --
--   --------------------------------------
--
--   procedure Add_System_Certificate_Authority (Store : X509_Store) is
--
--      function X509_STORE_set_default_paths
--        (Store : X509_Store)
--         return C.int;
--      pragma Import
--        (C, X509_STORE_set_default_paths, "X509_STORE_set_default_paths");
--
--   begin
--      if X509_STORE_set_default_paths (Store) /= 1 then
--         Raise_X509_Error;
--      end if;
--   end Add_System_Certificate_Authority;
--
--   -----------------------
--   -- Check_Private_Key --
--   -----------------------
--
--   procedure Check_Private_Key
--     (The_Certificate : Certificate;
--      The_Private_Key : Private_Key)
--   is
--
--      function X509_check_private_key
--        (The_Certificate : Certificate;
--         The_Private_Key : Private_Key)
--         return C.int;
--      pragma Import (C, X509_check_private_key, "X509_check_private_key");
--
--   begin
--      if X509_check_private_key (The_Certificate, The_Private_Key) /= 1 then
--         Raise_X509_Error;
--      end if;
--   end Check_Private_Key;
--
--   ------------
--   -- Create --
--   ------------
--
--   function Create return X509_Store is
--
--      function X509_STORE_new return X509_Store;
--      pragma Import (C, X509_STORE_new, "X509_STORE_new");
--
--      Aux : constant X509_Store := X509_STORE_new;
--
--   begin
--      if Aux = null then
--         Raise_X509_Error;
--      end if;
--
--      return Aux;
--   end Create;
--
--   function Create
--     (Store           : X509_Store;
--      The_Certificate : Certificate)
--      return X509_Context
--   is
--
--      function X509_STORE_CTX_new return X509_Context;
--      pragma Import (C, X509_STORE_CTX_new, "X509_STORE_CTX_new");
--
--      function X509_STORE_CTX_init
--        (Context         : X509_Context;
--         Store           : X509_Store;
--         The_Certificate : Certificate;
--         Chain           : Stack_Of_Certificate)
--         return C.int;
--      pragma Import (C, X509_STORE_CTX_init, "X509_STORE_CTX_init");
--
--      Context : constant X509_Context := X509_STORE_CTX_new;
--
--   begin
--      if Context = null then
--         Raise_X509_Error;
--      end if;
--
--    if X509_STORE_CTX_init (Context, Store, The_Certificate, null) /= 1 then
--         Raise_X509_Error;
--      end if;
--
--      return Context;
--   end Create;

   ------------
   -- Decode --
   ------------

   function Decode
     (Item : Ada.Streams.Stream_Element_Array) return Certificate_Chain
   is
      Result : Certificate_Chain;

   begin
      Result := Thin.d2i_X509_CHAIN (Item, Item'Length);

      if Result = null then
         Raise_X509_Error;
      end if;

      return Result;
   end Decode;

   ------------
   -- Decode --
   ------------

   function Decode (Item : Ada.Streams.Stream_Element_Array) return Name is
      Result : Name;

   begin
      Result := Thin.d2i_X509_NAME (Item, Item'Length);

      if Result = null then
         Raise_X509_Error;
      end if;

      return Result;
   end Decode;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (The_Certificate : in out Certificate) is
   begin
      if The_Certificate /= null then
         Thin.X509_free (The_Certificate);
         The_Certificate := null;
      end if;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (The_Name : in out Name) is
   begin
      if The_Name /= null then
         Thin.X509_NAME_free (The_Name);
         The_Name := null;
      end if;
   end Destroy;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate (The_Name : Name) return Name is
   begin
      if The_Name = null then
         return null;

      else
         return Thin.X509_NAME_dup (The_Name);
      end if;
   end Duplicate;

   ------------
   -- Encode --
   ------------

   function Encode
     (Item : Certificate_Chain) return Ada.Streams.Stream_Element_Array
   is
      Buffer : Stream_Element_Pointers.Pointer;
      Length : C.int;

   begin
      Thin.i2d_X509_CHAIN (Item, Buffer, Length);

      if Length < 0 or else Buffer = null then
         Raise_X509_Error;
      end if;

      declare
         Result : constant Ada.Streams.Stream_Element_Array :=
                    Value (Buffer, C.ptrdiff_t (Length));
      begin
         Thin.OPENSSL_free (Buffer);

         return Result;
      end;
   end Encode;

   ------------
   -- Encode --
   ------------

   function Encode (The_Name : Name) return Ada.Streams.Stream_Element_Array is
      Buffer : Stream_Element_Pointers.Pointer;
      Length : C.int;

   begin
      Thin.i2d_X509_NAME (The_Name, Buffer, Length);

      if Length < 0 or else Buffer = null then
         Raise_X509_Error;
      end if;

      declare
         Result : constant Ada.Streams.Stream_Element_Array :=
                    Value (Buffer, C.ptrdiff_t (Length));

      begin
         Thin.OPENSSL_free (Buffer);
         return Result;
      end;
   end Encode;

--   procedure Free (The_Private_Key : in out Private_Key) is
--
--      procedure EVP_PKEY_free (The_Private_Key : Private_Key);
--      pragma Import (C, EVP_PKEY_free, "EVP_PKEY_free");
--
--   begin
--      if The_Private_Key /= null then
--         EVP_PKEY_free (The_Private_Key);
--         The_Private_Key := null;
--      end if;
--   end Free;
--
--   procedure Free (Lookup : in out X509_Lookup) is
--
--      procedure X509_LOOKUP_free (Lookup : X509_Lookup);
--      pragma Import (C, X509_LOOKUP_free, "X509_LOOKUP_free");
--
--   begin
--      if Lookup /= null then
--         X509_LOOKUP_free (Lookup);
--         Lookup := null;
--      end if;
--   end Free;
--
--   procedure Free (Store : in out X509_Store) is
--
--      procedure X509_STORE_free (Store : X509_Store);
--      pragma Import (C, X509_STORE_free, "X509_STORE_free");
--
--   begin
--      if Store /= null then
--         X509_STORE_free (Store);
--         Store := null;
--      end if;
--   end Free;
--
--   procedure Free (Context : in out X509_Context) is
--
--      procedure X509_STORE_CTX_free (Store : X509_Context);
--      pragma Import (C, X509_STORE_CTX_free, "X509_STORE_CTX_free");
--
--   begin
--      if Context /= null then
--         X509_STORE_CTX_free (Context);
--         Context := null;
--      end if;
--   end Free;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Crypto_Locks := new Lock_Array (0 .. Thin.CRYPTO_num_locks - 1);
      for J in Crypto_Locks'Range loop
         PTM.Create (Crypto_Locks (J));
      end loop;

      Thin.Initialize
        (Locking_Function => PolyORB_Locking_Function'Address,
         Id_Function      => PolyORB_Id_Function'Address);

      Thin.ERR_load_PolyORB_strings;
   end Initialize;

--   ------------
--   -- Length --
--   ------------
--
--   function Length (Stack : Stack_Of_Certificate) return Natural is
--
--      function sk_X509_num
--        (Item : Stack_Of_Certificate)
--         return C.int;
--      pragma Import (C, sk_X509_num, "__PolyORB_sk_X509_num");
--
--   begin
--      return Natural (sk_X509_num (Stack));
--   end Length;

   ------------------------------
   -- PolyORB_Locking_Function --
   ------------------------------

   procedure PolyORB_Locking_Function
     (Mode : C.unsigned;
      N    : C.int;
      File : C.Strings.chars_ptr;
      Line : C.int)
   is
      pragma Unreferenced (File, Line);
      use type C.unsigned;
   begin
      if (Mode and Thin.CRYPTO_LOCK) /= 0 then
         PTM.Enter (Crypto_Locks (N));
      else
         PTM.Leave (Crypto_Locks (N));
      end if;
   end PolyORB_Locking_Function;

   -------------------------
   -- PolyORB_Id_Function --
   -------------------------

   function PolyORB_Id_Function return C.unsigned_long is
   begin
      return C.unsigned_long
        (System.Storage_Elements.To_Integer
         (PTT.To_Address (PTT.Current_Task)));
   end PolyORB_Id_Function;

   ----------------------
   -- Raise_X509_Error --
   ----------------------

   procedure Raise_X509_Error is

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

   begin
      Ada.Exceptions.Raise_Exception
        (X509_Error'Identity, Get_Errors_String);
   end Raise_X509_Error;

   ----------
   -- Read --
   ----------

   function Read (File_Name : String) return Certificate is

      function PEM_read_certificate_file
        (File : Interfaces.C.char_array)
         return Certificate;
      pragma Import (C, PEM_read_certificate_file,
                     "__PolyORB_PEM_read_certificate_file");

      Aux : constant Certificate
        := PEM_read_certificate_file (Interfaces.C.To_C (File_Name));

   begin
      if Aux = null then
         Raise_X509_Error;
      end if;

      return Aux;
   end Read;

--   function Read (File_Name : String) return Private_Key is
--
--      function PEM_read_PrivateKey_file
--        (File : Interfaces.C.char_array)
--         return Private_Key;
--      pragma Import (C, PEM_read_PrivateKey_file,
--                     "__PolyORB_PEM_read_PrivateKey_file");
--
--      Aux : constant Private_Key
--        := PEM_read_PrivateKey_file (Interfaces.C.To_C (File_Name));
--
--   begin
--      if Aux = null then
--         Raise_X509_Error;
--      end if;
--
--      return Aux;
--   end Read;

   ---------------------
   -- Subject_Name_Of --
   ---------------------

   function Subject_Name_Of
     (The_Certificate : Certificate) return PolyORB.X509.Name
   is
   begin
      return Thin.X509_get_subject_name (The_Certificate);
   end Subject_Name_Of;

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

      -----------------------
      -- X509_NAME_oneline --
      -----------------------

      function X509_NAME_oneline (The_Name : Name) return String is

         procedure X509_NAME_oneline
           (The_Name : Name;
            Buffer   : Interfaces.C.char_array;
            Length   : C.int);
         pragma Import (C, X509_NAME_oneline, "X509_NAME_oneline");

         Buffer : Interfaces.C.char_array (1 .. 1024);
         pragma Warnings (Off, Buffer);
         --  Buffer not needed to be initialized and modified, because
         --  of side effect of C function X509_NAME_oneline

      begin
         X509_NAME_oneline (The_Name, Buffer, Buffer'Length);

         return Interfaces.C.To_Ada (Buffer);
      end X509_NAME_oneline;

   end Thin;

   ---------------
   -- To_String --
   ---------------

   function To_String (The_Name : Name) return String
     renames Thin.X509_NAME_oneline;

--   ------------
--   -- Verify --
--   ------------
--
--   function Verify (Context : X509_Context) return Boolean is
--
--      function X509_verify_cert
--        (Context : X509_Context)
--         return C.int;
--      pragma Import (C, X509_verify_cert, "X509_verify_cert");
--
--   begin
--      return X509_verify_cert (Context) = 1;
--   end Verify;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"x509",
          Conflicts => Empty,
          Depends   => +"tasking.mutexes",
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;

end PolyORB.X509;
