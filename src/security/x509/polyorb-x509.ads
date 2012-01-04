------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         P O L Y O R B . X 5 0 9                          --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Streams;

package PolyORB.X509 is

   type Name is private;

   type Certificate is private;

   type Certificate_Chain is private;

--   type Private_Key is private;

   --  Exceptions

   X509_Error : exception;

   --  X.509 Name

   function Duplicate (The_Name : Name) return Name;

   procedure Destroy (The_Name : in out Name);

   function Decode (Item : Ada.Streams.Stream_Element_Array) return Name;

   function Encode (The_Name : Name) return Ada.Streams.Stream_Element_Array;

   function To_String (The_Name : Name) return String;

   --  X.509 Certificate

   function Read (File_Name : String) return Certificate;

   procedure Destroy (The_Certificate : in out Certificate);

   function Subject_Name_Of
     (The_Certificate : Certificate) return PolyORB.X509.Name;

   --  X.509 Certificate Chain

   function Decode
     (Item : Ada.Streams.Stream_Element_Array) return Certificate_Chain;

   function Encode
     (Item : Certificate_Chain) return Ada.Streams.Stream_Element_Array;

--   procedure Check_Private_Key
--     (The_Certificate : Certificate;
--      The_Private_Key : Private_Key);
--
--   --  Private Key
--
--   function Read (File_Name : String) return Private_Key;
--
--   procedure Free (The_Private_Key : in out Private_Key);
--
--   --  X509 Certificate Validation Stuff
--
--   type X509_Store is private;
--
--   type X509_Lookup is private;
--
--   type X509_Context is private;
--
--   type Stack_Of_Certificate is private;
--
--   --  X509 STORE
--
--   function Create return X509_Store;
--
--   procedure Free (Store : in out X509_Store);
--
--   procedure Add_System_Certificate_Authority (Store : X509_Store);
--
--   procedure Add_Certificate_Authority_File
--     (Store                      : X509_Store;
--      Certificate_Authority_File : String);
--
--   procedure Add_Certificate_Authority_Path
--     (Store                      : X509_Store;
--      Certificate_Authority_Path : String);
--
--   procedure Add_Certificate_Revocation_List_File
--     (Store                            : X509_Store;
--      Certificate_Revocation_List_File : String);
--
--   --  X509 LOOKUP
--
--   procedure Free (Lookup : in out X509_Lookup);
--
--   --  X509 STORE CTX
--
--   function Create
--     (Store           : X509_Store;
--      The_Certificate : Certificate)
--      return X509_Context;
--
--   function Verify (Context : X509_Context) return Boolean;
--
--  function Verify_Chain (Context : X509_Context) return Stack_Of_Certificate;
--
--   procedure Free (Context : in out X509_Context);
--
--   --  STACK OF Certificate
--
--   function Length (Stack : Stack_Of_Certificate) return Natural;

private

   --  X.509 Name

   type Name_Record is null record;
   pragma Convention (C, Name_Record);

   type Name is access all Name_Record;

   --  X.509 Certificate

   type Certificate_Record is null record;
   pragma Convention (C, Certificate_Record);

   type Certificate is access all Certificate_Record;

   type Certificate_Chain_Record is null record;
   pragma Convention (C, Certificate_Chain_Record);

   type Certificate_Chain is access all Certificate_Chain_Record;

--   type Private_Key_Record is null record;
--   pragma Convention (C, Private_Key_Record);
--
--   type Private_Key is access all Private_Key_Record;
--
--   type X509_Store_Record is null record;
--   pragma Convention (C, X509_Store_Record);
--
--   type X509_Store is access all X509_Store_Record;
--
--   type X509_Lookup_Record is null record;
--   pragma Convention (C, X509_Lookup_Record);
--
--   type X509_Lookup is access all X509_Lookup_Record;
--
--   type X509_Context_Record is null record;
--   pragma Convention (C, X509_Context_Record);
--
--   type X509_Context is access all X509_Context_Record;
--
--   type Stack_Of_Certificate_Record is null record;
--   pragma Convention (C, Stack_Of_Certificate_Record);
--
--   type Stack_Of_Certificate is access all Stack_Of_Certificate_Record;
--
--   pragma Import (C, Verify_Chain, "X509_STORE_CTX_get_chain");

end PolyORB.X509;
