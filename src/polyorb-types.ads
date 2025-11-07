------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        P O L Y O R B . T Y P E S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  Base data types for the whole middleware

with Interfaces;
with System;

with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;
with PolyORB.Utils.Unchecked_Deallocation;

package PolyORB.Types is

   pragma Preelaborate;

   subtype Address is System.Address;
   --  Provided as a subtype here so that generated code can avoid a direct
   --  dependency on System, which may clash with a used-defined identifier.

   --  Note that some of these names duplicate names in Standard. The reason is
   --  that this is patterned after package CORBA, which is required to do
   --  that.

   type    Short              is new Interfaces.Integer_16;
   type    Long               is new Interfaces.Integer_32;
   type    Long_Long          is new Interfaces.Integer_64;
   type    Unsigned_Short     is new Interfaces.Unsigned_16;
   type    Unsigned_Long      is new Interfaces.Unsigned_32;
   type    Unsigned_Long_Long is new Interfaces.Unsigned_64;
   pragma Warnings (Off); -- redefinition of entity in Standard
   type    Float              is new Interfaces.IEEE_Float_32;
   pragma Warnings (On);
   type    Double             is new Interfaces.IEEE_Float_64;
   type    Long_Double        is new Interfaces.IEEE_Extended_Float;
   subtype Char               is Standard.Character;
   subtype Wchar              is Standard.Wide_Character;
   type    Octet              is new Interfaces.Unsigned_8;
   pragma Warnings (Off); -- redefinition of entity in Standard
   subtype Boolean            is Standard.Boolean;
   type    String             is
     new Ada.Strings.Unbounded.Unbounded_String;
   type    Wide_String        is
     new Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   pragma Warnings (On);

   type    Short_Ptr              is access all Short;
   type    Long_Ptr               is access all Long;
   type    Long_Long_Ptr          is access all Long_Long;
   type    Unsigned_Short_Ptr     is access all Unsigned_Short;
   type    Unsigned_Long_Ptr      is access all Unsigned_Long;
   type    Unsigned_Long_Long_Ptr is access all Unsigned_Long_Long;
   type    Float_Ptr              is access all Float;
   type    Double_Ptr             is access all Double;
   type    Long_Double_Ptr        is access all Long_Double;
   type    Char_Ptr               is access all Char;
   type    Wchar_Ptr              is access all Wchar;
   type    Octet_Ptr              is access all Octet;
   type    Boolean_Ptr            is access all Boolean;
   type    String_Ptr             is access all String;
   type    Wide_String_Ptr        is access all Wide_String;

   --  and the deallocation method for each pointer type

   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free


     (Object => Short,


      Name   => Short_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Long,

      Name   => Long_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Long_Long,

      Name   => Long_Long_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Unsigned_Short,

      Name   => Unsigned_Short_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Unsigned_Long,

      Name   => Unsigned_Long_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Unsigned_Long_Long,

      Name   => Unsigned_Long_Long_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Float,

      Name   => Float_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Double,

      Name   => Double_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Long_Double,

      Name   => Long_Double_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Char,

      Name   => Char_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Wchar,

      Name   => Wchar_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Octet,

      Name   => Octet_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Boolean,

      Name   => Boolean_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => String,

      Name   => String_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Wide_String,

      Name   => Wide_String_Ptr);

   -----------------------------
   -- Trimmed_Image functions --
   -----------------------------

   --  The following return 'Image (X) without the leading space. The intent is
   --  that they are called with a type conversion (unless the type is already
   --  Long_Long or Unsigned_Long_Long).

   function Trimmed_Image (X : Long_Long) return Standard.String;
   function Trimmed_Image (X : Unsigned_Long_Long) return Standard.String;

   ---------------------------------
   -- String conversion functions --
   ---------------------------------

   function To_PolyORB_String (Source : Standard.String) return String;
   function To_Standard_String (Source : String) return Standard.String;

   function To_PolyORB_Wide_String
     (Source : Standard.Wide_String) return Wide_String;

   function To_Standard_Wide_String
     (Source : Wide_String) return Standard.Wide_String;

   type Identifier   is new PolyORB.Types.String;
   type RepositoryId is new PolyORB.Types.String;

   ------------------------------------------
   -- Synchronisation of request execution --
   ------------------------------------------

   --  XXX Do we really need this type ?
   --  Should be already managed in PolyORB.Any ...

   --  This type is declared here because it must be visible in the specs of
   --  Requests and References.

   type Synchronisation_Scope is
     (None,
      With_Transport,
      With_Server,
      With_Target);
   --  A 'synchronistaion scope' value is associated with each request object.

   --  When a request is not synchronised, the middleware returns to the caller
   --  before passing the request to the transport layer. The middleware MUST
   --  guarantee that the call is non-blocking.

   --  When a request is synchronised With_Transport, the middleware must not
   --  return to the caller before the corresponding message message has been
   --  accepted by the transport layer.

   --  When a request is synchronised With_Server, the middleware does not
   --  return before receiving a confirmation that the request message has been
   --  received by the server middleware.

   --  When a request is synchronised With_Target, the middlware does not
   --  return to the caller before receinving a confirmation that the request
   --  has been executed by the target object.

private

   pragma Inline (To_PolyORB_String);
   pragma Inline (To_Standard_String);
   pragma Inline (To_PolyORB_Wide_String);
   pragma Inline (To_Standard_Wide_String);

end PolyORB.Types;
