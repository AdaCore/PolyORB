------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         C O R B A . H E L P E R                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body CORBA.Helper is

   ----------------------
   -- TC_Repository_Id --
   ----------------------

   TC_RepositoryId_Cache : CORBA.TypeCode.Object;

   function TC_RepositoryId return CORBA.TypeCode.Object is
   begin
      return TC_RepositoryId_Cache;
   end TC_RepositoryId;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : CORBA.Any) return CORBA.RepositoryId is
      Result : constant CORBA.String := CORBA.From_Any (Item);
   begin
      return CORBA.RepositoryId (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : CORBA.RepositoryId) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.String (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_RepositoryId);
      return Result;
   end To_Any;

   -------------------
   -- TC_Identifier --
   -------------------

   TC_Identifier_Cache : CORBA.TypeCode.Object;

   function TC_Identifier return CORBA.TypeCode.Object is
   begin
      return TC_Identifier_Cache;
   end TC_Identifier;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : CORBA.Any) return CORBA.Identifier is
      Result : constant CORBA.String := CORBA.From_Any (Item);
   begin
      return CORBA.Identifier (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : CORBA.Identifier) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.String (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_Identifier);
      return Result;
   end To_Any;

   -------------------
   -- TC_ScopedName --
   -------------------

   TC_ScopedName_Cache : CORBA.TypeCode.Object;

   function TC_ScopedName return CORBA.TypeCode.Object is
   begin
      return TC_ScopedName_Cache;
   end TC_ScopedName;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : CORBA.Any) return CORBA.ScopedName is
      Result : constant CORBA.String := CORBA.From_Any (Item);
   begin
      return CORBA.ScopedName (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : CORBA.ScopedName) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.String (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_ScopedName);
      return Result;
   end To_Any;

   -------------------
   -- TC_Visibility --
   -------------------

   TC_Visibility_Cache : CORBA.TypeCode.Object;

   function TC_Visibility return CORBA.TypeCode.Object is
   begin
      return TC_Visibility_Cache;
   end TC_Visibility;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : CORBA.Any) return CORBA.Visibility is
      Result : constant CORBA.Short := CORBA.From_Any (Item);
   begin
      return CORBA.Visibility (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : CORBA.Visibility) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Short (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_Visibility);
      return Result;
   end To_Any;

   -------------------
   -- TC_PolicyType --
   -------------------

   TC_PolicyType_Cache : CORBA.TypeCode.Object;

   function TC_PolicyType return CORBA.TypeCode.Object is
   begin
      return TC_PolicyType_Cache;
   end TC_PolicyType;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : CORBA.Any) return CORBA.PolicyType is
      Result : constant CORBA.Unsigned_Long := CORBA.From_Any (Item);
   begin
      return CORBA.PolicyType (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : CORBA.PolicyType) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Unsigned_Long (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_PolicyType);
      return Result;
   end To_Any;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      use CORBA.TypeCode;

      function Build_TC_Alias_String
        (Name : Standard.String)
         return CORBA.TypeCode.Object;
      --  Build a typecode for type Name which is an alias of CORBA::String

      function Build_TC_Alias_String
        (Name : Standard.String) return CORBA.TypeCode.Object
      is
      begin
         return CORBA.TypeCode.Internals.Build_Alias_TC
           (Name   => To_CORBA_String (Name),
            Id     => To_CORBA_String ("IDL:omg.org/CORBA/" & Name & ":1.0"),
            Parent => CORBA.TC_String);
      end Build_TC_Alias_String;

   begin
      TC_RepositoryId_Cache := Build_TC_Alias_String ("RepositoryId");
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_RepositoryId_Cache);

      TC_Identifier_Cache   := Build_TC_Alias_String ("Identifier");
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_Identifier_Cache);

      TC_ScopedName_Cache   := Build_TC_Alias_String ("ScopedName");
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_ScopedName_Cache);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("Visibility");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Visibility:1.0");
      begin
         TC_Visibility_Cache := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => CORBA.TC_Short);
         CORBA.TypeCode.Internals.Disable_Reference_Counting
           (TC_Visibility_Cache);
      end;

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("PolicyType");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:CORBA/PolicyType:1.0");
      begin
         TC_PolicyType_Cache := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => CORBA.TC_Unsigned_Long);
         CORBA.TypeCode.Internals.Disable_Reference_Counting
           (TC_PolicyType_Cache);
      end;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"corba.helper",
       Conflicts => Empty,
       Depends   => +"corba" & "any",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end CORBA.Helper;
