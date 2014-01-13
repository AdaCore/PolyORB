------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . C O R B A _ P . N A M I N G _ T O O L S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2014, Free Software Foundation, Inc.          --
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

with CORBA.ORB;
with CosNaming.NamingContext.Helper;

with PolyORB.Utils;

package body PolyORB.CORBA_P.Naming_Tools is

   use CORBA.ORB;

   use CosNaming;
   use CosNaming.NamingContext;
   use CosNaming.NamingContext.Helper;

   use PolyORB.Utils;

   subtype NameComponent_Array is
     CosNaming.IDL_SEQUENCE_CosNaming_NameComponent.Element_Array;

   function Retrieve_Context (Name : CosNaming.Name) return Ref;
   --  Return a CosNaming.NamingContext.Ref that designates the NamingContext
   --  registered as Name.

   function Root_Context return Ref;
   --  Retrieve root naming context from ORB initial references

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Guard : in out Server_Guard) is
      Name : constant String := CORBA.To_Standard_String (Guard.Name);
   begin
      if Name /= "" then
         Unregister (Name);
      end if;
   end Finalize;

   ------------
   -- Locate --
   ------------

   function Locate (Name : CosNaming.Name) return CORBA.Object.Ref is
   begin
      return resolve (Root_Context, Name);
   end Locate;

   ------------
   -- Locate --
   ------------

   function Locate
     (Context : CosNaming.NamingContext.Ref;
      Name    : CosNaming.Name) return CORBA.Object.Ref
   is
   begin
      return resolve (Context, Name);
   end Locate;

   ------------
   -- Locate --
   ------------

   function Locate
     (IOR_Or_Name : String;
      Sep         : Character := '/') return CORBA.Object.Ref
   is
   begin
      if Has_Prefix (IOR_Or_Name, Prefix => "IOR:") then
         declare
            Obj : CORBA.Object.Ref;
         begin
            String_To_Object (CORBA.To_CORBA_String (IOR_Or_Name), Obj);
            return Obj;
         end;
      end if;

      return Locate (Parse_Name (IOR_Or_Name, Sep));
   end Locate;

   ------------
   -- Locate --
   ------------

   function Locate
     (Context     : CosNaming.NamingContext.Ref;
      IOR_Or_Name : String;
      Sep         : Character := '/') return CORBA.Object.Ref
   is
   begin
      if Has_Prefix (IOR_Or_Name, Prefix => "IOR:") then
         declare
            Obj : CORBA.Object.Ref;
         begin
            String_To_Object (CORBA.To_CORBA_String (IOR_Or_Name), Obj);
            return Obj;
         end;
      end if;

      return Locate (Context, Parse_Name (IOR_Or_Name, Sep));
   end Locate;

   ----------------------
   -- Retrieve_Context --
   ----------------------

   function Retrieve_Context (Name : CosNaming.Name) return Ref is
      Cur : NamingContext.Ref := Root_Context;
      Ref : NamingContext.Ref;
      N   : CosNaming.Name;
      NCA : constant NameComponent_Array := CosNaming.To_Element_Array (Name);

   begin
      for I in NCA'Range loop
         N := CosNaming.To_Sequence ((1 => NCA (I)));
         begin
            Ref := To_Ref (resolve (Cur, N));
         exception
            when NotFound =>
               Ref := NamingContext.Ref (bind_new_context (Cur, N));
         end;
         Cur := Ref;
      end loop;
      return Cur;
   end Retrieve_Context;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name   : String;
      Ref    : CORBA.Object.Ref;
      Rebind : Boolean := False;
      Sep    : Character := '/')
   is
      Context : NamingContext.Ref;
      NCA     : constant NameComponent_Array :=
                  CosNaming.To_Element_Array (Parse_Name (Name, Sep));
      N       : constant CosNaming.Name :=
                  CosNaming.To_Sequence ((1 => NCA (NCA'Last)));
   begin
      if NCA'Length = 1 then
         Context := Root_Context;
      else
         Context := Retrieve_Context
           (CosNaming.To_Sequence (NCA (NCA'First .. NCA'Last - 1)));
      end if;

      bind (Context, N, Ref);
   exception
      when NamingContext.AlreadyBound =>
         if Rebind then
            NamingContext.rebind (Context, N, Ref);
         else
            raise;
         end if;
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Guard  : in out Server_Guard;
      Name   : String;
      Ref    : CORBA.Object.Ref;
      Rebind : Boolean := False;
      Sep    : Character := '/')
   is
   begin
      Register (Name, Ref, Rebind, Sep);
      Guard.Name := CORBA.To_CORBA_String (Name);
   end Register;

   ------------------
   -- Root_Context --
   ------------------

   NameService_ObjectId   : constant ObjectId :=
                              To_CORBA_String ("NameService");
   NamingService_ObjectId : constant ObjectId :=
                              To_CORBA_String ("NamingService");

   function Root_Context return Ref is
      CORBA_Ref : CORBA.Object.Ref;
   begin
      --  First try standard initial reference

      CORBA_Ref := Resolve_Initial_References (NameService_ObjectId);

      --  Next fall back to legacy implementation-defined one

      if CORBA_Ref.Is_Nil then
         CORBA_Ref := Resolve_Initial_References (NamingService_ObjectId);
      end if;

      return To_Ref (CORBA_Ref);
   end Root_Context;

   ----------------
   -- Parse_Name --
   ----------------

   function Parse_Name
     (Name : String; Sep  : Character := '/') return CosNaming.Name
   is
      Result    : CosNaming.Name;
      Unescaped : String (Name'Range);
      First     : Integer := Unescaped'First;
      Last      : Integer;
      Last_Unescaped_Period : Integer := Unescaped'First - 1;

      Seen_Backslash : Boolean := False;
      End_Of_NC : Boolean := False;
   begin
      --  First ignore any leading separators

      while First <= Name'Last and then Name (First) = Sep loop
         First := First + 1;
      end loop;

      Last := First - 1;

      for J in First .. Name'Last loop
         if not Seen_Backslash and then Name (J) = '\' then
            Seen_Backslash := True;
         else
            --  Seen_Backslash and seeing an escaped character
            --  *or* seeing a non-escaped non-backslash character.

            if not Seen_Backslash and then Name (J) = Sep then
               --  Seeing a non-escaped Sep

               End_Of_NC := True;
            else
               --  Seeing a non-escaped non-backslash, non-Sep character,
               --  or seeing an escaped character.

               Last := Last + 1;
               Unescaped (Last) := Name (J);
               End_Of_NC := J = Name'Last;
            end if;

            if not Seen_Backslash and then Name (J) = '.' then
               Last_Unescaped_Period := Last;
            end if;

            if End_Of_NC then
               if Last_Unescaped_Period < First then
                  Last_Unescaped_Period := Last + 1;
               end if;

               Append
                 (Result, NameComponent'
                  (id   => To_CORBA_String
                             (Unescaped (First .. Last_Unescaped_Period - 1)),
                   kind => To_CORBA_String
                             (Unescaped (Last_Unescaped_Period + 1 .. Last))));

               Last_Unescaped_Period := Last;
               First := Last + 1;
            end if;

            Seen_Backslash := Name (J) = '\' and then not Seen_Backslash;
         end if;
      end loop;

      return Result;
   end Parse_Name;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Name : String) is
      N   : CosNaming.Name;
      NC  : NameComponent;
   begin
      NC.kind := To_CORBA_String ("");
      NC.id   := To_CORBA_String (Name);
      Append (N, NC);
      unbind (Root_Context, N);
   end Unregister;

end PolyORB.CORBA_P.Naming_Tools;
