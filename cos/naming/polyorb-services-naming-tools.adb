------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R . S E R V I C E S . N A M I N G . T O O L S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.References.IOR;

with PolyORB.Services.Naming.NamingContext.Client;
with PolyORB.Services.Naming.NamingContext.Helper;

package body PolyORB.Services.Naming.Tools is

   use PolyORB.Services.Naming.NamingContext.Helper;
   use PolyORB.Services.Naming.NamingContext.Client;
   use PolyORB.Services.Naming.NamingContext;

   subtype NameComponent_Array is
     PolyORB.Services.Naming.SEQUENCE_NameComponent.Element_Array;

   RNS  : PolyORB.Services.Naming.NamingContext.Ref;
   --  Reference to the Naming Service in use.
   --  XXX use a mechanism similar to Resolve_Initial_References.

   function Retrieve_Context
     (Name   : in PolyORB.Services.Naming.Name)
     return PolyORB.Services.Naming.NamingContext.Ref;
   --  Return a CosNaming.NamingContext.Ref that designates the
   --  NamingContext registered as Name.

   ----------
   -- Init --
   ----------

   procedure Init (Ref : PolyORB.References.Ref) is
   begin
      RNS := To_Ref (Ref);
   end Init;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Guard : in out Server_Guard) is
      Name : constant String
        := PolyORB.Types.To_Standard_String (Guard.Name);
   begin
      if Name /= "" then
         Unregister (Name);
      end if;
   end Finalize;

   ------------
   -- Locate --
   ------------

   function Locate
     (Name : PolyORB.Services.Naming.Name)
     return PolyORB.References.Ref is
   begin
      return Resolve (RNS, Name);
   end Locate;

   ------------
   -- Locate --
   ------------

   function Locate
     (Context : PolyORB.Services.Naming.NamingContext.Ref;
      Name    : PolyORB.Services.Naming.Name)
     return PolyORB.References.Ref
   is
   begin
      return Resolve (Context, Name);
   end Locate;

   ------------
   -- Locate --
   ------------

   function Locate
     (IOR_Or_Name : String;
      Sep : Character := '/')
     return PolyORB.References.Ref
   is
      use PolyORB.References.IOR;

   begin
      if IOR_Or_Name (IOR_Or_Name'First .. IOR_Or_Name'First + 3) = "IOR:" then
         return String_To_Object
           (PolyORB.Types.To_PolyORB_String (IOR_Or_Name));
      end if;

      return Locate (Parse_Name (IOR_Or_Name, Sep));
   end Locate;

   ------------
   -- Locate --
   ------------

   function Locate
     (Context     : PolyORB.Services.Naming.NamingContext.Ref;
      IOR_Or_Name : String;
      Sep         : Character := '/')
      return PolyORB.References.Ref
   is
      use PolyORB.References.IOR;
   begin
      if IOR_Or_Name (IOR_Or_Name'First .. IOR_Or_Name'First + 3) = "IOR:" then
         return String_To_Object
           (PolyORB.Types.To_PolyORB_String (IOR_Or_Name));
      end if;

      return Locate (Context, Parse_Name (IOR_Or_Name, Sep));
   end Locate;

   ----------------------
   -- Retrieve_Context --
   ----------------------

   function Retrieve_Context
     (Name   : in PolyORB.Services.Naming.Name)
     return PolyORB.Services.Naming.NamingContext.Ref
   is
      Cur : PolyORB.Services.Naming.NamingContext.Ref := RNS;
      Ref : PolyORB.Services.Naming.NamingContext.Ref;
      N : PolyORB.Services.Naming.Name;

      NCA : constant NameComponent_Array
        := PolyORB.Services.Naming.To_Element_Array (Name);

   begin
      for I in NCA'Range loop
         N := PolyORB.Services.Naming.To_Sequence ((1 => NCA (I)));
         begin
            Ref := To_Ref (Resolve (Cur, N));
         exception
            when NotFound =>
               Ref := Bind_New_Context (Cur, N);
         end;
         Cur := Ref;
      end loop;
      return Cur;
   end Retrieve_Context;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name   : in String;
      Ref    : in PolyORB.References.Ref;
      Rebind : in Boolean := False;
      Sep    : in Character := '/')
   is
      Context : NamingContext.Ref;
      NCA : constant NameComponent_Array :=
        PolyORB.Services.Naming.To_Element_Array (Parse_Name (Name, Sep));
      N : constant PolyORB.Services.Naming.Name
        := PolyORB.Services.Naming.To_Sequence ((1 => NCA (NCA'Last)));
   begin
      if NCA'Length = 1 then
         Context := RNS;
      else
         Context := Retrieve_Context
           (PolyORB.Services.Naming.To_Sequence
            (NCA (NCA'First .. NCA'Last - 1)));
      end if;

      Bind (Context, N, Ref);
   exception
      when NamingContext.AlreadyBound =>
         if Rebind then
            PolyORB.Services.Naming.NamingContext.Client.Rebind
              (Context, N, Ref);
         else
            raise;
         end if;
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Guard  : in out Server_Guard;
      Name   : in Standard.String;
      Ref    : in PolyORB.References.Ref;
      Rebind : in Boolean := False;
      Sep    : in Character := '/')
   is
   begin
      Register (Name, Ref, Rebind, Sep);
      Guard.Name := PolyORB.Types.To_PolyORB_String (Name);
   end Register;

   ----------------
   -- Parse_Name --
   ----------------

   function Parse_Name
     (Name : String;
      Sep  : Character := '/')
     return PolyORB.Services.Naming.Name
   is
      Result    : PolyORB.Services.Naming.Name;
      Unescaped : String (Name'Range);
      First     : Integer := Unescaped'First;
      Last      : Integer := Unescaped'First - 1;
      Last_Unescaped_Period : Integer := Unescaped'First - 1;

      Seen_Backslash : Boolean := False;
      End_Of_NC : Boolean := False;
   begin
      for I in Name'Range loop
         if not Seen_Backslash and then Name (I) = '\' then
            Seen_Backslash := True;
         else
            --  Seen_Backslash and seeing an escaped character
            --  *or* seeing a non-escaped non-backslash character.

            if not Seen_Backslash and then Name (I) = Sep then
               --  Seeing a non-escaped Sep
               End_Of_NC := True;
            else
               --  Seeing a non-escaped non-backslash, non-Sep
               --  character, or seeing an escaped character.
               Last := Last + 1;
               Unescaped (Last) := Name (I);
               End_Of_NC := I = Name'Last;
            end if;

            if not Seen_Backslash and then Name (I) = '.' then
               Last_Unescaped_Period := Last;
            end if;

            if End_Of_NC then
               if Last_Unescaped_Period < First then
                  Last_Unescaped_Period := Last + 1;
               end if;
               Append
                 (Result, NameComponent'
                  (Id   => To_PolyORB_String
                   (Unescaped (First .. Last_Unescaped_Period - 1)),
                   Kind => To_PolyORB_String
                   (Unescaped (Last_Unescaped_Period + 1 .. Last))));
               Last_Unescaped_Period := Last;
               First := Last + 1;
            end if;

            Seen_Backslash := Name (I) = '\' and then not Seen_Backslash;
         end if;
      end loop;

      return Result;
   end Parse_Name;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Name   : in String)
   is
      N   : PolyORB.Services.Naming.Name;
      NC  : NameComponent;
   begin
      NC.kind := To_PolyORB_String ("");
      NC.id   := To_PolyORB_String (Name);
      Append (N, NC);
      Unbind (RNS, N);
   end Unregister;

end PolyORB.Services.Naming.Tools;
