------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                   B R O C A . N A M I N G _ T O O L S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with CORBA.ORB;
with CosNaming.NamingContext.Helper;
use CosNaming, CosNaming.NamingContext, CosNaming.NamingContext.Helper;

package body Broca.Naming_Tools is

   subtype NameComponent_Array is
     CosNaming.IDL_SEQUENCE_CosNaming_NameComponent.Element_Array;

   function Retrieve_Context
     (Name   : in CosNaming.Name)
     return Ref;
   --  Return a CosNaming.NamingContext.Ref that designates the
   --  NamingContext registered as Name.

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

   function Locate
     (Name : CosNaming.Name)
     return CORBA.Object.Ref
   is
      RNS  : constant NamingContext.Ref :=
        To_Ref (CORBA.ORB.Resolve_Initial_References
                (CORBA.ORB.To_Corba_String ("NamingService")));
   begin
      return resolve (RNS, Name);
   end Locate;

   ------------
   -- Locate --
   ------------

   function Locate
     (IOR_Or_Name : String;
      Sep : Character := '/')
     return CORBA.Object.Ref is
   begin
      if IOR_Or_Name (IOR_Or_Name'First .. IOR_Or_Name'First + 3) = "IOR:" then
         declare
            Obj : CORBA.Object.Ref;
         begin
            CORBA.ORB.String_To_Object
              (CORBA.To_CORBA_String (IOR_Or_Name), Obj);
            return Obj;
         end;
      end if;

      return Locate (Parse_Name (IOR_Or_Name, Sep));
   end Locate;

   ----------------------
   -- Retrieve_Context --
   ----------------------

   function Retrieve_Context
     (Name   : in CosNaming.Name)
     return Ref
   is
      Cur : NamingContext.Ref :=
        To_Ref (CORBA.ORB.Resolve_Initial_References
                (CORBA.ORB.To_Corba_String ("NamingService")));
      Ref : NamingContext.Ref;
      N : CosNaming.Name;

      NCA : constant NameComponent_Array
        := CosNaming.To_Element_Array (Name);

   begin
      for I in NCA'Range loop
         N := CosNaming.To_Sequence ((1 => NCA (I)));
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
      Ref    : in CORBA.Object.Ref;
      Rebind : in Boolean := False;
      Sep    : in Character := '/')
   is
      Context : NamingContext.Ref;
      NCA : constant NameComponent_Array :=
        CosNaming.To_Element_Array (Parse_Name (Name, Sep));
      N : constant CosNaming.Name := CosNaming.To_Sequence
        ((1 => NCA (NCA'Last)));
   begin
      if NCA'Length = 1 then
         Context := To_Ref
           (CORBA.ORB.Resolve_Initial_References
            (CORBA.ORB.To_Corba_String ("NamingService")));
      else
         Context := Retrieve_Context
           (CosNaming.To_Sequence (NCA (NCA'First .. NCA'Last - 1)));
      end if;

      bind (Context, N, Ref);
   exception
      when Namingcontext.AlreadyBound =>
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
      Name   : in String;
      Ref    : in CORBA.Object.Ref;
      Rebind : in Boolean := False;
      Sep    : in Character := '/')
   is
   begin
      Register (Name, Ref, Rebind, Sep);
      Guard.Name := CORBA.To_CORBA_String (Name);
   end Register;

   ----------------
   -- Parse_Name --
   ----------------

   function Parse_Name
     (Name : String;
      Sep  : Character := '/')
     return CosNaming.Name
   is
      Result    : CosNaming.Name;
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
                  (Id   => To_CORBA_String
                   (Unescaped (First .. Last_Unescaped_Period - 1)),
                   Kind => To_CORBA_String
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
      RNS : constant NamingContext.Ref :=
        To_Ref (CORBA.ORB.Resolve_Initial_References
                (CORBA.ORB.To_Corba_String ("NamingService")));
      N   : CosNaming.Name;
      NC  : NameComponent;
   begin
      NC.Kind := CosNaming.To_CORBA_String ("");
      NC.Id   := CosNaming.To_CORBA_String (Name);
      Append (N, NC);
      Unbind (RNS, N);
   end Unregister;

end Broca.Naming_Tools;
