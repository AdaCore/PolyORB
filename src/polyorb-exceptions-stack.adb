------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . E X C E P T I O N S . S T A C K              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2004 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id: //droopi/main/src/polyorb-exceptions-stack.adb#10 $

with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Log;
with PolyORB.Tasking.Mutexes;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;

package body PolyORB.Exceptions.Stack is

   use PolyORB.Log;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log ("polyorb.exceptions.stack");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   --  When an exception with members is raised (Raise_Exception), we
   --  allocate an exception occurrence id and attach to the exception
   --  occurrence a message with a magic string and the id. The member
   --  is stored in dynamic structure with the id. When we call
   --  Get_Members, we retrieve the exception occurrence id from the
   --  attached message. The member may have been removed in the
   --  meantime if too many exceptions were raised between the call to
   --  Raise_Exception and Get_Members (very rare). We have to keep
   --  the list size in a max size because the user may not retrieve
   --  the member of an exception with members. In this case, the
   --  members will never be deallocated. This limit forces some kind
   --  of garbage collection.

   Magic : constant String := "PO_Exc_Occ";

   type Exc_Occ_Id_Type is new Natural;

   Seed_Id : Exc_Occ_Id_Type := 1;
   Null_Id : constant Exc_Occ_Id_Type := 0;

   type Exc_Occ_Node is record
      Id   : Exc_Occ_Id_Type;
      Mbr  : Exception_Members_Access;
   end record;

   package Exc_Occ_Lists is new PolyORB.Utils.Chained_Lists
     (Exc_Occ_Node, Doubly_Chained => True);
   use Exc_Occ_Lists;

   Exc_Occ_List : Exc_Occ_Lists.List;

   Exc_Occ_Lock : Mutex_Access;
   --  Mutex used to safely access Exc_Occ list

   Max_Exc_Occ_List_Size : constant Natural := 100;

   function Image (V : Exc_Occ_Id_Type) return String;
   --  Store the magic string and the exception occurrence id

   function Value (M : String) return Exc_Occ_Id_Type;
   --  Extract the exception occurrence id from the exception
   --  message. Return Null_Id if the exception message has no the
   --  expected format.

   procedure Dump_All_Occurrences;
   --  Dump the occurrence list (not protected)

   --------------------------
   -- Dump_All_Occurrences --
   --------------------------

   procedure Dump_All_Occurrences is
      It : Iterator := First (Exc_Occ_List);

   begin
      O ("Dump_All_Occurrences:");

      if Exc_Occ_List = Empty then
         O ("No stored exceptions.");
         return;
      end if;

      while not Last (It) loop
         O ("  " & Image (Value (It).all.Id));
         Next (It);
      end loop;
   end Dump_All_Occurrences;

   --------------------------
   -- Get_Or_Purge_Members --
   --------------------------

   procedure Get_Or_Purge_Members
     (Exc_Occ     :     Ada.Exceptions.Exception_Occurrence;
      Exc_Mbr     : out PolyORB.Exceptions.Exception_Members'Class;
      Get_Members :     Boolean);
   --  Internal implementation of Get_Members and Purge_Members.
   --  If Get_Members is true, the retrieved members object is
   --  assigned to Exc_Mbr, else the object is discarded and no
   --  assignment is made.

   procedure Get_Or_Purge_Members
     (Exc_Occ     :     Ada.Exceptions.Exception_Occurrence;
      Exc_Mbr     : out PolyORB.Exceptions.Exception_Members'Class;
      Get_Members :     Boolean)
   is
      Exc_Occ_Id : Exc_Occ_Id_Type;
      It : Iterator;

   begin
      Enter (Exc_Occ_Lock);
      pragma Debug (O ("Get_Members: "
                       & Ada.Exceptions.Exception_Name (Exc_Occ)));
      pragma Debug (O ("    message: "
                       & Ada.Exceptions.Exception_Message (Exc_Occ)));
      pragma Debug (Dump_All_Occurrences);

      --  If Exc_Occ_Id = Null_Id, the exception has no member

      Exc_Occ_Id := Value (Ada.Exceptions.Exception_Message (Exc_Occ));
      if Exc_Occ_Id = Null_Id then
         Leave (Exc_Occ_Lock);
         return;
      end if;

      --  Scan the list using the exception occurrence id

      It := First (Exc_Occ_List);
      while not Last (It) loop
         exit when Value (It).all.Id = Exc_Occ_Id;

         Next (It);
      end loop;

      if Value (It).all.Id /= Exc_Occ_Id then
         Leave (Exc_Occ_Lock);

         --  Too many exceptions were raised and this member is no
         --  longer available.

         --  PolyORB.Exceptions.Raise_Imp_Limit;
         raise Program_Error;
      end if;

      --  Update out parameter

      if Get_Members then
         Exc_Mbr := Value (It).all.Mbr.all;
         --  May raise Constraint_Error if the tags do not match

      end if;

      --  Remove member from list

      Free (Value (It).all.Mbr);
      Remove (Exc_Occ_List, It);

      Leave (Exc_Occ_Lock);

   exception
      when others =>

         --  Remove member from list

         Free (Value (It).all.Mbr);
         Remove (Exc_Occ_List, It);

         Leave (Exc_Occ_Lock);
         raise;
   end Get_Or_Purge_Members;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (Exc_Occ : in  Ada.Exceptions.Exception_Occurrence;
      Exc_Mbr : out PolyORB.Exceptions.Exception_Members'Class)
   is
   begin
      Get_Or_Purge_Members (Exc_Occ, Exc_Mbr, Get_Members => True);
   end Get_Members;

   -------------------
   -- Purge_Members --
   -------------------

   procedure Purge_Members
     (Exc_Occ : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      declare
         Dummy : System_Exception_Members;
      begin
         Get_Or_Purge_Members (Exc_Occ, Dummy, Get_Members => False);
      exception
         when others =>
            null;
      end;
   end Purge_Members;

   -----------
   -- Image --
   -----------

   function Image (V : Exc_Occ_Id_Type) return String is
   begin
      return Magic & Exc_Occ_Id_Type'Image (V);
   end Image;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception
     (Exc_Id  : in Ada.Exceptions.Exception_Id;
      Exc_Mbr : in PolyORB.Exceptions.Exception_Members'Class)
   is
      New_Node : Exc_Occ_Node;

   begin
      Enter (Exc_Occ_Lock);

      --  Keep the list size to a max size. Otherwise, remove the
      --  oldest member (first in the list).

      if Length (Exc_Occ_List) = Max_Exc_Occ_List_Size then
         Extract_First (Exc_Occ_List, New_Node);
         Free (New_Node.Mbr);
      end if;

      pragma Debug (O ("Assigning ID: " & Image (Seed_Id)));
      pragma Debug (Dump_All_Occurrences);

      --  Generate a fresh exception occurrence id

      New_Node.Id := Seed_Id;
      New_Node.Mbr
        := new PolyORB.Exceptions.Exception_Members'Class'(Exc_Mbr);

      if Seed_Id = Exc_Occ_Id_Type'Last then
         Seed_Id := Null_Id;
      end if;
      Seed_Id := Seed_Id + 1;

      --  Append to the list

      Append (Exc_Occ_List, New_Node);

      pragma Debug (O ("Raise ("
                       & Ada.Exceptions.Exception_Name (Exc_Id)
                       & ", " & Image (New_Node.Id) & ")."));
      pragma Debug (Dump_All_Occurrences);
      Leave (Exc_Occ_Lock);

      Ada.Exceptions.Raise_Exception (Exc_Id, Image (New_Node.Id));
      raise Program_Error;
   end Raise_Exception;

   -----------
   -- Value --
   -----------

   function Value (M : String) return Exc_Occ_Id_Type is
      V : Exc_Occ_Id_Type := 0;
      N : Natural := M'First;

   begin
      if M'Length <= Magic'Length + 1 then
         return Null_Id;
      end if;

      --  Look for the magic string

      for J in Magic'Range loop
         if Magic (J) /= M (N) then
            return Null_Id;
         end if;
         N := N + 1;
      end loop;

      if M (N) /= ' ' then
         return Null_Id;
      end if;
      N := N + 1;

      --  Scan the exception occurrence id

      while N <= M'Last loop
         if M (N) not in '0' .. '9' then
            return Null_Id;
         end if;

         V := V * 10 + Character'Pos (M (N)) - Character'Pos ('0');
         N := N + 1;
      end loop;

      return V;
   end Value;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Create (Exc_Occ_Lock);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"exceptions.stack",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => +"tasking.mutexes",
       Provides  => PolyORB.Initialization.String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access));
end PolyORB.Exceptions.Stack;
