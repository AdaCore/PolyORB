------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--               B R O C A . E X C E P T I O N S . S T A C K                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
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

with Ada.Unchecked_Deallocation;
--  with System.Storage_Elements;

with Broca.Debug;
with Broca.Soft_Links; use Broca.Soft_Links;

package body Broca.Exceptions.Stack is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.exceptions.stack");
   procedure O is new Broca.Debug.Output (Flag);

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

   Magic : constant String := "AB_Exc_Occ";

   type IDL_Exception_Members_Ptr is
      access all IDL_Exception_Members'Class;

   type Exc_Occ_Id_Type is new Natural;

   Seed_Id : Exc_Occ_Id_Type := 1;
   Null_Id : constant Exc_Occ_Id_Type := 0;

   type Exc_Occ_Node;
   type Exc_Occ_List is access Exc_Occ_Node;
   type Exc_Occ_Node is
      record
         Id   : Exc_Occ_Id_Type;
         Mbr  : IDL_Exception_Members_Ptr;
         Next : Exc_Occ_List;
      end record;

   Exc_Occ_Head : Exc_Occ_List;
   Exc_Occ_Tail : Exc_Occ_List;

   Exc_Occ_List_Size     : Natural := 0;
   Max_Exc_Occ_List_Size : constant Natural := 100;

   procedure Free is
      new Ada.Unchecked_Deallocation
        (Exc_Occ_Node, Exc_Occ_List);

   procedure Free is
      new Ada.Unchecked_Deallocation
        (IDL_Exception_Members'Class,
         IDL_Exception_Members_Ptr);

   function Image (V : Exc_Occ_Id_Type) return String;
   --  Store the magic string and the exception occurrence id.

   function Value (M : String) return Exc_Occ_Id_Type;
   --  Extract the exception occurrence id from the exception
   --  message. Return Null_Id if the exception message has no the
   --  expected format.

   procedure Dump_All_Occurrences;
   --  Dump the occurrence list (not protected).

   --------------------------
   -- Dump_All_Occurrences --
   --------------------------

   procedure Dump_All_Occurrences
   is
      Current : Exc_Occ_List := Exc_Occ_Head;
   begin
      O ("Dump_All_Occurrences:");

      if Current = null then
         O ("No stored exceptions.");
         return;
      end if;

      while Current /= null loop
         --  O ("At " & System.Storage_Elements.To_Integer
         --     (Current.all'Address)'Img & ":");
         O ("  " & Image (Current.Id));
         Current := Current.Next;
      end loop;
   end Dump_All_Occurrences;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Or_Purge_Members
     (Exc_Occ     : CORBA.Exception_Occurrence;
      Exc_Mbr     : out IDL_Exception_Members'Class;
      Get_Members : Boolean);
   --  Internal implementation of Get_Members and Purge_Members.
   --  If Get_Members is true, the retrieved members object is
   --  assigned to Exc_Mbr, else the object is discarded and no
   --  assignment is made.

   procedure Get_Or_Purge_Members
     (Exc_Occ     : CORBA.Exception_Occurrence;
      Exc_Mbr     : out IDL_Exception_Members'Class;
      Get_Members : Boolean)
   is
      Exc_Occ_Id : Exc_Occ_Id_Type;
      Current    : Exc_Occ_List;
      Previous   : Exc_Occ_List;

   begin
      Enter_Critical_Section;
      pragma Debug (O ("Get_Members: "
                       & Ada.Exceptions.Exception_Name (Exc_Occ)));
      pragma Debug (O ("    message: "
                       & Ada.Exceptions.Exception_Message (Exc_Occ)));
      pragma Debug (Dump_All_Occurrences);

      --  If Exc_Occ_Id = Null_Id, the exception has no member.

      Exc_Occ_Id := Value (Ada.Exceptions.Exception_Message (Exc_Occ));
      if Exc_Occ_Id = Null_Id then
         Leave_Critical_Section;
         return;
      end if;

      --  Scan the list using the exception occurrence id.

      Current := Exc_Occ_Head;
      while Current /= null loop
         exit when Current.Id = Exc_Occ_Id;

         Previous := Current;
         Current  := Current.Next;
      end loop;

      if Current = null then
         Leave_Critical_Section;

         --  Too many exceptions were raised and this member is no
         --  longer available.

         Broca.Exceptions.Raise_Imp_Limit;
      end if;

      --  Remove member from list.

      if Previous /= null then
         Previous.Next := Current.Next;
      else
         Exc_Occ_Head := Current.Next;
      end if;

      if Exc_Occ_Tail = Current then
         Exc_Occ_Tail := Previous;
      end if;

      --  Update out parameter. An exception can be raised here.


      if Get_Members then
         Exc_Mbr := Current.Mbr.all;
         --  May raise Constraint_Error if the tags do not match.
      end if;

      Free (Current.Mbr);
      Free (Current);
      Exc_Occ_List_Size := Exc_Occ_List_Size - 1;
      Leave_Critical_Section;

   exception
      when others =>
         if Current /= null then
            if Current.Mbr /= null then
               Free (Current.Mbr);
            end if;
            Free (Current);
         end if;
         Exc_Occ_List_Size := Exc_Occ_List_Size - 1;
         Leave_Critical_Section;
         raise;
   end Get_Or_Purge_Members;

   procedure Get_Members
     (Exc_Occ : in CORBA.Exception_Occurrence;
      Exc_Mbr : out IDL_Exception_Members'Class) is
   begin
      Get_Or_Purge_Members (Exc_Occ, Exc_Mbr, Get_Members => True);
   end Get_Members;

   procedure Purge_Members
     (Exc_Occ : in CORBA.Exception_Occurrence) is
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
      return Magic & V'Img;
   end Image;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception
     (Exc_Id  : in Ada.Exceptions.Exception_Id;
      Exc_Mbr : in IDL_Exception_Members'Class)
   is
      Current    : Exc_Occ_List;
      Exc_Occ_Id : Exc_Occ_Id_Type;

   begin
      Enter_Critical_Section;

      --  Keep the list size to a max size. Otherwise, remove the
      --  oldest member (first in the list).

      if Exc_Occ_List_Size = Max_Exc_Occ_List_Size then
         Current := Exc_Occ_Head;
         Exc_Occ_Head := Exc_Occ_Head.Next;
         if Current.Mbr /= null then
            Free (Current.Mbr);
         end if;

      else
         Exc_Occ_List_Size := Exc_Occ_List_Size + 1;
         Current := new Exc_Occ_Node;
      end if;

      pragma Debug (O ("Assigning ID: " & Image (Seed_Id)));
      pragma Debug (Dump_All_Occurrences);

      --  Generate a fresh exception occurrence id.

      Exc_Occ_Id   := Seed_Id;
      Current.Id   := Seed_Id;
      Current.Mbr  := new IDL_Exception_Members'Class'(Exc_Mbr);
      Current.Next := null;

      if Seed_Id = Exc_Occ_Id_Type'Last then
         Seed_Id := Null_Id;
      end if;
      Seed_Id := Seed_Id + 1;

      --  Append to the list.

      if Exc_Occ_Head = null then
         Exc_Occ_Head := Current;
      end if;
      if Exc_Occ_Tail /= null then
         Exc_Occ_Tail.Next := Current;
      end if;
      Exc_Occ_Tail := Current;

      pragma Debug (O ("Raise ("
                       & Ada.Exceptions.Exception_Name (Exc_Id)
                       & ", " & Image (Exc_Occ_Id) & ")."));
      pragma Debug (Dump_All_Occurrences);
      Leave_Critical_Section;

      Ada.Exceptions.Raise_Exception (Exc_Id, Image (Exc_Occ_Id));
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

      --  Look for the magic string.

      for I in Magic'Range loop
         if Magic (I) /= M (N) then
            return Null_Id;
         end if;
         N := N + 1;
      end loop;

      if M (N) /= ' ' then
         return Null_Id;
      end if;
      N := N + 1;

      --  Scan the exception occurrence id.

      while N <= M'Last loop
         if M (N) not in '0' .. '9' then
            return Null_Id;
         end if;

         V := V * 10 + Character'Pos (M (N)) - Character'Pos ('0');
         N := N + 1;
      end loop;

      return V;
   end Value;

end Broca.Exceptions.Stack;
