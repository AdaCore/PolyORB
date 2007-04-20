------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . E X C E P T I O N S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
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

with Ada.Unchecked_Conversion;
with Ada.Characters.Latin_1;

pragma Warnings (Off);
with System.Exception_Table;
with System.Standard_Library;
pragma Warnings (On);
--  Mapping between exception names and exception ids.
--  GNAT internal exception table is used to maintain a list of all exceptions.

with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;

package body PolyORB.Exceptions is

   use Ada.Exceptions;

   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Types;
   use PolyORB.Utils;

   package L is new PolyORB.Log.Facility_Log ("polyorb.exceptions");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   -----------------------------
   -- User exception handling --
   -----------------------------

   type Exception_Info is record
      TC     : PolyORB.Any.TypeCode.Local_Ref;
      Raiser : Raise_From_Any_Procedure;
   end record;

   package Exception_Lists is new
     PolyORB.Utils.Chained_Lists (Exception_Info);

   function Find_Exception_Info
     (For_Exception : PolyORB.Types.RepositoryId)
     return Exception_Info;
   --  Return Exception_Info associated to 'For_Exception'

   All_Exceptions : Exception_Lists.List;
   --  Exception list, use to associate an exception typecode with a raiser
   --  function that retrieves member data from an Any and raises the exception
   --  with the appropriate information in the occurrence.

   All_Exceptions_Lock : Mutex_Access;
   --  Mutex used to safely access All_Exceptions list.

   --  When an exception with members is raised (Raise_Exception), we allocate
   --  an exception occurrence id and attach to the exception occurrence a
   --  message with a magic string and the id. The member is stored in dynamic
   --  structure with the id. When we call Get_Members, we retrieve the
   --  exception occurrence id from the attached message. The member may have
   --  been removed in the meantime if too many exceptions were raised between
   --  the call to Raise_Exception and Get_Members (very rare). We have to keep
   --  the list size in a max size because the user may not retrieve the member
   --  of an exception with members. In this case, the members will never be
   --  deallocated. This limit forces some kind of garbage collection.

   --  If exception is raised with optional user defined message then this
   --  message is appended to exception occurrence message after a magic string
   --  and the id, separating from id by the LF character. This character is
   --  used to detect the end of id.

   Magic : constant String := "PO_Exc_Occ";

   type Exc_Occ_Id_Type is new Natural;

   Seed_Id : Exc_Occ_Id_Type := 1;
   Null_Id : constant Exc_Occ_Id_Type := 0;

   type Exc_Occ_Node is record
      Id  : Exc_Occ_Id_Type;
      Mbr : Exception_Members_Access;
      Msg : Types.String;
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
   --  Extract the exception occurrence id from the exception message. Return
   --  Null_Id if the exception message has no the expected format.

   procedure Dump_All_Occurrences;
   --  Dump the occurrence list (not protected)

   procedure Get_Or_Purge_Members
     (Exc_Occ     :     Ada.Exceptions.Exception_Occurrence;
      Exc_Mbr     : out Exception_Members'Class;
      Get_Members :     Boolean);
   --  Internal implementation of Get_Members and Purge_Members. If Get_Members
   --  is true, the retrieved members object is assigned to Exc_Mbr, else the
   --  object is discarded and no assignment is made.

   function Get_ExcepId_By_RepositoryId
     (RepoId  : Standard.String)
      return Ada.Exceptions.Exception_Id;
   --  Return the corresponding Ada Exception_Id for a repository id

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
      Exc_Mbr     : out Exception_Members'Class;
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

         --  Too many exceptions were raised and this member is no longer
         --  available.

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

   -----------
   -- Image --
   -----------

   function Image (V : Exc_Occ_Id_Type) return String is
   begin
      return Magic & Exc_Occ_Id_Type'Image (V);
   end Image;

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

      --  Scan the exception occurrence id until end of id or LF character is
      --  found.

      while N <= M'Last and then M (N) /= Ada.Characters.Latin_1.LF loop
         if M (N) not in '0' .. '9' then
            return Null_Id;
         end if;

         V := V * 10 + Character'Pos (M (N)) - Character'Pos ('0');
         N := N + 1;
      end loop;

      return V;
   end Value;

   ----------------------
   -- User_Get_Members --
   ----------------------

   procedure User_Get_Members
     (Occurrence :     Ada.Exceptions.Exception_Occurrence;
      Members    : out Exception_Members'Class)
   is
   begin
      Get_Or_Purge_Members (Occurrence, Members, Get_Members => True);
   end User_Get_Members;

   ------------------------
   -- User_Purge_Members --
   ------------------------

   procedure User_Purge_Members
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      declare
         Dummy : System_Exception_Members;
      begin
         Get_Or_Purge_Members (Occurrence, Dummy, Get_Members => False);
      exception
         when others =>
            null;
      end;
   end User_Purge_Members;

   --------------------------
   -- User_Raise_Exception --
   --------------------------

   procedure User_Raise_Exception
     (Id      : Ada.Exceptions.Exception_Id;
      Members : Exception_Members'Class;
      Message : String := "")
   is
      New_Node : Exc_Occ_Node;

   begin
      Enter (Exc_Occ_Lock);

      --  Keep the list size to a max size. Otherwise, remove the oldest member
      --  (first in the list).

      if Length (Exc_Occ_List) = Max_Exc_Occ_List_Size then
         Extract_First (Exc_Occ_List, New_Node);
         Free (New_Node.Mbr);
      end if;

      pragma Debug (O ("Assigning ID: " & Image (Seed_Id)));
      pragma Debug (Dump_All_Occurrences);

      --  Generate a fresh exception occurrence id

      New_Node.Id := Seed_Id;
      New_Node.Mbr := new Exception_Members'Class'(Members);
      New_Node.Msg := To_PolyORB_String (Message);

      if Seed_Id = Exc_Occ_Id_Type'Last then
         Seed_Id := Null_Id;
      end if;
      Seed_Id := Seed_Id + 1;

      --  Append to the list

      Append (Exc_Occ_List, New_Node);

      pragma Debug (O ("Raise ("
                       & Ada.Exceptions.Exception_Name (Id)
                       & ", " & Image (New_Node.Id) & ")."));
      pragma Debug (Dump_All_Occurrences);
      Leave (Exc_Occ_Lock);

      if Message = "" then
         Ada.Exceptions.Raise_Exception (Id, Image (New_Node.Id));

      else
         Ada.Exceptions.Raise_Exception
           (Id, Image (New_Node.Id) & Ada.Characters.Latin_1.LF & Message);
      end if;

      raise Program_Error;
   end User_Raise_Exception;

   -----------------------------------
   -- Raise_User_Exception_From_Any --
   -----------------------------------

   procedure Raise_User_Exception_From_Any
     (Repository_Id : PolyORB.Types.RepositoryId;
      Occurence     : PolyORB.Any.Any;
      Message       : Standard.String := "")
   is
   begin
      Find_Exception_Info (Repository_Id).Raiser.all (Occurence, Message);
   end Raise_User_Exception_From_Any;

   ----------------------------
   -- Default_Raise_From_Any --
   ----------------------------

   procedure Default_Raise_From_Any (Occurrence : Any.Any) is
   begin
      if not Is_Empty (Occurrence) then
         Ada.Exceptions.Raise_Exception
           (Get_ExcepId_By_RepositoryId
              (To_Standard_String
                 (TypeCode.Id (Get_Type_Obj (Occurrence)))));
      end if;
   end Default_Raise_From_Any;

   ------------------------
   -- Register_Exception --
   ------------------------

   procedure Register_Exception
     (TC     : PolyORB.Any.TypeCode.Local_Ref;
      Raiser : Raise_From_Any_Procedure) is
   begin
      pragma Debug
        (O ("Registering exception: "
            & Types.To_Standard_String (TypeCode.Id (TC))));

      Enter (All_Exceptions_Lock);
      Exception_Lists.Append (All_Exceptions, (TC => TC, Raiser => Raiser));
      Leave (All_Exceptions_Lock);
   end Register_Exception;

   -------------------------
   -- Find_Exception_Info --
   -------------------------

   function Find_Exception_Info
     (For_Exception : PolyORB.Types.RepositoryId)
     return Exception_Info
   is
      use Exception_Lists;

      Id : constant Types.RepositoryId := For_Exception;
      It : Exception_Lists.Iterator;
      Info : Exception_Info;

   begin
      pragma Debug
        (O ("Looking up einfo for " & To_Standard_String (For_Exception)));

      Enter (All_Exceptions_Lock);
      It := First (All_Exceptions);

      while not Last (It) loop
         exit when PolyORB.Any.TypeCode.Id (Value (It).TC) = Id;
         Next (It);
      end loop;

      if Last (It) then
         Leave (All_Exceptions_Lock);

         pragma Debug (O ("no einfo found, returning 'Unknown' exception"));
         --         Raise_Unknown;
      end if;

      Info := Value (It).all;
      Leave (All_Exceptions_Lock);

      return Info;
   end Find_Exception_Info;

   ---------------------------------
   -- Exception utility functions --
   ---------------------------------

   --------------------
   -- Exception_Name --
   --------------------

   function Exception_Name
     (Repository_Id : Standard.String)
      return Standard.String
   is
      Colon1 : constant Integer
        := Find (Repository_Id, Repository_Id'First, ':');
      Colon2 : constant Integer
        := Find (Repository_Id, Colon1 + 1, ':');

   begin
      pragma Debug (O ("Exception_Name " & Repository_Id));

      if Repository_Id'First <= Colon1
        and then Colon1 <= Colon2
        and then Colon2 <= Repository_Id'Last
      then
         return Repository_Id (Colon1 + 1 .. Colon2 - 1);
      else
         return Repository_Id;
      end if;
   end Exception_Name;

   --------------------------------
   -- Exception_Name_To_Error_Id --
   --------------------------------

   procedure Exception_Name_To_Error_Id
     (Name     :     String;
      Is_Error : out Boolean;
      Id       : out Error_Id)
   is
      Prefix_Length  : constant Natural := PolyORB_Exc_Prefix'Length;
      Version_Length : constant Natural := PolyORB_Exc_Version'Length;

   begin
      if Name'Length > Prefix_Length + Version_Length
        and then Name (Name'First .. Name'First + Prefix_Length - 1)
        = PolyORB_Exc_Prefix
      then
         declare
            Error_Id_Name : constant String
              := Name (Name'First + Prefix_Length ..
                       Name'Last - Version_Length) & "_E";

         begin
            pragma Debug (O ("Error_Id_Name : " & Error_Id_Name));

            Is_Error := True;
            Id := Error_Id'Value (Error_Id_Name);
         end;
      else
         Is_Error := False;
         Id := No_Error;
      end if;

      pragma Debug (O (Name & " is a PolyORB error ? "
                       & Boolean'Image (Is_Error)));
   end Exception_Name_To_Error_Id;

   -------------------------
   -- Get_ExcepId_By_Name --
   -------------------------

   function Get_ExcepId_By_Name
     (Name : Standard.String)
      return Ada.Exceptions.Exception_Id
   is
      function To_Exception_Id is new Ada.Unchecked_Conversion
        (System.Standard_Library.Exception_Data_Ptr,
         Ada.Exceptions.Exception_Id);

      Internal_Name : String := Name;
   begin
      if Internal_Name = "" then
         return Ada.Exceptions.Null_Id;
      end if;

      for J in Internal_Name'Range loop
         if Internal_Name (J) = '/' then
            Internal_Name (J) := '.';
         end if;
      end loop;

      pragma Debug (O ("Exception Id : " & Internal_Name));

      return To_Exception_Id
        (System.Exception_Table.Internal_Exception (Internal_Name));
   end Get_ExcepId_By_Name;

   ---------------------------------
   -- Get_ExcepId_By_RepositoryId --
   ---------------------------------

   function Get_ExcepId_By_RepositoryId
     (RepoId : Standard.String)
      return Ada.Exceptions.Exception_Id
   is
   begin
      return Get_ExcepId_By_Name (Exception_Name (RepoId));
   end Get_ExcepId_By_RepositoryId;

   ------------------------
   -- Occurrence_To_Name --
   ------------------------

   function Occurrence_To_Name
     (Occurrence : Ada.Exceptions.Exception_Occurrence) return String
   is
      Name : String := Ada.Exceptions.Exception_Name (Occurrence);
   begin
      for J in Name'Range loop
         if Name (J) = '.' then
            Name (J) := '/';
         end if;
      end loop;

      return Name;
   end Occurrence_To_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Create (All_Exceptions_Lock);
      Create (Exc_Occ_Lock);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"exceptions",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => +"tasking.mutexes",
       Provides  => PolyORB.Initialization.String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Exceptions;
