------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . E X C E P T I O N S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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

--  $Id$

with Ada.Unchecked_Conversion;

pragma Warnings (Off);
with System.Exception_Table;
with System.Standard_Library;
pragma Warnings (On);
--  Mapping between exception names and exception ids.
--  GNAT internal exception table is used to maintain a list of
--  all exceptions.

with PolyORB.Any;
with PolyORB.Exceptions.Stack;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Log;
with PolyORB.Tasking.Mutexes;
with PolyORB.Types;
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
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : PolyORB.Any.Any)
     return Completion_Status is
   begin
      return Completion_Status'Val
        (Unsigned_Long'
         (From_Any (PolyORB.Any.Get_Aggregate_Element
                    (Item, TC_Unsigned_Long, 0))));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Completion_Status)
     return Any.Any
   is
      Result : Any.Any := Get_Empty_Any_Aggregate (TC_Completion_Status);
   begin
      Add_Aggregate_Element
        (Result, To_Any (Unsigned_Long (Completion_Status'Pos (Item))));
      return Result;
   end To_Any;

   --------------------------
   -- TC_Completion_Status --
   --------------------------

   TC_Completion_Status_Cache : TypeCode.Object;

   function TC_Completion_Status
     return PolyORB.Any.TypeCode.Object
   is
      use type PolyORB.Types.Unsigned_Long;

      TC : TypeCode.Object renames TC_Completion_Status_Cache;

   begin
      if TypeCode.Parameter_Count (TC) /= 0 then
         return TC_Completion_Status_Cache;
      end if;

      TC := TypeCode.TC_Enum;
      TypeCode.Add_Parameter
        (TC, To_Any (To_PolyORB_String ("completion_status")));
      TypeCode.Add_Parameter
        (TC, To_Any (To_PolyORB_String
                     ("IDL:omg.org/CORBA/completion_status:1.0")));

      for C in Completion_Status'Range loop
         TypeCode.Add_Parameter
           (TC, To_Any (To_PolyORB_String (Completion_Status'Image (C))));
      end loop;

      return TC;
   end TC_Completion_Status;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Name   : Standard.String;
      Member : Null_Members)
     return PolyORB.Any.Any
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Member);
      pragma Warnings (On); --  WAG:3.15

      TC : TypeCode.Object := TypeCode.TC_Except;
      Shift : Natural := 0;

      Repository_Id : PolyORB.Types.String;
   begin
      --  Name
      TypeCode.Add_Parameter (TC, To_Any (To_PolyORB_String (Name)));

      if Name (Name'First .. Name'First + PolyORB_Exc_Root'Length - 1)
        = PolyORB_Exc_Root then
         Shift := PolyORB_Exc_Root'Length + 1;
      end if;

      --  RepositoryId : 'INTERNAL:<Name>:1.0'
      Repository_Id := To_PolyORB_String (PolyORB_Exc_Prefix)
        & To_PolyORB_String (Name (Name'First + Shift .. Name'Last))
        & PolyORB_Exc_Version;

      TypeCode.Add_Parameter (TC, To_Any (Repository_Id));

      return Get_Empty_Any_Aggregate (TC);
   end To_Any;

   -------------------------------
   -- System_Exception_TypeCode --
   -------------------------------

   function System_Exception_TypeCode
     (Name : Standard.String)
     return Any.TypeCode.Object
   is
      TC    : TypeCode.Object := TypeCode.TC_Except;
      Shift : Natural := 0;

      Repository_Id : PolyORB.Types.String;
   begin
      --  Name
      TypeCode.Add_Parameter (TC, To_Any (To_PolyORB_String (Name)));

      if Name (Name'First .. Name'First + PolyORB_Exc_Root'Length - 1)
        = PolyORB_Exc_Root then
         Shift := PolyORB_Exc_Root'Length + 1;
      end if;

      --  RepositoryId : 'INTERNAL:<Name>:1.0'
      Repository_Id := To_PolyORB_String (PolyORB_Exc_Prefix)
        & To_PolyORB_String (Name (Name'First + Shift .. Name'Last))
        & PolyORB_Exc_Version;

      TypeCode.Add_Parameter (TC, To_Any (Repository_Id));

      --  Component 'minor'
      TypeCode.Add_Parameter
        (TC, To_Any (TC_Unsigned_Long));
      TypeCode.Add_Parameter
        (TC, To_Any (To_PolyORB_String ("minor")));

      --  Component 'completed'
      TypeCode.Add_Parameter
        (TC, To_Any (TC_Completion_Status));
      TypeCode.Add_Parameter
        (TC, To_Any (To_PolyORB_String ("completed")));

      pragma Debug (O ("Built Exception TypeCode for: "
                       & To_Standard_String (Repository_Id)));
      pragma Debug (O (" " & PolyORB.Any.Image (TC)));

      return TC;
   end System_Exception_TypeCode;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Name   : Standard.String;
      Member : System_Exception_Members)
     return PolyORB.Any.Any
   is
      TC : PolyORB.Any.TypeCode.Object;
      Result : PolyORB.Any.Any;
   begin
      --  Construct exception typecode
      TC := System_Exception_TypeCode (Name);

      Result := Get_Empty_Any_Aggregate (TC);
      Add_Aggregate_Element (Result, To_Any (Member.Minor));
      Add_Aggregate_Element (Result, To_Any (Member.Completed));

      return Result;
   end To_Any;

   -----------------------------
   -- User exception handling --
   -----------------------------

   type Exception_Info is record
      TC     : PolyORB.Any.TypeCode.Object;
      Raiser : Raise_From_Any_Procedure;
   end record;

   package Exception_Lists is new
     PolyORB.Utils.Chained_Lists (Exception_Info);

   function Find_Exception_Info
     (For_Exception : PolyORB.Types.RepositoryId)
     return Exception_Info;
   --  Return Exception_Info associated to 'For_Exception'.

   All_Exceptions : Exception_Lists.List;
   --  Exception list, use to associate an exception typecode
   --  with a raiser function that retrieves member data from
   --  an Any and raises the exception with the appropriate
   --  information in the occurrence.

   All_Exceptions_Lock : Mutex_Access;
   --  Mutex used to safely access All_Exceptions list.

   ----------------------
   -- User_Get_Members --
   ----------------------

   procedure User_Get_Members
     (Occurrence :     Ada.Exceptions.Exception_Occurrence;
      Members    : out Exception_Members'Class)
     renames PolyORB.Exceptions.Stack.Get_Members;
   --  Extract members from a user exception occurence.

   ------------------------
   -- User_Purge_Members --
   ------------------------

   procedure User_Purge_Members
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
     renames PolyORB.Exceptions.Stack.Purge_Members;

   --------------------------
   -- User_Raise_Exception --
   --------------------------

   procedure User_Raise_Exception
     (Id      : Ada.Exceptions.Exception_Id;
      Members : Exception_Members'Class)
     renames PolyORB.Exceptions.Stack.Raise_Exception;

   -----------------------------------
   -- Raise_User_Exception_From_Any --
   -----------------------------------

   procedure Raise_User_Exception_From_Any
     (Repository_Id : PolyORB.Types.RepositoryId;
      Occurence     : PolyORB.Any.Any) is
   begin
      Find_Exception_Info (Repository_Id).Raiser.all (Occurence);
   end Raise_User_Exception_From_Any;

   ----------------------------
   -- Default_Raise_From_Any --
   ----------------------------

   procedure Default_Raise_From_Any
     (Occurrence : Any.Any)
   is
      use PolyORB.Any;
   begin
      if not Is_Empty (Occurrence) then
         Ada.Exceptions.Raise_Exception
           (Get_ExcepId_By_RepositoryId
              (To_Standard_String
                 (TypeCode.Id (Get_Type (Occurrence)))));
      end if;
   end Default_Raise_From_Any;

   ------------------------
   -- Register_Exception --
   ------------------------

   procedure Register_Exception
     (TC     : in PolyORB.Any.TypeCode.Object;
      Raiser : in Raise_From_Any_Procedure) is
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
      use PolyORB.Types;
      use Exception_Lists;

      Id : constant Types.RepositoryId := For_Exception;
      It : Iterator;
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

   ---------------------------------
   -- Get_ExcepId_By_RepositoryId --
   ---------------------------------

   function Get_ExcepId_By_RepositoryId
     (RepoId : Standard.String)
      return Ada.Exceptions.Exception_Id
   is
      function To_Exception_Id is new Ada.Unchecked_Conversion
        (System.Standard_Library.Exception_Data_Ptr,
         Ada.Exceptions.Exception_Id);

      Internal_Name : Standard.String  := Exception_Name (RepoId);
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

   end Get_ExcepId_By_RepositoryId;

   ------------------------
   -- Occurrence_To_Name --
   ------------------------

   function Occurrence_To_Name
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
     return PolyORB.Types.RepositoryId
   is
      Name : String := Ada.Exceptions.Exception_Name (Occurrence);
   begin
      for J in Name'Range loop
         if Name (J) = '.' then
            Name (J) := '/';
         end if;
      end loop;

      return PolyORB.Types.To_PolyORB_String (Name);
   end Occurrence_To_Name;

   -----------------------------------------------
   -- PolyORB Internal Error handling functions --
   -----------------------------------------------

   -----------
   -- Found --
   -----------

   function Found
     (Error : in Error_Container)
     return Boolean is
   begin
      return Error.Kind /= No_Error;
   end Found;

   -----------
   -- Throw --
   -----------

   procedure Throw
     (Error  : in out Error_Container;
      Kind   : in     Error_Id;
      Member : in     Exception_Members'Class;
      Where  : in     String := GNAT.Source_Info.Source_Location) is
   begin
      if Error.Kind /= No_Error then
         pragma Debug (O ("*** Abort *** "
                          & Error_Id'Image (Error.Kind)));

         Free (Error.Member);
      end if;

      if False
        or else
        (Kind in ORB_System_Error
         and then Member not in System_Exception_Members'Class)
        or else
        (Kind in POA_Error
         and then Kind /= InvalidPolicy_E
         and then Member not in Null_Members'Class)
        or else
        (Kind = InvalidPolicy_E
         and then Member not in InvalidPolicy_Members'Class)
        or else
        (Kind in POAManager_Error
         and then Member not in Null_Members'Class)
      then
         pragma Debug (O ("Wrong Error_Id/Exception_Member for : "
                          & Error_Id'Image (Kind)));
         null;
      end if;

      Error.Kind := Kind;
      Error.Member := new Exception_Members'Class'(Member);

      pragma Debug (O ("*** Throw *** "
                       & Error_Id'Image (Error.Kind)
                       & " at "
                       & Where));
   end Throw;

   -----------
   -- Catch --
   -----------

   procedure Catch
     (Error : in out Error_Container) is
   begin
      Error.Kind := No_Error;
      Free (Error.Member);
   end Catch;

   --------------
   -- Is_Error --
   --------------

   function Is_Error
     (Error : in Error_Container)
     return Boolean is
   begin
      return Error.Kind /= No_Error;
   end Is_Error;

   ------------------
   -- Error_To_Any --
   ------------------

   function Error_To_Any
     (Error : in Error_Container)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any;
      Error_Name : constant String :=  Error_Id'Image (Error.Kind);
      Exception_Name : constant String
        := Error_Name (Error_Name'First .. Error_Name'Last - 2);

   begin
      pragma Debug (O ("Error_To_Any: enter."));
      pragma Debug (O ("Error is: " & Error_Name));
      pragma Debug (O ("Exception name is: " & Exception_Name));

      if Error.Kind in ORB_System_Error then
         Result := To_Any (Exception_Name,
                           System_Exception_Members (Error.Member.all));
      elsif Error.Kind in POA_Error then
         if Error.Member.all in Null_Members'Class then
            Result := To_Any (Exception_Name,
                              Null_Members (Error.Member.all));
         else
            raise Program_Error;
         end if;
      else
         raise Program_Error;
      end if;

      pragma Debug (O ("Error_To_Any: leave."));
      return Result;
   end Error_To_Any;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Create (All_Exceptions_Lock);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"exceptions",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access));
end PolyORB.Exceptions;
