------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . C O R B A _ P . E X C E P T I O N S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

--  $Id: //droopi/main/src/corba/polyorb-corba_p-exceptions.adb#12 $

with Ada.Exceptions;
with Ada.Unchecked_Conversion;

pragma Warnings (Off);
with System.Exception_Table;
with System.Standard_Library;
pragma Warnings (On);
--  Mapping between exception names and exception ids.
--  GNAT internal exception table is used to maintain a list of
--  all exceptions.

with CORBA;

with PolyORB.Any;
with PolyORB.Exceptions;
with PolyORB.Log;
with PolyORB.Utils;
with PolyORB.Types;

package body PolyORB.CORBA_P.Exceptions is

   use Ada.Exceptions;

   use PolyORB.Any;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.corba_p.exceptions");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ---------------------------------
   -- Get_ExcepId_By_RepositoryId --
   ---------------------------------

   procedure Get_ExcepId_By_RepositoryId
     (RepoId              : in     Standard.String;
      ExcpId              :    out Ada.Exceptions.Exception_Id;
      Is_CORBA_System_Exc :    out Boolean);
   --  Return the corresponding Ada Exception_Id for
   --  a repository id.

   procedure Get_ExcepId_By_RepositoryId
     (RepoId              : in     Standard.String;
      ExcpId              :    out Ada.Exceptions.Exception_Id;
      Is_CORBA_System_Exc :    out Boolean)
   is

      use PolyORB.Utils;

      Colon1 : constant Integer := Find (RepoId, RepoId'First, ':');
      Colon2 : constant Integer := Find (RepoId, Colon1 + 1, ':');

      function To_Exception_Id is new Ada.Unchecked_Conversion
        (System.Standard_Library.Exception_Data_Ptr,
         Ada.Exceptions.Exception_Id);

      --  A repository ID is of the form 'MODEL:X/Y/Z:VERSION'

      Model : constant String   := RepoId (RepoId'First .. Colon1 - 1);
      Internal_Name : String    := RepoId (Colon1 + 1 .. Colon2 - 1);

      Result : Ada.Exceptions.Exception_Id;
   begin
      pragma Debug (O ("RepoId : " & RepoId));
      Is_CORBA_System_Exc := False;

      if RepoId = "" then
         ExcpId := Ada.Exceptions.Null_Id;
         return;
      end if;

      for J in Internal_Name'Range loop
         if Internal_Name (J) = '/' then
            Internal_Name (J) := '.';
         end if;
      end loop;

      pragma Debug (O ("Internal_Name : " & Internal_Name));
      Result := To_Exception_Id
        (System.Exception_Table.Internal_Exception
           (Internal_Name));

      if Result = Ada.Exceptions.Null_Id then
         ExcpId := CORBA.Unknown'Identity;
         Is_CORBA_System_Exc := True;
      else
         ExcpId := Result;
         Is_CORBA_System_Exc
           := (Model = "IDL"
           and then Internal_Name'Length > 5
           and then Internal_Name
                 (Internal_Name'First .. Internal_Name'First + 5)
                 = "CORBA.");
      end if;
   end Get_ExcepId_By_RepositoryId;

   --------------------
   -- Raise_From_Any --
   --------------------

   procedure Raise_From_Any (Occurrence : Any.Any) is
      Repository_Id : constant PolyORB.Types.RepositoryId
        := Any.TypeCode.Id (PolyORB.Any.Get_Type (Occurrence));

      System_Id : Ada.Exceptions.Exception_Id;
      Is_CORBA_System_Exc : Boolean;
   begin
      Get_ExcepId_By_RepositoryId
        (PolyORB.Types.To_Standard_String (Repository_Id),
         System_Id,
         Is_CORBA_System_Exc);

      if Is_CORBA_System_Exc then
         --  This is a system exception.

         declare
            Minor : constant PolyORB.Types.Unsigned_Long
              := From_Any
              (Get_Aggregate_Element
               (Occurrence, TC_Unsigned_Long,
                PolyORB.Types.Unsigned_Long (0)));

            Completed : constant CORBA.Completion_Status
              := PolyORB.Exceptions.From_Any
              (Get_Aggregate_Element
               (Occurrence, PolyORB.Exceptions.TC_Completion_Status,
                PolyORB.Types.Unsigned_Long (1)));
         begin
            PolyORB.Exceptions.Raise_System_Exception
              (System_Id,
               PolyORB.Exceptions.System_Exception_Members'
                 (Minor => Minor, Completed => Completed));

            raise Program_Error;
            --  Not reached.

         end;
      end if;

      declare
         EInfo : constant PolyORB.Exceptions.Exception_Info :=
           PolyORB.Exceptions.Find_Exception_Info (Repository_Id);
      begin
         EInfo.Raiser.all (Occurrence);
      end;

      raise Program_Error;
      --  Never reached (Raiser raises an exception.)
   end Raise_From_Any;

end PolyORB.CORBA_P.Exceptions;
