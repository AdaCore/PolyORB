------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . C O R B A _ P . E X C E P T I O N S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
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

--  $Id: //droopi/main/src/corba/polyorb-corba_p-exceptions.adb#17 $

with CORBA;

with PolyORB.Any;
with PolyORB.Exceptions;
with PolyORB.Log;
with PolyORB.Types;
with PolyORB.Utils;

package body PolyORB.CORBA_P.Exceptions is

   use Ada.Exceptions;

   use PolyORB.Any;
   use PolyORB.Exceptions;
   use PolyORB.Log;
   use PolyORB.Types;
   use PolyORB.Utils;

   package L is new PolyORB.Log.Facility_Log ("polyorb.corba_p.exceptions");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   function Is_System_Exception (Name : String) return Boolean;

   function To_CORBA_Exception (Name : String) return String;
   --  Map PolyORB system exception Repository Ids into CORBA
   --  ones; leave other repository IDs unchanged.

   procedure Raise_System_Exception_From_Any
     (System_Id  : Ada.Exceptions.Exception_Id;
      Occurrence : PolyORB.Any.Any);

   -------------------------
   -- Is_System_Exception --
   -------------------------

   --  IMPLEMENTATION NOTE : Every change to this function should be
   --  duplicated to PolyORB.GIOP_P.Exceptions.Is_System_Exception.

   function Is_System_Exception
     (Name : String)
     return Boolean
   is
      Prefix_Length : constant Natural := PolyORB_Exc_Prefix'Length;
      Version_Length : constant Natural
        := To_Standard_String (PolyORB_Exc_Version)'Length;

      Result : Boolean := False;
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

            Result := Error_Id'Value (Error_Id_Name) in ORB_System_Error;
         end;
      end if;

      pragma Debug (O (Name & " is a system exception ? "
                       & Boolean'Image (Result)));
      return Result;
   end Is_System_Exception;

   ------------------------
   -- To_CORBA_Exception --
   ------------------------

   function To_CORBA_Exception
     (Name : String)
     return String
   is
      use PolyORB.Utils;

      Colon1 : constant Integer := Find (Name, Name'First, ':');
      Slash  : constant Integer := Find (Name, Colon1 + 1, '/');

      Root      : String renames Name (Name'First .. Slash);
      Base_Name : String renames Name (Slash + 1 .. Name'Last);
   begin
      pragma Debug (O ("To_CORBA_Exception: name was " & Name));

      if Root = PolyORB_Exc_Prefix then
         pragma Debug (O ("To_CORBA_Exception: new name is "
                          & "IDL:CORBA/" & Base_Name));

         return "IDL:CORBA/" & Base_Name;

      else
         pragma Debug (O (" not changed: " & Name));

         return Name;
      end if;
   end To_CORBA_Exception;

   --------------------
   -- Raise_From_Any --
   --------------------

   procedure Raise_From_Any
     (Occurrence : Any.Any)
   is
      Repository_Id : constant PolyORB.Types.RepositoryId
        := Any.TypeCode.Id (PolyORB.Any.Get_Type (Occurrence));

      EId : constant String := To_Standard_String (Repository_Id);

   begin
      pragma Debug (O ("Raise_From_Any: enter"));

      if Is_System_Exception (EId) then
         declare
            CORBA_Repository_Id : constant String
              := To_CORBA_Exception (EId);

            System_Id : constant Ada.Exceptions.Exception_Id
              := PolyORB.Exceptions.Get_ExcepId_By_RepositoryId
              (CORBA_Repository_Id);

         begin
            pragma Debug (O ("Raising " & Exception_Name (System_Id)));

            Raise_System_Exception_From_Any (System_Id, Occurrence);
         end;

      else
         pragma Debug (O ("Raising " & EId));

         Raise_User_Exception_From_Any (Repository_Id, Occurrence);

      end if;

      raise Program_Error;
      --  Never reached (Raiser raises an exception.)
   end Raise_From_Any;

   -------------------------------------
   -- Raise_System_Exception_From_Any --
   -------------------------------------

   procedure Raise_System_Exception_From_Any
     (System_Id  : Ada.Exceptions.Exception_Id;
      Occurrence : PolyORB.Any.Any)
   is
      Minor : constant PolyORB.Types.Unsigned_Long
        := From_Any
        (Get_Aggregate_Element
         (Occurrence, TC_Unsigned_Long,
          PolyORB.Types.Unsigned_Long (0)));

      Completed : constant Completion_Status
        := PolyORB.Exceptions.From_Any
        (Get_Aggregate_Element
         (Occurrence, PolyORB.Exceptions.TC_Completion_Status,
          PolyORB.Types.Unsigned_Long (1)));

      Str : Standard.String (1 .. 5);
      Val : PolyORB.Types.Unsigned_Long;

   begin
      --  Marshall Minor and Completed fields of EXCP_MEMB into a string.
      --  A trivial marshalling is used:
      --  Str (1 .. 4)   Minor (MSB first)
      --  Str (5)        Completed

      Str (5) := Character'Val (Completion_Status'Pos (Completed));
      Val := Minor;

      for J in 1 .. 4 loop
         Str (J) := Character'Val (Val / 2 ** 24);
         Val := (Val mod 2 ** 24) * 256;
      end loop;

      --  Raise the exception.
      Ada.Exceptions.Raise_Exception (System_Id, Str);


      raise Program_Error;
   end Raise_System_Exception_From_Any;

   -----------------------------
   -- System_Exception_To_Any --
   -----------------------------

   function System_Exception_To_Any
     (E : Ada.Exceptions.Exception_Occurrence)
      return PolyORB.Any.Any
   is
      Name    : RepositoryId;
      Members : CORBA.System_Exception_Members;
      TC      : TypeCode.Object;
      Result  : Any.Any;

   begin
      pragma Debug (O ("System_Exception_To_Any: enter."));
      pragma Debug (O ("Exception_Name: " & Exception_Name (E)));
      pragma Debug (O ("Exception_Message: " & Exception_Message (E)));
      pragma Debug (O ("Exception_Information: " & Exception_Information (E)));

      begin
         Name := Occurrence_To_Name (E);
         CORBA.Get_Members (E, Members);
      exception
         when others =>
            Name := To_PolyORB_String ("UNKNOWN");
            Members := (1, CORBA.Completed_Maybe);
      end;

      --  Construct exception typecode
      TC := System_Exception_TypeCode (To_Standard_String (Name));

      Result := Get_Empty_Any_Aggregate (TC);
      Add_Aggregate_Element (Result, CORBA.To_Any (Members.Minor));
      Add_Aggregate_Element (Result, CORBA.To_Any (Members.Completed));

      pragma Debug (O ("System_Exception_To_Any: leave"));
      return Result;
   end System_Exception_To_Any;

   ----------------------
   -- Raise_From_Error --
   ----------------------

   procedure Raise_From_Error
     (Error : in out PolyORB.Exceptions.Error_Container) is
   begin
      pragma Debug (O ("About to raise exception: "
                       & Error_Id'Image (Error.Kind)));

      pragma Assert (Is_Error (Error));

      if Error.Kind in ORB_System_Error then
         pragma Debug (O ("Raising CORBA Exception"));
         CORBA_Raise_From_Error (Error);

      elsif Error.Kind in POA_Error then
         pragma Debug (O ("Raising PORTABLESERVER.POA Exception"));
         POA_Raise_From_Error (Error);

      elsif Error.Kind in POAManager_Error then
         pragma Debug (O ("Raising PORTABLESERVER.POAManager Exception"));
         POAManager_Raise_From_Error (Error);

      elsif Error.Kind in PolyORB_Internal_Error then
         --  PolyORB internal errors are mapped to CORBA.Unknown

         pragma Debug (O ("Raising CORBA.UNKNOWN"));
         CORBA.Raise_Unknown (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      --  Never reached (Raiser raises an exception.)
   end Raise_From_Error;

end PolyORB.CORBA_P.Exceptions;
