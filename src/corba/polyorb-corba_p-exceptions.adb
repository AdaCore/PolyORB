------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . C O R B A _ P . E X C E P T I O N S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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

--  $Id: //droopi/main/src/corba/polyorb-corba_p-exceptions.adb#21 $

with CORBA;

with PolyORB.Any;
with PolyORB.Exceptions;
with PolyORB.Log;
with PolyORB.Types;

package body PolyORB.CORBA_P.Exceptions is

   use Ada.Exceptions;

   use PolyORB.Any;
   use PolyORB.Exceptions;
   use PolyORB.Log;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.corba_p.exceptions");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Exception_Name_To_Error_Id
     (Name     :     String;
      Is_Error : out Boolean;
      Id       : out Error_Id);
   --  Convert an exception name into a PolyORB's Error Id.

   --------------------------------
   -- Exception_Name_To_Error_Id --
   --------------------------------

   procedure Exception_Name_To_Error_Id
     (Name     :     String;
      Is_Error : out Boolean;
      Id       : out Error_Id)
   is
      Prefix_Length : constant Natural := PolyORB_Exc_Prefix'Length;
      Version_Length : constant Natural
        := To_Standard_String (PolyORB_Exc_Version)'Length;

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

   --------------------
   -- Raise_From_Any --
   --------------------

   procedure Raise_From_Any
     (Occurrence : Any.Any)
   is
      Repository_Id : constant PolyORB.Types.RepositoryId
        := Any.TypeCode.Id (PolyORB.Any.Get_Type (Occurrence));

      EId : constant String := To_Standard_String (Repository_Id);

      Is_Error : Boolean;
      Id       : Error_Id;
      Error    : Error_Container;

   begin
      pragma Debug (O ("Raise_From_Any: enter"));

      Exception_Name_To_Error_Id (EId, Is_Error, Id);

      if Is_Error then

         Error.Kind := Id;

         case Error.Kind is
            when ORB_System_Error =>
               Error.Member := new System_Exception_Members'
                 (Minor =>
                    From_Any
                  (Get_Aggregate_Element
                   (Occurrence, TC_Unsigned_Long,
                    PolyORB.Types.Unsigned_Long (0))),
                  Completed =>
                    From_Any
                  (Get_Aggregate_Element
                   (Occurrence, PolyORB.Exceptions.TC_Completion_Status,
                    PolyORB.Types.Unsigned_Long (1))));

            when others =>
               Error.Member := new Null_Members'(Null_Member);
         end case;

         pragma Debug (O ("Raising " & Error_Id'Image (Error.Kind)));
         Raise_From_Error (Error);

      else
         pragma Debug (O ("Raising " & EId));
         Raise_User_Exception_From_Any (Repository_Id, Occurrence);

      end if;

      raise Program_Error;
      --  Never reached (Raiser raises an exception.)
   end Raise_From_Any;

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
            pragma Debug (O ("No matching system exception found, "
                             & "will use CORBA.UNKNOWN"));
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
