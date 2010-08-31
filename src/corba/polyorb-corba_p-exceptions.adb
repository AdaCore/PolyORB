------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . C O R B A _ P . E X C E P T I O N S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2010, Free Software Foundation, Inc.          --
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

with PolyORB.Errors.Helper;
with PolyORB.Exceptions;
with PolyORB.Log;
with PolyORB.QoS.Exception_Informations;
with PolyORB.Types;

package body PolyORB.CORBA_P.Exceptions is

   use Ada.Exceptions;

   use PolyORB.Any;
   use PolyORB.Errors;
   use PolyORB.Errors.Helper;
   use PolyORB.Exceptions;
   use PolyORB.Log;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.corba_p.exceptions");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   ------------------------
   -- Is_Forward_Request --
   ------------------------

   function Is_Forward_Request
     (Occurrence : PolyORB.Any.Any) return Boolean
   is
      use type PolyORB.Any.TypeCode.Local_Ref;
   begin
      return not Is_Empty (Occurrence)
        and then Get_Type (Occurrence) = TC_ForwardRequest;
   end Is_Forward_Request;

   ------------------------------
   -- Is_Needs_Addressing_Mode --
   ------------------------------

   function Is_Needs_Addressing_Mode
     (Occurrence : PolyORB.Any.Any)
     return Boolean
   is
      use type PolyORB.Any.TypeCode.Local_Ref;

   begin
      return not Is_Empty (Occurrence)
        and then Get_Type (Occurrence) = TC_NeedsAddressingMode;
   end Is_Needs_Addressing_Mode;

   -------------------------
   -- Is_System_Exception --
   -------------------------

   function Is_System_Exception
     (Occurrence : PolyORB.Any.Any)
      return Boolean
   is
      Repository_Id : constant PolyORB.Types.RepositoryId
        := Any.TypeCode.Id (PolyORB.Any.Get_Type (Occurrence));
      EId           : constant String := To_Standard_String (Repository_Id);

      Is_Error : Boolean;
      Id       : Error_Id;

   begin
      Exception_Name_To_Error_Id (EId, Is_Error, Id);

      return
        Is_Error
          and then Id in ORB_System_Error;
   end Is_System_Exception;

   --------------------
   -- Raise_From_Any --
   --------------------

   procedure Raise_From_Any
     (Occurrence : Any.Any;
      Message    : String := "")
   is
      Repository_Id : constant PolyORB.Types.RepositoryId :=
                        Any.TypeCode.Id (PolyORB.Any.Get_Type (Occurrence));

      EId : constant String := To_Standard_String (Repository_Id);

      Is_Error : Boolean;
      Id       : Error_Id;
      Error    : Error_Container;

   begin
      pragma Debug (C, O ("Raise_From_Any: enter"));

      Exception_Name_To_Error_Id (EId, Is_Error, Id);

      if Is_Error then

         Error.Kind := Id;

         case Error.Kind is
            when ORB_System_Error =>
               Error.Member :=
                 new System_Exception_Members'(From_Any (Occurrence));

            when others =>
               Error.Member := new Null_Members'(Null_Member);
         end case;

         pragma Debug (C, O ("Raising " & Error_Id'Image (Error.Kind)));
         Raise_From_Error (Error, Message);

      else
         pragma Debug (C, O ("Raising " & EId));
         Raise_User_Exception_From_Any (Repository_Id, Occurrence, Message);
      end if;

      raise Program_Error;
      --  Never reached (Raiser raises an exception.)
   end Raise_From_Any;

   -----------------------------
   -- System_Exception_To_Any --
   -----------------------------

   function System_Exception_To_Any
     (E : Ada.Exceptions.Exception_Occurrence) return CORBA.Any is
   begin
      return CORBA.Any (PolyORB.Any.Any'(System_Exception_To_Any (E)));
   end System_Exception_To_Any;

   function System_Exception_To_Any
     (E : Ada.Exceptions.Exception_Occurrence)
      return PolyORB.Any.Any
   is
      Repository_Id : RepositoryId;
      Members       : CORBA.System_Exception_Members;
      TC            : PolyORB.Any.TypeCode.Local_Ref;
      Result        : PolyORB.Any.Any;

   begin
      pragma Debug (C, O ("System_Exception_To_Any: enter."));
      pragma Debug (C, O ("Exception_Name: " & Exception_Name (E)));
      pragma Debug (C, O ("Exception_Message: " & Exception_Message (E)));

      begin
         Repository_Id := To_PolyORB_String (Occurrence_To_Name (E));
         CORBA.Get_Members (E, Members);
      exception
         when others =>
            pragma Debug (C, O ("No matching system exception found, "
                             & "will use CORBA/UNKNOWN"));
            Repository_Id := To_PolyORB_String ("CORBA/UNKNOWN");
            Members := (1, CORBA.Completed_Maybe);
      end;

      declare
         CORBA_Exception_Namespace : constant String := "CORBA/";
         --  All CORBA System exceptions are prefixed by this string

         Name : constant String := To_Standard_String (Repository_Id);

      begin
         --  Construct exception typecode

         TC := System_Exception_TypeCode
           (Name (Name'First + CORBA_Exception_Namespace'Length .. Name'Last));
         --  Remove 'CORBA.' from exception name to produce a name
         --  compatible with internal naming scheme for exceptions.

      end;

      Result := Get_Empty_Any_Aggregate (TC);
      Add_Aggregate_Element (Result,
         PolyORB.Any.Any (CORBA.To_Any (Members.Minor)));
      Add_Aggregate_Element (Result,
        PolyORB.Any.Any (CORBA.To_Any (Members.Completed)));

      pragma Debug (C, O ("System_Exception_To_Any: leave"));
      return Result;
   end System_Exception_To_Any;

   ----------------------
   -- Raise_From_Error --
   ----------------------

   procedure Raise_From_Error
     (Error   : in out PolyORB.Errors.Error_Container;
      Message : String := "")
   is
   begin
      pragma Debug (C, O ("About to raise exception: "
                       & Error_Id'Image (Error.Kind)));

      pragma Assert (Is_Error (Error));

      if Error.Kind in ORB_System_Error then
         pragma Debug (C, O ("Raising CORBA Exception"));
         CORBA_Raise_From_Error (Error, Message);

      elsif Error.Kind in POA_Error then
         pragma Debug (C, O ("Raising PORTABLESERVER.POA Exception"));
         POA_Raise_From_Error (Error, Message);

      elsif Error.Kind in POAManager_Error then
         pragma Debug (C, O ("Raising PORTABLESERVER.POAManager Exception"));
         POAManager_Raise_From_Error (Error, Message);

      elsif Error.Kind in PolyORB_Internal_Error then
         --  PolyORB internal errors are mapped to CORBA.Unknown

         pragma Debug (C, O ("Raising CORBA.UNKNOWN"));
         CORBA.Raise_Unknown (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      --  Never reached (Raiser raises an exception.)
   end Raise_From_Error;

   ------------------------------
   -- Request_Raise_Occurrence --
   ------------------------------

   procedure Request_Raise_Occurrence (R : in out Requests.Request_Access) is
   begin
      if not Any.Is_Empty (R.Exception_Info) then
         declare
            Exception_Occurrence : constant Any.Any := R.Exception_Info;
            Exception_Information :
              constant String :=
                PolyORB.QoS.Exception_Informations.
                  Get_Exception_Information (R.all);
            Last : Integer;
         begin
            Requests.Destroy_Request (R);

            --  Truncate exception information to first 150 characters

            if Exception_Information'Length <= 150 then
               Last := Exception_Information'Last;
            else
               Last := Exception_Information'First + 149;
            end if;

            --  Strip trailing newline

            if Last >= Exception_Information'First
              and then Exception_Information (Last) = ASCII.LF
            then
               Last := Last - 1;
            end if;

            --  Raise exception, including original exception information if
            --  present.

            if Last >= Exception_Information'First then
               Raise_From_Any
                 (Exception_Occurrence,
                  "<Original exception info: "
                  & Exception_Information (Exception_Information'First .. Last)
                  & ">");
            else
               Raise_From_Any (Exception_Occurrence);
            end if;
         end;
      end if;
   end Request_Raise_Occurrence;

end PolyORB.CORBA_P.Exceptions;
