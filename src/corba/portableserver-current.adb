------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O R T A B L E S E R V E R . C U R R E N T                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2010, Free Software Foundation, Inc.          --
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

with PolyORB.CORBA_P.Initial_References;
with PolyORB.Errors;
with PolyORB.Initialization;
with PolyORB.POA;
with PolyORB.POA_Policies.Id_Assignment_Policy;
with PolyORB.POA_Types;
with PolyORB.Servants;
with PolyORB.Smart_Pointers;
with PolyORB.Tasking.Threads.Annotations;
with PolyORB.Types;
with PolyORB.Utils.Strings.Lists;

with PortableServer.POA;
with PortableServer.Current.Helper;

package body PortableServer.Current is

   use PolyORB.Errors;
   use PolyORB.Annotations;
   use PolyORB.Binding_Data;
   use PolyORB.POA;
   use PolyORB.POA_Policies.Id_Assignment_Policy;
   use PolyORB.POA_Types;
   use PolyORB.Tasking.Threads.Annotations;
   use PolyORB.Types;
   use PortableServer.Current.Helper;

   function Create return CORBA.Object.Ref;

   function Find_POA
     (Profile : Profile_Access)
      return PolyORB.POA.Obj_Adapter_Access;
   --  Find POA which manage object specified by profile

   ------------
   -- Create --
   ------------

   function Create return CORBA.Object.Ref
   is
      Result  : Local_Ref;
      Current : constant PolyORB.Smart_Pointers.Entity_Ptr :=
                  new Current_Object;

   begin
      Set (Result, Current);
      return CORBA.Object.Ref (Result);
   end Create;

   --------------
   -- Find_POA --
   --------------

   function Find_POA
     (Profile : Profile_Access)
      return PolyORB.POA.Obj_Adapter_Access
   is
      U_Oid  : Unmarshalled_Oid;
      Obj_OA : PolyORB.POA.Obj_Adapter_Access;
      Error  : Error_Container;

   begin
      Oid_To_U_Oid (Get_Object_Key (Profile.all).all, U_Oid, Error);
      if Found (Error) then
         raise Program_Error;
      end if;

      Find_POA
        (PolyORB.POA.Obj_Adapter_Access (Get_OA (Profile.all)),
         To_Standard_String (U_Oid.Creator),
         True,
         Obj_OA,
         Error);
      if Found (Error) then
         raise Program_Error;
      end if;

      return Obj_OA;
   end Find_POA;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Obj             : not null access Current_Object;
      Logical_Type_Id : Standard.String) return Boolean
   is
      pragma Unreferenced (Obj);
   begin
      return
        CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/PortableServer/Current:1.0")
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

   ------------
   -- To_Ref --
   ------------

   function To_Ref (Self : CORBA.Object.Ref'Class) return Local_Ref
   is
      Result : Local_Ref;
   begin
      if CORBA.Object.Entity_Of (Self).all
        not in Current_Object'Class then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      Set (Result, CORBA.Object.Entity_Of (Self));
      return Result;
   end To_Ref;

   -------------
   -- Get_POA --
   -------------

   function Get_POA (Self : Local_Ref) return PortableServer.POA_Forward.Ref is
      pragma Unreferenced (Self);

      use type PolyORB.Requests.Request_Access;

      Note  : PortableServer_Current_Note;

   begin
      Get_Note (Get_Current_Thread_Notepad.all, Note,
                Null_PortableServer_Current_Note);

      if Note.Request = null then
         Raise_NoContext ((CORBA.IDL_Exception_Members with null record));
      end if;

      return
        POA.Convert.To_Forward
        (POA.Internals.To_CORBA_POA (Find_POA (Note.Profile)));
   end Get_POA;

   -------------------
   -- Get_Object_Id --
   -------------------

   function Get_Object_Id (Self : Local_Ref) return ObjectId is
      pragma Unreferenced (Self);

      use type PolyORB.Requests.Request_Access;

      Note  : PortableServer_Current_Note;
      Error : Error_Container;
      Oid   : PolyORB.Objects.Object_Id_Access;

   begin
      Get_Note (Get_Current_Thread_Notepad.all, Note,
                Null_PortableServer_Current_Note);

      if Note.Request = null then
         Raise_NoContext ((CORBA.IDL_Exception_Members with null record));
      end if;

      Object_Identifier
        (Find_POA (Note.Profile).Id_Assignment_Policy.all,
         Get_Object_Key (Note.Profile.all),
         Oid,
         Error);
      if Found (Error) then
         raise Program_Error;
      end if;

      declare
         Result : constant ObjectId
           := Internals.To_PortableServer_ObjectId (Oid.all);

      begin
         Free (Oid);
         return Result;
      end;
   end Get_Object_Id;

   -------------------
   -- Get_Reference --
   -------------------

   function Get_Reference (Self : Local_Ref) return CORBA.Object.Ref is
      pragma Unreferenced (Self);
      use type PolyORB.Requests.Request_Access;
      Note   : PortableServer_Current_Note;
   begin
      Get_Note (Get_Current_Thread_Notepad.all, Note,
                Null_PortableServer_Current_Note);

      if Note.Request = null then
         Raise_NoContext ((CORBA.IDL_Exception_Members with null record));
      end if;

      return CORBA.Object.Internals.To_CORBA_Ref (Note.Request.Target);
   end Get_Reference;

   -----------------
   -- Get_Servant --
   -----------------

   function Get_Servant (Self : Local_Ref) return Servant is
      pragma Unreferenced (Self);

      use type PolyORB.Requests.Request_Access;

      Note    : PortableServer_Current_Note;
      Error   : Error_Container;
      Neutral : PolyORB.Servants.Servant_Access;

   begin
      Get_Note (Get_Current_Thread_Notepad.all, Note,
                Null_PortableServer_Current_Note);

      if Note.Request = null then
         Raise_NoContext ((CORBA.IDL_Exception_Members with null record));
      end if;

      Id_To_Servant (Find_POA (Note.Profile),
                     Get_Object_Key (Note.Profile.all).all,
                     Neutral,
                     Error);
      if Found (Error) then
         raise Program_Error;
      end if;

      return Servant (CORBA.Impl.Internals.To_CORBA_Servant (Neutral));
   end Get_Servant;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out NoContext_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= NoContext'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := NoContext_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      use PolyORB.CORBA_P.Initial_References;
   begin
      Register_Initial_Reference ("POACurrent", Create'Access);
      PortableServer.PortableServer_Current_Registered := True;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;
   use PolyORB.Utils.Strings.Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"portableserver.current",
       Conflicts => Empty,
       Depends   => +"corba.initial_references",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PortableServer.Current;
