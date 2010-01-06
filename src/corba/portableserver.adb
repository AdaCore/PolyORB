------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O R T A B L E S E R V E R                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2009, Free Software Foundation, Inc.          --
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

with Ada.Tags;
with Ada.Unchecked_Conversion;

with PolyORB.CORBA_P.Names;
with PolyORB.CORBA_P.Interceptors_Hooks;

with PolyORB.Any;
with PolyORB.Errors;
with PolyORB.Exceptions;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Servants.Iface;
with PolyORB.Tasking.Threads.Annotations;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;

package body PortableServer is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("portableserver");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   use PolyORB.CORBA_P.Interceptors_Hooks;
   use PolyORB.Utils.Strings;

   ---------------------------------------
   -- Information about a skeleton unit --
   ---------------------------------------

   type Skeleton_Info is record
      Type_Id     : String_Ptr;
      Is_A        : Internals.Servant_Class_Predicate;
      Target_Is_A : Internals.Servant_Class_Is_A_Operation;
      Dispatcher  : Internals.Request_Dispatcher;
   end record;

   function Find_Info (For_Servant : Servant) return Skeleton_Info;

   package Skeleton_Lists is new PolyORB.Utils.Chained_Lists (Skeleton_Info);

   All_Skeletons : Skeleton_Lists.List;

   Skeleton_Unknown : exception;

   type Dispatcher_Note is new PolyORB.Annotations.Note with record
      Skeleton : Internals.Request_Dispatcher;
   end record;

   Null_Dispatcher_Note : constant Dispatcher_Note :=
                            (PolyORB.Annotations.Note with Skeleton => null);

   procedure Default_Invoke
     (Servant : access PSPCE.Entity'Class;
      Request : PolyORB.Requests.Request_Access;
      Profile : PolyORB.Binding_Data.Profile_Access);
   --  This is the default server side invocation handler

   --------------------
   -- Default_Invoke --
   --------------------

   procedure Default_Invoke
     (Servant : access PSPCE.Entity'Class;
      Request : PolyORB.Requests.Request_Access;
      Profile : PolyORB.Binding_Data.Profile_Access)
   is
      pragma Unreferenced (Profile);
   begin
      --  Redispatch

      Invoke (DynamicImplementation'Class (Servant.all)'Access, Request);
   end Default_Invoke;

   ---------------------
   -- Execute_Servant --
   ---------------------

   function Execute_Servant
     (Self : not null access DynamicImplementation;
      Msg  : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class
   is
      use PolyORB.Servants.Iface;

   begin
      pragma Debug (C, O ("Execute_Servant: enter"));

      if Msg in Execute_Request then
         declare
            use CORBA.ServerRequest;
            use PolyORB.Annotations;
            use PolyORB.Binding_Data;
            use PolyORB.Errors;
            use PolyORB.Requests;
            use PolyORB.Tasking.Threads.Annotations;

            R         : constant Request_Access := Execute_Request (Msg).Req;
            P         : constant Profile_Access := Execute_Request (Msg).Pro;
            Error     : Error_Container;

         begin
            if PortableServer_Current_Registered then
               declare
                  Notepad   : constant Notepad_Access
                    := Get_Current_Thread_Notepad;
                  Save_Note : PortableServer_Current_Note;
                  Note      : constant PortableServer_Current_Note
                    := (PolyORB.Annotations.Note with Request => R,
                        Profile => P);

               begin
                  --  Save POA Current note

                  Get_Note (Notepad.all, Save_Note,
                            Null_PortableServer_Current_Note);

                  --  Set new POA Current note

                  Set_Note (Notepad.all, Note);

                  --  Process invocation

                  PolyORB.CORBA_P.Interceptors_Hooks.Server_Invoke
                    (DynamicImplementation'Class (Self.all)'Access, R, P);

                  --  Restore original POA Current note

                  Set_Note (Notepad.all, Save_Note);
               end;

            else
               --  Process invocation

               PolyORB.CORBA_P.Interceptors_Hooks.Server_Invoke
                 (DynamicImplementation'Class (Self.all)'Access, R, P);
            end if;

            --  Implementation Note: As part of PortableInterceptors
            --  specifications, an interception point may raise an exception
            --  before Arguments is called. An exception may also have been
            --  raised by Arguments itself, in which case Arguments_Called is
            --  True and the R.Exception_Info Any is non-empty. We set out
            --  arguments only if no exception was raised.

            --  Note: At this point the stack frame of the skel has been exited
            --  and the shadow any's for IN mode arguments now have dangling
            --  content pointers. In particular this means that any call to
            --  Image (R.Out_Args) is likely to fail on such arguments.

            if R.Arguments_Called
                 and then
               PolyORB.Any.Is_Empty (R.Exception_Info)
            then
               pragma Debug
                 (C, O ("Execute_Servant: executed, setting out args"));
               Set_Out_Args (R, Error);

               if Found (Error) then
                  raise Program_Error;
                  --  XXX We should do something if we find a PolyORB exception

               end if;
            end if;

            pragma Debug (C, O ("Execute_Servant: leave"));
            return Executed_Request'(Req => R);
         end;

      else
         pragma Debug (C, O ("Execute_Servant: bad message, leave"));
         raise Program_Error;

      end if;
   end Execute_Servant;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Self    : access Servant_Base;
      Request : CORBA.ServerRequest.Object_Ptr)
   is
      use type Internals.Request_Dispatcher;

      P_Servant : constant PolyORB.Servants.Servant_Access :=
        CORBA.Impl.To_PolyORB_Servant
        (CORBA.Impl.Object (Servant (Self).all)'Access);

      Notepad : constant PolyORB.Annotations.Notepad_Access
        := PolyORB.Servants.Notepad_Of (P_Servant);

      Dispatcher : Dispatcher_Note;

   begin
      pragma Debug (C, O ("Invoke on a static skeleton: enter"));

      --  Information about servant's skeleton is cached in its notepad.

      PolyORB.Annotations.Get_Note
        (Notepad.all, Dispatcher, Null_Dispatcher_Note);

      if Dispatcher.Skeleton = null then
         pragma Debug (C, O ("Cacheing information about skeleton"));

         Dispatcher.Skeleton := Find_Info (Servant (Self)).Dispatcher;
         PolyORB.Annotations.Set_Note (Notepad.all, Dispatcher);
      end if;

      Dispatcher.Skeleton (Servant (Self), Request);

      pragma Debug (C, O ("Invoke on a static skeleton: leave"));
   end Invoke;

   package body Internals is

      -----------------
      -- Get_Type_Id --
      -----------------

      function Get_Type_Id (For_Servant : Servant) return Standard.String is
      begin
         return Find_Info (For_Servant).Type_Id.all;
      exception
         when Skeleton_Unknown =>
            return PolyORB.CORBA_P.Names.OMG_RepositoryId ("CORBA/OBJECT");
      end Get_Type_Id;

      -----------------------
      -- Register_Skeleton --
      -----------------------

      procedure Register_Skeleton
        (Type_Id     : String;
         Is_A        : Servant_Class_Predicate;
         Target_Is_A : Servant_Class_Is_A_Operation;
         Dispatcher  : Request_Dispatcher := null)
      is
         use Skeleton_Lists;
      begin
         pragma Debug (C, O ("Register_Skeleton: Enter."));

         Prepend (All_Skeletons,
                  (Type_Id     => +Type_Id,
                   Is_A        => Is_A,
                   Target_Is_A => Target_Is_A,
                   Dispatcher  => Dispatcher));

         pragma Debug (C, O ("Registered : type_id = " & Type_Id));

      end Register_Skeleton;

      -----------------
      -- Target_Is_A --
      -----------------

      function Target_Is_A
        (For_Servant     : Servant;
         Logical_Type_Id : Standard.String) return CORBA.Boolean
      is
      begin
         return Find_Info (For_Servant).Target_Is_A (Logical_Type_Id);
      end Target_Is_A;

      -----------------------------------
      -- Target_Most_Derived_Interface --
      -----------------------------------

      function Target_Most_Derived_Interface
        (For_Servant : Servant) return Standard.String
      is
      begin
         return Find_Info (For_Servant).Type_Id.all;
      end Target_Most_Derived_Interface;

      --------------------------
      -- To_PolyORB_Object_Id --
      --------------------------

      function To_PolyORB_Object_Id
        (Id : ObjectId) return PolyORB.Objects.Object_Id
      is
         use CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet;
         use PolyORB.Objects;

         Elements : Element_Array := To_Element_Array (Id);

         subtype Oid_Subtype is Object_Id (1 .. Elements'Length);
         Result : Oid_Subtype;
         for Result'Address use Elements'Address;
         pragma Import (Ada, Result);

      begin
         return Result;
      end To_PolyORB_Object_Id;

      --------------------------------
      -- To_PortableServer_ObjectId --
      --------------------------------

      function To_PortableServer_ObjectId
        (Id : PolyORB.Objects.Object_Id) return ObjectId
      is
         use CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet;

         subtype Elements_Subtype is Element_Array (1 .. Id'Length);

         Elements : Elements_Subtype;
         for Elements'Address use Id'Address;
         pragma Import (Ada, Elements);

      begin
         return To_Sequence (Elements);
      end To_PortableServer_ObjectId;

   end Internals;

   ---------------
   -- Find_Info --
   ---------------

   function Find_Info (For_Servant : Servant) return Skeleton_Info is
      use Skeleton_Lists;
      It : Iterator := First (All_Skeletons);
   begin
      pragma Debug
        (C, O ("Find_Info: servant of type "
            & Ada.Tags.External_Tag (For_Servant'Tag)));

      while not Last (It) loop
         pragma Debug (C, O ("... skeleton id: " & Value (It).Type_Id.all));
         exit when Value (It).Is_A (For_Servant);
         Next (It);
      end loop;

      if Last (It) then
         raise Skeleton_Unknown;
      end if;
      pragma Debug
        (C, O ("Selected skeleton of Type_Id " & Value (It).Type_Id.all));

      return Value (It).all;
   end Find_Info;

   ------------------------
   -- String_To_ObjectId --
   ------------------------

   function String_To_ObjectId (Id : String) return ObjectId is
      use CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet;

      function To_Octet is
        new Ada.Unchecked_Conversion (Character, CORBA.Octet);

      Aux : Element_Array (Id'Range);

   begin
      for J in Aux'Range loop
         Aux (J) := To_Octet (Id (J));
      end loop;

      return To_Sequence (Aux);
   end String_To_ObjectId;

   ------------------------
   -- ObjectId_To_String --
   ------------------------

   function ObjectId_To_String (Id : ObjectId) return String is
      use CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet;

      function To_Character is
        new Ada.Unchecked_Conversion (CORBA.Octet, Character);

      Aux    : constant Element_Array := To_Element_Array (Id);
      Result : String (Aux'Range);

   begin
      for J in Result'Range loop
         Result (J) := To_Character (Aux (J));
      end loop;

      return Result;
   end ObjectId_To_String;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out ForwardRequest_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= ForwardRequest'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out NotAGroupObject_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= NotAGroupObject'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := NotAGroupObject_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   ---------------------------
   -- Raise_NotAGroupObject --
   ---------------------------

   procedure Raise_NotAGroupObject
     (Excp_Memb : NotAGroupObject_Members)
   is
      pragma Unreferenced (Excp_Memb);

   begin
      raise NotAGroupObject;
   end Raise_NotAGroupObject;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.CORBA_P.Interceptors_Hooks.Server_Invoke
        := Default_Invoke'Access;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"portableserver",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PortableServer;
