------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O R T A B L E S E R V E R                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Tags;

with PolyORB.CORBA_P.Names;
with PolyORB.CORBA_P.Interceptors_Hooks;

with PolyORB.Annotations;
with PolyORB.Binding_Data;
with PolyORB.Errors;
with PolyORB.Exceptions;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Requests;
with PolyORB.Servants.Iface;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;

package body PortableServer is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("portableserver");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ---------------------------------------
   -- Information about a skeleton unit --
   ---------------------------------------

   type Skeleton_Info is record
      Type_Id     : CORBA.RepositoryId;
      Is_A        : Internals.Servant_Class_Predicate;
      Target_Is_A : Internals.Servant_Class_Is_A_Operation;
      Dispatcher  : Internals.Request_Dispatcher;
   end record;

   function Find_Info
     (For_Servant : Servant)
     return Skeleton_Info;

   package Skeleton_Lists is new PolyORB.Utils.Chained_Lists
     (Skeleton_Info);

   All_Skeletons : Skeleton_Lists.List;

   Skeleton_Unknown : exception;

   type Dispatcher_Note is new PolyORB.Annotations.Note with record
      Skeleton : Internals.Request_Dispatcher;
   end record;

   Null_Dispatcher_Note : constant Dispatcher_Note
     := (PolyORB.Annotations.Note with Skeleton => null);

   procedure Default_Invoke
     (Servant : access PolyORB.Smart_Pointers.Entity'Class;
      Request : in     PolyORB.Requests.Request_Access;
      Profile : in     PolyORB.Binding_Data.Profile_Access);
   --  This is the default server side invocation handler.

   --------------------
   -- Default_Invoke --
   --------------------

   procedure Default_Invoke
     (Servant : access PolyORB.Smart_Pointers.Entity'Class;
      Request : in     PolyORB.Requests.Request_Access;
      Profile : in     PolyORB.Binding_Data.Profile_Access)
   is
      pragma Unreferenced (Profile);
   begin
      Invoke (DynamicImplementation'Class (Servant.all)'Access,
              Request);
      --  Redispatch
   end Default_Invoke;

   ---------------------
   -- Execute_Servant --
   ---------------------

   function Execute_Servant
     (Self : access DynamicImplementation;
      Msg  :        PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class
   is
      use PolyORB.Servants.Iface;

   begin
      pragma Debug (O ("Execute_Servant: enter"));

      if Msg in Execute_Request then
         declare
            use PolyORB.Binding_Data;
            use PolyORB.Requests;
            use CORBA.ServerRequest;
            use PolyORB.Errors;

            R : constant Request_Access := Execute_Request (Msg).Req;
            P : constant Profile_Access := Execute_Request (Msg).Pro;
            Error : Error_Container;
         begin
            PolyORB.CORBA_P.Interceptors_Hooks.Server_Invoke
              (DynamicImplementation'Class (Self.all)'Access, R, P);

            if R.Arguments_Called then

               --  Implementation Note: As part of PortableInterceptors
               --  specifications, an interception point may raise an
               --  exception before Arguments is called.
               --
               --  As a consequence, set out arguments iff the
               --  skeleton called Arguments.

               pragma Debug
                 (O ("Execute_Servant: executed, setting out args"));
               Set_Out_Args (R, Error);

               if Found (Error) then
                  raise Program_Error;
                  --  XXX We should do something if we find a PolyORB exception

               end if;
            end if;

            pragma Debug (O ("Execute_Servant: leave"));
            return Executed_Request'(Req => R);
         end;

      else
         pragma Debug (O ("Execute_Servant: bad message, leave"));
         raise Program_Error;

      end if;
   end Execute_Servant;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Self    : access Servant_Base;
      Request : in     CORBA.ServerRequest.Object_Ptr)
   is
      use type Internals.Request_Dispatcher;

      P_Servant : constant PolyORB.Servants.Servant_Access :=
        CORBA.Impl.To_PolyORB_Servant
        (CORBA.Impl.Object (Servant (Self).all)'Access);

      Notepad : constant PolyORB.Annotations.Notepad_Access
        := PolyORB.Servants.Notepad_Of (P_Servant);

      Dispatcher : Dispatcher_Note;

   begin
      pragma Debug (O ("Invoke on a static skeleton: enter"));

      --  Information about servant's skeleton is cached in its notepad.

      PolyORB.Annotations.Get_Note
        (Notepad.all, Dispatcher, Null_Dispatcher_Note);

      if Dispatcher.Skeleton = null then
         pragma Debug (O ("Cacheing information about skeleton"));

         Dispatcher.Skeleton := Find_Info (Servant (Self)).Dispatcher;
         PolyORB.Annotations.Set_Note (Notepad.all, Dispatcher);
      end if;

      Dispatcher.Skeleton (Servant (Self), Request);

      pragma Debug (O ("Invoke on a static skeleton: leave"));
   end Invoke;

   package body Internals is

      -----------------
      -- Get_Type_Id --
      -----------------

      function Get_Type_Id
        (For_Servant : in Servant)
        return CORBA.RepositoryId
      is
      begin
         return Find_Info (For_Servant).Type_Id;

      exception
         when Skeleton_Unknown =>
            return CORBA.To_CORBA_String
              (PolyORB.CORBA_P.Names.OMG_RepositoryId ("CORBA/OBJECT"));
      end Get_Type_Id;

      -----------------------
      -- Register_Skeleton --
      -----------------------

      procedure Register_Skeleton
        (Type_Id     : in CORBA.RepositoryId;
         Is_A        : in Servant_Class_Predicate;
         Target_Is_A : in Servant_Class_Is_A_Operation;
         Dispatcher  : in Request_Dispatcher := null)
      is
         use Skeleton_Lists;

      begin
         pragma Debug (O ("Register_Skeleton: Enter."));

         Prepend (All_Skeletons,
                  (Type_Id     => Type_Id,
                   Is_A        => Is_A,
                   Target_Is_A => Target_Is_A,
                   Dispatcher  => Dispatcher));

         pragma Debug (O ("Registered : type_id = " &
                          CORBA.To_Standard_String (Type_Id)));

      end Register_Skeleton;

      -----------------
      -- Target_Is_A --
      -----------------

      function Target_Is_A
        (For_Servant     : in Servant;
         Logical_Type_Id : in CORBA.RepositoryId)
        return CORBA.Boolean
      is
      begin
         return
           Find_Info (For_Servant).Target_Is_A
            (CORBA.To_Standard_String (Logical_Type_Id));
      end Target_Is_A;

      -----------------------------------
      -- Target_Most_Derived_Interface --
      -----------------------------------

      function Target_Most_Derived_Interface
        (For_Servant : in Servant)
        return CORBA.RepositoryId
      is
      begin
         return Find_Info (For_Servant).Type_Id;
      end Target_Most_Derived_Interface;

   end Internals;

   ---------------
   -- Find_Info --
   ---------------

   function Find_Info
     (For_Servant : Servant)
     return Skeleton_Info
   is
      use Skeleton_Lists;

      It : Iterator := First (All_Skeletons);

   begin
      pragma Debug
        (O ("Find_Info: servant of type "
            & Ada.Tags.External_Tag (For_Servant'Tag)));

      while not Last (It) loop
         pragma Debug (O ("... skeleton id: "
           & CORBA.To_Standard_String (Value (It).Type_Id)));
         exit when Value (It).Is_A (For_Servant);
         Next (It);
      end loop;

      if Last (It) then
         raise Skeleton_Unknown;
      end if;

      return Value (It).all;
   end Find_Info;

   ------------------------
   -- String_To_ObjectId --
   ------------------------

   function String_To_ObjectId (Id : String) return ObjectId is
      Oid : ObjectId (1 .. Id'Length);
      pragma Import (Ada, Oid);
      for Oid'Address use Id (Id'First)'Address;
   begin
      return Oid;
   end String_To_ObjectId;

   ------------------------
   -- ObjectId_To_String --
   ------------------------

   function ObjectId_To_String (Id : ObjectId) return String is
      Str : String (1 .. Id'Length);
      pragma Import (Ada, Str);
      for Str'Address use Id (Id'First)'Address;
   begin
      return Str;
   end ObjectId_To_String;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
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
     (From : in  Ada.Exceptions.Exception_Occurrence;
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

   --------------------------
   -- Raise_ForwardRequest --
   --------------------------

   procedure Raise_ForwardRequest
     (Excp_Memb : in ForwardRequest_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15

   begin
      raise Program_Error;
   end Raise_ForwardRequest;

   ---------------------------
   -- Raise_NotAGroupObject --
   ---------------------------

   procedure Raise_NotAGroupObject
     (Excp_Memb : in NotAGroupObject_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15

   begin
      raise NotAGroupObject;
   end Raise_NotAGroupObject;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : in CORBA.Any)
     return ThreadPolicyValue
   is
      Index : CORBA.Any :=
        CORBA.Get_Aggregate_Element (Item,
                                     CORBA.TC_Unsigned_Long,
                                     CORBA.Unsigned_Long (0));
      Position : constant CORBA.Unsigned_Long := CORBA.From_Any (Index);
   begin
      return ThreadPolicyValue'Val (Position);
   end From_Any;

   function From_Any
     (Item : in CORBA.Any)
     return LifespanPolicyValue
   is
      Index : CORBA.Any :=
        CORBA.Get_Aggregate_Element (Item,
                                     CORBA.TC_Unsigned_Long,
                                     CORBA.Unsigned_Long (0));
      Position : constant CORBA.Unsigned_Long := CORBA.From_Any (Index);
   begin
      return LifespanPolicyValue'Val (Position);
   end From_Any;

   function From_Any
     (Item : in CORBA.Any)
     return IdUniquenessPolicyValue
   is
      Index : CORBA.Any :=
        CORBA.Get_Aggregate_Element (Item,
                                     CORBA.TC_Unsigned_Long,
                                     CORBA.Unsigned_Long (0));
      Position : constant CORBA.Unsigned_Long := CORBA.From_Any (Index);
   begin
      return IdUniquenessPolicyValue'Val (Position);
   end From_Any;

   function From_Any
     (Item : in CORBA.Any)
     return IdAssignmentPolicyValue
   is
      Index : CORBA.Any :=
        CORBA.Get_Aggregate_Element (Item,
                                     CORBA.TC_Unsigned_Long,
                                     CORBA.Unsigned_Long (0));
      Position : constant CORBA.Unsigned_Long := CORBA.From_Any (Index);
   begin
      return IdAssignmentPolicyValue'Val (Position);
   end From_Any;

   function From_Any
     (Item : in CORBA.Any)
     return ImplicitActivationPolicyValue
   is
      Index : CORBA.Any :=
        CORBA.Get_Aggregate_Element (Item,
                                     CORBA.TC_Unsigned_Long,
                                     CORBA.Unsigned_Long (0));
      Position : constant CORBA.Unsigned_Long := CORBA.From_Any (Index);
   begin
      return ImplicitActivationPolicyValue'Val (Position);
   end From_Any;

   function From_Any
     (Item : in CORBA.Any)
     return ServantRetentionPolicyValue
   is
      Index : CORBA.Any :=
        CORBA.Get_Aggregate_Element (Item,
                                     CORBA.TC_Unsigned_Long,
                                     CORBA.Unsigned_Long (0));
      Position : constant CORBA.Unsigned_Long := CORBA.From_Any (Index);
   begin
      return ServantRetentionPolicyValue'Val (Position);
   end From_Any;

   function From_Any
     (Item : in CORBA.Any)
     return RequestProcessingPolicyValue
   is
      Index : CORBA.Any :=
        CORBA.Get_Aggregate_Element (Item,
                                     CORBA.TC_Unsigned_Long,
                                     CORBA.Unsigned_Long (0));
      Position : constant CORBA.Unsigned_Long := CORBA.From_Any (Index);
   begin
      return RequestProcessingPolicyValue'Val (Position);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : in ThreadPolicyValue)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.Get_Empty_Any_Aggregate (TC_ThreadPolicyValue);
   begin
      CORBA.Add_Aggregate_Element
        (Result,
         CORBA.To_Any
         (CORBA.Unsigned_Long (ThreadPolicyValue'Pos (Item))));
      return Result;
   end To_Any;

   function To_Any
     (Item : in LifespanPolicyValue)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.Get_Empty_Any_Aggregate (TC_LifespanPolicyValue);
   begin
      CORBA.Add_Aggregate_Element
        (Result,
         CORBA.To_Any
         (CORBA.Unsigned_Long (LifespanPolicyValue'Pos (Item))));
      return Result;
   end To_Any;

   function To_Any
     (Item : in IdUniquenessPolicyValue)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.Get_Empty_Any_Aggregate (TC_IdUniquenessPolicyValue);
   begin
      CORBA.Add_Aggregate_Element
        (Result,
         CORBA.To_Any
         (CORBA.Unsigned_Long (IdUniquenessPolicyValue'Pos (Item))));
      return Result;
   end To_Any;

   function To_Any
     (Item : in IdAssignmentPolicyValue)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.Get_Empty_Any_Aggregate (TC_IdAssignmentPolicyValue);
   begin
      CORBA.Add_Aggregate_Element
        (Result,
         CORBA.To_Any
         (CORBA.Unsigned_Long (IdAssignmentPolicyValue'Pos (Item))));
      return Result;
   end To_Any;

   function To_Any
     (Item : in ImplicitActivationPolicyValue)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.Get_Empty_Any_Aggregate (TC_ImplicitActivationPolicyValue);
   begin
      CORBA.Add_Aggregate_Element
        (Result,
         CORBA.To_Any
         (CORBA.Unsigned_Long (ImplicitActivationPolicyValue'Pos (Item))));
      return Result;
   end To_Any;

   function To_Any
     (Item : in ServantRetentionPolicyValue)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.Get_Empty_Any_Aggregate (TC_ServantRetentionPolicyValue);
   begin
      CORBA.Add_Aggregate_Element
        (Result,
         CORBA.To_Any
         (CORBA.Unsigned_Long (ServantRetentionPolicyValue'Pos (Item))));
      return Result;
   end To_Any;

   function To_Any
     (Item : in RequestProcessingPolicyValue)
     return CORBA.Any
   is
      Result : CORBA.Any :=
        CORBA.Get_Empty_Any_Aggregate (TC_RequestProcessingPolicyValue);
   begin
      CORBA.Add_Aggregate_Element
        (Result,
         CORBA.To_Any
         (CORBA.Unsigned_Long (RequestProcessingPolicyValue'Pos (Item))));
      return Result;
   end To_Any;

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
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"portablserver",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access));
end PortableServer;
