------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.OBJ_ADAPTERS.GROUP_OBJECT_ADAPTER                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

--  Special Object Adapter to manage group servants

with PolyORB.Binding_Data;
with PolyORB.Log;
with PolyORB.Obj_Adapter_QoS;
with PolyORB.Servants.Group_Servants;
with PolyORB.Utils;

package body PolyORB.Obj_Adapters.Group_Object_Adapter is

   use PolyORB.Errors;
   use PolyORB.Log;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.obj_adapters.group_object_adapter");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   ------------
   -- Create --
   ------------

   procedure Create (GOA : access Group_Object_Adapter) is
   begin
      Initialize (GOA.Registered_Groups);
      Create (GOA.Lock);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (GOA : access Group_Object_Adapter) is
   begin
      Finalize (GOA.Registered_Groups);
      Destroy (GOA.Lock);
      Destroy (Obj_Adapter (GOA.all)'Access);
   end Destroy;

   ------------------------------
   -- Handle_Request_Execution --
   ------------------------------

   function Handle_Request_Execution
     (Self      : access Simple_Executor;
      Msg       : PolyORB.Components.Message'Class;
      Requestor : PolyORB.Components.Component_Access)
     return PolyORB.Components.Message'Class
   is
      use PolyORB.Servants;

      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      --  At this stage, PolyORB.ORB.Run has already affected a thread
      --  to handle the request execution, in which this current call
      --  is executed. Thus we just need to call the Execute_Servant
      --  procedure to go on with the request execution.

      return Execute_Servant (Servant_Access (Requestor), Msg);
   end Handle_Request_Execution;

   --------------------------------------
   -- Interface to application objects --
   --------------------------------------

   ------------
   -- Export --
   ------------

   procedure Export
     (GOA   : access Group_Object_Adapter;
      Obj   :        Servants.Servant_Access;
      Key   :        Objects.Object_Id_Access;
      Oid   :    out Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Key);
      pragma Warnings (On);

      use PolyORB.Objects;
      use type PolyORB.Servants.Servant_Access;

      GS : PolyORB.Servants.Servant_Access;

   begin
      PolyORB.Servants.Group_Servants.Get_Group_Object_Id
        (Obj, Oid, Error);
      if Found (Error) then
         Throw (Error, NotAGroupObject_E, Null_Members'(Null_Member));
         return;
      end if;

      Enter (GOA.Lock);
      GS := Lookup (GOA.Registered_Groups, Image (Oid.all), null);
      if GS /= null then
         pragma Debug (O ("Group "
                          & Image (Oid.all)
                          & " has been registered before"));
         Throw (Error, NotAGroupObject_E, Null_Members'(Null_Member));
         --  XXX Need to add a GroupAlreadyRegistered exception ?

      else
         --  Register the group

         pragma Debug
           (O ("Group servant : " & Image (Oid.all) & " exported"));
         Insert (GOA.Registered_Groups, Image (Oid.all), Obj);
         PolyORB.Servants.Set_Executor (Obj, GOA.S_Exec'Access);
         --  XXX questionable
      end if;
      Leave (GOA.Lock);
   end Export;

   --------------
   -- Unexport --
   --------------

   procedure Unexport
     (GOA   : access Group_Object_Adapter;
      Id    :        Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.Objects;
      use PolyORB.Servants.Group_Servants;
      use type PolyORB.Servants.Servant_Access;

      GS : PolyORB.Servants.Servant_Access;

   begin
      Enter (GOA.Lock);

      GS := Lookup (GOA.Registered_Groups, Oid_To_Hex_String (Id.all), null);

      if GS = null then
         pragma Debug (O ("Invalid group : " & Oid_To_Hex_String (Id.all)));
         Throw (Error,
                Invalid_Object_Id_E,
                Null_Members'(Null_Member));
      else
         Destroy_Group_Servant (GS);
         Delete (GOA.Registered_Groups, Oid_To_Hex_String (Id.all));
         pragma Debug (O ("Group removed with success : "
                          & Oid_To_Hex_String (Id.all)));
      end if;

      Leave (GOA.Lock);
   end Unexport;

   ----------------
   -- Object_Key --
   ----------------

   procedure Object_Key
     (GOA     : access Group_Object_Adapter;
      Id      :        Objects.Object_Id_Access;
      User_Id :    out Objects.Object_Id_Access;
      Error   : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (GOA, Id);
      pragma Warnings (On);

   begin
      --  No user id in this OA
      User_Id := null;

      Throw (Error,
             Invalid_Object_Id_E,
             Null_Members'(Null_Member));
   end Object_Key;

   -------------
   -- Get_QoS --
   -------------

   procedure Get_QoS
     (OA    : access Group_Object_Adapter;
      Id    :        Objects.Object_Id;
      QoS   :    out PolyORB.QoS.QoS_Parameters;
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Id);
      pragma Unreferenced (Error);
   begin
      QoS := PolyORB.Obj_Adapter_QoS.Get_Object_Adapter_QoS (OA);
   end Get_QoS;

   ----------------------------------------------------
   -- Interface to ORB (acting on behalf of clients) --
   ----------------------------------------------------

   ------------------------
   -- Get_Empty_Arg_List --
   ------------------------

   function Get_Empty_Arg_List
     (GOA    : access Group_Object_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String)
      return Any.NVList.Ref
   is
      pragma Warnings (Off);
      pragma Unreferenced (GOA, Oid, Method);
      pragma Warnings (On);

      Result : Any.NVList.Ref;

   begin
      pragma Debug (O ("Get empty args list called, return empty list"));
      return Result;
   end Get_Empty_Arg_List;

   ----------------------
   -- Get_Empty_Result --
   ----------------------

   function Get_Empty_Result
     (GOA    : access Group_Object_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String)
      return Any.Any
   is
      pragma Warnings (Off);
      pragma Unreferenced (GOA, Oid, Method);
      pragma Warnings (On);

      Result : Any.Any;

   begin
      pragma Debug (O ("Get empty result list called, return no type"));
      return Result;
   end Get_Empty_Result;

   ------------------
   -- Find_Servant --
   ------------------

   procedure Find_Servant
     (GOA     : access Group_Object_Adapter;
      Id      : access Objects.Object_Id;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.Objects;
      use type PolyORB.Servants.Servant_Access;

   begin
      pragma Debug (O ("Find_Servant " & Oid_To_Hex_String (Id.all)));

      Enter (GOA.Lock);

      Servant := Lookup (GOA.Registered_Groups,
                         Oid_To_Hex_String (Id.all), null);
      if Servant = null then
         pragma Debug (O ("Servant not found"));
         Throw (Error,
                Invalid_Object_Id_E,
                Null_Members'(Null_Member));
      else
         pragma Debug (O ("Servant found"));
         null;
      end if;

      Leave (GOA.Lock);
   end Find_Servant;

   ---------------------
   -- Release_Servant --
   ---------------------

   procedure Release_Servant
     (GOA     : access Group_Object_Adapter;
      Id      : access Objects.Object_Id;
      Servant : in out Servants.Servant_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (GOA, Id);
      pragma Warnings (On);

   begin
      --  Nothing to do

      Servant := null;
   end Release_Servant;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group
     (The_Ref              : PolyORB.References.Ref;
      Allow_Group_Creation : Boolean := False)
     return PolyORB.Servants.Servant_Access
   is
      use PolyORB.Binding_Data;
      use PolyORB.Objects;
      use PolyORB.References;
      use PolyORB.Smart_Pointers;

      Profs : constant Profile_Array
        := Profiles_Of (The_Ref);
      Error : Error_Container;
      GS : PolyORB.Servants.Servant_Access;

   begin
      pragma Debug (O ("Get group from ref"));

      for J in Profs'Range loop
         declare
            OA_Entity : constant PolyORB.Smart_Pointers.Entity_Ptr
              := Get_OA (Profs (J).all);
         begin
            if OA_Entity /= null
              and then OA_Entity.all in Group_Object_Adapter'Class
            then
               pragma Debug (O ("Searching group using a group profile"));

               Find_Servant
                 (Group_Object_Adapter (OA_Entity.all)'Access,
                  Get_Object_Key (Profs (J).all),
                  GS,
                  Error);

               if not Found (Error) then
                  pragma Debug (O ("Group found"));
                  return GS;
               end if;

               if Allow_Group_Creation then
                  pragma Debug (O ("Create a new group"));
                  GS := PolyORB.Servants.Group_Servants.Create_Group_Servant
                    (Get_Object_Key (Profs (J).all));

                  declare
                     Oid   : Object_Id_Access;
                     Error : Error_Container;
                  begin
                     Export
                       (Group_Object_Adapter (OA_Entity.all)'Access,
                        GS,
                        null,
                        Oid,
                        Error);

                     if Found (Error)
                       or else Oid.all /= Get_Object_Key (Profs (J).all).all
                     then
                        pragma Debug (O ("Exporting group error"));
                        return null;
                     end if;

                     pragma Debug (O ("Group Exported"));
                     return GS;
                  end;
               end if;
            end if;
         end;
      end loop;

      pragma Debug (O ("Group not found"));
      return null;
   end Get_Group;

end PolyORB.Obj_Adapters.Group_Object_Adapter;
