------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        B R O C A . R O O T P O A                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Tags;

with CORBA;
with CORBA.Object;
with CORBA.Impl;

with PortableServer; use PortableServer;
with PortableServer.POA;
with PortableServer.ServantManager;
with PortableServer.AdapterActivator;
with PortableServer.ServantManager.Impl;
with PortableServer.ServantActivator.Impl;
with PortableServer.ServantLocator.Impl;

with Broca.Flags;
with Broca.Refs;
with Broca.Exceptions;
with Broca.ORB;
with Broca.POA; use Broca.POA;
with Broca.Vararray;
with Broca.Buffers; use Broca.Buffers;
with Broca.CDR;
with Broca.Server;
with Broca.Locks;
with Broca.GIOP;
with Broca.Task_Attributes;

pragma Elaborate_All (CORBA.Object);
pragma Elaborate_All (Broca.Vararray);
pragma Elaborate_All (Broca.Refs);
pragma Elaborate_All (Broca.Server);
pragma Elaborate_All (Broca.POA);

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.RootPOA is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.rootpoa");
   procedure O is new Broca.Debug.Output (Flag);

   ---------------------------------------
   -- An implementation of a POAManager --
   ---------------------------------------

   package POA_Vararray is new Broca.Vararray
     (Element      => Broca.POA.Ref,
      Null_Element => Broca.POA.Nil_Ref,
      Index_Type   => Natural);

   protected type State_Type is
      function Get_State return Processing_State_Type;
      procedure Set_State (State : in Processing_State_Type);
      entry Wait_For_Completion;
      procedure Inc_Usage_If_Active (State : out Processing_State_Type);
      procedure Dec_Usage;
      procedure Inc_Usage;
   private
      Nbr_Usage : Natural := 0;
      Current_State : Processing_State_Type := Holding;
   end State_Type;

   protected body State_Type is
      function Get_State return Processing_State_Type is
      begin
         return Current_State;
      end Get_State;

      procedure Set_State (State : Processing_State_Type) is
      begin
         Current_State := State;
      end Set_State;

      entry Wait_For_Completion
      when Nbr_Usage = 0 or else Current_State = Active is
      begin
         null;
      end Wait_For_Completion;

      procedure Inc_Usage_If_Active (State : out Processing_State_Type) is
      begin
         State := Current_State;
         if Current_State = Active then
            Nbr_Usage := Nbr_Usage + 1;
         end if;
      end Inc_Usage_If_Active;

      procedure Dec_Usage is
      begin
         Nbr_Usage := Nbr_Usage - 1;
      end Dec_Usage;

      procedure Inc_Usage is
      begin
         Nbr_Usage := Nbr_Usage + 1;
      end Inc_Usage;
   end State_Type;

   type POA_Manager_Type is new Broca.POA.POAManager_Object with
      record
         --  9.3.2  Processing States
         --  A POA manager is created in the holding state.
         State : State_Type;

         POAs : POA_Vararray.Var_Array_Type := POA_Vararray.Null_Var_Array;
      end record;

   procedure Activate
     (Self : in out POA_Manager_Type);

   procedure Hold_Requests
     (Self                : in out POA_Manager_Type;
      Wait_For_Completion : in CORBA.Boolean);

   procedure Discard_Requests
     (Self                : in out POA_Manager_Type;
      Wait_For_Completion : in CORBA.Boolean);

   procedure Deactivate
     (Self                : in out POA_Manager_Type;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean);

   procedure Register
     (Self  : in out POA_Manager_Type;
      A_POA : Broca.POA.Ref'Class);

   procedure Unregister
     (Self  : in out POA_Manager_Type;
      A_POA : Broca.POA.Ref'Class);

   procedure Inc_Usage_If_Active
     (Self  : in out POA_Manager_Type;
      State : out Processing_State_Type);

   procedure Dec_Usage
     (Self : in out POA_Manager_Type);

   procedure Inc_Usage
     (Self : in out POA_Manager_Type);

   function Is_Inactive
     (Self : in POA_Manager_Type)
     return Boolean;

   procedure State_Changed_Iterator
     (El  : in Broca.POA.Ref;
      Arg : in Boolean);

   procedure Etherealize_Iterator
     (El  : in Broca.POA.Ref;
      Arg : in Boolean);

   procedure State_Changed_Iterator
     (El : Broca.POA.Ref; Arg : Boolean) is
   begin
      --  Avoid a warning.
      if Arg then
         null;
      end if;
      Broca.ORB.POA_State_Changed (El);
   end State_Changed_Iterator;

   procedure Etherealize_Iterator
     (El : Broca.POA.Ref;
      Arg : Boolean) is
   begin
      --  Avoid a warning.
      if Arg then
         null;
      end if;

      declare
         O : constant Broca.POA.POA_Object_Ptr
           := POA_Object_Of (El);
      begin
         if O.Servant_Policy = RETAIN
           and then O.Request_Policy = USE_SERVANT_MANAGER
         then
            Deactivate (O);
         end if;
      end;
   end Etherealize_Iterator;

   procedure Activate (Self : in out POA_Manager_Type) is
   begin
      Self.State.Set_State (Active);
      POA_Vararray.Iterate
        (Self.POAs, State_Changed_Iterator'Access, False);
   end Activate;

   procedure Hold_Requests (Self : in out POA_Manager_Type;
                            Wait_For_Completion : CORBA.Boolean) is
   begin
      Self.State.Set_State (Holding);
      if Wait_For_Completion then
         Self.State.Wait_For_Completion;
      end if;
   end Hold_Requests;

   procedure Discard_Requests (Self : in out POA_Manager_Type;
                               Wait_For_Completion : CORBA.Boolean) is
   begin
      Self.State.Set_State (Discarding);
   end Discard_Requests;

   procedure Deactivate
     (Self : in out POA_Manager_Type;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean) is
   begin
      Self.State.Set_State (Inactive);

      --  This flushes the queue.  Can be done as soon as now, since
      --  requests will be discarded.
      POA_Vararray.Iterate
        (Self.POAs, State_Changed_Iterator'Access, False);

      if Etherealize_Objects then
         POA_Vararray.Iterate (Self.POAs, Etherealize_Iterator'Access, False);
      end if;

      if Wait_For_Completion then
         Self.State.Wait_For_Completion;
      end if;
   end Deactivate;

   procedure Register
     (Self : in out POA_Manager_Type;
      A_POA : Broca.POA.Ref'Class) is
   begin
      POA_Vararray.Insert (Self.POAs, Broca.POA.Ref (A_POA));
   end Register;

   procedure Unregister
     (Self : in out POA_Manager_Type;
      A_POA : Broca.POA.Ref'Class) is
   begin
      POA_Vararray.Remove (Self.POAs, Broca.POA.Ref (A_POA));
   end Unregister;

   procedure Inc_Usage_If_Active (Self : in out POA_Manager_Type;
                                  State : out Processing_State_Type) is
   begin
      Self.State.Inc_Usage_If_Active (State);
   end Inc_Usage_If_Active;

   procedure Inc_Usage (Self : in out POA_Manager_Type) is
   begin
      Self.State.Inc_Usage;
   end Inc_Usage;

   procedure Dec_Usage (Self : in out POA_Manager_Type) is
   begin
      Self.State.Dec_Usage;
   end Dec_Usage;

   function Is_Inactive (Self : in POA_Manager_Type) return Boolean is
   begin
      return Self.State.Get_State = Inactive;
   end Is_Inactive;

   type POA_Manager_Ptr is access all POA_Manager_Type;

   --  The default POA manager is the POAManager used by the RootPOA, and
   --  created during the elaboration.
   Default_POA_Manager : POA_Manager_Ptr;

   --  The ghost POA manager is used by ghost POA, ie poa created by
   --  find_POA before calling an AdapterActivator.
   Ghost_POA_Manager : POAManager_Object_Ptr;

   --  FIXME: These should be Refs.

   --------------------------------
   -- An implementation of a POA --
   --------------------------------

   package PSSM renames PortableServer.ServantManager;
   package PSSA renames PortableServer.ServantActivator;
   package PSSL renames PortableServer.ServantLocator;

   type Object_Map_Entry_State is (Free, Reserved, Active, To_Be_Destroyed);
   type Rw_Lock_Ptr is access Broca.Locks.Rw_Lock_Type;

   type Object_Map_Entry is record
      State : Object_Map_Entry_State := Free;
      --  State of the entry.

      Skeleton : Broca.POA.Skeleton_Ptr;
      --  SKELETON must not be null when state is active.

      Date : Natural := 0;
      --  Date.
      --  Changed each time the entry becomes free, so that dangling IOR
      --  are not valid.

      Requests_Lock : Rw_Lock_Ptr := null;
      --  Number of requests to this object.  Used for etherealize.
   end record;

   type Slot_Index is new CORBA.Unsigned_Long;
   Bad_Slot : constant Slot_Index := -1;

   type Object_Map_Type is array (Slot_Index range <>)
     of Object_Map_Entry;
   type Object_Map_Ptr is access Object_Map_Type;

   type Object is new Broca.POA.POA_Object with record
      Servant_Lock : Broca.Locks.Mutex_Type;
      --  Lock for serialization of requests to incarnate/etherealize.

      Requests_Lock : Broca.Locks.Rw_Lock_Type;
      --  Number of current requests.
      --  The lock is used to count the number of requests (R) or to prevent
      --  any new requests (W).
      --  This is used only for single_thread_model policy.

      Object_Map : Object_Map_Ptr := null;
      --  The object map.
      --  It is valid only if the servant retention policy is RETAIN.

      Last_Slot : Slot_Index := Bad_Slot;
      --  Only used if NON_RETAIN and SYSTEM_ID to allocate uniq oid.

      Map_Lock : Broca.Locks.Mutex_Type;
      --  The map can always be read, but protected against multiple
      --  write accesses by the lock.
      --  FIXME: this is a kludge.
      --  To be true, OBJECT_MAP must be atomic, but it isn't since it is
      --  a fat pointer.
   end record;

   type Object_Ptr is access all Object;

   function Activate_Object
     (Self : access Object;
      P_Servant : PortableServer.Servant)
     return PortableServer.ObjectId;

   procedure Activate_Object_With_Id
     (Self : access Object;
      Oid : ObjectId;
      P_Servant : PortableServer.Servant);

   function Create_Reference
     (Self : access Object;
      Intf : CORBA.RepositoryId)
     return CORBA.Object.Ref;

   function Create_Reference_With_Id
     (Self : access Object;
      Oid : ObjectId;
      Intf : CORBA.RepositoryId)
     return CORBA.Object.Ref;

   function Skeleton_To_Servant
     (Self : access Object;
      Skeleton : Broca.POA.Skeleton_Ptr)
     return Servant;

   procedure Deactivate_Object
     (Self : access Object;
      Oid : ObjectId);

   procedure Deactivate
     (Self : access Object);

   procedure GIOP_Invoke
     (Self       : access Object;
      Key        : access Encapsulation;
      Operation  : CORBA.Identifier;
      Request_Id : CORBA.Unsigned_Long;
      Response_Expected : CORBA.Boolean;
      Message    : access Buffer_Type;
      Reply      : access Buffer_Type);

   function Create_POA
     (Self          : access Object;
      Adapter_Name  : CORBA.String;
      A_POAManager  : POAManager_Object_Ptr;
      Thread_Policy : ThreadPolicyValue;
      Lifespan_Policy : LifespanPolicyValue;
      Uniqueness_Policy : IdUniquenessPolicyValue;
      Id_Assign_Policy : IdAssignmentPolicyValue;
      Activation_Policy : ImplicitActivationPolicyValue;
      Servant_Policy : ServantRetentionPolicyValue;
      Request_Policy : RequestProcessingPolicyValue)
     return POA_Object_Ptr;

   function Find_POA
     (Self         : access Object;
      Adapter_Name : CORBA.String;
      Activate_It  : CORBA.Boolean)
     return Broca.POA.Ref'Class;

   procedure Destroy_POA (Self : access Object;
                          Etherealize_Objects : CORBA.Boolean;
                          Wait_For_Completion : CORBA.Boolean);

   procedure Cleanup (Self : access Object);

   function Servant_To_Skeleton
     (Self                             : access Object;
      P_Servant                        : Servant;
      Called_From_Servant_To_Reference : Boolean := False)
     return Broca.POA.Skeleton_Ptr;

   function Id_To_Skeleton
     (Self : access Object; Oid : ObjectId)
     return Skeleton_Ptr;

   function Key_To_Skeleton
     (Self : access Object;
      Key  : Object_Key_Ptr)
     return Skeleton_Ptr;

   function Slot_Index_To_ObjectId
     (Slot : Slot_Index)
     return ObjectId;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Object_Map_Type, Name => Object_Map_Ptr);

   function Slot_By_Object_Id
     (Self : access Object'Class;
      Oid  : ObjectId)
     return Slot_Index;

   function Slot_By_Servant
     (Self : access Object'Class;
      P_Servant : Servant)
     return Slot_Index;

   function Slot_By_Skeleton
     (Self : access Object'Class;
      Skeleton : Broca.POA.Skeleton_Ptr)
      return Slot_Index;

   --  Return how many times the servant was activated.
   function Nbr_Slots_For_Servant
     (Self : access Object'Class;
      P_Servant : Servant)
     return Natural;

   function Reserve_A_Slot
     (Self : access Object'Class)
     return Slot_Index;

   procedure Build_Key_For_ObjectId
     (Buffer : access Buffer_Type;
      Oid : ObjectId);

   function Clean_Slot
     (Self : access Object'Class;
      Slot : Slot_Index)
     return Boolean;
   procedure Unlink_POA (Self : POA_Object_Ptr);

   --  Find a skeleton to be destroyed
   function Get_Slot_To_Destroy
     (Self : access Object'Class)
     return Slot_Index;

   procedure Set_Cleanup_Call_Back
     (Self : access Object'Class);

   function Nbr_Slots_For_Servant
     (Self : access Object'Class;
      P_Servant : Servant)
     return Natural
   is
      Res : Natural := 0;
   begin
      if Self.Object_Map /= null then
         for I in Self.Object_Map.all'Range loop
            if Self.Object_Map (I).Skeleton /= null
              and then Self.Object_Map (I).Skeleton.P_Servant = P_Servant
            then
               Res := Res + 1;
            end if;
         end loop;
      end if;
      return Res;
   end Nbr_Slots_For_Servant;

   function To_POA_Ref
     (The_POA : access Object'Class)
     return Broca.POA.Ref;
   --  Return a reference to the given POA object.

   function To_POA_Ref
     (The_POA : access Object'Class)
     return Broca.POA.Ref is
   begin
      return To_POA_Ref (Broca.POA.POA_Object_Ptr (The_POA));
   end To_POA_Ref;

   -----------------------
   -- Slot_By_Object_Id --
   -----------------------

   function Slot_By_Object_Id
     (Self : access Object'Class;
      Oid  : ObjectId)
     return Slot_Index is
   begin
      if Self.Object_Map /= null then
         for I in Self.Object_Map.all'Range loop
            if Self.Object_Map (I).Skeleton /= null
              and then Self.Object_Map (I).Skeleton.Object_Id = Oid
            then
               return I;
            end if;
         end loop;
      end if;
      return Bad_Slot;
   end Slot_By_Object_Id;

   ---------------------
   -- Slot_By_Servant --
   ---------------------

   function Slot_By_Servant
     (Self      : access Object'Class;
      P_Servant : Servant)
     return Slot_Index is
   begin
      if Self.Object_Map /= null then
         for I in Self.Object_Map.all'Range loop
            if Self.Object_Map (I).Skeleton /= null
              and then Self.Object_Map (I).Skeleton.P_Servant = P_Servant
            then
               return I;
            end if;
         end loop;
      end if;
      return Bad_Slot;
   end Slot_By_Servant;

   ----------------------
   -- Slot_By_Skeleton --
   ----------------------

   function Slot_By_Skeleton
     (Self : access Object'Class;
      Skeleton : Broca.POA.Skeleton_Ptr)
      return Slot_Index is
   begin
      if Self.Object_Map /= null then
         for I in Self.Object_Map.all'Range loop
            if Self.Object_Map (I).Skeleton = Skeleton then
               return I;
            end if;
         end loop;
      end if;
      return Bad_Slot;
   end Slot_By_Skeleton;

   --------------------
   -- Reserve_A_Slot --
   --------------------

   function Reserve_A_Slot
     (Self : access Object'Class)
     return Slot_Index
   is
      Slot           : Slot_Index;
      Found          : Boolean;
   begin
      Self.Map_Lock.Lock;

      case Self.Servant_Policy is
         when NON_RETAIN =>
            Slot := Self.Last_Slot + 1;
            Self.Last_Slot := Slot;

         when RETAIN =>
            --  Find a slot.
            if Self.Object_Map = null then
               pragma Debug (O ("Allocated inital active object map."));
               Self.Object_Map := new Object_Map_Type (1 .. 8);

               for I in Self.Object_Map.all'Range loop
                  Self.Object_Map (I).Requests_Lock
                    := new Broca.Locks.Rw_Lock_Type;
               end loop;

               Slot := 1;
            else
               Found := False;
               for I in Self.Object_Map.all'Range loop
                  if Self.Object_Map (I).State = Free then
                     Slot  := I;
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  declare
                     --  FIXME: Memory leak.
                     --  The object map is never shrunk.
                     New_Object_Map : constant Object_Map_Ptr
                       := new Object_Map_Type
                       (1 .. 2 * Self.Object_Map.all'Last);
                  begin
                     Slot := Self.Object_Map.all'Last + 1;
                     New_Object_Map (Self.Object_Map.all'Range)
                       := Self.Object_Map (Self.Object_Map.all'Range);
                     for I in Self.Object_Map.all'Last + 1
                       .. New_Object_Map.all'Last loop
                        New_Object_Map (I).Requests_Lock
                          := new Broca.Locks.Rw_Lock_Type;
                     end loop;
                     Free (Self.Object_Map);
                     Self.Object_Map := New_Object_Map;
                  end;
               end if;
            end if;
            Self.Object_Map (Slot).State := Reserved;
      end case;

      Self.Map_Lock.Unlock;
      return Slot;
   end Reserve_A_Slot;

   procedure Build_Key_For_Slot
     (Buffer : access Buffer_Type;
      Self : access Object; Slot : Slot_Index);

   function Key_To_Slot
     (Self : access Object;
      Key : access Buffer_Type)
     return Slot_Index;

   function Key_To_ObjectId
     (Key : access Buffer_Type)
     return ObjectId;

   --  Possible only if RETAIN policy.
   procedure Build_Key_For_Slot
     (Buffer : access Buffer_Type;
      Self : access Object; Slot : Slot_Index)
   is
      use Broca.CDR;

   begin
      if Self.Lifespan_Policy = PERSISTENT then
         Marshall (Buffer, Broca.Flags.Boot_Time);
      end if;

      Marshall (Buffer, CORBA.Unsigned_Long (Slot));
      Marshall (Buffer, CORBA.Unsigned_Long
                (Self.Object_Map (Slot).Date));

      if Self.Lifespan_Policy = PERSISTENT then
         PortableServer.Marshall
           (Buffer, Self.Object_Map (Slot).Skeleton.Object_Id);
      end if;

   end Build_Key_For_Slot;

   function Key_To_Slot
     (Self : access Object;
      Key : access Buffer_Type)
     return Slot_Index
   is
      use Broca.CDR;

   begin
      if Self.Lifespan_Policy = PERSISTENT then
         return Slot_By_Object_Id
           (Self, Unmarshall (Key));
      end if;

      declare
         Res  : constant Slot_Index
           := Slot_Index
           (CORBA.Unsigned_Long'(Unmarshall (Key)));
         Date : constant CORBA.Unsigned_Long
           := Unmarshall (Key);
      begin
         if Self.Object_Map /= null
           and then Res in Self.Object_Map.all'Range
           and then Self.Object_Map (Res).Date = Natural (Date)
           and then Self.Object_Map (Res).State = Active
         then
            return Res;
         end if;
      end;

      --  The object does not exist in the active object map.
      return Bad_Slot;
   end Key_To_Slot;

   --  Possible only if NON_RETAIN policy.
   procedure Build_Key_For_ObjectId
     (Buffer : access Buffer_Type;
      Oid : ObjectId) is
   begin
      Marshall (Buffer, Oid);
   end Build_Key_For_ObjectId;

   function Key_To_ObjectId
     (Key : access Buffer_Type)
     return ObjectId is
   begin
      return Unmarshall (Key);
   end Key_To_ObjectId;

   --  if SELF has NON_RETAIN policy, SLOT is not used.
   function Create_Skeleton
     (Self      : access Object;
      Slot      : Slot_Index;
      P_Servant : PortableServer.Servant;
      Type_Id   : CORBA.RepositoryId;
      Oid       : ObjectId)
     return Broca.POA.Skeleton_Ptr;

   function Create_Skeleton
     (Self      : access Object;
      Slot      : Slot_Index;
      P_Servant : PortableServer.Servant;
      Type_Id   : CORBA.RepositoryId;
      Oid       : ObjectId)
     return Broca.POA.Skeleton_Ptr
   is
      Key : aliased Buffer_Type;
      Obj : Broca.POA.Skeleton_Ptr;

   begin
      Obj := new Broca.POA.Skeleton;
      Obj.Type_Id := Type_Id;
      Obj.P_Servant := P_Servant;
      Obj.Object_Id := Oid;

      Obj.POA := To_POA_Ref (Self);

      Broca.CDR.Start_Encapsulation (Key'Access);

      --  The servant is now active
      if Self.Servant_Policy = RETAIN then
         Self.Object_Map (Slot).Skeleton := Obj;
         Self.Object_Map (Slot).State    := Active;
         Build_Key_For_Slot (Key'Access, Self, Slot);
      else
         Build_Key_For_ObjectId (Key'Access, Oid);
      end if;

      --  FIXME: Memory leak. This allocation
      --    should be freed when the skeleton
      --    is not in use anymore.
      Obj.Object_Key := new Encapsulation'
        (Encapsulate (Key'Access));

      Release (Key);

      pragma Debug (O ("ObjectId created"));

      return Obj;
   end Create_Skeleton;

   function Activate_Object
     (Self : access Object;
      P_Servant : PortableServer.Servant)
     return PortableServer.ObjectId
   is
      Slot : Slot_Index;
      Oid : ObjectId;
      Obj : Broca.POA.Skeleton_Ptr;
   begin
      if Self.Uniqueness_Policy = UNIQUE_ID
        and then Slot_By_Servant (Self, P_Servant) /= Bad_Slot
      then
         raise PortableServer.POA.ServantAlreadyActive;
      end if;

      Slot := Reserve_A_Slot (Self);

      Oid := Slot_Index_To_ObjectId (Slot);
      Obj := Create_Skeleton
        (Self, Slot, P_Servant, Get_Type_Id (P_Servant), Oid);
      return Oid;
   end Activate_Object;

   procedure Activate_Object_With_Id
     (Self      : access Object;
      Oid       : in ObjectId;
      P_Servant : in PortableServer.Servant)
   is
      Slot : Slot_Index;
      Obj : Broca.POA.Skeleton_Ptr;

   begin
      Slot := Slot_By_Object_Id (Self, Oid);
      if Slot = Bad_Slot then
         if Self.Id_Assign_Policy = SYSTEM_ID then
            Broca.Exceptions.Raise_Bad_Param;
         end if;
      elsif Self.Object_Map (Slot).State = Active then
         raise PortableServer.POA.ObjectAlreadyActive;
      end if;
      if Self.Uniqueness_Policy = UNIQUE_ID
        and then Slot_By_Servant (Self, P_Servant) /= Bad_Slot
      then
         raise PortableServer.POA.ServantAlreadyActive;
      end if;

      if Slot = Bad_Slot then
         Slot := Reserve_A_Slot (Self);
      end if;

      Obj := Create_Skeleton
        (Self, Slot, P_Servant, Get_Type_Id (P_Servant), Oid);
   end Activate_Object_With_Id;

   function Create_Reference
     (Self : access Object;
      Intf : CORBA.RepositoryId)
     return CORBA.Object.Ref is
   begin
      return Create_Reference_With_Id
        (Self,
         Slot_Index_To_ObjectId (Reserve_A_Slot (Self)),
         Intf);
   end Create_Reference;

   function Create_Reference_With_Id
     (Self : access Object;
      Oid : ObjectId;
      Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref
   is
      Res : CORBA.Object.Ref;
      Slot : Slot_Index;

   begin
      if Self.Servant_Policy = RETAIN then
         Slot := Slot_By_Object_Id (Self, Oid);
         if Self.Id_Assign_Policy = SYSTEM_ID then
            if Slot_By_Object_Id (Self, Oid) = Bad_Slot then
               Broca.Exceptions.Raise_Bad_Param;
            end if;
         end if;
      else
         Slot := Bad_Slot;
      end if;

      CORBA.Object.Set
        (Res,
         CORBA.Impl.Object_Ptr
         (Create_Skeleton (Self, Slot, null, Intf, Oid)));

      return Res;
   end Create_Reference_With_Id;

   function Servant_To_Skeleton
     (Self                             : access Object;
      P_Servant                        : Servant;
      Called_From_Servant_To_Reference : Boolean := False)
     return Broca.POA.Skeleton_Ptr
   is
      Slot : Slot_Index;
      Obj  : Broca.POA.Skeleton_Ptr;
   begin
      if Self.Servant_Policy = RETAIN
        and then Self.Uniqueness_Policy = UNIQUE_ID
      then
         Slot := Slot_By_Servant (Self, P_Servant);

         if Slot /= Bad_Slot then
            return Self.Object_Map (Slot).Skeleton;
         end if;
      end if;

      if Self.Servant_Policy = RETAIN
        and then Self.Activation_Policy = IMPLICIT_ACTIVATION
        and then (Self.Uniqueness_Policy = MULTIPLE_ID
                  or else Nbr_Slots_For_Servant (Self, P_Servant) = 0)
      then
         Slot := Reserve_A_Slot (Self);

         Obj := Create_Skeleton
           (Self, Slot, P_Servant, Get_Type_Id (P_Servant),
            Slot_Index_To_ObjectId (Slot));

         return Self.Object_Map (Slot).Skeleton;
      end if;

      if Called_From_Servant_To_Reference
        or else Self.Request_Policy = USE_DEFAULT_SERVANT
      then
         return Id_To_Skeleton (Self, Task_Attributes.Current_Object);
      end if;

      raise PortableServer.POA.ServantNotActive;
   end Servant_To_Skeleton;

   function Skeleton_To_Servant
     (Self : access Object;
      Skeleton : Broca.POA.Skeleton_Ptr)
     return Servant
   is
      Slot : Slot_Index;
   begin
      if Self.Servant_Policy = RETAIN then
         Slot := Slot_By_Skeleton (Self, Skeleton);
         if Slot /= Bad_Slot then
            return Skeleton.P_Servant;
         end if;
      end if;
      if Self.Request_Policy = USE_DEFAULT_SERVANT
        and then Self.Default_Servant /= null
      then
         return Self.Default_Servant;
      end if;
      raise PortableServer.POA.ObjectNotActive;
   end Skeleton_To_Servant;

   function Id_To_Skeleton
     (Self : access Object;
      Oid : ObjectId)
     return Skeleton_Ptr
   is
      Slot : Slot_Index;
   begin
      Slot := Slot_By_Object_Id (Self, Oid);
      if Slot = Bad_Slot then
         raise PortableServer.POA.ObjectNotActive;
      end if;

      return Self.Object_Map (Slot).Skeleton;
   end Id_To_Skeleton;

   function Key_To_Skeleton
     (Self : access Object;
      Key  : Object_Key_Ptr)
     return Skeleton_Ptr
   is
      use Broca.Opaque;

      Slot : Slot_Index;

   begin
      if Self.Object_Map /= null then
         for I in Self.Object_Map'Range loop
            if Self.Object_Map (I).Skeleton /= null
              and then Self.Object_Map (I).Skeleton.Object_Key.all = Key.all
            then
               Slot := I;
               exit;
            end if;
         end loop;
      end if;
      if Slot = Bad_Slot then
         raise PortableServer.POA.ObjectNotActive;
      end if;

      return Self.Object_Map (Slot).Skeleton;
   end Key_To_Skeleton;

   subtype Slot_Index_Data is
     IDL_SEQUENCE_Octet.Element_Array
       (0 .. (Slot_Index'Size + CORBA.Octet'Size - 1) / CORBA.Octet'Size - 1);

   function To_SI_Data is
      new Ada.Unchecked_Conversion (Slot_Index, Slot_Index_Data);

   function Slot_Index_To_ObjectId
     (Slot : Slot_Index)
     return ObjectId is
   begin
      return To_Sequence (To_SI_Data (Slot));
   end Slot_Index_To_ObjectId;

   function Get_Slot_To_Destroy
     (Self : access Object'Class)
     return Slot_Index is
   begin
      Self.Map_Lock.Lock;
      if Self.Object_Map /= null then
         for I in Self.Object_Map.all'Range loop
            if Self.Object_Map (I).State = To_Be_Destroyed then
               Self.Object_Map (I).State := Reserved;
               Self.Map_Lock.Unlock;
               return I;
            end if;
         end loop;
      end if;
      Self.Map_Lock.Unlock;
      return Bad_Slot;
   end Get_Slot_To_Destroy;

   --  Try to free a slot.
   --  Return true if cleanup should be called.
   --  The Object Map must be locked.
   function Clean_Slot
     (Self : access Object'Class; Slot : Slot_Index)
     return Boolean is
   begin
      if not PSSM.Is_Nil (Self.Servant_Manager)
        and then Self.Object_Map (Slot).State = Active
      then
         Self.Object_Map (Slot).State := To_Be_Destroyed;
         return True;
      else
         Self.Object_Map (Slot).State := Reserved;
         Self.Object_Map (Slot).Date := Self.Object_Map (Slot).Date + 1;
         Self.Object_Map (Slot).Skeleton := null;
         Self.Object_Map (Slot).State := Free;
         return False;
      end if;
   end Clean_Slot;

   --  FIXME:
   --  procedure Cleanup seems to be written with the assumption that
   --  Self.Servant_Manager is always a ServantActivator, and never
   --  a ServantLocator. Why is it so?
   --     Thomas, 2000-05-27.

   procedure Cleanup (Self : access Object)
   is
      Slot : Slot_Index;
      Servant_Manager : constant PSSA.Impl.Object_Ptr
        := PSSA.Impl.Object_Ptr
        (PSSM.Object_Of (Self.Servant_Manager));

      --  FIXME: Constraint_Error will be raised if
      --    Self.Servant_Manager is not a PS.ServantActivator.

      A_Servant : PortableServer.Servant;
      A_POA : PortableServer.POA.Ref;
      Is_Cleanup : Boolean;

   begin
      Is_Cleanup := False
        or else Self.POA_Manager = null
        or else Broca.POA.Is_Inactive (Self.POA_Manager.all);

      if not PSSM.Is_Nil (Self.Servant_Manager) then
         PortableServer.POA.Set
           (A_POA, CORBA.Impl.Object_Ptr (Self));

         loop
            Slot := Get_Slot_To_Destroy (Self);
            exit when Slot = Bad_Slot;

            A_Servant := Self.Object_Map (Slot).Skeleton.P_Servant;

            Self.Object_Map (Slot).Requests_Lock.Lock_W;
            --  Wait for completions on all outstanding requests before
            --  etherealize the object.

            Self.Servant_Lock.Lock;
            --  Serialization of calls to incarnate/etherealize.

            begin
               PSSA.Impl.Etherealize
                 (Servant_Manager.all,
                  Self.Object_Map (Slot).Skeleton.Object_Id,
                  PortableServer.POA.Convert.To_Forward (A_POA),
                  A_Servant,
                  Is_Cleanup,
                  Nbr_Slots_For_Servant (Self, A_Servant) > 1);

               Self.Servant_Lock.Unlock;
            exception
               when others =>
                  Self.Servant_Lock.Unlock;
                  raise;
            end;

            if Clean_Slot (Self, Slot) then
               Broca.Exceptions.Raise_Internal (614);
            end if;

         end loop;
      end if;

      if Self.POA_Manager /= null then
         Broca.POA.Dec_Usage (Self.POA_Manager.all);
      end if;

      --  FIXME: Should ensure that the object pointed by A_Ref
      --     is destroyed.
   end Cleanup;

   procedure Set_Cleanup_Call_Back (Self : access Object'Class) is
   begin
      if Self.POA_Manager /= null then
         Broca.POA.Inc_Usage (Self.POA_Manager.all);
      end if;
      Broca.Server.Request_Cleanup (To_POA_Ref (Self));
   end Set_Cleanup_Call_Back;

   --  Called by the poa manager.
   procedure Deactivate (Self : access Object)
   is
      To_Clean : Boolean;
   begin
      Self.Map_Lock.Lock;
      if Self.Object_Map /= null then
         To_Clean := False;
         for I in Self.Object_Map.all'Range loop
            if Self.Object_Map (I).State = Active then
               To_Clean := Clean_Slot (Self, I) or To_Clean;
            end if;
         end loop;
         if To_Clean then
            Broca.Server.Request_Cleanup (To_POA_Ref (Self));
         end if;
      end if;
      Self.Map_Lock.Unlock;
   end Deactivate;

   procedure Deactivate_Object
     (Self : access Object; Oid : ObjectId)
   is
      Slot : Slot_Index;

   begin
      Slot := Slot_By_Object_Id (Self, Oid);
      if Slot = Bad_Slot then
         raise PortableServer.POA.ObjectNotActive;
      end if;
      if Clean_Slot (Self, Slot) then
         Broca.Server.Request_Cleanup (To_POA_Ref (Self));
      end if;
   end Deactivate_Object;

   procedure GIOP_Invoke
     (Self       : access Object;
      Key        : access Encapsulation;
      Operation  : CORBA.Identifier;
      Request_Id : CORBA.Unsigned_Long;
      Response_Expected : CORBA.Boolean;
      Message    : access Buffer_Type;
      Reply      : access Buffer_Type)
   is
      Slot            : Slot_Index;
      A_Servant       : Servant := null;
      Servant_Manager : constant PSSM.Impl.Object_Ptr
        := PSSM.Impl.Object_Ptr
        (PSSM.Object_Of (Self.Servant_Manager));
      A_POA           : PortableServer.POA.Ref;
      Oid             : ObjectId;
      The_Cookie      : PSSL.Cookie;
      Key_Buffer      : aliased Buffer_Type;
      Need_Postinvoke : Boolean := False;
   begin
      pragma Debug (O ("GIOP_Invoke: enter"));
      --  See 9.3.7
      Self.Requests_Lock.Lock_R;
      pragma Debug (O ("GIOP_Invoke: Got Read lock on request."));
      pragma Debug (O ("GIOP_Invoke: Servant Policy is "
                       & Self.Servant_Policy'Img));

      Decapsulate (Key, Key_Buffer'Access);

      --  Find the ObjectId in the Active Map if RETAIN Policy.
      if Self.Servant_Policy = RETAIN then
         Slot := Key_To_Slot (Self, Key_Buffer'Access);
         if Slot /= Bad_Slot then
            A_Servant := Self.Object_Map (Slot).Skeleton.P_Servant;
         end if;
      end if;

      PortableServer.POA.Set
        (A_POA, Broca.Refs.Ref_Ptr (Self));
      pragma Debug (O ("GIOP_Invoke: POA is set."));

      if A_Servant = null then
         pragma Debug (O ("GIOP_Invoke: A_Servant is null"));
         case Self.Request_Policy is
            when USE_ACTIVE_OBJECT_MAP_ONLY =>
               Release (Key_Buffer);
               pragma Debug
                 (O ("GIOP_invoke : USE_ACTIVE_OBJECT_MAP_ONLY policy"));
               Broca.Exceptions.Raise_Object_Not_Exist;

            when USE_DEFAULT_SERVANT =>
               Release (Key_Buffer);
               pragma Debug
                 (O ("GIOP_Invoke: USE_DEFAULT_SERVANT policy"));

               if Self.Default_Servant = null then
                  Broca.Exceptions.Raise_Obj_Adapter;
               end if;

               A_Servant := Self.Default_Servant;
               if Self.Servant_Policy = RETAIN
                 and then Slot /= Bad_Slot
               then
                  --  FIXME: persistent
                  Self.Object_Map (Slot).Skeleton.P_Servant := A_Servant;
               end if;

            when USE_SERVANT_MANAGER =>
               pragma Debug (O ("GIOP_Invoke: USE_SERVANT_MANAGER policy"));
               if PSSM.Is_Nil (Self.Servant_Manager) then
                  Release (Key_Buffer);
                  Broca.Exceptions.Raise_Obj_Adapter;
               end if;

               Oid := Key_To_ObjectId (Key_Buffer'Access);
               Release (Key_Buffer);
               if Self.Servant_Policy = RETAIN then
                  Release (Key_Buffer);
                  Self.Servant_Lock.Lock;
                  begin
                     PSSA.Impl.Incarnate
                       (PSSA.Impl.Object'Class
                        (Servant_Manager.all),
                        Oid,
                        A_POA,
                        A_Servant);
                     --  FIXME: Constraint_Error will be raised if
                     --     Self.Servant_Manager is not a PS.ServantActivator
                     Self.Servant_Lock.Unlock;
                  exception
                     when others =>
                        Self.Servant_Lock.Unlock;
                        raise;
                  end;
                  if Self.Uniqueness_Policy = UNIQUE_ID
                    and then Nbr_Slots_For_Servant (Self, A_Servant) > 0
                  then
                     Broca.Exceptions.Raise_Obj_Adapter;
                  end if;
                  if Slot = Bad_Slot then
                     --  FIXME: persistent.
                     Broca.Exceptions.Raise_Internal (615);
                  end if;

                  Self.Object_Map (Slot).Skeleton.P_Servant := A_Servant;
               else
                  PSSL.Impl.Preinvoke
                    (PSSL.Impl.Object'Class
                     (Servant_Manager.all),
                     Oid, A_POA, Operation, The_Cookie, A_Servant);
                  --  FIXME: Constraint_Error will be raised if
                  --     Self.Servant_Manager is not a PS.ServantLocator.

                  Need_Postinvoke := True;
               end if;
         end case;
      else
         Release (Key_Buffer);
      end if;

      begin
         if Self.Servant_Policy = RETAIN then
            pragma Debug (O ("GIOP_Invoke: RETAIN policy, Slot = "
                             & Slot'Img));
            Self.Object_Map (Slot).Requests_Lock.Lock_R;
            pragma Debug (O ("GIOP_Invoke: Got Read lock on request (2)."));
         end if;

--           pragma Debug (O ("GIOP_Invoke: Preparing POA_Task_Attributes."));
--           declare
--              My_Task_Attributes : constant POA_Task_Attribute
--                := (Oid, PortableServer.POA.Convert.To_Forward (A_POA));
--           begin
--              pragma Debug (O ("GIOP_Invoke: Setting POA_Task_Attributes."));
--              --  Attributes.Set_Value (My_Task_Attributes);
--              pragma Debug (O ("GIOP_Invoke: Did set POA_Task_Attributes."));
--           end;
--

         Task_Attributes.Set_Current_Object (Oid);
         Task_Attributes.Set_Current_POA
           (PortableServer.POA.Convert.To_Forward (A_POA));
         Task_Attributes.Set_Has_Context;

         begin
            pragma Debug
              (O ("GIOP_Invoke: call giop_dispatch for " &
                  CORBA.To_Standard_String (Operation)));
            pragma Debug
              (O ("GIOP_Invoke: call giop_dispatch with A_Servant " &
                  Ada.Tags.External_Tag (A_Servant.all'Tag)));
            GIOP_Dispatch
              (A_Servant, CORBA.To_Standard_String (Operation), Request_Id,
               Response_Expected, Message, Reply);
            pragma Debug (O ("GIOP_Invoke: giop_dispatch returned"));
         exception
            when E : others =>
               pragma Debug (O ("GIOP_Invoke: system exception " &
                                Ada.Exceptions.Exception_Name (E)));
               if Response_Expected then
                  Broca.CDR.Marshall
                    (Reply, CORBA.Unsigned_Long (Broca.GIOP.No_Context));
                  Broca.CDR.Marshall (Reply, Request_Id);
                  Broca.GIOP.Marshall
                    (Reply, Broca.GIOP.System_Exception);
                  Broca.CDR.Marshall (Reply, E);
               end if;
         end;

         Task_Attributes.Set_Has_No_Context;

         if Self.Servant_Policy = RETAIN then
            Self.Object_Map (Slot).Requests_Lock.Unlock_R;
         end if;

         if Need_Postinvoke then
            PSSL.Impl.Postinvoke
              (PSSL.Impl.Object'Class
               (Servant_Manager.all),
               Oid, A_POA, Operation, The_Cookie, A_Servant);
         end if;

         Self.Requests_Lock.Unlock_R;
         POA_Manager_Ptr (Self.POA_Manager).State.Dec_Usage;

      exception
         when others =>
            if Self.Servant_Policy = RETAIN then
               Self.Object_Map (Slot).Requests_Lock.Unlock_R;
            end if;
            raise;
      end;

   exception
      when others =>
         Self.Requests_Lock.Unlock_R;
         POA_Manager_Ptr (Self.POA_Manager).State.Dec_Usage;
         raise;
   end GIOP_Invoke;

   function Create_POA
     (Self          : access Object;
      Adapter_Name  : CORBA.String;
      A_POAManager  : POAManager_Object_Ptr;
      Thread_Policy : ThreadPolicyValue;
      Lifespan_Policy : LifespanPolicyValue;
      Uniqueness_Policy : IdUniquenessPolicyValue;
      Id_Assign_Policy : IdAssignmentPolicyValue;
      Activation_Policy : ImplicitActivationPolicyValue;
      Servant_Policy : ServantRetentionPolicyValue;
      Request_Policy : RequestProcessingPolicyValue)
     return POA_Object_Ptr
   is
      use CORBA;
      Child : POA_Object_Ptr;
      Res : Object_Ptr;

   begin
      --  Fail if there is already a POA with the same name.
      Child := Self.Children;
      while Child /= null loop
         if Child.Name = Adapter_Name then
            exit when Child.POA_Manager = Ghost_POA_Manager;
            --  Failure
            raise PortableServer.POA.AdapterAlreadyExists;
         end if;
         Child := Child.Brother;
      end loop;

      if Child = null then
         Res := new Object;

         --  Link it.
         Res.Parent := POA_Object_Ptr (Self);
         Res.Brother := Self.Children;
         Self.Children := POA_Object_Ptr (Res);

         --  Internal data.
         Res.Name := Adapter_Name;
      else
         Res := Object_Ptr (Child);
      end if;

      --  Policies
      Res.Thread_Policy := Thread_Policy;
      Res.Lifespan_Policy := Lifespan_Policy;
      Res.Uniqueness_Policy := Uniqueness_Policy;
      Res.Id_Assign_Policy := Id_Assign_Policy;
      Res.Servant_Policy := Servant_Policy;
      Res.Request_Policy := Request_Policy;
      Res.Activation_Policy := Activation_Policy;
      if Thread_Policy = SINGLE_THREAD_MODEL then
         Res.Requests_Lock.Set_Max_Count (1);
      end if;

      --  9.3.2
      --  Unless an explicit POA manager object is provided at POA creation
      --  time, a POA manager is created when a POA is created and is
      --  automatically associed with that POA.
      if A_POAManager = null then
         Res.POA_Manager := A_POAManager;
      else
         Res.POA_Manager := new POA_Manager_Type;
      end if;
      Broca.Refs.Inc_Usage (Broca.Refs.Ref_Ptr (Res.POA_Manager));
      Register (Res.POA_Manager.all, To_POA_Ref (Res));

      Broca.Server.Register_POA (To_POA_Ref (Res));

      return POA_Object_Ptr (Res);
   end Create_POA;

   procedure Unlink_POA (Self : POA_Object_Ptr) is
      A_POA : POA_Object_Ptr;
   begin
      if Self.Parent /= null then
         A_POA := Self.Parent.Children;
         if A_POA = POA_Object_Ptr (Self) then
            Self.Parent.Children := Self.Brother;
         else
            while A_POA.Brother /= null loop
               if A_POA.Brother = POA_Object_Ptr (Self) then
                  A_POA.Brother := Self.Brother;
                  exit;
               end if;
            end loop;
         end if;
      end if;
   end Unlink_POA;

   procedure Destroy_POA
     (Self : access Object;
      Etherealize_Objects : CORBA.Boolean;
      Wait_For_Completion : CORBA.Boolean)
   is
      procedure Unregister_All (Self : POA_Object_Ptr);

      procedure Destroy_All (Self : POA_Object_Ptr);

      procedure Unregister_All (Self : POA_Object_Ptr)
      is
         Self_Ref : constant Broca.POA.Ref
           := To_POA_Ref (Self);
      begin
         if Self.Children /= null then
            Unregister_All (Self.Children);
         end if;
         if Self.Brother /= null then
            Unregister_All (Self.Brother);
         end if;

         Broca.Server.Unregister_POA (Self_Ref);
         Broca.ORB.POA_State_Changed (Self_Ref);

         Broca.Refs.Dec_Usage
           (Broca.Refs.Ref_Ptr (Self.POA_Manager));
         Self.POA_Manager := null;
      end Unregister_All;

      procedure Destroy_All (Self : POA_Object_Ptr) is
      begin
         if Self.Children /= null then
            Destroy_All (Self.Children);

            Broca.Refs.Dec_Usage (Broca.Refs.Ref_Ptr (Self.Children));
            Self.Children := null;
         end if;

         if Self.Brother /= null then
            Destroy_All (Self.Brother);

            Broca.Refs.Dec_Usage (Broca.Refs.Ref_Ptr (Self.Brother));
            Self.Brother := null;
         end if;

         PortableServer.AdapterActivator.Set
           (Self.Activator, CORBA.Impl.Object_Ptr'(null));
         Broca.Refs.Dec_Usage (Broca.Refs.Ref_Ptr (Self.POA_Manager));
         PSSM.Set (Self.Servant_Manager, CORBA.Impl.Object_Ptr'(null));
         if Etherealize_Objects and then Self.Servant_Policy = RETAIN then
            Deactivate (Self);
         end if;
         --  FIXME
         --          if Wait_For_Completion then
         --             null;
         --          end if;
      end Destroy_All;

      POA : POA_Object_Ptr := POA_Object_Ptr (Self);
   begin
      All_POAs_Lock.Lock_W;
      if POA.Parent /= null then
         POA_Object_Ptr (POA.Parent).Link_Lock.Lock_W;
         Unlink_POA (POA);
         POA_Object_Ptr (POA.Parent).Link_Lock.Unlock_W;
      end if;
      Unregister_All (POA_Object_Ptr (Self));
      --  Now, the POA is unknown by the server and unreachable.
      All_POAs_Lock.Unlock_W;
      Destroy_All (POA_Object_Ptr (Self));
   end Destroy_POA;

   function Find_POA
     (Self         : access Object;
      Adapter_Name : CORBA.String;
      Activate_It  : CORBA.Boolean)
     return Broca.POA.Ref'Class
   is
      use CORBA;
      Child : POA_Object_Ptr;
      Res : POA_Object_Ptr;
      POA_Ref : PortableServer.POA_Forward.Ref;
      Created : Boolean;
   begin
      --  Find the POA.
      << Again >> null;
      Child := Self.Children;
      while Child /= null loop
         if Child.Name = Adapter_Name then
            if Child.POA_Manager = Ghost_POA_Manager then
               --  The poa is under creation.
               Broca.Refs.Inc_Usage (Broca.Refs.Ref_Ptr (Child));
               All_POAs_Lock.Unlock_W;
               Child.Creation_Lock.Wait;
               All_POAs_Lock.Lock_W;
               Broca.Refs.Dec_Usage (Broca.Refs.Ref_Ptr (Child));
               goto Again;
            end if;
            return To_POA_Ref (Child);
         end if;
         Child := Child.Brother;
      end loop;

      --  9.3.8  find_POA
      --  If a child POA with the specified name does not exist and the value
      --  of the ACTIVATE_IT parameter is true, the target POA's
      --  AdapterActivator, if one exists, is invoked,...
      if not Activate_It
        or else PortableServer.AdapterActivator.Is_Nil (Self.Activator)
      then
         --  Can't call.
         return Nil_Ref;
      else
         --  Create a child POA with the same name and with the
         --  ghost_poa_manager.
         --  This child POA prevents simultaneous creation of several POAs
         --  with the same name.
         Res := Create_POA
           (Object_Ptr (Self), Adapter_Name, Ghost_POA_Manager,
            ORB_CTRL_MODEL, PortableServer.TRANSIENT, UNIQUE_ID, SYSTEM_ID,
            IMPLICIT_ACTIVATION, RETAIN, USE_ACTIVE_OBJECT_MAP_ONLY);
         PortableServer.POA_Forward.Set
           (POA_Ref,
            CORBA.Impl.Object_Ptr (Self));

         All_POAs_Lock.Unlock_W;

         begin
            Created := PortableServer.AdapterActivator.Unknown_Adapter
              (Self.Activator, POA_Ref, Adapter_Name);
         exception
            when E : others =>
               --  FIXME:
               --  9.3.3
               --  If unknown_adapter raises a system exception, the ORB will
               --  report an OBJ_ADAPTER exception.
               pragma Debug (O ("Unknown_Adapter raised "
                                & Ada.Exceptions.Exception_Name (E)));
               pragma Debug (O (Ada.Exceptions.Exception_Information (E)));
               Created := False;
         end;

         if Created and then Res.POA_Manager = Ghost_POA_Manager then
            --  This is not an internal error, but a user error:
            --  UNKNOWN_ADAPTER returned true without creating a POA.
            Created := False;
         end if;

         if Created then
            Res.Creation_Lock.Unlock;
            All_POAs_Lock.Lock_W;
            return To_POA_Ref (Res);
         else
            declare
               Res_Ref : constant Broca.POA.Ref
                 := To_POA_Ref (Res);
            begin
               --  Destroy res.
               All_POAs_Lock.Lock_W;
               --  Can't call destroy_POA to avoid a dead-lock.
               Unlink_POA (Res);
               Broca.Server.Unregister_POA (Res_Ref);
               Res.Creation_Lock.Unlock;
               Broca.ORB.POA_State_Changed (Res_Ref);
            end;

            --  FIXME:  the memory must be freed by dec_usage when poa_ref
            --  is finalized.  Check this.
            return Nil_Ref;
         end if;
      end if;
   end Find_POA;

   Root_POA : Broca.POA.Ref;

   procedure Setup (Root_POA : in out Object);

   procedure Setup (Root_POA : in out Object) is
   begin
      Root_POA.POA_Manager :=
        Broca.POA.POAManager_Object_Ptr (Default_POA_Manager);
      --  9.3.8
      --  The parent of the root POA is null.
      Root_POA.Parent := null;
      --  9.3.8
      --  The name of the root POA is system-dependent and should not be relied
      --  upon the application.
      Root_POA.Name := CORBA.To_CORBA_String ("root");
      Root_POA.Thread_Policy := ORB_CTRL_MODEL;
      Root_POA.Lifespan_Policy := TRANSIENT;
      Root_POA.Uniqueness_Policy := UNIQUE_ID;
      Root_POA.Id_Assign_Policy := SYSTEM_ID;
      Root_POA.Servant_Policy := RETAIN;
      Root_POA.Request_Policy := USE_ACTIVE_OBJECT_MAP_ONLY;
      Root_POA.Activation_Policy := IMPLICIT_ACTIVATION;
   end Setup;

begin
   --  Build the default POAManager.
   pragma Debug (O ("elaboration begins here"));

   --  9.3.2  Processing States
   --  The RootPOA is therefore initially in the holding state.

   Default_POA_Manager := new POA_Manager_Type;
   Broca.Refs.Inc_Usage (Broca.Refs.Ref_Ptr (Default_POA_Manager));
   pragma Debug (O ("Default POA Manager created"));

   --  Build the ghost POA manager.

   Ghost_POA_Manager := new POA_Manager_Type;
   Broca.Refs.Inc_Usage (Broca.Refs.Ref_Ptr (Ghost_POA_Manager));
   pragma Debug (O ("Ghost POA Manager created"));

   --  Build the RootPOA.

   declare
      P : constant Broca.POA.POA_Object_Ptr
        := new Object;
   begin
      Setup (Object (P.all));
      pragma Debug (O ("Creating Root_POA ref."));
      Root_POA := To_POA_Ref (P);
      pragma Debug (O ("Registering default POA manager."));
      Register (Default_POA_Manager.all, Root_POA);
      pragma Debug (O ("Registering root POA."));
      Broca.Server.Register_POA (Root_POA);

      pragma Assert (P.Index = Root_POA_Index);
   end;

   --  Register the RootPOA in initial_references array.

   pragma Debug (O ("Exporting Root_POA ref."));
   declare
      Root_POA_Object_Ref : CORBA.Object.Ref;
   begin
      pragma Debug (O ("Creating object ref."));
      CORBA.Object.Set (Root_POA_Object_Ref, Object_Of (Root_POA));

      pragma Debug (O ("Registering initial reference."));
      Broca.ORB.Register_Initial_Reference
        (Broca.ORB.Root_POA_ObjectId,
         CORBA.Object.Ref (Root_POA_Object_Ref));
      pragma Debug (O ("Done."));
   end;

end Broca.RootPOA;
