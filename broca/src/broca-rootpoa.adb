with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with CORBA;
with CORBA.Object;
with PortableServer; use PortableServer;
with PortableServer.POA;
with PortableServer.AdapterActivator;
with PortableServer.ServantManager;
with PortableServer.ServantActivator.Impl;
with PortableServer.ServantLocator.Impl;
with Broca.Refs;
with Broca.Exceptions;
with Broca.POA; use Broca.POA;
with Broca.Sequences;
with Broca.ORB;
with Broca.Vararray;
with Broca.Buffers;
with Broca.Server;
with Broca.Inet_Server;
with Broca.Locks;
with Broca.Marshalling;
with Broca.Flags;
with Ada.Task_Attributes;
pragma Elaborate_All (Broca.Vararray);
pragma Elaborate_All (Broca.ORB);
pragma Elaborate_All (Broca.Refs);
pragma Elaborate_All (CORBA.Object);
pragma Elaborate_All (Broca.Server);

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Rootpoa is
   Flag : constant Natural := Broca.Debug.Is_Active ("broca.rootpoa");
   procedure O is new Broca.Debug.Output (Flag);

   ---------------------------------------
   -- An implementation of a POAManager --
   ---------------------------------------

   package Poa_Vararray is new Broca.Vararray
     (Element => Broca.POA.POA_Object_Ptr,
      Null_Element => null,
      Index_Type => Natural);

   protected type State_Type is
      function Get_State return Processing_State_Type;
      procedure Set_State (State : Processing_State_Type);
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

   type Poa_Manager_Type is new Broca.POA.POAManager_Object with
      record
         --  9.3.2  Processing States
         --  A POA manager is created in the holding state.
         State : State_Type;

         Poas : Poa_Vararray.Var_Array_Type := Poa_Vararray.Null_Var_Array;
      end record;
   procedure Activate (Self : in out Poa_Manager_Type);
   procedure Hold_Requests (Self : in out Poa_Manager_Type;
                            Wait_For_Completion : CORBA.Boolean);
   procedure Discard_Requests (Self : in out Poa_Manager_Type;
                               Wait_For_Completion : CORBA.Boolean);
   procedure Deactivate
     (Self : in out Poa_Manager_Type;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean);

   procedure Register (Self : in out Poa_Manager_Type;
                       A_Poa : POA_Object_Ptr);
   procedure Unregister (Self : in out Poa_Manager_Type;
                         A_Poa : POA_Object_Ptr);
   procedure Inc_Usage_If_Active (Self : in out Poa_Manager_Type;
                                  State : out Processing_State_Type);
   procedure Dec_Usage (Self : in out Poa_Manager_Type);
   procedure Inc_Usage (Self : in out Poa_Manager_Type);
   function Is_Inactive (Self : in Poa_Manager_Type) return Boolean;

   procedure State_Changed_Iterator
     (El : Broca.POA.POA_Object_Ptr; Arg : Boolean);
   procedure Etherealize_Iterator
     (El : Broca.POA.POA_Object_Ptr; Arg : Boolean);

   procedure State_Changed_Iterator
     (El : Broca.POA.POA_Object_Ptr; Arg : Boolean) is
   begin
      --  Avoid a warning.
      if Arg then
         null;
      end if;
      Broca.ORB.POA_State_Changed (El);
   end State_Changed_Iterator;

   procedure Etherealize_Iterator
     (El : Broca.POA.POA_Object_Ptr; Arg : Boolean) is
   begin
      --  Avoid a warning.
      if Arg then
         null;
      end if;
      if El.Servant_Policy = RETAIN
        and then El.Request_Policy = USE_SERVANT_MANAGER
      then
         Deactivate (El);
      end if;
   end Etherealize_Iterator;

   procedure Activate (Self : in out Poa_Manager_Type) is
   begin
      Self.State.Set_State (Active);
      Poa_Vararray.Iterate
        (Self.Poas, State_Changed_Iterator'Access, False);
   end Activate;

   procedure Hold_Requests (Self : in out Poa_Manager_Type;
                            Wait_For_Completion : CORBA.Boolean) is
   begin
      Self.State.Set_State (Holding);
      if Wait_For_Completion then
         Self.State.Wait_For_Completion;
      end if;
   end Hold_Requests;

   procedure Discard_Requests (Self : in out Poa_Manager_Type;
                               Wait_For_Completion : CORBA.Boolean) is
   begin
      Self.State.Set_State (Discarding);
   end Discard_Requests;

   procedure Deactivate
     (Self : in out Poa_Manager_Type;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean) is
   begin
      Self.State.Set_State (Inactive);

      --  This flushes the queue.  Can be done as soon as now, since
      --  requests will be discarded.
      Poa_Vararray.Iterate
        (Self.Poas, State_Changed_Iterator'Access, False);

      if Etherealize_Objects then
         Poa_Vararray.Iterate (Self.Poas, Etherealize_Iterator'Access, False);
      end if;

      if Wait_For_Completion then
         Self.State.Wait_For_Completion;
      end if;
   end Deactivate;

   procedure Register
     (Self : in out Poa_Manager_Type; A_Poa : POA_Object_Ptr) is
   begin
      Poa_Vararray.Insert (Self.Poas, A_Poa);
   end Register;

   procedure Unregister
     (Self : in out Poa_Manager_Type; A_Poa : POA_Object_Ptr) is
   begin
      Poa_Vararray.Remove (Self.Poas, A_Poa);
   end Unregister;

   procedure Inc_Usage_If_Active (Self : in out Poa_Manager_Type;
                                  State : out Processing_State_Type) is
   begin
      Self.State.Inc_Usage_If_Active (State);
   end Inc_Usage_If_Active;

   procedure Inc_Usage (Self : in out Poa_Manager_Type) is
   begin
      Self.State.Inc_Usage;
   end Inc_Usage;

   procedure Dec_Usage (Self : in out Poa_Manager_Type) is
   begin
      Self.State.Dec_Usage;
   end Dec_Usage;

   function Is_Inactive (Self : in Poa_Manager_Type) return Boolean is
   begin
      return Self.State.Get_State = Inactive;
   end Is_Inactive;

   type POA_Manager_Ptr is access all Poa_Manager_Type;

   --  The default POA manager is the POAManager used by the RootPOA, and
   --  created during the elaboration.
   Default_Poa_Manager : POA_Manager_Ptr;

   --  The ghost POA manager is used by ghost POA, ie poa created by
   --  find_POA before calling an AdapterActivator.
   Ghost_Poa_Manager : POAManager_Object_Ptr;

   ----------------------------------
   --  An implementation of a POA  --
   ----------------------------------

   type POA_Task_Attribute is record
      Current_Object    : PortableServer.ObjectId;
      Current_POA       : PortableServer.POA_Forward.Ref;
   end record;

   Nil_Attribute : POA_Task_Attribute;

   package Attributes is new Ada.Task_Attributes
     (Attribute => POA_Task_Attribute,
      Initial_Value => Nil_Attribute);

   type Cell_State_Type is (Free, Reserved, Active, To_Be_Destroyed);
   --  A servant_cell_type is in fact an object map entry.
   --  FIXME: change name.
   type Servant_Cell_Type is
      record
         --  SKELETON must not be null when state is active.
         Skeleton : Broca.POA.Skeleton_Ptr;

         --  State of the entry.
         State : Cell_State_Type := Free;

         --  Date.
         --  Changed each time the entry becomes free, so that dangling IOR
         --  are not valid.
         Date : Natural := 0;

         --  Number of requests to this object.  Used for etherealize.
         Requests_Lock : Broca.Locks.Rw_Lock_Type;
      end record;

   type Slot_Index_Type is new CORBA.Unsigned_Long;
   Bad_Slot_Index : constant Slot_Index_Type := -1;
   type Servant_Cell_Ptr is access Servant_Cell_Type;
   type Servant_Cell_Ptr_Array is array (Slot_Index_Type range <>)
     of Servant_Cell_Ptr;
   type Servant_Cell_Ptr_Array_Ptr is access Servant_Cell_Ptr_Array;

   type Object is new Broca.POA.POA_Object with
      record
         --  Lock for serialization of requests to incarnate/etherealize.
         Servant_Lock : Broca.Locks.Mutex_Type;

         --  Number of current requests.
         --  The lock is used to count the number of requests (R) or to prevent
         --  any new requests (W).
         --  This is used only for single_thread_model policy.
         Requests_Lock : Broca.Locks.Rw_Lock_Type;

         --  The object map.
         --  It is valid only if the servant retention policy is RETAIN.
         Object_Map : Servant_Cell_Ptr_Array_Ptr := null;

         --  Only used if NON_RETAIN and SYSTEM_ID to allocate uniq oid.
         Last_Slot : Slot_Index_Type := Bad_Slot_Index;

         --  The map can always be read, but protected against multiple
         --  write accesses by the lock.
         --  FIXME: this is a kludge.
         --  To be true, OBJECT_MAP must be atomic, but it isn't since it is
         --  a fat pointer.
         Map_Lock : Broca.Locks.Mutex_Type;
      end record;
   type Object_Ptr is access all Object;
   function Activate_Object
     (Self : access Object; P_Servant : PortableServer.Servant)
      return PortableServer.ObjectId;
   procedure Activate_Object_With_Id
     (Self : access Object;
      Oid : ObjectId;
      P_Servant : PortableServer.Servant);
   function Create_Reference (Self : access Object; Intf : CORBA.RepositoryId)
                              return CORBA.Object.Ref;
   function Create_Reference_With_Id
     (Self : access Object; Oid : ObjectId; Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref;
   function Servant_To_Id (Self : access Object; P_Servant : Servant)
                           return ObjectId;
   function Skeleton_To_Servant
     (Self : access Object; Skeleton : Broca.POA.Skeleton_Ptr)
     return Servant;
   procedure Deactivate_Object (Self : access Object; Oid : ObjectId);

   procedure Deactivate (Self : access Object);

   procedure GIOP_Invoke
     (Self : access Object;
      Key : in out Broca.Buffers.Buffer_Descriptor;
      Operation : CORBA.Identifier;
      Request_Id : CORBA.Unsigned_Long;
      Reponse_Expected : CORBA.Boolean;
      Message : in out Broca.Buffers.Buffer_Descriptor);
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
      return POA_Object_Ptr;
   procedure Destroy_POA (Self : access Object;
                          Etherealize_Objects : CORBA.Boolean;
                          Wait_For_Completion : CORBA.Boolean);
   procedure Cleanup (Self : access Object);

   function Servant_To_Skeleton (Self : access Object; P_Servant : Servant)
      return Broca.POA.Skeleton_Ptr;
   function Id_To_Skeleton (Self : access Object; Oid : ObjectId)
     return Skeleton_Ptr;

   subtype Objectid_Type is
     Broca.Sequences.Octet_Sequences.Element_Array (0 .. 3);

   function Slot_Index_Type_To_Objectid_Type is new Ada.Unchecked_Conversion
     (Source => Slot_Index_Type, Target => Objectid_Type);

   function Objectid_Type_To_Slot_Index_Type is new Ada.Unchecked_Conversion
     (Source => Objectid_Type, Target => Slot_Index_Type);

   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Object => Servant_Cell_Ptr_Array, Name => Servant_Cell_Ptr_Array_Ptr);

   --  Disable "should be in package spec" warning.
   pragma Warnings (Off);

   function Slot_By_Object_Id (Self : access Object; Name : ObjectId)
                               return Slot_Index_Type;
   function Slot_By_Servant (Self : access Object; P_Servant : Servant)
                             return Slot_Index_Type;
   function Slot_By_Skeleton
     (Self : access Object; Skeleton : Broca.POA.Skeleton_Ptr)
      return Slot_Index_Type;

   --  Return how many times the servant was activated.
   function Nbr_Slots_For_Servant (Self : access Object; P_Servant : Servant)
     return Natural;
   function Reserve_A_Slot (Self : access Object) return Slot_Index_Type;
   function Build_Key_For_ObjectId (Oid : ObjectId)
                                    return Broca.Buffers.Buffer_Descriptor;
   function Clean_Slot (Self : access Object; Slot : Slot_Index_Type)
                        return Boolean;
   procedure Unlink_POA (Self : POA_Object_Ptr);

   --  Find a skeleton to be destroyed
   function Get_Slot_To_Destroy (Self : access Object) return Slot_Index_Type;

   procedure Set_Cleanup_Call_Back (Self : access Object);
   pragma Warnings (On);

   function Slot_By_Object_Id (Self : access Object; Name : ObjectId)
     return Slot_Index_Type is
   begin
      if Self.Object_Map /= null then
         for I in Self.Object_Map.all'Range loop
            if Self.Object_Map (I).Skeleton /= null
              and then Self.Object_Map (I).Skeleton.Object_Id = Name
            then
               return I;
            end if;
         end loop;
      end if;
      return Bad_Slot_Index;
   end Slot_By_Object_Id;

   function Nbr_Slots_For_Servant (Self : access Object; P_Servant : Servant)
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

   function Slot_By_Servant (Self : access Object; P_Servant : Servant)
     return Slot_Index_Type is
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
      return Bad_Slot_Index;
   end Slot_By_Servant;

   function Slot_By_Skeleton
     (Self : access Object; Skeleton : Broca.POA.Skeleton_Ptr)
      return Slot_Index_Type is
   begin
      if Self.Object_Map /= null then
         for I in Self.Object_Map.all'Range loop
            if Self.Object_Map (I).Skeleton = Skeleton then
               return I;
            end if;
         end loop;
      end if;
      return Bad_Slot_Index;
   end Slot_By_Skeleton;

   -------------------
   -- Reserve_A_Slot --
   -------------------

   function Reserve_A_Slot
     (Self : access Object)
     return Slot_Index_Type
   is
      New_Object_Map : Servant_Cell_Ptr_Array_Ptr;
      Old_Object_Map : Servant_Cell_Ptr_Array_Ptr;
      Slot           : Slot_Index_Type;
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
               Self.Object_Map := new Servant_Cell_Ptr_Array'(1 .. 8 => null);
               Slot := 1;
            else
               Found := False;
               for I in Self.Object_Map.all'Range loop
                  if Self.Object_Map (I) = null
                    or else Self.Object_Map (I).State = Free
                  then
                     Slot  := I;
                     Found := True;
                     exit;
                  end if;
               end loop;
               if not Found then
                  New_Object_Map := new Servant_Cell_Ptr_Array'
                    (1 .. 2 * Self.Object_Map.all'Last => null);
                  Slot := Self.Object_Map.all'Last + 1;
                  New_Object_Map (Self.Object_Map.all'Range) :=
                    Self.Object_Map.all;
                  Old_Object_Map := Self.Object_Map;
                  Self.Object_Map := New_Object_Map;
                  Unchecked_Deallocation (Old_Object_Map);
               end if;
            end if;
            if Self.Object_Map (Slot) = null then
               Self.Object_Map (Slot) := new Servant_Cell_Type;
            end if;
            Self.Object_Map (Slot).State := Reserved;
      end case;

      Self.Map_Lock.Unlock;
      return Slot;
   end Reserve_A_Slot;

   procedure Marshall_Objectid (Buf : in out Broca.Buffers.Buffer_Descriptor;
                                Oid : ObjectId);
   procedure Marshall_Size_Objectid
     (Buf : in out Broca.Buffers.Buffer_Descriptor; Oid : ObjectId);
   procedure Unmarshall_Objectid (Buf : in out Broca.Buffers.Buffer_Descriptor;
                                  Oid : out ObjectId);
   function Build_Key_For_Slot (Self : access Object; Slot : Slot_Index_Type)
                                return Broca.Buffers.Buffer_Descriptor;
   procedure Key_To_Slot
     (Self : access Object;
      Key : in out Broca.Buffers.Buffer_Descriptor;
      Slot : out Slot_Index_Type);
   procedure Key_To_ObjectId (Key : in out Broca.Buffers.Buffer_Descriptor;
                              Oid : out ObjectId);

   procedure Marshall_Objectid (Buf : in out Broca.Buffers.Buffer_Descriptor;
                                Oid : ObjectId)
   is
   begin
      Broca.Sequences.Marshall (Buf, Broca.Sequences.Octet_Sequence (Oid));
   end Marshall_Objectid;

   procedure Marshall_Size_Objectid
     (Buf : in out Broca.Buffers.Buffer_Descriptor; Oid : ObjectId) is
   begin
      Broca.Sequences.Compute_New_Size
        (Buf, Broca.Sequences.Octet_Sequence (Oid));
   end Marshall_Size_Objectid;

   procedure Unmarshall_Objectid (Buf : in out Broca.Buffers.Buffer_Descriptor;
                                  Oid : out ObjectId)
   is
      Seq_Oct : Broca.Sequences.Octet_Sequence;
   begin
      Broca.Sequences.Unmarshall (Buf, Seq_Oct);
      Oid := ObjectId (Seq_Oct);
   end Unmarshall_Objectid;

   --  Possible only if RETAIN policy.
   function Build_Key_For_Slot (Self : access Object; Slot : Slot_Index_Type)
                                return Broca.Buffers.Buffer_Descriptor
   is
      use Broca.Marshalling;
      use Broca.Buffers;
      Res : Buffer_Descriptor;
   begin
      if Self.Lifespan_Policy = PERSISTENT then
         Allocate_Buffer_And_Clear_Pos
           (Res, 4 + 4 + 4 +
            Buffer_Index_Type
            (Length (Self.Object_Map (Slot).Skeleton.Object_Id)));
      else
         Allocate_Buffer_And_Clear_Pos  (Res, 4 + 4);
      end if;
      if Self.Lifespan_Policy = PERSISTENT then
         Marshall (Res, Broca.Flags.Boot_Time);
      end if;
      Marshall (Res, CORBA.Unsigned_Long (Slot));
      Marshall (Res, CORBA.Unsigned_Long (Self.Object_Map (Slot).Date));
      if Self.Lifespan_Policy = PERSISTENT then
         Marshall_Objectid (Res, Self.Object_Map (Slot).Skeleton.Object_Id);
      end if;
      return Res;
   end Build_Key_For_Slot;

   procedure Key_To_Slot
     (Self : access Object;
      Key : in out Broca.Buffers.Buffer_Descriptor;
      Slot : out Slot_Index_Type)
   is
      use Broca.Marshalling;
      Res : Slot_Index_Type;
      A_Long : CORBA.Unsigned_Long;
      Date : CORBA.Unsigned_Long;
      Oid : ObjectId;
   begin
      if Self.Lifespan_Policy = PERSISTENT then
         Unmarshall_Objectid (Key, Oid);
         Slot := Slot_By_Object_Id (Self, Oid);
         return;
      end if;
      Unmarshall (Key, A_Long);
      Res := Slot_Index_Type (A_Long);
      Unmarshall (Key, Date);
      if Self.Object_Map = null
        or else Res not in Self.Object_Map.all'Range
        or else Self.Object_Map (Res).Date /= Natural (Date)
        or else Self.Object_Map (Res).State /= Active
      then
         --  The object does not exist in the map.
         Slot := Bad_Slot_Index;
         return;
      else
         Slot := Res;
         return;
      end if;
   end Key_To_Slot;

   --  Possible only if NON_RETAIN policy.
   function Build_Key_For_ObjectId (Oid : ObjectId)
                                    return Broca.Buffers.Buffer_Descriptor
   is
      use Broca.Marshalling;
      use Broca.Buffers;
      Res : Buffer_Descriptor;
   begin
      Marshall_Size_Objectid (Res, Oid);
      Allocate_Buffer (Res);
      Marshall_Objectid (Res, Oid);
      return Res;
   end Build_Key_For_ObjectId;

   procedure Key_To_ObjectId (Key : in out Broca.Buffers.Buffer_Descriptor;
                              Oid : out ObjectId) is
   begin
      Unmarshall_Objectid (Key, Oid);
   end Key_To_ObjectId;

   --  if SELF has NON_RETAIN policy, SLOT is not used.
   function Create_Skeleton
     (Self      : access Object;
      Slot      : Slot_Index_Type;
      P_Servant : PortableServer.Servant;
      Type_Id   : CORBA.RepositoryId;
      Oid       : ObjectId)
     return Broca.POA.Skeleton_Ptr;

   function Create_Skeleton
     (Self      : access Object;
      Slot      : Slot_Index_Type;
      P_Servant : PortableServer.Servant;
      Type_Id   : CORBA.RepositoryId;
      Oid       : ObjectId)
     return Broca.POA.Skeleton_Ptr
   is
      Key : Broca.Buffers.Buffer_Descriptor;
      Obj : Broca.POA.Skeleton_Ptr;
   begin
      Obj := new Broca.POA.Skeleton;
      Obj.P_Servant := P_Servant;
      Obj.Object_Id := Oid;
      Obj.POA := POA_Object_Ptr (Self);
      Broca.Refs.Inc_Usage (Broca.Refs.Ref_Ptr (Obj));

      --  The servant is now active
      if Self.Servant_Policy = RETAIN then
         Self.Object_Map (Slot).Skeleton := Obj;
         Self.Object_Map (Slot).State := Active;
         Key := Build_Key_For_Slot (Self, Slot);
      else
         Key := Build_Key_For_ObjectId (Oid);
      end if;

      Broca.Buffers.Rewind (Key);
      Broca.Server.Build_IOR
        (Obj.IOR, Type_Id, Broca.POA.POA_Object_Ptr (Self), Key);
      Broca.Buffers.Rewind (Obj.IOR);

      Broca.Buffers.Destroy (Key);

      Broca.Server.Log ("ObjectId created");

      return Obj;
   end Create_Skeleton;

   function Activate_Object (Self : access Object;
                             P_Servant : PortableServer.Servant)
                             return PortableServer.ObjectId
   is
      Slot : Slot_Index_Type;
      Oid : ObjectId;
      Obj : Broca.POA.Skeleton_Ptr;
   begin
      if Self.Uniqueness_Policy = UNIQUE_ID
        and then Slot_By_Servant (Self, P_Servant) /= Bad_Slot_Index
      then
         raise PortableServer.POA.ServantAlreadyActive;
      end if;

      Slot := Reserve_A_Slot (Self);

      Oid := To_Sequence (Slot_Index_Type_To_Objectid_Type (Slot));
      Obj := Create_Skeleton
        (Self, Slot, P_Servant, Get_Type_Id (P_Servant.all), Oid);
      return Oid;
   end Activate_Object;

   procedure Activate_Object_With_Id
     (Self : access Object; Oid : ObjectId; P_Servant : PortableServer.Servant)
   is
      Slot : Slot_Index_Type;
      Obj : Broca.POA.Skeleton_Ptr;
   begin
      Slot := Slot_By_Object_Id (Self, Oid);
      if Slot = Bad_Slot_Index then
         if Self.Id_Assign_Policy = SYSTEM_ID then
            Broca.Exceptions.Raise_Bad_Param;
         end if;
      elsif Self.Object_Map (Slot).State = Active then
         raise PortableServer.POA.ObjectAlreadyActive;
      end if;
      if Self.Uniqueness_Policy = UNIQUE_ID
        and then Slot_By_Servant (Self, P_Servant) /= Bad_Slot_Index
      then
         raise PortableServer.POA.ServantAlreadyActive;
      end if;

      if Slot = Bad_Slot_Index then
         Slot := Reserve_A_Slot (Self);
      end if;

      Obj := Create_Skeleton
        (Self, Slot, P_Servant, Get_Type_Id (P_Servant.all), Oid);
   end Activate_Object_With_Id;

   function Create_Reference (Self : access Object; Intf : CORBA.RepositoryId)
                              return CORBA.Object.Ref
   is
      Res : CORBA.Object.Ref;
      Slot : Slot_Index_Type;
      Obj : Broca.POA.Skeleton_Ptr;
      Oid : ObjectId;
   begin
      --  Allocate an objectId.
      Slot := Reserve_A_Slot (Self);
      Oid := To_Sequence (Slot_Index_Type_To_Objectid_Type (Slot));

      Obj := Create_Skeleton (Self, Slot, null, Intf, Oid);

      --  Create the reference.
      CORBA.Object.Set (Res, Broca.Refs.Ref_Ptr (Obj));
      return Res;
   end Create_Reference;

   function Create_Reference_With_Id
     (Self : access Object; Oid : ObjectId; Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref
   is
      Res : CORBA.Object.Ref;
      Slot : Slot_Index_Type;
      Obj : Broca.POA.Skeleton_Ptr;
   begin
      if Self.Servant_Policy = RETAIN then
         Slot := Slot_By_Object_Id (Self, Oid);
         if Self.Id_Assign_Policy = SYSTEM_ID then
            if Slot_By_Object_Id (Self, Oid) = Bad_Slot_Index then
               Broca.Exceptions.Raise_Bad_Param;
            end if;
         end if;
      else
         Slot := Bad_Slot_Index;
      end if;

      Obj := Create_Skeleton (Self, Slot, null, Intf, Oid);

      --  Create the reference.
      CORBA.Object.Set (Res, Broca.Refs.Ref_Ptr (Obj));
      return Res;
   end Create_Reference_With_Id;

   function Servant_To_Id (Self : access Object; P_Servant : Servant)
                           return ObjectId
   is
      Slot : Slot_Index_Type;
      Oid : ObjectId;
      Obj : Broca.POA.Skeleton_Ptr;
   begin
      if Self.Uniqueness_Policy = UNIQUE_ID then
         Slot := Slot_By_Servant (Self, P_Servant);
         if Slot /= Bad_Slot_Index then
            return Self.Object_Map (Slot).Skeleton.Object_Id;
         end if;
      end if;
      if Self.Activation_Policy = IMPLICIT_ACTIVATION
        and then (Self.Uniqueness_Policy = MULTIPLE_ID
                  or else Nbr_Slots_For_Servant (Self, P_Servant) = 0)
      then
         Slot := Reserve_A_Slot (Self);
         Oid := To_Sequence (Slot_Index_Type_To_Objectid_Type (Slot));
         Obj := Create_Skeleton
           (Self, Slot, P_Servant, Get_Type_Id (P_Servant.all), Oid);
         return Oid;
      end if;
      raise PortableServer.POA.ServantNotActive;
   end Servant_To_Id;

   function Servant_To_Skeleton (Self : access Object; P_Servant : Servant)
      return Broca.POA.Skeleton_Ptr
   is
      Slot : Slot_Index_Type;
      Oid : ObjectId;
      Obj : Broca.POA.Skeleton_Ptr;
   begin
      if Self.Uniqueness_Policy = UNIQUE_ID then
         Slot := Slot_By_Servant (Self, P_Servant);
         if Slot /= Bad_Slot_Index then
            return Self.Object_Map (Slot).Skeleton;
         end if;
      end if;
      if Self.Activation_Policy = IMPLICIT_ACTIVATION
        and then (Self.Uniqueness_Policy = MULTIPLE_ID
                  or else Nbr_Slots_For_Servant (Self, P_Servant) = 0)
      then
         Slot := Reserve_A_Slot (Self);
         Oid := To_Sequence (Slot_Index_Type_To_Objectid_Type (Slot));
         Obj := Create_Skeleton
           (Self, Slot, P_Servant, Get_Type_Id (P_Servant.all), Oid);
         return Self.Object_Map (Slot).Skeleton;
      end if;
      raise PortableServer.POA.ServantNotActive;
   end Servant_To_Skeleton;

   function Skeleton_To_Servant
     (Self : access Object; Skeleton : Broca.POA.Skeleton_Ptr)
     return Servant
   is
      Slot : Slot_Index_Type;
   begin
      if Self.Servant_Policy = RETAIN then
         Slot := Slot_By_Skeleton (Self, Skeleton);
         if Slot /= Bad_Slot_Index then
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

   function Id_To_Skeleton (Self : access Object; Oid : ObjectId)
     return Skeleton_Ptr
   is
      Slot : Slot_Index_Type;
   begin
      Slot := Slot_By_Object_Id (Self, Oid);
      if Slot = Bad_Slot_Index then
         raise PortableServer.POA.ObjectNotActive;
      end if;

      return Self.Object_Map (Slot).Skeleton;
   end Id_To_Skeleton;

   function Get_Slot_To_Destroy (Self : access Object) return Slot_Index_Type
   is
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
      return Bad_Slot_Index;
   end Get_Slot_To_Destroy;

   --  Try to free a slot.
   --  Return true if cleanup should be called.
   --  The Object Map must be locked.
   function Clean_Slot (Self : access Object; Slot : Slot_Index_Type)
                        return Boolean is
      use Broca.Refs;
   begin
      if PortableServer.ServantManager.Get (Self.Servant_Manager) /= null
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

   procedure Cleanup (Self : access Object)
   is
      Slot : Slot_Index_Type;
      Sm : Broca.POA.Internal_Skeleton_Ptr;
      A_Servant : PortableServer.Servant;
      A_Poa : PortableServer.POA.Ref;
      Is_Cleanup : Boolean;
      A_Ref : Broca.Refs.Ref_Ptr;
   begin
      Is_Cleanup := Self.POA_Manager = null
        or else Broca.POA.Is_Inactive (Self.POA_Manager.all);

      loop
         Slot := Get_Slot_To_Destroy (Self);
         exit when Slot = Bad_Slot_Index;

         if Broca.Refs."/="
           (PortableServer.ServantManager.Get (Self.Servant_Manager), null)
         then
            Sm := To_Internal_Skeleton (Self.Servant_Manager);
            A_Servant := Self.Object_Map (Slot).Skeleton.P_Servant;
            PortableServer.POA.Set (A_Poa, Broca.Refs.Ref_Ptr (Self));

            --  Wait for completions on all outstanding requests before
            --  etherealize the object.
            Self.Object_Map (Slot).Requests_Lock.Lock_W;

            --  Serialization of calls to incarnate/etherealize.
            Self.Servant_Lock.Lock;

            begin
               PortableServer.ServantActivator.Impl.Etherealize
                 (PortableServer.ServantActivator.Impl.Object'Class
                  (Sm.P_Servant.all),
                  Self.Object_Map (Slot).Skeleton.Object_Id,
                  PortableServer.POA.Convert.To_Forward (A_Poa),
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
         end if;
      end loop;

      if Self.POA_Manager /= null then
         Broca.POA.Dec_Usage (Self.POA_Manager.all);
      end if;

      --  FIXME: not very clean: destroy the object self.
      A_Ref := Broca.Refs.Ref_Ptr (Self);
      Broca.Refs.Dec_Usage (A_Ref);
   end Cleanup;

   procedure Set_Cleanup_Call_Back (Self : access Object) is
   begin
      Broca.Refs.Inc_Usage (Broca.Refs.Ref_Ptr (Self));
      if Self.POA_Manager /= null then
         Broca.POA.Inc_Usage (Self.POA_Manager.all);
      end if;
      Broca.Server.Request_Cleanup (Broca.POA.POA_Object_Ptr (Self));
   end Set_Cleanup_Call_Back;

   --  Called by the poa manager.
   procedure Deactivate (Self : access Object) is
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
            Broca.Refs.Inc_Usage (Broca.Refs.Ref_Ptr (Self));
            Broca.Server.Request_Cleanup (Broca.POA.POA_Object_Ptr (Self));
         end if;
      end if;
      Self.Map_Lock.Unlock;
   end Deactivate;

   procedure Deactivate_Object (Self : access Object; Oid : ObjectId) is
      Slot : Slot_Index_Type;
   begin
      Slot := Slot_By_Object_Id (Self, Oid);
      if Slot = Bad_Slot_Index then
         raise PortableServer.POA.ObjectNotActive;
      end if;
      if Clean_Slot (Self, Slot) then
         Broca.Refs.Inc_Usage (Broca.Refs.Ref_Ptr (Self));
         Broca.Server.Request_Cleanup (Broca.POA.POA_Object_Ptr (Self));
      end if;
   end Deactivate_Object;

   procedure GIOP_Invoke
     (Self : access Object;
      Key : in out Broca.Buffers.Buffer_Descriptor;
      Operation : CORBA.Identifier;
      Request_Id : CORBA.Unsigned_Long;
      Reponse_Expected : CORBA.Boolean;
      Message : in out Broca.Buffers.Buffer_Descriptor)
   is
      use PortableServer;
      Slot : Slot_Index_Type;
      A_Servant : Servant;
      Skel : Internal_Skeleton_Ptr;
      A_Poa : PortableServer.POA.Ref;
      Oid : ObjectId;
      The_Cookie : PortableServer.ServantLocator.Cookie;
   begin
      pragma Debug (O ("Giop_Invoke: enter"));
      --  See 9.3.7
      Self.Requests_Lock.Lock_R;
      pragma Debug (O ("Giop_Invoke: Got Read lock on request."));

      --  Find the ObjectId in the Activa Map if RETAIN Policy.
      if Self.Servant_Policy = RETAIN then
         Key_To_Slot (Self, Key, Slot);
         if Slot /= Bad_Slot_Index then
            A_Servant := Self.Object_Map (Slot).Skeleton.P_Servant;
         else
            A_Servant := null;
         end if;
      else
         A_Servant := null;
      end if;

      PortableServer.POA.Set (A_Poa, Broca.Refs.Ref_Ptr (Self));
      pragma Debug (O ("Giop_Invoke: POA is set."));

      if A_Servant = null then
         pragma Debug (O ("Giop_Invoke: A_Servant is null"));
         case Self.Request_Policy is
            when USE_ACTIVE_OBJECT_MAP_ONLY =>
               pragma Debug
                 (O ("Giop_invoke : USE_ACTIVE_OBJECT_MAP_ONLY policy"));
               if Slot /= Bad_Slot_Index then
                  A_Servant := Self.Object_Map (Slot).Skeleton.P_Servant;
               else
                  Broca.Exceptions.Raise_Object_Not_Exist;
               end if;

            when USE_DEFAULT_SERVANT =>
               pragma Debug
                 (O ("Giop_Invoke: USE_DEFAULT_SERVANT policy"));
               if Self.Default_Servant = null then
                  Broca.Exceptions.Raise_Obj_Adapter;
               else
                  A_Servant := Self.Default_Servant;
                  if Self.Servant_Policy = RETAIN
                    and then Slot /= Bad_Slot_Index
                  then
                     --  FIXME: persistent
                     Self.Object_Map (Slot).Skeleton.P_Servant := A_Servant;
                  end if;
               end if;

            when USE_SERVANT_MANAGER =>
               pragma Debug (O ("Giop_Invoke: USE_SERVANT_MANAGER policy"));
               if Broca.Refs."="
                 (PortableServer.ServantManager.Get (Self.Servant_Manager),
                  null)
               then
                  Broca.Exceptions.Raise_Obj_Adapter;
               else
                  Skel := To_Internal_Skeleton (Self.Servant_Manager);
               end if;
               if Self.Servant_Policy = RETAIN then
                  Self.Servant_Lock.Lock;
                  begin
                     PortableServer.ServantActivator.Impl.Incarnate
                       (PortableServer.ServantActivator.Impl.Object'Class
                        (Skel.P_Servant.all),
                        Oid,
                        A_Poa,
                        A_Servant);
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
                  if Slot = Bad_Slot_Index then
                     --  FIXME: persistent.
                     Broca.Exceptions.Raise_Internal (615);
                  else
                     Self.Object_Map (Slot).Skeleton.P_Servant := A_Servant;
                  end if;
               else
                  Key_To_ObjectId (Key, Oid);
                  PortableServer.ServantLocator.Impl.Preinvoke
                    (PortableServer.ServantLocator.Impl.Object'Class
                     (Skel.P_Servant.all),
                     Oid, A_Poa, Operation, The_Cookie, A_Servant);
                  Attributes.Set_Value
                    (POA_Task_Attribute'
                     (Oid, PortableServer.POA.Convert.To_Forward (A_Poa)));
                  GIOP_Dispatch
                    (A_Servant, CORBA.To_Standard_String (Operation),
                     Request_Id, Reponse_Expected, Message);
                  PortableServer.ServantLocator.Impl.Postinvoke
                    (PortableServer.ServantLocator.Impl.Object'Class
                     (Skel.P_Servant.all),
                     Oid, A_Poa, Operation, The_Cookie, A_Servant);
                  Self.Requests_Lock.Unlock_R;
                  return;
               end if;
         end case;
      end if;

      begin
         if Self.Servant_Policy = RETAIN then
            pragma Debug (O ("Giop_Invoke: RETAIN policy"));
            Self.Object_Map (Slot).Requests_Lock.Lock_R;
            pragma Debug (O ("Giop_Invoke: Got Read lock on request (2)."));
         end if;

         pragma Debug (O ("Giop_Invoke: Preparing POA_Task_Attributes."));
         declare
            My_Task_Attributes : constant POA_Task_Attribute
              := (Oid, PortableServer.POA.Convert.To_Forward (A_Poa));
         begin
            pragma Debug (O ("Giop_Invoke: Setting POA_Task_Attributes."));
            Attributes.Set_Value (My_Task_Attributes);
            pragma Debug (O ("Giop_Invoke: Did set POA_Task_Attributes."));
         end;

         pragma Debug
           (O ("Giop_Invoke: call giop_dispatch for " &
               CORBA.To_Standard_String (Operation)));
         GIOP_Dispatch
           (A_Servant, CORBA.To_Standard_String (Operation), Request_Id,
            Reponse_Expected, Message);
         pragma Debug (O ("Giop_Invoke: giop_dispatch returned"));

         if Self.Servant_Policy = RETAIN then
            Self.Object_Map (Slot).Requests_Lock.Unlock_R;
         end if;
         Self.Requests_Lock.Unlock_R;
         POA_Manager_Ptr (Self.POA_Manager).State.Dec_Usage;
         return;
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
            exit when Child.POA_Manager = Ghost_Poa_Manager;
            --  Failure
            raise PortableServer.POA.AdapterAlreadyExists;
         end if;
         Child := Child.Brother;
      end loop;

      if Child = null then
         Res := new Object;
         Broca.Refs.Inc_Usage (Broca.Refs.Ref_Ptr (Res));
         --  Link it.
         Res.Parent := POA_Object_Ptr (Self);
         Res.Brother := Self.Children;
         Self.Children := POA_Object_Ptr (Res);

         --  Internal data.
         Res.Name := Adapter_Name;
      else
         Res := Object_Ptr (Child);
      end if;

      Broca.Server.Register_POA (POA_Object_Ptr (Res));

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
      if A_POAManager /= null then
         Res.POA_Manager := A_POAManager;
      else
         Res.POA_Manager := new Poa_Manager_Type;
      end if;
      Broca.Refs.Inc_Usage (Broca.Refs.Ref_Ptr (Res.POA_Manager));

      return POA_Object_Ptr (Res);
   end Create_POA;

   procedure Unlink_POA (Self : POA_Object_Ptr) is
      A_Poa : POA_Object_Ptr;
   begin
      if Self.Parent /= null then
         A_Poa := Self.Parent.Children;
         if A_Poa = POA_Object_Ptr (Self) then
            Self.Parent.Children := Self.Brother;
         else
            while A_Poa.Brother /= null loop
               if A_Poa.Brother = POA_Object_Ptr (Self) then
                  A_Poa.Brother := Self.Brother;
                  exit;
               end if;
            end loop;
         end if;
      end if;
   end Unlink_POA;

   procedure Destroy_POA (Self : access Object;
                          Etherealize_Objects : CORBA.Boolean;
                          Wait_For_Completion : CORBA.Boolean)
   is
      procedure Unregister_All (Self : POA_Object_Ptr);
      procedure Destroy_All (Self : POA_Object_Ptr);
      procedure Unregister_All (Self : POA_Object_Ptr) is
      begin
         if Self.Children /= null then
            Unregister_All (Self.Children);
         end if;
         if Self.Brother /= null then
            Unregister_All (Self.Brother);
         end if;
         Broca.Server.Unregister_POA (Self);
         Broca.ORB.POA_State_Changed (Self);
         Broca.Refs.Dec_Usage (Broca.Refs.Ref_Ptr (Self.POA_Manager));
         Self.POA_Manager := null;
      end Unregister_All;

      procedure Destroy_All (Self : POA_Object_Ptr) is
      begin
         if Self.Children /= null then
            Destroy_All (Self.Children);
            Broca.Refs.Dec_Usage (Broca.Refs.Ref_Ptr (Self.Children));
         end if;
         if Self.Brother /= null then
            Destroy_All (Self.Brother);
            Broca.Refs.Dec_Usage (Broca.Refs.Ref_Ptr (Self.Brother));
         end if;
         PortableServer.AdapterActivator.Set (Self.Activator, null);
         Broca.Refs.Dec_Usage (Broca.Refs.Ref_Ptr (Self.POA_Manager));
         PortableServer.ServantManager.Set (Self.Servant_Manager, null);
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
      Unregister_All (POA);
      --  Now, the POA is unknown by the server and unreachable.
      All_POAs_Lock.Unlock_W;
      Destroy_All (POA_Object_Ptr (Self));
   end Destroy_POA;

   function Find_POA
     (Self         : access Object;
      Adapter_Name : CORBA.String;
      Activate_It  : CORBA.Boolean)
      return POA_Object_Ptr
   is
      use CORBA;
      Child : POA_Object_Ptr;
      Res : POA_Object_Ptr;
      Poa_Ref : PortableServer.POA_Forward.Ref;
      Created : Boolean;
   begin
      --  Find the POA.
      << Again >> null;
      Child := Self.Children;
      while Child /= null loop
         if Child.Name = Adapter_Name then
            if Child.POA_Manager = Ghost_Poa_Manager then
               --  The poa is under creation.
               Broca.Refs.Inc_Usage (Broca.Refs.Ref_Ptr (Child));
               All_POAs_Lock.Unlock_W;
               Child.Creation_Lock.Wait;
               All_POAs_Lock.Lock_W;
               Broca.Refs.Dec_Usage (Broca.Refs.Ref_Ptr (Child));
               goto Again;
            end if;
            return Child;
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
         return null;
      else
         --  Create a child POA with the same name and with the
         --  ghost_poa_manager.
         --  This child POA prevents simultaneous creation of several POAs
         --  with the same name.
         Res := Create_POA
           (Object_Ptr (Self), Adapter_Name, Ghost_Poa_Manager,
            ORB_CTRL_MODEL, PortableServer.TRANSIENT, UNIQUE_ID, SYSTEM_ID,
            IMPLICIT_ACTIVATION, RETAIN, USE_ACTIVE_OBJECT_MAP_ONLY);
         PortableServer.POA_Forward.Set (Poa_Ref, Broca.Refs.Ref_Ptr (Self));

         All_POAs_Lock.Unlock_W;

         begin
            Created := PortableServer.AdapterActivator.Unknown_Adapter
              (Self.Activator, Poa_Ref, Adapter_Name);
         exception
            when others =>
               --  FIXME:
               --  9.3.3
               --  If unknown_adapter raises a system exception, the ORB will
               --  report an OBJ_ADAPTER exception.
               Created := False;
         end;

         if Created and then Res.POA_Manager = Ghost_Poa_Manager then
            --  This is not an internal error, but a user error:
            --  UNKNOWN_ADAPTER returned true without creating a POA.
            Created := False;
         end if;

         if Created then
            Res.Creation_Lock.Unlock;
            All_POAs_Lock.Lock_W;
            return Res;
         else
            --  Destroy res.
            All_POAs_Lock.Lock_W;
            --  Can't call destroy_POA to avoid a dead-lock.
            Unlink_POA (Res);
            Broca.Server.Unregister_POA (Res);
            Res.Creation_Lock.Unlock;
            Broca.ORB.POA_State_Changed (Res);
            --  FIXME:  the memory must be freed by dec_usage when poa_ref
            --  is finalized.  Check this.
            return null;
         end if;
      end if;
   end Find_POA;

   Root_POA : Broca.POA.POA_Object_Ptr;
begin
   --  Build the default POAManager.
   --  9.3.2  Processing States
   --  The RootPOA is therefore initially in the holding state.
   Default_Poa_Manager := new Poa_Manager_Type;
   Broca.Refs.Inc_Usage (Broca.Refs.Ref_Ptr (Default_Poa_Manager));

   --  Build the ghost POA manager.
   Ghost_Poa_Manager := new Poa_Manager_Type;
   Broca.Refs.Inc_Usage (Broca.Refs.Ref_Ptr (Ghost_Poa_Manager));

   --  Build the RootPOA.
   Root_POA := new Object;
   Broca.Refs.Inc_Usage (Broca.Refs.Ref_Ptr (Root_POA));
   Root_POA.POA_Manager :=
     Broca.POA.POAManager_Object_Ptr (Default_Poa_Manager);
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
   Register (Default_Poa_Manager.all, Root_POA);
   Broca.Server.Register_POA (Root_POA);
   if Root_POA.Index /= Root_POA_Index then
      raise Program_Error;
   end if;

   --  Register the RootPOA in initial_references array.
   declare
      Obj_Ref : CORBA.Object.Ref;
   begin
      CORBA.Object.Set (Obj_Ref, Broca.Refs.Ref_Ptr (Root_POA));
      Broca.ORB.Register_Initial_Reference
        (Broca.ORB.Root_POA_ObjectId, Obj_Ref);
   end;
end Broca.Rootpoa;
