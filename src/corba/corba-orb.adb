--  The following subprograms still have to be implemented :
--
-- create_alias_tc --
-- create_array_tc --
-- create_fixed_tc --
-- create_interface_tc --
-- Create_List --
-- create_native_tc --
-- Create_Policy --
-- create_recursive_sequence_tc --
-- create_sequence_tc --
-- create_string_tc --
-- create_wstring_tc --
-- Get_Default_Context --
-- Get_Service_Information --
-- List_Initial_Services --
-- Perform_Work --
-- Resolve_Initial_References --
-- Shutdown --
-- Work_Pending --

with Ada.Exceptions;

with Sequences.Unbounded;

with Droopi.Dynamic_Dict;
pragma Elaborate_All (Droopi.Dynamic_Dict);

with Droopi.ORB;
with Droopi.ORB.Task_Policies;
with Droopi.Objects;
with Droopi.References.IOR;
with Droopi.Setup;
with Droopi.Smart_Pointers;

with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

package body CORBA.ORB is

   use Droopi.Log;
   use Droopi.ORB.Task_Policies;
   use Droopi.ORB;
   use Droopi.Setup;

   package L is new Droopi.Log.Facility_Log ("corba.orb");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   package Referenced_Objects is
      new Droopi.Dynamic_Dict (CORBA.Object.Ref);

--    type Referenced_Object is record
--       Identifier : ObjectId;
--       Reference  : CORBA.Object.Ref;
--    end record;

--    package Referenced_Objects is new
--      Sequences.Unbounded (Referenced_Object);

--    Identifiers : Referenced_Objects.Sequence
--      := Referenced_Objects.Null_Sequence;

   ---------------------
   -- create_alias_tc --
   ---------------------

   function create_alias_tc
     (Id            : in CORBA.RepositoryId;
      Name          : in CORBA.Identifier;
      Original_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object
   is
   begin
      raise Droopi.Not_Implemented;
      return create_alias_tc (Id, Name, Original_Type);
   end create_alias_tc;

   ---------------------
   -- create_array_tc --
   ---------------------

   function create_array_tc
     (Length       : in CORBA.Unsigned_Long;
      Element_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object
   is
   begin
      raise Droopi.Not_Implemented;
      return create_array_tc (Length, Element_Type);
   end create_array_tc;

   ---------------------
   -- create_fixed_tc --
   ---------------------

   function create_fixed_tc
     (IDL_Digits : in CORBA.Unsigned_Short;
      scale      : in CORBA.Short)
      return CORBA.TypeCode.Object
   is
   begin
      raise Droopi.Not_Implemented;
      return create_fixed_tc (IDL_Digits, scale);
   end create_fixed_tc;

   -------------------------
   -- create_interface_tc --
   -------------------------

   function create_interface_tc
     (Id   : in CORBA.RepositoryId;
      Name : in CORBA.Identifier)
      return CORBA.TypeCode.Object
   is
   begin
      raise Droopi.Not_Implemented;
      return create_interface_tc (Id, Name);
   end create_interface_tc;

   -----------------
   -- Create_List --
   -----------------

   procedure Create_List
     (Count    : in     CORBA.Long;
      New_List :    out CORBA.NVList.Ref)
   is
   begin
      if Count /= 0 then
         raise Droopi.Not_Implemented;
         --  XXX How should the list be populated?
      else
         CORBA.NVList.Create (New_List);
      end if;
   end Create_List;

   ----------------------
   -- create_native_tc --
   ----------------------

   function create_native_tc
     (Id   : in RepositoryId;
      Name : in Identifier)
      return CORBA.TypeCode.Object
   is
   begin
      raise Droopi.Not_Implemented;
      return create_native_tc (Id, Name);
   end create_native_tc;

   -------------------
   -- Create_Policy --
   -------------------

   procedure Create_Policy
     (The_Type : in PolicyType;
      Val      : Any)
   is
   begin
      null;
   end Create_Policy;

   ----------------------------------
   -- create_recursive_sequence_tc --
   ----------------------------------

   function create_recursive_sequence_tc
     (Bound  : in CORBA.Unsigned_Long;
      Offset : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object
   is
   begin
      raise Droopi.Not_Implemented;
      return create_recursive_sequence_tc (Bound, Offset);
   end create_recursive_sequence_tc;

   ------------------------
   -- create_sequence_tc --
   ------------------------

   function create_sequence_tc
     (Bound        : in CORBA.Unsigned_Long;
      Element_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object
   is
   begin
      raise Droopi.Not_Implemented;
      return create_sequence_tc (Bound, Element_Type);
   end create_sequence_tc;

   ----------------------
   -- create_string_tc --
   ----------------------

   function create_string_tc
     (Bound : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object
   is
   begin
      raise Droopi.Not_Implemented;
      return create_string_tc (Bound);
   end create_string_tc;

   -----------------------
   -- create_wstring_tc --
   -----------------------

   function create_wstring_tc
     (Bound : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object
   is
   begin
      raise Droopi.Not_Implemented;
      return create_wstring_tc (Bound);
   end create_wstring_tc;

   -------------------------
   -- Get_Default_Context --
   -------------------------

   function Get_Default_Context
      return CORBA.Context.Ref
   is
   begin
      raise Droopi.Not_Implemented;
      return Get_Default_Context;
   end Get_Default_Context;

   -----------------------------
   -- Get_Service_Information --
   -----------------------------

   procedure Get_Service_Information
     (Service_Type        : in     CORBA.ServiceType;
      Service_Information :    out ServiceInformation;
      Returns             :    out CORBA.Boolean)
   is
   begin
      raise Droopi.Not_Implemented;
   end Get_Service_Information;

   ---------------------------
   -- List_Initial_Services --
   ---------------------------

   function List_Initial_Services return ObjectIdList is
   begin
      raise Droopi.Not_Implemented;
      return List_Initial_Services;
   end List_Initial_Services;

   ------------------
   -- Perform_Work --
   ------------------

   procedure Perform_Work is
   begin
      Perform_Work (The_ORB);
   end Perform_Work;

   --------------------------------
   -- Resolve_Initial_References --
   --------------------------------

   function Resolve_Initial_References
     (Identifier : ObjectId)
     return CORBA.Object.Ref
   is
      Id : constant Standard.String
        := To_Standard_String (Identifier);
   begin
      return Referenced_Objects.Lookup (Id);
   exception
      when E : others =>
         pragma Debug
           (O ("Got exception while looking up " & Id & ":"));
         pragma Debug (O (Ada.Exceptions.Exception_Information (E)));
         raise CORBA.InvalidName;
   end Resolve_Initial_References;

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      Droopi.ORB.Run (The_ORB, May_Poll => True);
   end Run;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Wait_For_Completion : in Boolean) is
   begin
      Shutdown (The_ORB, Wait_For_Completion);
   end Shutdown;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
      return CORBA.String
   is
      use Droopi.Smart_Pointers;

      E : constant Entity_Ptr := CORBA.Object.Entity_Of (Obj);
   begin
      if E /= null and then E.all in Object.Reference_Info'Class then
         return Droopi.References.IOR.Object_To_String
           (Object.Reference_Info (E.all).IOR);
      else
         --  XXX
         --  This ref does not contain a Reference_Info,
         --  i.e. it is likely to be directly a pointer to
         --  a CORBA.Impl.Object'Class. Can this happen?
         --  I.e. do we have provided a means for the
         --  user to construct such a ref (most likely
         --  the standard CORBA API does allow it, because
         --  the user sees the Set primitive of C.O.Ref).
         --  Is it legitimate to call O_to_S on such a Ref?
         --  Maybe not, for the Servant would not have been
         --  activated. Or can it have??? To be determined.
         raise Program_Error;
      end if;
   end Object_To_String;

   ----------------------
   -- String_To_Object --
   ----------------------

   procedure String_To_Object
     (From : in     CORBA.String;
      To   : in out CORBA.Object.Ref'Class)
   is
      Ref_Info : constant Droopi.Smart_Pointers.Entity_Ptr
        := new Object.Reference_Info;
   begin
      Object.Reference_Info (Ref_Info.all).IOR
        := Droopi.References.IOR.String_To_Object (From);
      CORBA.Object.Set (To, Ref_Info);
   end String_To_Object;

   ------------------
   -- Work_Pending --
   ------------------

   function Work_Pending return Boolean is
   begin
      return Work_Pending (The_ORB);
   end Work_Pending;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (ORB_Name : in Standard.String)
   is
      My_Policy : Tasking_Policy_Access;
   begin
      if The_ORB /= null then
         raise Initialization_Failure;
      end if;
      My_Policy := new No_Tasking;
      --  ??? Must implement other policies !!

      The_ORB := new Droopi.ORB.ORB_Type (My_Policy);
      Droopi.ORB.Create (The_ORB.all);
   end Initialize;

   ----------------------
   -- Create_Reference --
   ----------------------

   function Create_Reference (Object : in CORBA.Object.Ref)
                             return Droopi.References.Ref
   is
      Result : Droopi.References.Ref;

      Oid    : Droopi.Objects.Object_Id_Access
        := new Droopi.Objects.Object_Id'
        (CORBA.Object.To_Droopi_Object (Object));
   begin
      if The_ORB = null then
         raise Internal;
      end if;

      Droopi.ORB.Create_Reference
        (The_ORB,
         Oid,
         Result);

      return Result;
   end Create_Reference;

end CORBA.ORB;

