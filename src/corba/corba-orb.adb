-- The following subprograms still have to be implemented :
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
-- Object_To_String --
-- Perform_Work --
-- Resolve_Initial_References --
-- Run --
-- Shutdown --
-- String_To_Object --
-- Work_Pending --

package body CORBA.ORB is

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
      return create_alias_tc (Id, Name, Original_Type);
   end create_alias_tc;

   ---------------------
   -- create_array_tc --
   ---------------------

   function create_array_tc
     (Length       : in CORBA.Unsigned_long;
      Element_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object
   is
   begin
      return create_array_tc (Length, Element_Type);
   end create_array_tc;

   ---------------------
   -- create_fixed_tc --
   ---------------------

   function create_fixed_tc
     (IDL_Digits : in CORBA.Unsigned_short;
      scale      : in CORBA.Short)
      return CORBA.TypeCode.Object
   is
   begin
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
      null;
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
     (Bound  : in CORBA.Unsigned_long;
      Offset : in CORBA.Unsigned_long)
      return CORBA.TypeCode.Object
   is
   begin
      return create_recursive_sequence_tc (Bound, Offset);
   end create_recursive_sequence_tc;

   ------------------------
   -- create_sequence_tc --
   ------------------------

   function create_sequence_tc
     (Bound        : in CORBA.Unsigned_long;
      Element_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object
   is
   begin
      return create_sequence_tc (Bound, Element_Type);
   end create_sequence_tc;

   ----------------------
   -- create_string_tc --
   ----------------------

   function create_string_tc
     (Bound : in CORBA.Unsigned_long)
      return CORBA.TypeCode.Object
   is
   begin
      return create_string_tc (Bound);
   end create_string_tc;

   -----------------------
   -- create_wstring_tc --
   -----------------------

   function create_wstring_tc
     (Bound : in CORBA.Unsigned_long)
      return CORBA.TypeCode.Object
   is
   begin
      return create_wstring_tc (Bound);
   end create_wstring_tc;

   -------------------------
   -- Get_Default_Context --
   -------------------------

   function Get_Default_Context
      return CORBA.Context.Ref
   is
   begin
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
      null;
   end Get_Service_Information;

   ---------------------------
   -- List_Initial_Services --
   ---------------------------

   function List_Initial_Services return ObjectIdList is
   begin
      return List_Initial_Services;
   end List_Initial_Services;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
      return CORBA.String
   is
   begin
      return Object_To_String (Obj);
   end Object_To_String;

   ------------------
   -- Perform_Work --
   ------------------

   procedure Perform_Work is
   begin
      null;
   end Perform_Work;

   --------------------------------
   -- Resolve_Initial_References --
   --------------------------------

   function Resolve_Initial_References
     (Identifier : ObjectId)
      return CORBA.Object.Ref
   is
   begin
      return Resolve_Initial_References (Identifier);
   end Resolve_Initial_References;

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      null;
   end Run;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Wait_For_Completion : in Boolean) is
   begin
      null;
   end Shutdown;

   ----------------------
   -- String_To_Object --
   ----------------------

   procedure String_To_Object
     (From : in     CORBA.String;
      To   : in out CORBA.Object.Ref'Class)
   is
   begin
      null;
   end String_To_Object;

   ------------------
   -- Work_Pending --
   ------------------

   function Work_Pending return Boolean is
   begin
      return Work_Pending;
   end Work_Pending;

end CORBA.ORB;

