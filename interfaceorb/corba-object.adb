--  This package corresponds to the CORBA 2.0 specification. It contains
--  the definition of type CORBA.Object.Ref, which is the base class of all
--  proxy objects

with Ada.Exceptions;

with Interfaces.C.Strings;

with IOP;
with OmniObject;
with OmniRopeAndKey; use OmniRopeAndKey;

with Adabroker_Debug;
use Adabroker_Debug;
pragma Elaborate (Adabroker_Debug);

package body CORBA.Object is

   Debug : constant Boolean := Adabroker_Debug.Is_Active ("corba.object");

   use type OmniObject.Object_Ptr;

   procedure Create_Ref
     (Most_Derived_Repoid : in CORBA.String;
      Profiles            : in IOP.Tagged_Profile_List;
      Release             : in CORBA.Boolean;
      To                  : in out Ref'Class);

   function Get_Dynamic_Type_From_Repository_Id
     (Repoid : in CORBA.String)
      return CORBA.Object.Constant_Ref_Ptr;

   function Get_Profile_List
     (Obj : in Ref'Class)
      return IOP.Tagged_Profile_List;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Self : in Ref) return Boolean is
   begin
      return  Self.Omniobj = null;
   end Is_Nil;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Ref) is
   begin
      Finalize (Self);
   end Release;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : in Ref;
      Logical_Type_Id : in CORBA.String)
      return CORBA.Boolean is
   begin
      return (Repository_Id = Logical_Type_Id);
   end Is_A;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Logical_Type_Id : in CORBA.String)
      return CORBA.Boolean is
   begin
      return (Repository_Id = Logical_Type_Id);
   end Is_A;

   ------------------
   -- Non_Existent --
   ------------------

   function Non_Existent (Self : in Ref) return CORBA.Boolean is
   begin
      if Is_Nil (Self) then
         return True;
      end if;
      return OmniObject.Non_Existent (Self.Omniobj.all);
   end Non_Existent;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent
     (Self  : in Ref;
      Other : in Ref)
      return CORBA.Boolean
   is
      Rak       : OmniRopeAndKey.Controlled_Wrapper;
      Other_Rak : OmniRopeAndKey.Controlled_Wrapper;
      S1, S2    : CORBA.Boolean;
   begin
      --  This is copied from corbaObject.cc L160. Here, Refs are proxy
      --  objects
      OmniObject.Get_Rope_And_Key (Self.Omniobj.all,  Rak.Real, S1);
      OmniObject.Get_Rope_And_Key (Other.Omniobj.all, Other_Rak.Real, S2);

      return S1 and S2 and (Rak.Real = Other_Rak.Real);
   end Is_Equivalent;

   ----------
   -- Hash --
   ----------

   function Hash
     (Self    : in Ref;
      Maximum : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
   begin
      if Is_Nil (Self) then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Cannot call function hash on a nil reference");
      end if;
      return OmniObject.Hash (Self.Omniobj.all, Maximum);
   end Hash;

   -----------
   -- To_Ref --
   ------------

   function To_Ref (The_Ref : in Ref'Class) return Ref is
   begin
      return Ref (The_Ref);
   end To_Ref;


   -------------------------------
   -- dynamic typing of objects --
   -------------------------------

   type Cell;
   type Cell_Ptr is access all Cell;
   type Cell is record
      ID    : CORBA.String;
      Value : CORBA.Object.Constant_Ref_Ptr;
      Next  : Cell_Ptr;
   end record;
   List : Cell_Ptr := null;

   --  Warning : should be protected.

   procedure Free is new Ada.Unchecked_Deallocation (Cell, Cell_Ptr);

   --------------
   -- Register --
   --------------

   procedure Register
     (Repoid   : in CORBA.String;
      Dyn_Type : in CORBA.Object.Constant_Ref_Ptr)
   is
      Tmp : Cell_Ptr;
   begin
      pragma Debug
        (Output (Debug,
          "Registering " & To_Standard_String (Repoid) &
                 " in the dynamic type list"));

      Tmp  := new Cell'(Repoid, Dyn_Type, List);
      List := Tmp;
   end Register;

   -----------------------------------------
   -- Get_Dynamic_Type_From_Repository_Id --
   -----------------------------------------

   function Get_Dynamic_Type_From_Repository_Id
     (Repoid : in CORBA.String)
      return CORBA.Object.Constant_Ref_Ptr
   is
      Tmp : Cell_Ptr := List;
   begin
      loop
         if Tmp = null then
            Ada.Exceptions.Raise_Exception
              (AdaBroker_Fatal_Error'Identity,
               "CORBA.Get_Dynamic_Type_From_Repository_Id" &
               CORBA.CRLF &
               "No match found for " &
               CORBA.To_Standard_String (Repoid));

         elsif Tmp.all.ID = Repoid then
            return Tmp.all.Value;
         else
            Tmp := Tmp.all.Next;
         end if;
      end loop;
   end Get_Dynamic_Type_From_Repository_Id;

   -------------------------
   -- AdaBroker  specific --
   -------------------------

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Ref_Ptr) is
   begin
      Private_Free (Self);
   end Free;

   -----------------------
   -- Get_Repository_Id --
   -----------------------

   function Get_Repository_Id
     (Self : in Ref) return CORBA.String is
   begin
      return Repository_Id;
   end Get_Repository_Id;

   ------------------------
   -- Get_OmniObject_Ptr --
   ------------------------

   function Get_OmniObject_Ptr
     (Self : in Ref'Class)
      return OmniObject.Object_Ptr
   is
   begin
      return Self.Omniobj;
   end Get_OmniObject_Ptr;

   ----------------------
   -- Get_Dynamic_Type --
   ----------------------

   function Get_Dynamic_Type
     (Self : in Ref)
      return Ref'Class is
   begin
      if Is_Nil (Self) then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Get_Dynamic_Type should not be called on a nil reference" &
            CORBA.CRLF &
            "Use Is_nil first to check your object is not a nil reference");
      else
         return Self.Dynamic_Type.all;
      end if;
   end Get_Dynamic_Type;

   -------------------
   -- Internal_Copy --
   -------------------

   procedure Internal_Copy
     (From : in Ref'Class;
      To   : in out Ref'Class) is
   begin
      pragma Debug
        (Output (Debug, "CORBA.Object.Internal_Copy(Ref) : entering ..."));

      Finalize (To);
      To.Omniobj      := From.Omniobj;
      To.Dynamic_Type := From.Dynamic_Type;

      pragma Debug
        (Output (Debug, "CORBA.Object.Internal_Copy(Ref) : adjusting ..."));

      Adjust (To);

      pragma Debug
        (Output (Debug, "CORBA.Object.Internal_Copy(Ref) : exiting ... OK"));
   end Internal_Copy;

   -------------------
   -- Internal_Copy --
   -------------------

   procedure Internal_Copy
     (From     : in OmniObject.Implemented_Object'Class;
      Dyn_Type : in Constant_Ref_Ptr;
      To       : in out Ref'Class)
   is
   begin
      pragma Debug
        (Output (Debug, "CORBA.Object.Internal_Copy(Impl) : entering ..."));

      Finalize (To);
      To.Omniobj      := OmniObject.Get_Object_Ptr (From);
      To.Dynamic_Type := Dyn_Type;

      pragma Debug
        (Output (Debug, "CORBA.Object.Internal_Copy(Impl) : adjusting ..."));

      Adjust (To);

      pragma Debug
        (Output (Debug, "CORBA.Object.Internal_Copy(Impl) : exiting ... OK"));
   end Internal_Copy;

   ---------------------------------------------
   -- registering new interfaces into the ORB --
   ---------------------------------------------

   -----------------------------------
   -- C_Create_Proxy_Object_Factory --
   -----------------------------------

   procedure C_Create_Proxy_Object_Factory
     (Repoid : in Interfaces.C.Strings.chars_ptr);
   pragma Import
     (CPP, C_Create_Proxy_Object_Factory, "createProxyObjectFactory__FPCc");
   --  Corresponds to void createProxyObjectFactory(const char* repoID) see
   --  proxyObjectFactory_C2Ada.hh

   ---------------------------------
   -- Create_Proxy_Object_Factory --
   ---------------------------------

   procedure Create_Proxy_Object_Factory
     (Repoid : in CORBA.String)
   is
      C_Repoid : Interfaces.C.Strings.chars_ptr;
      Tmp      : Standard.String := CORBA.To_Standard_String (Repoid);
   begin
      C_Repoid := Interfaces.C.Strings.New_String (Tmp);

      --  Never deallocated because it is stored in a global variable in
      --  omniORB (proxyStubs)
      C_Create_Proxy_Object_Factory (C_Repoid);
   end Create_Proxy_Object_Factory;

   ---------------------------
   -- Marshalling operators --
   ---------------------------

   ----------------
   -- Create_Ref --
   ----------------

   procedure Create_Ref
     (Most_Derived_Repoid : in CORBA.String;
      Profiles            : in IOP.Tagged_Profile_List;
      Release             : in CORBA.Boolean;
      To                  : in out Ref'Class)
   is
      Most_Derived_Type : Constant_Ref_Ptr;
   begin
      --  Check if the omniobject we got can be put into To (type implied
      --  the Repoid)

      Most_Derived_Type :=
        Get_Dynamic_Type_From_Repository_Id (Most_Derived_Repoid);

      --  Most_Derived_Type is now an object of the most derived type of
      --  the new created object

      if Is_A (Most_Derived_Type.all, Get_Repository_Id (To)) then

         --  Get the OmniObject
         To.Omniobj := OmniObject.Create_OmniObject
           (Most_Derived_Repoid,
            Profiles,
            Release);

         --  If the result is correct
         if To.Omniobj /= null then
            To.Dynamic_Type :=
              Get_Dynamic_Type_From_Repository_Id (Most_Derived_Repoid);
            return;
         end if;
      end if;

      --  The operation is illegal return Nil_Ref in the right class
      pragma Debug
        (Output (Debug,
                 "CORBA.Object.Create_Ref : cannot put a " &
                 CORBA.To_Standard_String (Most_Derived_Repoid) &
                 "In a " & CORBA.To_Standard_String (Get_Repository_Id (To))));
      To.Omniobj      := null;
      To.Dynamic_Type := null;
   end Create_Ref;

   ----------------------
   -- Get_Profile_List --
   ----------------------

   function Get_Profile_List
     (Obj : in Ref'Class)
      return IOP.Tagged_Profile_List
   is
   begin
      return OmniObject.Get_Profile_List (Obj.Omniobj.all);
      --  Calls the corresponding function on the underlying omniobject
   end Get_Profile_List;

   ----------------
   -- Align_Size --
   ----------------

   function Align_Size
     (Obj            : in Ref'Class;
      Initial_Offset : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
   begin
      --  Calls the corresponding function on the underlying omniobject
      return OmniObject.Align_Size (Obj.Omniobj, Initial_Offset);
   end Align_Size;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Obj : in Ref'Class;
      S   : in out NetBufferedStream.Object'Class) is
   begin
      --  Calls the corresponding function on the underlying omniobject
      OmniObject.Marshall (Obj.Omniobj, S);
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Obj : in Ref'Class;
      S   : in out MemBufferedStream.Object'Class)
   is
   begin
      --  Calls the corresponding function on the underlying omniobject
      OmniObject.Marshall (Obj.Omniobj, S);
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Obj : out Ref'Class;
      S   : in out NetBufferedStream.Object'Class)
   is
      Repoid : CORBA.String;
      List   : IOP.Tagged_Profile_List;
      Tmp    : Ref'Class := CORBA.Object.Nil_Ref;
   begin
      pragma Debug
        (Output (Debug,
                 "CORBA.Object.Unmarshall : unmarshalling " &
                 To_Standard_String (Get_Repository_Id (Obj))));

      --  First unmarshall the Repoid
      NetBufferedStream.Unmarshall (Repoid, S);

      pragma Debug
        (Output (Debug,
                 "CORBA.Object.Unmarshall : found " &
                 To_Standard_String (Repoid)));

      --  Then the profile list
      IOP.Unmarshall (List, S);
      pragma Debug
        (Output (Debug,
                 "CORBA.Object.Unmarshall : IOP_List unmarshalled"));

      --  And at last create the object reference to be returned
      if IOP.Length (List) = CORBA.Unsigned_Long (0)
        and then CORBA.Length (Repoid) = CORBA.Unsigned_Long (0)
      then
         pragma Debug (Output (Debug,
                               "CORBA.Object.Unmarshall : Nil Ref created"));

         --  Either a nil object reference
         Obj := Tmp;
      else
         pragma Debug (Output (Debug,
                               "CORBA.Object.Unmarshall : Creating Ref"));

         --  Or a real object reference
         CORBA.Object.Create_Ref (Repoid, List, True, Obj);

         pragma Debug (Output (Debug,
                               "CORBA.Object.Unmarshall : Ref created"));
         if Is_Nil (Obj) then
            pragma Debug
              (Output (Debug,
                       "CORBA.Object.Unmarshall : WARNING : Created Nil Ref"));
            null;
         end if;
      end if;
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Obj : out Ref'Class;
      S   : in out MemBufferedStream.Object'Class) is
      Repoid : CORBA.String;
      List   : IOP.Tagged_Profile_List;
      Tmp    : Ref'Class := CORBA.Object.Nil_Ref;
   begin
      --  First unmarshall the Repo_Id
      MemBufferedStream.Unmarshall (Repoid, S);

      --  Then the profile list
      IOP.Unmarshall (List, S);

      --  at last create the object reference to be returned
      if IOP.Length (List) = CORBA.Unsigned_Long (0)
        and then CORBA.Length (Repoid) = CORBA.Unsigned_Long (0)
      then
         --  a nil object reference
         Obj := Tmp;
      else
         --  a real object reference
         CORBA.Object.Create_Ref (Repoid, List, True, Obj);
      end if;
   end Unmarshall;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Ref) is
   begin
      pragma Debug (Output (Debug, "CORBA.Object.Initialize"));
      Self.Omniobj      := null;
      Self.Dynamic_Type := null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Ref) is
   begin
      pragma Debug (Output (Debug, "CORBA.Object.Adjust : entering ..."));
      if not Is_Nil (Self) then
         pragma Debug
           (Output (Debug, "CORBA.Object.Adjust : not nil -> duplicating"));

         Self.Omniobj := OmniObject.OmniObject_Duplicate (Self.Omniobj);
      end if;
      pragma Debug (Output (Debug, "CORBA.Object.Adjust : exiting ... OK"));
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Ref) is
   begin
      pragma Debug (Output (Debug, "CORBA.Object.Finalize : start"));

      if not Is_Nil (Self) then
         pragma Debug
           (Output (Debug,
                    "CORBA.Object.Finalize : finalizing non nil Ref"));

         OmniObject.OmniObject_Destructor (Self.Omniobj);

         Self.Omniobj      := null;
         Self.Dynamic_Type := null;
      else
         pragma Debug
           (Output (Debug,
                    "CORBA.Object.Finalize : cannot finalize nil ref"));
         null;
      end if;
      pragma Debug (Output (Debug, "CORBA.Object.Finalize : done"));
   end Finalize;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (Self : in Ref'Class)
      return CORBA.String is
   begin
      if Is_Nil (Self) then
         return OmniObject.Object_To_String (null);
      else
         return OmniObject.Object_To_String (Self.Omniobj);
      end if;
   end Object_To_String;

   ----------------------
   -- String_To_Object --
   ----------------------

   procedure String_To_Object
     (From : in CORBA.String;
      To   : out CORBA.Object.Ref'class)
   is
      Repoid            : CORBA.String;
      Most_Derived_Type : Constant_Ref_Ptr;
   begin
      --  Get the omniobject
      To.Omniobj := OmniObject.String_To_Object (From);

      --  If the result is correct
      if To.Omniobj /= null then

         --  Just print a message to tell if the C++ object is a proxy or a
         --  local object

         if  OmniObject.Is_Proxy (To.Omniobj.all) then
            pragma Debug
              (Output (Debug,
                       "CORBA.Object.String_To_Object :" &
                       " creating Ref with local object inside"));
            null;
         else
            pragma Debug
              (Output (Debug,
                       "CORBA.Object.String_To_Object :" &
                       " creating Ref with local object inside"));
            null;
         end if;

         --  Check if the omniobject we got can be put into To (type
         --  implied the repoId)
         Repoid := OmniObject.Get_Repository_Id (To.Omniobj.all);

         pragma Debug
           (Output (Debug,
                    "CORBA.Object.String_To_Object : repoid = "
                    & CORBA.To_Standard_String (Repoid)));

         Most_Derived_Type :=
           Get_Dynamic_Type_From_Repository_Id (Repoid);

         --  Dyn_type is now an object of the most derived type of the new
         --  created object

         if Is_A (Most_Derived_Type.all, Get_Repository_Id (To)) then
            To.Dynamic_Type := Most_Derived_Type;
            return;
         end if;
      end if;

      --  Otherwise, the operation is illegal return Nil_Ref in the right
      --  class

      To.Omniobj      := null;
      To.Dynamic_Type := null;
   end String_To_Object;

begin

   Register (Repository_Id, Nil_Ref'Access);
   --  Registers the fact that a new IDL interface : the root of all the
   --  others can be used in the program

end CORBA.Object;
