--  This package corresponds to the CORBA 2.0 specification. It contains
--  the definition of type CORBA.Object.Ref, which is the base class of all
--  proxy objects

with Ada.Exceptions;
with Ada.Strings.Unbounded;

with Interfaces.C.Strings;

with AdaBroker.IOP;
with AdaBroker.OmniObject;
with AdaBroker.OmniRopeAndKey;
with AdaBroker.Debug;
pragma Elaborate (AdaBroker.Debug);

use AdaBroker.OmniRopeAndKey;

package body CORBA.Object is

   Flag : constant Natural := AdaBroker.Debug.Is_Active ("corba.object");
   procedure O is new AdaBroker.Debug.Output (Flag);

   use type OmniObject.Object_Ptr;

   procedure Create_Ref
     (Most_Derived_Repository : in CORBA.String;
      Profiles                : in IOP.Tagged_Profile_List;
      Release                 : in CORBA.Boolean;
      To                      : in out Ref'Class);

   function Get_Profile_List
     (Obj : in Ref'Class)
      return IOP.Tagged_Profile_List;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Self : in Ref) return Boolean is
   begin
      return  Self.OmniObj = null;
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
      return OmniObject.Non_Existent (Self.OmniObj.all);
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
      OmniObject.Get_Rope_And_Key (Self.OmniObj.all,  Rak.Real, S1);
      OmniObject.Get_Rope_And_Key (Other.OmniObj.all, Other_Rak.Real, S2);

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
      return OmniObject.Hash (Self.OmniObj.all, Maximum);
   end Hash;

   -----------
   -- To_Ref --
   ------------

--    function To_Ref (The_Ref : in Ref'Class) return Ref is
--    begin
--       return Ref (The_Ref);
--    end To_Ref;


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
     (Repository : in CORBA.String;
      Dyn_Type   : in CORBA.Object.Constant_Ref_Ptr)
   is
      Tmp : Cell_Ptr;
   begin
      pragma Debug (O ("Registering " & To_Standard_String (Repository) &
                       " in the dynamic type list"));

      Tmp  := new Cell'(Repository, Dyn_Type, List);
      List := Tmp;
   end Register;

   -----------------------------------------
   -- Get_Dynamic_Type_From_Repository_Id --
   -----------------------------------------

   function Get_Dynamic_Type_From_Repository_Id
     (Repository : in CORBA.String)
      return CORBA.Object.Constant_Ref_Ptr
   is
      Tmp : Cell_Ptr := List;
   begin
      loop
         if Tmp = null then
            Ada.Exceptions.Raise_Exception
              (AdaBroker_Fatal_Error'Identity,
               "CORBA.Get_Dynamic_Type_From_Repository_Id" & CORBA.CRLF &
               "No match found for " &
               CORBA.To_Standard_String (Repository));

         elsif Tmp.all.ID = Repository then
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
      return Self.OmniObj;
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
      pragma Debug (O ("Internal_Copy (Ref) : enter"));

      Finalize (To);
      To.OmniObj      := From.OmniObj;
      To.Dynamic_Type := From.Dynamic_Type;

      pragma Debug (O ("Internal_Copy (Ref) : adjust"));

      Adjust (To);

      pragma Debug (O ("Internal_Copy (Ref) : leave"));
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
      pragma Debug (O ("Internal_Copy (Impl) : enter"));

      Finalize (To);
      To.OmniObj      := OmniObject.Get_Object_Ptr (From);
      To.Dynamic_Type := Dyn_Type;

      pragma Debug (O ("Internal_Copy (Impl) : adjust"));

      Adjust (To);

      pragma Debug (O ("Internal_Copy (Impl) : leave"));
   end Internal_Copy;

   ---------------------------------------------
   -- registering new interfaces into the ORB --
   ---------------------------------------------

   -----------------------------------
   -- C_Create_Proxy_Object_Factory --
   -----------------------------------

   procedure C_Create_Proxy_Object_Factory
     (Repository : in Interfaces.C.Strings.chars_ptr);
   pragma Import
     (CPP, C_Create_Proxy_Object_Factory, "createProxyObjectFactory__FPCc");
   --  Corresponds to void createProxyObjectFactory(const char* repoID) see
   --  proxyObjectFactory_C2Ada.hh

   ---------------------------------
   -- Create_Proxy_Object_Factory --
   ---------------------------------

   procedure Create_Proxy_Object_Factory
     (Repository : in CORBA.String)
   is
      C_Repository : Interfaces.C.Strings.chars_ptr;
      Tmp          : Standard.String := CORBA.To_Standard_String (Repository);
   begin
      C_Repository := Interfaces.C.Strings.New_String (Tmp);

      --  Never deallocated because it is stored in a global variable in
      --  omniORB (proxyStubs)
      C_Create_Proxy_Object_Factory (C_Repository);
   end Create_Proxy_Object_Factory;

   ---------------------------
   -- Marshalling operators --
   ---------------------------

   ----------------
   -- Create_Ref --
   ----------------

   procedure Create_Ref
     (Most_Derived_Repository : in CORBA.String;
      Profiles            : in IOP.Tagged_Profile_List;
      Release             : in CORBA.Boolean;
      To                  : in out Ref'Class)
   is
      Most_Derived_Type : Constant_Ref_Ptr;
   begin
      --  Check if the omniobject we got can be put into To (type implied
      --  the Repository)

      Most_Derived_Type :=
        Get_Dynamic_Type_From_Repository_Id (Most_Derived_Repository);

      --  Most_Derived_Type is now an object of the most derived type of
      --  the new created object

      if Is_A (Most_Derived_Type.all, Get_Repository_Id (To)) then

         --  Get the OmniObject
         To.OmniObj := OmniObject.Create_OmniObject
           (Most_Derived_Repository,
            Profiles,
            Release);

         --  If the result is correct
         if To.OmniObj /= null then
            To.Dynamic_Type :=
              Get_Dynamic_Type_From_Repository_Id (Most_Derived_Repository);
            return;
         end if;
      end if;

      --  The operation is illegal return Nil_Ref in the right class
      pragma Debug (O ("Create_Ref : cannot put a " &
                       CORBA.To_Standard_String (Most_Derived_Repository) &
                       " in a " &
                       CORBA.To_Standard_String (Get_Repository_Id (To))));
      To.OmniObj      := null;
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
      return OmniObject.Get_Profile_List (Obj.OmniObj.all);
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
      return OmniObject.Align_Size (Obj.OmniObj, Initial_Offset);
   end Align_Size;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Obj : in Ref'Class;
      S   : in out NetBufferedStream.Object'Class) is
   begin
      --  Calls the corresponding function on the underlying omniobject
      OmniObject.Marshall (Obj.OmniObj, S);
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
      OmniObject.Marshall (Obj.OmniObj, S);
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Obj : out Ref'Class;
      S   : in out NetBufferedStream.Object'Class)
   is
      use Ada.Strings.Unbounded;

      Repository : CORBA.String;
      List       : IOP.Tagged_Profile_List;
      Tmp        : Ref'Class := CORBA.Object.Nil_Ref;
   begin
      pragma Debug (O ("Unmarshall : unmarshalling " &
                       To_Standard_String (Get_Repository_Id (Obj))));

      --  First unmarshall the Repository
      NetBufferedStream.Unmarshall (Repository, S);

      pragma Debug (O ("Unmarshall : found " &
                       To_Standard_String (Repository)));

      --  Then the profile list
      IOP.Unmarshall (List, S);
      pragma Debug (O ("Unmarshall : IOP_List unmarshalled"));

      --  And at last create the object reference to be returned
      if IOP.Length (List) = 0
        and then Length (Unbounded_String (Repository)) = 0
      then
         pragma Debug (O ("Unmarshall : Nil Ref created"));

         --  Either a nil object reference
         Obj := Tmp;
      else
         pragma Debug (O ("Unmarshall : create Ref enter"));

         --  Or a real object reference
         CORBA.Object.Create_Ref (Repository, List, True, Obj);

         pragma Debug (O ("Unmarshall : create Ref leave"));

         if Is_Nil (Obj) then
            pragma Debug (O ("Unmarshall : Nil Ref created"));
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
      use Ada.Strings.Unbounded;

      Repository : CORBA.String;
      List       : IOP.Tagged_Profile_List;
      Tmp        : Ref'Class := CORBA.Object.Nil_Ref;
   begin
      --  First unmarshall the Repo_Id
      MemBufferedStream.Unmarshall (Repository, S);

      --  Then the profile list
      IOP.Unmarshall (List, S);

      --  at last create the object reference to be returned
      if IOP.Length (List) = 0
        and then Length (Unbounded_String (Repository)) = 0
      then
         --  a nil object reference
         Obj := Tmp;
      else
         --  a real object reference
         CORBA.Object.Create_Ref (Repository, List, True, Obj);
      end if;
   end Unmarshall;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Ref) is
   begin
      pragma Debug (O ("CORBA.Object.Initialize"));
      Self.OmniObj      := null;
      Self.Dynamic_Type := null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Ref) is
   begin
      pragma Debug (O ("adjust : enter"));
      if not Is_Nil (Self) then
         pragma Debug (O ("adjust : not nil -> duplicating"));

         Self.OmniObj := OmniObject.OmniObject_Duplicate (Self.OmniObj);
      end if;
      pragma Debug (O ("adjust : leave"));
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Ref) is
   begin
      pragma Debug (O ("finalize : enter"));

      if not Is_Nil (Self) then
         pragma Debug (O ("finalize : finalize non nil Ref"));

         OmniObject.OmniObject_Destructor (Self.OmniObj);

         Self.OmniObj      := null;
         Self.Dynamic_Type := null;
      else
         pragma Debug (O ("finalize : cannot finalize nil ref"));
         null;
      end if;
      pragma Debug (O ("finalize : leave"));
   end Finalize;

begin

   Register (Repository_Id, Nil_Ref'Access);
   --  Registers the fact that a new IDL interface : the root of all the
   --  others can be used in the program

end CORBA.Object;
