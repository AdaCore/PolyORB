
--  This package is wrapped around a C++ class whose name is
--  Ada_OmniObject.  It provides two types of methods : the C functions of
--  the Ada_OmniObject class and their equivalent in Ada. (he first ones
--  have a C_ prefix.)  In addition, there is a raise_ada_exception
--  function that allows C functions to raise the ada No_Initialisation
--  exception.

with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Characters.Latin_1;
use  Ada.Characters;

with System;

with CORBA;

with Constants;
with CORBA.Exceptions;
with Omni;
with Giop;
with Giop_S;

with Adabroker_Debug;
 use Adabroker_Debug;

package body OmniObject is

   use type System.Address;
   use type CORBA.String;
   use type CORBA.Unsigned_Long;

   Omniobject : constant Boolean := Adabroker_Debug.Is_Active("omniobject");

   --  Implemented_Object this is the type of local implementations of
   --  objects it is the root of all XXX.Impl.Object

   -------------------
   -- miscellaneous --
   -------------------

   ------------
   -- C_Init --
   ------------

   procedure C_Init_Local_Object
     (Self : in out Object'Class;
      Repoid : in Interfaces.C.Strings.Chars_Ptr);

   pragma Import
     (CPP, C_Init_Local_Object, "initLocalObject__14Ada_OmniObjectPCc");
   --  Wrapper around Ada_OmniObject::initLocalObject (see
   --  Ada_OmniObject.hh)

   -----------------------
   -- Init_Local_Object --
   -----------------------

   procedure Init_Local_Object
     (Self   : in out Implemented_Object;
      Repoid : in CORBA.String;
      Disp   : in Dispatch_Procedure;
      Isa    : in Is_A_Function)
   is
      C_Repoid : Interfaces.C.Strings.Chars_Ptr;
   begin
      pragma Debug (Output (Omniobject, "Omniobject.Init_Local_Object"));
      if not Is_Nil (Self) then
         C_Repoid := Interfaces.C.Strings.New_String
           (CORBA.To_Standard_String (Repoid));
         C_Init_Local_Object (Self.Omniobj.all, C_Repoid);
         Interfaces.C.Strings.Free (C_Repoid);
         Self.Dispatch := Disp;
         Self.Is_A := Isa;
      else
         Ada.Exceptions.Raise_Exception
           (CORBA.AdaBroker_Fatal_Error'Identity,
            "Omniobject.Init(Implemented_Object, CORBA.String" & CORBA.CRLF &
            "Cannot be called on a nil object");
      end if;
   end Init_Local_Object;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Self : in Implemented_Object) return CORBA.Boolean is
   begin
      return Self.Omniobj = null;
   end Is_Nil;

   -----------------------
   -- Get_Repository_Id --
   -----------------------

   function Get_Repository_Id
     (Self : in Implemented_Object)
      return CORBA.String is
   begin
      if Is_Nil (Self) then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Get_Repository_Id cannot be invoked on a nil object");
      end if;
      return Get_Repository_Id (Self.Omniobj.all);
   end Get_Repository_Id;

   ----------
   -- Hash --
   ----------

   function Hash
     (Self    : in Implemented_Object;
      Maximum : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long is
   begin
      if Is_Nil (Self) then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Cannot call hash on a nil object");
      end if;
      return Hash (Self.Omniobj.all, Maximum);
   end Hash;

   --------------------
   -- Get_Object_Ptr --
   --------------------

   function Get_Object_Ptr
     (Self : in Implemented_Object)
      return Object_Ptr is
   begin
      return Self.Omniobj;
   end Get_Object_Ptr;

   ------------------------------
   -- registering into the ORB --
   ------------------------------

   ---------------------
   -- Object_Is_Ready --
   ---------------------

   procedure Object_Is_Ready
     (Self : in Implemented_Object'Class) is
   begin
      if not Is_Nil (Self) then
         Omniobject_Is_Ready (Self.Omniobj);
      else
         Ada.Exceptions.Raise_Exception
           (CORBA.Adabroker_Fatal_Error'Identity,
            "Omniobject.Object_Is_Ready(Implemented_Object)" & CORBA.CRLF
            & "Cannot be called on nil object");
      end if;
   end Object_Is_Ready;

   --------------------
   -- Dispose_Object --
   --------------------

   procedure Dispose_Object
     (Self : in Implemented_Object'Class)
   is
   begin
      if not Is_Nil (Self) then
         Omniobject_Dispose (Self.Omniobj);
      else
         Ada.Exceptions.Raise_Exception
           (CORBA.Adabroker_Fatal_Error'Identity,
            "Omniobject.Dispose_Object(Implemented_Object)" & CORBA.CRLF
            & "Cannot be called on nil object");
      end if;
   end Dispose_Object;

   --------------------
   -- object <-> IOR --
   --------------------

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (Self : in Implemented_Object'class)
      return CORBA.String
   is
   begin
      if Is_Nil (Self) then
         return Object_To_String (null);
      else
         return Object_To_String(Self.Omniobj);
      end if;
   end Object_To_String;

   ---------------------------
   -- marshalling operators --
   ---------------------------

   ----------------
   -- Align_Size --
   ----------------

   function Align_Size
     (Obj            : in Implemented_Object_Ptr;
      Initial_Offset : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
   begin
      if Obj = null then
         --  Never reached normally
         Ada.Exceptions.Raise_Exception
           (CORBA.AdaBroker_Fatal_Error'Identity ,
            "Null pointer argument in function Align_Size in corba-object.");
      else
         --  Calls the corresponding function on the underlying omniobject
         return Align_Size (Obj.all.Omniobj, Initial_Offset);
      end if;
   end Align_Size;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Obj : in Implemented_Object_Ptr;
      S   : in out NetBufferedStream.Object'Class) is
   begin
      if Obj = null then
         --  Never reached normally
         Ada.Exceptions.Raise_Exception
           (CORBA.AdaBroker_Fatal_Error'Identity ,
            "Null pointer argument in procedure Marshall in corba-object.");
      else
         --  Calls the corresponding function on the underlying omniobject
         Marshall (Obj.all.Omniobj, S);
      end if;
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Obj : in Implemented_Object_Ptr;
      S   : in out MemBufferedStream.Object'Class)
   is
   begin
      if Obj = null then
         --  Never reached normally
         Ada.Exceptions.Raise_Exception
           (CORBA.AdaBroker_Fatal_Error'Identity ,
            "Null pointer argument in procedure Marshall in corba-object.");
      else
         --  Calls the corresponding function on the underlying omniobject
         Marshall (Obj.all.Omniobj, S);
      end if;
   end Marshall;

   --  Omniobject this type is imported from C++ it is the equivalent of
   --  omniObject

   ---------------------------
   -- marshalling operators --
   ---------------------------

   ----------------
   -- Align_Size --
   ----------------

   function Align_Size
     (Obj            : in Object_Ptr;
      Initial_Offset : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
      Offset : CORBA.Unsigned_Long;
   begin
      --  Alignment for type CORBA.unsigned_long
      Offset := Omni.Align_To (Initial_Offset, Omni.ALIGN_4);

      if Obj = null then
         --  If object null, the marshall will need 12 Bytes
         return Offset + 12;

      else
         --  If not null, add size of unsigned_long (size of Repo_Id), size
         --  of Repoid itself and size of profiles
         declare
            Repoid : CORBA.String := Get_Repository_Id (Obj.all);
         begin
            Offset := Offset + 4;
            Offset := Offset + CORBA.Length (Repoid) + 1;
            Offset := Iop.Align_Size (Get_Profile_List (Obj.all), Offset);
            return Offset;
         end;
      end if;
   end Align_Size;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Obj : in Object_Ptr;
      S   : in out NetBufferedStream.Object'Class)
   is
   begin
      if Obj = null then

         --  If object null, marshall null object
         NetBufferedStream.Marshall (CORBA.Unsigned_Long (1), S);
         NetBufferedStream.Marshall (CORBA.Char (Latin_1.Nul), S);
         NetBufferedStream.Marshall (CORBA.Unsigned_Long (0), S);
      else
         --  Else marshall the Repo_ID and the iopProfiles
         NetBufferedStream.Marshall (Get_Repository_Id (Obj.all), S);
         Iop.Marshall (Get_Profile_List (Obj.all), S);
      end if;
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Obj : in Object_Ptr;
      S   : in out MemBufferedStream.Object'Class)
   is
   begin
      if Obj = null then
         --  If object null, marshall null object
         MemBufferedStream.Marshall (CORBA.Unsigned_Long (1), S);
         MemBufferedStream.Marshall (CORBA.Char (Latin_1.nul), S);
         MemBufferedStream.Marshall (CORBA.Unsigned_Long (0), S);
      else
         --  Else marshall the Repo_ID and the iopProfiles
         MemBufferedStream.Marshall (Get_Repository_Id (Obj.all), S);
         IOP.Marshall (Get_Profile_List (Obj.all), S);
      end if;
   end Marshall;

   ----------------------------------
   -- constructors and destructors --
   ----------------------------------

   function C_Create_Omniobject
     (Most_Derived_Repoid : in Interfaces.C.Strings.Chars_ptr;
      Profiles            : in System.Address;
      Release             : in Sys_Dep.C_Boolean)
      return System.Address;

   pragma Import
     (CPP, C_Create_Omniobject,
      "ada_create_objref__14Ada_OmniObjectPCcPt25_CORBA_Unbounded_Sequence" &
      "1ZQ23IOP13TaggedProfileb");
   --  Corresponds to Ada_OmniObject::ada_create_objref see
   --  Ada_OmniObject.hh

   -----------------------
   -- Create_Omniobject --
   -----------------------

   function Create_Omniobject
     (Most_Derived_Repoid : in CORBA.String;
      Profiles            : in IOP.Tagged_Profile_List;
      Release             : in CORBA.Boolean)
      return Object_Ptr
   is
      C_Mdr      : Interfaces.C.Strings.Chars_Ptr;
      C_R        : Sys_Dep.C_Boolean;
      C_Profiles : System.Address;
      C_Result   : System.Address;
      Result     : Object_Ptr;
   begin
      C_Mdr := Interfaces.C.Strings.New_String
        (CORBA.To_Standard_String (Most_Derived_Repoid));

      --  Never deallocatd, it will be stored in the object (?? to be
      --  checked ??)
      C_R := Sys_Dep.Boolean_Ada_To_C (Release);
      C_Profiles := System.Address (Profiles);

      C_Result := C_Create_Omniobject (C_Mdr, C_Profiles, C_R);
      Result   :=  To_Object_Ptr (Address_To_Object.To_Pointer (C_Result));

      if Result /= null then
         Result.all.Implobj := null;
         pragma Debug
           (Output (Omniobject,
                    "Omniobject.Create_Omniobject : setting" &
                    " result.all.implobj to null"));
      else
         pragma Debug
           (Output (Omniobject,
                    "Omniobject.Create_Omniobject : WARNING :" &
                    " C++ function returned NULL"));
         null;
      end if;

      return Result;
   end Create_Omniobject;

   ----------------------------
   -- C_Omniobject_Duplicate --
   ----------------------------

   function C_Omniobject_Duplicate
     (Self : in System.Address)
      return System.Address;
   pragma Import
     (CPP, C_Omniobject_Duplicate,
      "objectDuplicate__14Ada_OmniObjectP14Ada_OmniObject");
   --  Calls Ada_OmniObject::objectDuplicate

   --------------------------
   -- Omniobject_Duplicate --
   --------------------------

   function Omniobject_Duplicate
     (Self : in Object_Ptr)
      return Object_Ptr
   is
      C_Result : System.Address;
      C_Arg    : System.Address;
      Result   : Object_Ptr;
   begin
      pragma Debug
        (Output (Omniobject,
                 "Omniobject.Omniobject_Duplicate : entering ..."));

      C_Arg := Address_To_Object.To_Address (From_Object_Ptr (Self));

      pragma Debug
        (Output (Omniobject,
                 "Omniobject.Omniobject_Duplicate : got the C++ pointer," &
                 " calling the C++ function"));

      C_Result := C_Omniobject_Duplicate (C_Arg);

      pragma Debug
        (Output (Omniobject,
                 "Omniobject.Omniobject_Duplicate : got the answer of" &
                 " the C++ function"));

      Result := To_Object_Ptr (Address_To_Object.To_Pointer (C_Result));

      pragma Debug
        (Output (Omniobject,
                 "Omniobject.Omniobject_Duplicate : got the Ada pointer "));

      if Result /= null then
         Result.all.Implobj := null;
      end if;

      pragma Debug
        (Output (Omniobject,
                 "Omniobject.Omniobject_Duplicate : exiting ... OK"));
      return Result;
   end Omniobject_Duplicate;

   -----------------------------
   -- C_Omniobject_Destructor --
   -----------------------------

   procedure C_Omniobject_Destructor (Self : in System.Address);
   pragma Import
     (CPP, C_Omniobject_Destructor,
      "Destructor__14Ada_OmniObjectP14Ada_OmniObject");
   --  Calls Ada_OmniObject::Destructor

   ---------------------------
   -- Omniobject_Destructor --
   ---------------------------

   procedure Omniobject_Destructor (Self : in Object_Ptr) is
   begin
      pragma Debug
        (Output (Omniobject, "Omniobject.Omniobject_Destructor : start"));
      pragma Debug
        (Output (Omniobject,
                 "Omniobject.Omniobject_Destructor : Self = null ? " &
                 Boolean'Image (Self = null)));

      C_Omniobject_Destructor
        (Address_To_Object.To_Address (From_Object_Ptr (Self)));

      pragma Debug (Output (Omniobject,
                            "Omniobject.Omniobject_Destructor : done"));
   end Omniobject_Destructor;

   ------------------------------
   -- registering into the ORB --
   ------------------------------

   ---------------------------
   -- C_Omniobject_Is_Ready --
   ---------------------------

   procedure C_Omniobject_Is_Ready (Self : in System.Address);
   pragma Import
     (CPP, C_Omniobject_Is_Ready, "objectIsReady__14Ada_OmniObject");
   --  Calls Ada_OmniObject::objectIsReady

   -------------------------
   -- Omniobject_Is_Ready --
   -------------------------

   procedure Omniobject_Is_Ready
     (Self : in Object_Ptr)
   is
   begin
      C_Omniobject_Is_Ready
        (Address_To_Object.To_Address (From_Object_Ptr (Self)));
   end Omniobject_Is_Ready;

   --------------------------
   -- C_Omniobject_Dispose --
   --------------------------

   procedure C_Omniobject_Dispose (Self : in System.Address);
   pragma Import
     (CPP, C_Omniobject_Dispose, "disposeObject__14Ada_OmniObject");
   --  Calls Ada_OmniObject::disposeObject

   ------------------------
   -- Omniobject_Dispose --
   ------------------------

   procedure Omniobject_Dispose (Self : in Object_Ptr) is
   begin
      C_Omniobject_Dispose
        (Address_To_Object.To_Address (From_Object_Ptr (Self)));
   end Omniobject_Dispose;


   --------------------
   -- object <-> IOR --
   --------------------

   ------------------------
   -- C_String_To_Object --
   ------------------------

   function C_String_To_Object
     (Repoid : in Interfaces.C.Strings.Chars_Ptr)
      return System.Address;
   pragma Import
     (CPP, C_String_To_Object, "string_to_ada_object__14Ada_OmniObjectPCc");
   --  Corresponds to Ada_OmniObject::string_to_ada_object

   ----------------------
   -- String_To_Object --
   ----------------------

   function String_To_Object
     (Repoid : in CORBA.String)
      return Object_Ptr
   is
      C_Repoid : Interfaces.C.Strings.Chars_Ptr;
      C_Result : System.Address;
      Result   : Object_Ptr;
   begin
      -- Transform arguments into C types ...
      C_Repoid := Interfaces.C.Strings.New_String
        (CORBA.To_Standard_String (Repoid));

      --  Call C function
      C_Result := C_String_To_Object (C_Repoid);

      --  Free arguments
      Interfaces.C.Strings.Free (C_Repoid);

      --  Transform result
      if  C_Result = System.Null_Address then
         return null;

      else
         Result := To_Object_Ptr (Address_To_Object.To_Pointer (C_Result));
         Result.all.Implobj := null;
         return Result;
      end if;
   end String_To_Object;

   ------------------------
   -- C_Object_To_String --
   ------------------------

   function C_Object_To_String
     (Obj : in System.Address)
      return Interfaces.C.Strings.Chars_Ptr;

   pragma Import
     (CPP, C_Object_To_String,
      "ada_object_to_string__14Ada_OmniObjectP14Ada_OmniObject");
   --  Corresponds to ada_object_to_string

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (Obj_ptr : in Object_Ptr)
      return CORBA.String
   is
      C_Obj_Ptr : System.Address;
      C_Result  : Interfaces.C.Strings.Chars_Ptr;
   begin
      C_Obj_Ptr := Address_To_Object.To_Address (From_Object_Ptr (Obj_Ptr));
      C_Result  := C_Object_To_String (C_Obj_ptr);
      return CORBA.To_CORBA_String(Interfaces.C.Strings.Value(C_Result));
   end Object_To_String;


   -------------------
   -- miscellaneous --
   -------------------

   ------------
   -- C_Hash --
   ------------

   function C_Hash
     (Self    : in Object'Class;
      Maximum : in Interfaces.C.Unsigned_Long)
      return Interfaces.C.Unsigned_Long;

   pragma Import (CPP, C_Hash, "hash__14Ada_OmniObjectUl");
   --  Calls Ada_OmniObject::hash

   ----------
   -- Hash --
   ----------

   function Hash
     (Self    : in Object'Class;
      Maximum : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long is
   begin
      return CORBA.Unsigned_Long
        (C_Hash (Self, Interfaces.C.Unsigned_Long (Maximum)));
   end Hash;

   --------------------
   -- C_Non_Existent --
   --------------------

   function C_Non_Existent (Self : in Object'Class) return Sys_Dep.C_Boolean;
   pragma Import (CPP, C_Non_Existent, "non_existent__14Ada_OmniObject");
   --  Calls Ada_OmniObject::non_existent

   ------------------
   -- Non_Existent --
   ------------------

   function Non_Existent (Self : in Object'Class) return CORBA.Boolean is
   begin
      return Sys_Dep.Boolean_C_To_Ada (C_Non_Existent (Self));
   end Non_Existent;

   ------------------------
   -- C_Get_Rope_And_Key --
   ------------------------

   procedure C_Get_Rope_And_Key
     (Self    : in Object'Class;
      L       : in out Omniropeandkey.Object;
      Success : out Sys_Dep.C_Boolean);
   pragma Import
     (CPP, C_Get_Rope_And_Key,
      "getRopeAndKey__14Ada_OmniObjectR18Ada_OmniRopeAndKeyRb");
   --  Wrapper around Ada_OmniObject function getRopeAndKey (see
   --  Ada_OmniObject.hh)

   ----------------------
   -- Get_Rope_And_Key --
   ----------------------

   procedure Get_Rope_And_Key
     (Self    : in Object'Class;
      L       : in out Omniropeandkey.Object;
      Success : out CORBA.Boolean )
   is
      C_Success : Sys_Dep.C_Boolean;
   begin
      if not Is_Proxy (Self) then
         Ada.Exceptions.Raise_Exception
           (CORBA.AdaBroker_Fatal_Error'Identity,
            "Omniobject.Get_Rope_And_Key cannot be called on a local object");
      end if;

      --  Calls the C function ...
      C_Get_Rope_And_Key (Self, L, C_Success);

      --  Transforms the result into an Ada type
      Success := Sys_Dep.Boolean_C_To_Ada (C_Success);
   end Get_Rope_And_Key;

   -------------------------
   -- C_Get_Repository_Id --
   -------------------------
   function C_Get_Repository_Id
     (Self : in Object'class)
      return Interfaces.C.Strings.Chars_Ptr;

   pragma Import
     (CPP, C_Get_Repository_Id, "getRepositoryID__14Ada_OmniObject");
   --  Corresponds to Ada_OmniObject::getRepositoryID

   -----------------------
   -- Get_Repository_Id --
   -----------------------
   function Get_Repository_Id
     (Self : in Object'class)
      return CORBA.String
   is
      C_Result : Interfaces.C.Strings.Chars_Ptr;
   begin
      C_Result := C_Get_Repository_Id (Self);
      return CORBA.To_CORBA_String (Interfaces.C.Strings.Value (C_Result));
   end Get_Repository_Id;

   ------------------------
   -- C_Get_Profile_List --
   ------------------------

   function C_Get_Profile_List
     (Self : in Object'Class)
      return System.Address;
   pragma Import (CPP, C_Get_Profile_List, "iopProfiles__14Ada_OmniObject");
   --  Returns the Profile list of an object wrapper around C function
   --  Ada_OmniObject::iopProfiles() (see Ada_OmniObject.hh)

   ----------------------
   -- Get_Profile_List --
   ----------------------

   function Get_Profile_List
     (Self : in Object'Class)
      return IOP.Tagged_Profile_List
   is
      Result : System.Address;
   begin
      --  Calls the C function ...
      Result := C_Get_Profile_List (Self);

      --  Transforms the result in an Ada type
      return Iop.Tagged_Profile_List (Result);
   end Get_Profile_List;

   --  Implemented_Object this is the type of local implementations of
   --  objects it is the root of all XXX.Impl.Object

   ----------------------------
   -- finalization operators --
   ----------------------------

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self: in out Implemented_Object)
   is
      type Ptr is access all Implemented_Object;
      function To_Implemented_Object_Ptr is
        new Ada.Unchecked_Conversion (Ptr, Implemented_Object_Ptr);
      Tmp : Object_Ptr := Object_Ptr_Constructor;
   begin
      pragma Debug (Output (Omniobject, "Omniobject.Initialize"));
      Tmp.all.Implobj := To_Implemented_Object_Ptr (Self'Access);
      Self.Omniobj  := Tmp;
      Self.Dispatch := null;
      Self.Is_A     := null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self: in out Implemented_Object) is
   begin
      null;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self: in out Implemented_Object)
   is
   begin
      pragma Debug (Output (Omniobject, "Omniobject.Finalize"));

      if not Is_Nil (Self) then
         pragma Debug
           (Output (Omniobject, "Omniobject.Finalizing non nil object"));

         Omniobject_Destructor (Self.Omniobj);

         pragma Debug
           (Output (Omniobject, "Omniobject.Finalize :object destroyed"));

         Self.Omniobj  := null;
         Self.Dispatch := null;
         Self.Is_A     := null;
         pragma Debug (Output (Omniobject, "Omniobject.Finalize : OK"));
      end if;
   end Finalize;

   --  Omniobject this type is imported from C++ it is the equivalent of
   --  omniObject

   ----------------
   -- C_Is_Proxy --
   ----------------

   function C_Is_Proxy (Self : in Object'Class) return Sys_Dep.C_Boolean;
   pragma Import (CPP, C_Is_Proxy, "is_proxy__14Ada_OmniObject");

   --------------
   -- Is_Proxy --
   --------------

   function Is_Proxy
     (Self : in Object'Class)
      return Boolean is
   begin
      return Sys_Dep.Boolean_C_To_Ada (C_Is_Proxy (Self));
   end Is_Proxy;

   -------------------------
   -- C_Set_Repository_Id --
   -------------------------

   procedure C_Set_Repository_Id
     (Self   : in out Object'Class;
      Repoid : Interfaces.C.Strings.Chars_Ptr);
   pragma Import
     (CPP, C_Set_Repository_Id, "setRepositoryID__14Ada_OmniObjectPCc");
   --  Corresponds to Ada_OmniObject::setRepositoryID

   -----------------------
   -- Set_Repository_Id --
   -----------------------

   procedure Set_Repository_Id
     (Self   : in out Object'class;
      Repoid : in CORBA.String)
   is
      C_Repoid : Interfaces.C.Strings.Chars_Ptr;
   begin
      if Is_Proxy (Self) then
         Ada.Exceptions.Raise_Exception
           (CORBA.AdaBroker_Fatal_Error'Identity,
            "Omniobject.Set_Repository_Id(Object'class)" & CORBA.CRLF &
            "Cannot be called on proxy objects");
      end if;
      C_Repoid := Interfaces.C.Strings.New_String
        (CORBA.To_Standard_String (Repoid));

      C_Set_Repository_Id (Self, C_Repoid);

      Interfaces.C.Strings.Free (C_Repoid);
   end Set_Repository_Id;

   ------------------------
   -- C_Set_Rope_And_Key --
   ------------------------

   procedure C_Set_Rope_And_Key
     (Self    : in out Object'Class;
      L       : in out OmniRopeAndKey.Object;
      KeepIOP : in Sys_Dep.C_Boolean);
   pragma Import
     (CPP,C_Set_Rope_And_Key,
      "setRopeAndKey__14Ada_OmniObjectRC18Ada_OmniRopeAndKeyb");
   --  Wrapper around Ada_OmniObject function setRopeAndKey (see
   --  Ada_OmniObject.hh)

   ----------------------
   -- Set_Rope_And_Key --
   ----------------------

   procedure Set_Rope_And_Key
     (Self    : in out Object'Class;
      L       : in out Omniropeandkey.Object;
      KeepIOP : in Boolean := True)
   is
      C_KeepIOP : Sys_Dep.C_Boolean;
   begin
      --  Transforms the arguments into a C type ...
      C_KeepIOP := Sys_Dep.Boolean_Ada_To_C (KeepIOP);

      --  Calls the C procedure
      C_Set_Rope_And_Key (Self,L,C_KeepIOP);
   end Set_Rope_And_Key;

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch
     (Self                  : in Object'Class;
      Orls                  : in out Giop_S.Object;
      Orl_Op                : in Standard.String;
      Orl_Response_Expected : in CORBA.Boolean;
      Success               : out CORBA.Boolean)
   is

      procedure Marshall_System_Exception
        (Repoid : in Constants.Exception_ID;
         Exbd   : in CORBA.Ex_Body'Class;
         Orls   : in out Giop_S.Object)
      is
         Msgsize : CORBA.Unsigned_Long := 0;
      begin
         pragma Debug
           (Output (Omniobject, "Dispatch : Marshalling System Exception : " &
                    Constants.To_Standard_String (Repoid)));
         pragma Debug
           (Output (Omniobject,"Dispatch : Marshalling System Exception : " &
                    " msgsize = " & CORBA.Unsigned_Long'Image (Msgsize)));

         Msgsize := Giop_S.Reply_Header_Size;

         pragma Debug
           (Output (Omniobject, "Dispatch : Marshalling System Exception : " &
                    " msgsize = " & CORBA.Unsigned_Long'Image (Msgsize)));

         Msgsize := Netbufferedstream.Align_Size
           (CORBA.To_CORBA_String (Repoid), Msgsize);

         pragma Debug
           (Output (Omniobject, "Dispatch : Marshalling System Exception : " &
                    " msgsize = " & CORBA.Unsigned_Long'Image (Msgsize)));

         Msgsize := Netbufferedstream.Align_Size (Exbd, Msgsize);

         pragma Debug
           (Output (Omniobject, "Dispatch : Marshalling System Exception : " &
                    " msgsize = " & CORBA.Unsigned_Long'Image (Msgsize)));

         Giop_S.Initialize_Reply (Orls, Giop.SYSTEM_EXCEPTION, Msgsize);

         pragma Debug
           (Output (Omniobject, "Dispatch : Marshalling System Exception : " &
                    " Initialize reply OK "));

         NetBufferedStream.Marshall (CORBA.To_CORBA_String (Repoid), Orls);

         pragma Debug
           (Output (Omniobject, "Dispatch : Marshalling System Exception : " &
                    " repoid marhshalled OK"));

         NetBufferedStream.Marshall (Exbd, Orls);

         pragma Debug
           (Output (Omniobject, "Dispatch : Marshalling System Exception : " &
                    " Ex_Body marshalled OK"));

         Giop_S.Reply_Completed (Orls);

         pragma Debug
           (Output (Omniobject, "Dispatch : Marshalling System Exception : " &
                    " reply completed OK"));
      end Marshall_System_Exception;

   begin
      pragma Debug
        (Output (Omniobject,
                 "Omniobject.Dispatch : about to make dispatching call"));
      --  Check there is no error

      if Self.Implobj = null then
         pragma Debug
           (Output (Omniobject,
                    "Omniobject.Dispatch : raise Fatal error"));

         Ada.Exceptions.Raise_Exception
           (CORBA.Adabroker_Fatal_Error'Identity,
            "Omniobject.Dispatch should not be called on a proxy object");

      else
         begin
            pragma Debug
              (Output (Omniobject,
                       "Omniobject.Dispatch : making dispatching call"));

            Self.Implobj.all.Dispatch
              (Self.Implobj,
               Orls,
               Orl_Op,
               Orl_Response_Expected,
               success);

            pragma Debug
              (Output (Omniobject,
                       "Omniobject.Dispatch : finished dispatching call"));
            return;

         exception
            when E : CORBA.Unknown =>
               declare
                  Exbd : CORBA.Unknown_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Unknown_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Bad_param =>
               declare
                  Exbd : CORBA.Bad_Param_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Bad_param_Repoid, Exbd, Orls);
               end;

            when E : CORBA.No_memory =>
               declare
                  Exbd : CORBA.No_Memory_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.No_memory_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Imp_Limit =>
               declare
                  Exbd : CORBA.Imp_Limit_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Imp_Limit_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Comm_failure =>
               declare
                  Exbd : CORBA.Comm_Failure_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Comm_Failure_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Inv_Objref =>
               declare
                  Exbd : CORBA.Inv_Objref_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Inv_Objref_Repoid, Exbd, Orls);
               end;

            when E : CORBA.No_Permission =>
               declare
                  Exbd : CORBA.No_permission_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.No_permission_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Internal =>
               declare
                  Exbd : CORBA.Internal_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Internal_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Marshal =>
               declare
                  Exbd : CORBA.Marshal_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Unknown_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Initialization_Failure =>
               declare
                  Exbd : CORBA.Initialization_Failure_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Initialization_Failure_Repoid, Exbd, Orls);
               end;

            when E : CORBA.No_Implement =>
               declare
                  Exbd : CORBA.No_Implement_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.No_Implement_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Bad_Typecode =>
               declare
                  Exbd : CORBA.Bad_Typecode_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Bad_Typecode_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Bad_Operation =>
               declare
                  Exbd : CORBA.Bad_operation_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Bad_Operation_Repoid, Exbd, Orls);
               end;

            when E : CORBA.No_Resources =>
               declare
                  Exbd : CORBA.No_Resources_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.No_Resources_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Persist_Store =>
               declare
                  Exbd : CORBA.Persist_store_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Persist_Store_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Bad_Inv_Order =>
               declare
                  Exbd : CORBA.Bad_Inv_Order_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Bad_Inv_Order_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Transient =>
               declare
                  Exbd : CORBA.Transient_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Transient_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Free_Mem =>
               declare
                  Exbd : CORBA.Free_Mem_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Free_Mem_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Inv_Ident =>
               declare
                  Exbd : CORBA.Inv_Ident_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Inv_Ident_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Inv_Flag =>
               declare
                  Exbd : CORBA.Inv_Flag_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Inv_Flag_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Intf_repos =>
               declare
                  Exbd : CORBA.Intf_Repos_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Intf_Repos_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Bad_Context =>
               declare
                  Exbd : CORBA.Bad_Context_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Bad_Context_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Obj_Adapter =>
               declare
                  Exbd : CORBA.Obj_Adapter_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Obj_Adapter_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Data_Conversion =>
               declare
                  Exbd : CORBA.Data_Conversion_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Data_Conversion_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Object_Not_exist =>
               declare
                  Exbd : CORBA.Object_Not_Exist_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Object_Not_Exist_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Transaction_Required =>
               declare
                  Exbd : CORBA.Transaction_Required_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Transaction_Required_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Transaction_Rolledback =>
               declare
                  Exbd : CORBA.Transaction_Rolledback_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Transaction_Rolledback_Repoid, Exbd, Orls);
               end;

            when E : CORBA.Wrong_Transaction =>
               declare
                  Exbd : CORBA.Wrong_Transaction_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.Wrong_Transaction_Repoid, Exbd, Orls);
               end;

            when E: CORBA.Adabroker_Fatal_Error |
              CORBA.No_Initialisation_Error     |
              CORBA.C_Out_Of_Range              |
              CORBA.OmniORB_Fatal_Error         |
              CORBA.Dummy_User                  =>
               pragma Debug
                 (Output (Omniobject,
                          "Omniobject.Dispatch : caught a serious error : " &
                          Ada.Exceptions.Exception_Name (E)));
               raise;

            when others =>
               declare
                  Exbd : CORBA.Unknown_Members;
               begin
                  pragma Debug
                    (Output (Omniobject,
                             "Omniobject.Dispatch : caught other exception"));
                  Exbd := (0, CORBA.COMPLETED_MAYBE);
                  Marshall_System_Exception
                    (Constants.Unknown_Repoid, Exbd, Orls);
               end;
         end;
         Success := True;
      end if;
   end Dispatch;

   ----------------
   -- C_Dispatch --
   ----------------
   procedure C_Dispatch
     (Self                  : in Object'Class;
      Orls                  : in out Giop_S.Object;
      Orl_Op                : in Interfaces.C.Strings.Chars_Ptr;
      Orl_Response_Expected : in Sys_Dep.C_Boolean;
      Success               : out Sys_Dep.C_Boolean)
   is
      Ada_Orl_Op : Standard.String := Interfaces.C.Strings.Value (Orl_OP);
      Ada_Orl_Response_Expected : CORBA.Boolean;
      Ada_Success : CORBA.Boolean;
   begin
      pragma Debug (Output (Omniobject,
                            "OmniObject.C_Dispatch : starting"));
      pragma Debug (Output (Omniobject,
                            "Omniobject.C_Dispatch : arriving in Ada code"));

      Ada_Orl_Response_Expected :=
        Sys_Dep.Boolean_C_To_Ada (Orl_Response_Expected);

      Dispatch (Self,
                Orls,
                Ada_Orl_Op,
                Ada_Orl_Response_Expected,
                Ada_Success);

      Success := Sys_Dep.Boolean_Ada_To_C (Ada_Success);
      pragma Debug
        (Output (Omniobject,
                 "Omniobject.C_Dispatch : Exiting successfully ..."));
   end C_Dispatch;

   ------------
   -- C_Is_A --
   ------------

   function C_Is_A
     (Self   : in Object'Class;
      Repoid : in Interfaces.C.Strings.Chars_Ptr)
      return  Sys_Dep.C_Boolean
   is
      Rep : CORBA.String;
   begin
      --  Never called, never used
      Rep := CORBA.To_CORBA_String (Interfaces.C.Strings.Value (Repoid));
      return Sys_Dep.Boolean_Ada_To_C (Self.Implobj.all.Is_A (Rep));
   end C_Is_A;


   ---------------------
   -- memory handling --
   ---------------------

   ------------------------------
   -- C_Object_Ptr_Constructor --
   ------------------------------

   function C_Object_Ptr_Constructor return System.Address;
   pragma Import
     (CPP, C_Object_Ptr_Constructor, "Constructor__14Ada_OmniObject");
   --  This is a workaround for gnat 3.11p we cannot write toto :
   --  Object_Ptr := new Object we have to call the C++ constructor to
   --  create objects this constructor is only used in
   --  Initialize(Implemented_Object) it must not be ued otherwise, because
   --  it creates non-initialized objects which are initialized afrewards
   --  in Initialize(Implemented_Object) see Create_Omniobject : another
   --  way of creatins omniobject.Object

   ----------------------------
   -- Object_Ptr_Constructor --
   ----------------------------

   function Object_Ptr_Constructor return Object_Ptr
   is
      C_Result : System.Address;
   begin
      C_Result := C_Object_Ptr_Constructor;
      return To_Object_Ptr (Address_To_Object.To_Pointer (C_Result));
   end Object_Ptr_Constructor;

end OmniObject;
