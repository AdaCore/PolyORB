------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                    A D A B R O K E R . O M N I O R B                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.7 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
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

with CORBA;                use CORBA;
with CORBA.Object;         use CORBA.Object;
with CORBA.Object.OmniORB; use CORBA.Object.OmniORB;

with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings;

with Ada.Strings.Unbounded;
with AdaBroker; use AdaBroker;
with AdaBroker.Constants;
with AdaBroker.Omni;
with AdaBroker.GIOP;
with AdaBroker.GIOP_S;

with AdaBroker.Debug;
pragma Elaborate_All (AdaBroker.Debug);

use Interfaces.C.Strings;

package body AdaBroker.OmniORB is

   use type System.Address;
   use type CORBA.Unsigned_Long;

   Flag : constant Natural := AdaBroker.Debug.Is_Active ("adabroker.omniorb");
   procedure O is new AdaBroker.Debug.Output (Flag);

   procedure Dispatch
     (Self                  : in OmniObject'Class;
      Orls                  : in out GIOP_S.Object;
      Orl_Op                : in Standard.String;
      Orl_Response_Expected : in CORBA.Boolean;
      Success               : out CORBA.Boolean);

   procedure C_Init_Local_Object
     (Self : in out OmniObject'Class;
      Repoid : in Strings.chars_ptr);
   pragma Import
     (CPP, C_Init_Local_Object, "initLocalObject__14Ada_OmniObjectPCc");
   --  Ada_OmniObject::initLocalObject (see Ada_OmniObject.hh)

   -----------------------------
   -- Initialize_Local_Object --
   -----------------------------

   procedure Initialize_Local_Object
     (Self : in ImplObject)
   is
      RepoID   : CORBA.String;
      C_RepoID : chars_ptr;
   begin
      pragma Debug (O ("Initialize_Local_Object : enter"));

      if Self.OmniObj = null then
         pragma Debug (O ("Initialize_Local_Object : leave with exception"));

         Ada.Exceptions.Raise_Exception
           (CORBA.AdaBroker_Fatal_Error'Identity,
            "cannot initialize a nil object");
      end if;

      RepoID := Id_To_Rep (Self.OmniObj.Interface);
      C_RepoID := New_String (To_Standard_String (RepoID));
      C_Init_Local_Object (Self.OmniObj.all, C_RepoID);
      Free (C_RepoID);

      pragma Debug (O ("Initialize_Local_Object : enter"));
   end Initialize_Local_Object;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Self : in ImplObject) return CORBA.Boolean is
   begin
      return Self.OmniObj = null;
   end Is_Nil;

   ----------
   -- Hash --
   ----------

   function Hash
     (Self    : in ImplObject;
      Maximum : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long is
   begin
      if Self.OmniObj = null then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "cannot hash a nil implemented object");
      end if;

      return Hash (Self.OmniObj.all, Maximum);
   end Hash;

   --------------------
   -- Get_OmniObject_Ptr --
   --------------------

   function Get_OmniObject_Ptr
     (Self : in ImplObject)
      return OmniObject_Ptr is
   begin
      return Self.OmniObj;
   end Get_OmniObject_Ptr;

   ---------------------
   -- Object_Is_Ready --
   ---------------------

   procedure Object_Is_Ready
     (Self : in ImplObject'Class) is
   begin
      if Self.OmniObj /= null then
         OmniObject_Is_Ready (Self.OmniObj);
      else
         Ada.Exceptions.Raise_Exception
           (CORBA.AdaBroker_Fatal_Error'Identity,
            "cannot ready a nil implemented object");
      end if;
   end Object_Is_Ready;

   --------------------
   -- Dispose_Object --
   --------------------

   procedure Dispose_Object
     (Self : in ImplObject'Class)
   is
   begin
      if Self.OmniObj /= null then
         Dispose_OmniObject (Self.OmniObj);
      else
         Ada.Exceptions.Raise_Exception
           (CORBA.AdaBroker_Fatal_Error'Identity,
            "cannot dispose a nil implemented object");
      end if;
   end Dispose_Object;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (Self : in ImplObject'Class)
      return CORBA.String
   is
      Result : CORBA.String;
   begin
      if Self.OmniObj = null then
         return Object_To_String (null);
      else
         return Object_To_String (Self.OmniObj);
      end if;
   end Object_To_String;

   ----------------
   -- Align_Size --
   ----------------

   function Align_Size
     (Obj            : in ImplObject_Ptr;
      Initial_Offset : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
   begin
      if Obj = null then
         Ada.Exceptions.Raise_Exception
           (CORBA.AdaBroker_Fatal_Error'Identity,
            "cannot align size on a nil implemented object");
      else
         return Align_Size (Obj.OmniObj, Initial_Offset);
      end if;
   end Align_Size;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Obj : in ImplObject_Ptr;
      S   : in out NetBufferedStream.Object'Class) is
   begin
      if Obj = null then
         Ada.Exceptions.Raise_Exception
           (CORBA.AdaBroker_Fatal_Error'Identity,
            "cannot marshall a nil implemented object");
      else
         Marshall (Obj.OmniObj, S);
      end if;
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Obj : in ImplObject_Ptr;
      S   : in out MemBufferedStream.Object'Class)
   is
   begin
      if Obj = null then
         Ada.Exceptions.Raise_Exception
           (CORBA.AdaBroker_Fatal_Error'Identity,
            "cannot marshall a nil implemented object");
      else
         Marshall (Obj.OmniObj, S);
      end if;
   end Marshall;

   ----------------
   -- Align_Size --
   ----------------

   function Align_Size
     (Obj            : in OmniObject_Ptr;
      Initial_Offset : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
      Offset : CORBA.Unsigned_Long;
   begin
      --  Alignment for type CORBA.unsigned_long
      Offset := Omni.Align_To (Initial_Offset, Omni.ALIGN_4);

      if Obj = null then
         --  A nil object is marshalled on 12 bytes.

         return Offset + 12;

      else
         --  Add size of unsigned_long, size of RepoID itself and size
         --  of profiles.

         declare
            use Ada.Strings.Unbounded;

            Rep : Unbounded_String
              := Unbounded_String (Id_To_Rep (Obj.Interface));
         begin
            Offset := Offset + 4;
            Offset := Offset + CORBA.Unsigned_Long (Length (Rep)) + 1;
            Offset := IOP.Align_Size (Get_Profile_List (Obj.all), Offset);
            return Offset;
         end;
      end if;
   end Align_Size;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Obj : in OmniObject_Ptr;
      S   : in out NetBufferedStream.Object'Class)
   is
   begin
      if Obj = null then
         --  If object null, marshall null object
         NetBufferedStream.Marshall (CORBA.Unsigned_Long (1), S);
         NetBufferedStream.Marshall (CORBA.Char (Latin_1.NUL), S);
         NetBufferedStream.Marshall (CORBA.Unsigned_Long (0), S);
      else
         --  Else marshall the Repo_ID and the iopProfiles
         NetBufferedStream.Marshall (Id_To_Rep (Obj.Interface), S);
         IOP.Marshall (Get_Profile_List (Obj.all), S);
      end if;
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Obj : in OmniObject_Ptr;
      S   : in out MemBufferedStream.Object'Class)
   is
   begin
      if Obj = null then
         --  If object null, marshall null object
         MemBufferedStream.Marshall (CORBA.Unsigned_Long (1), S);
         MemBufferedStream.Marshall (CORBA.Char (Latin_1.NUL), S);
         MemBufferedStream.Marshall (CORBA.Unsigned_Long (0), S);
      else
         --  Else marshall the Repo_ID and the iopProfiles
         MemBufferedStream.Marshall (Id_To_Rep (Obj.Interface), S);
         IOP.Marshall (Get_Profile_List (Obj.all), S);
      end if;
   end Marshall;

   function C_Create_OmniObject
     (RepoID   : in Strings.chars_ptr;
      Profiles : in System.Address;
      Release  : in Sysdep.Bool)
      return System.Address;

   pragma Import
     (CPP, C_Create_OmniObject,
      "ada_create_objref__14Ada_OmniObjectPCcPt25_CORBA_Unbounded_Sequence" &
      "1ZQ23IOP13TaggedProfileb");
   --  Corresponds to Ada_OmniObject::ada_create_objref see
   --  Ada_OmniObject.hh

   -----------------------
   -- Create_OmniObject --
   -----------------------

   function Create_OmniObject
     (RepoID   : in CORBA.String;
      Profiles : in IOP.Tagged_Profile_List)
      return OmniObject_Ptr
   is
      C_RepoID   : chars_ptr;
      C_Release  : Sysdep.Bool;
      C_Profiles : System.Address;
      C_Result   : System.Address;
      Result     : OmniObject_Ptr;
   begin
      C_RepoID := New_String (To_Standard_String (RepoID));

      --  Never deallocated, stored in the object (to be checked).

      C_Release  := Sysdep.To_Bool (True);
      C_Profiles := System.Address (Profiles);

      C_Result   := C_Create_OmniObject (C_RepoID, C_Profiles, C_Release);
      Result     := To_OmniObject_Ptr
        (Address_To_OmniObject.To_Pointer (C_Result));

      if Result /= null then

         --  Set up field Interface as soon as we can.
         Set_Interface_Rep (Result.all, RepoID);

         pragma Debug
           (O ("Create_OmniObject : set Result.ImplObj to null"));

         Result.ImplObj := null;

      else
         pragma Debug (O ("Create_OmniObject : C++ function returned null"));
         null;
      end if;

      return Result;
   end Create_OmniObject;

   function C_Duplicate_OmniObject
     (Self : in System.Address)
      return System.Address;
   pragma Import
     (CPP, C_Duplicate_OmniObject,
      "objectDuplicate__14Ada_OmniObjectP14Ada_OmniObject");
   --  Call Ada_OmniObject::objectDuplicate

   --------------------------
   -- Duplicate_OmniObject --
   --------------------------

   function Duplicate_OmniObject
     (Self : in OmniObject_Ptr)
      return OmniObject_Ptr
   is
      C_Result : System.Address;
      C_Arg    : System.Address;
      Result   : OmniObject_Ptr;
   begin
      pragma Debug (O ("Duplicate_OmniObject : enter"));

      C_Arg := Address_To_OmniObject.To_Address (From_OmniObject_Ptr (Self));

      pragma Debug (O ("Duplicate_OmniObject : invoke C++ operation"));

      C_Result := C_Duplicate_OmniObject (C_Arg);
      Result := To_OmniObject_Ptr
        (Address_To_OmniObject.To_Pointer (C_Result));

      pragma Debug (O ("Duplicate_OmniObject : convert C++ to Ada "));

      if Result /= null then

         --  Why do we have to reset this to null?
         pragma Debug
           (O ("Duplicate_OmniObject : set Result.ImplObj to null"));

         Result.ImplObj := null;
      end if;

      pragma Debug (O ("Duplicate_OmniObject : leave"));

      return Result;
   end Duplicate_OmniObject;

   procedure C_Destruct_OmniObject (Self : in System.Address);
   pragma Import
     (CPP, C_Destruct_OmniObject,
      "Destructor__14Ada_OmniObjectP14Ada_OmniObject");
   --  Call Ada_OmniObject::Destructor

   -------------------------
   -- Destruct_OmniObject --
   -------------------------

   procedure Destruct_OmniObject (Self : in OmniObject_Ptr) is
   begin
      pragma Debug (O ("Destruct_OmniObject : enter"));

      C_Destruct_OmniObject
        (Address_To_OmniObject.To_Address (From_OmniObject_Ptr (Self)));

      pragma Debug (O ("Destruct_OmniObject : leave"));
   end Destruct_OmniObject;

   procedure C_OmniObject_Is_Ready (Self : in System.Address);
   pragma Import
     (CPP, C_OmniObject_Is_Ready, "objectIsReady__14Ada_OmniObject");
   --  Call Ada_OmniObject::objectIsReady

   -------------------------
   -- OmniObject_Is_Ready --
   -------------------------

   procedure OmniObject_Is_Ready
     (Self : in OmniObject_Ptr)
   is
   begin
      C_OmniObject_Is_Ready
        (Address_To_OmniObject.To_Address (From_OmniObject_Ptr (Self)));
   end OmniObject_Is_Ready;

   procedure C_Dispose_OmniObject (Self : in System.Address);
   pragma Import
     (CPP, C_Dispose_OmniObject, "disposeObject__14Ada_OmniObject");
   --  Call Ada_OmniObject::disposeObject

   ------------------------
   -- Dispose_OmniObject --
   ------------------------

   procedure Dispose_OmniObject (Self : in OmniObject_Ptr) is
   begin
      C_Dispose_OmniObject
        (Address_To_OmniObject.To_Address (From_OmniObject_Ptr (Self)));
   end Dispose_OmniObject;

   function C_String_To_Object
     (From : in Strings.chars_ptr)
      return System.Address;
   pragma Import
     (CPP, C_String_To_Object, "string_to_ada_object__14Ada_OmniObjectPCc");
   --  Corresponds to Ada_OmniObject::string_to_ada_object

   ----------------------
   -- String_To_Object --
   ----------------------

   function String_To_Object
     (From : in CORBA.String)
      return OmniObject_Ptr
   is
      C_From   : Strings.chars_ptr;
      C_Result : System.Address;
      Result   : OmniObject_Ptr;
   begin
      --  Transform arguments into C types ...
      C_From := Strings.New_String (To_Standard_String (From));

      --  Call C function
      C_Result := C_String_To_Object (C_From);

      --  Free arguments
      Strings.Free (C_From);

      --  Transform result
      if  C_Result = System.Null_Address then
         return null;

      else
         Result := To_OmniObject_Ptr
           (Address_To_OmniObject.To_Pointer (C_Result));
         Result.Interface := Rep_To_Id (Get_Rep_From_ORB (Result.all));
         Result.ImplObj   := null;
         return Result;
      end if;
   end String_To_Object;

   ------------------------
   -- C_Object_To_String --
   ------------------------

   function C_Object_To_String
     (Obj : in System.Address)
      return Strings.chars_ptr;

   pragma Import
     (CPP, C_Object_To_String,
      "ada_object_to_string__14Ada_OmniObjectP14Ada_OmniObject");
   --  Corresponds to ada_object_to_string

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (Obj : in OmniObject_Ptr)
      return CORBA.String
   is
      C_Obj    : System.Address;
      C_Result : Strings.chars_ptr;
   begin
      pragma Debug (O ("invoke Object_To_String"));
      C_Obj    := Address_To_OmniObject.To_Address (From_OmniObject_Ptr (Obj));
      pragma Debug (O ("invoke C_Object_To_String"));
      C_Result := C_Object_To_String (C_Obj);
      pragma Debug (O ("invoke Object_To_String done"));
      return To_CORBA_String (Strings.Value (C_Result));
   end Object_To_String;

   function C_Hash
     (Self    : in OmniObject'Class;
      Maximum : in Interfaces.C.unsigned_long)
      return Interfaces.C.unsigned_long;

   pragma Import (CPP, C_Hash, "hash__14Ada_OmniObjectUl");
   --  Call Ada_OmniObject::hash

   ----------
   -- Hash --
   ----------

   function Hash
     (Self    : in OmniObject'Class;
      Maximum : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long is
   begin
      return CORBA.Unsigned_Long
        (C_Hash (Self, Interfaces.C.unsigned_long (Maximum)));
   end Hash;

   function C_Is_Equivalent
     (Self  : in OmniObject'Class;
      Other : in System.Address)
      return Sysdep.Bool;

   pragma Import
     (CPP, C_Is_Equivalent,
      "is_equivalent__14Ada_OmniObjectP14Ada_OmniObject");
   --  Call Ada_OmniObject::is_equivalent.

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent
     (Self  : in OmniObject'Class;
      Other : in OmniObject_Ptr)
     return CORBA.Boolean
   is
      C_Result : Sysdep.Bool;
      C_Other  : System.Address
        := Address_To_OmniObject.To_Address (From_OmniObject_Ptr (Other));
   begin
      C_Result := C_Is_Equivalent (Self, C_Other);
      return Sysdep.To_Boolean (C_Result);
   end Is_Equivalent;

   --------------------
   -- C_Non_Existent --
   --------------------

   function C_Non_Existent (Self : in OmniObject'Class) return Sysdep.Bool;
   pragma Import (CPP, C_Non_Existent, "non_existent__14Ada_OmniObject");
   --  Call Ada_OmniObject::non_existent

   ------------------
   -- Non_Existent --
   ------------------

   function Non_Existent (Self : in OmniObject'Class) return CORBA.Boolean is
   begin
      return Sysdep.To_Boolean (C_Non_Existent (Self));
   end Non_Existent;

   ------------------------
   -- C_Get_Rope_And_Key --
   ------------------------

   procedure C_Get_Rope_And_Key
     (Self    : in OmniObject'Class;
      L       : in out OmniRopeAndKey.Object;
      Success : out Sysdep.Bool);
   pragma Import
     (CPP, C_Get_Rope_And_Key,
      "getRopeAndKey__14Ada_OmniObjectR18Ada_OmniRopeAndKeyRb");
   --  Wrapper around Ada_OmniObject function getRopeAndKey (see
   --  Ada_OmniObject.hh)

   ----------------------
   -- Get_Rope_And_Key --
   ----------------------

   procedure Get_Rope_And_Key
     (Self    : in OmniObject'Class;
      L       : in out OmniRopeAndKey.Object;
      Success : out CORBA.Boolean)
   is
      C_Success : Sysdep.Bool;
   begin
      if not Is_Proxy (Self) then
         Ada.Exceptions.Raise_Exception
           (CORBA.AdaBroker_Fatal_Error'Identity,
            "cannot get rope and key on a local object");
      end if;

      --  Call the C function ...
      C_Get_Rope_And_Key (Self, L, C_Success);

      --  Transform the result into an Ada type
      Success := Sysdep.To_Boolean (C_Success);
   end Get_Rope_And_Key;

   ----------------------
   -- Get_Rep_From_ORB --
   ----------------------

   function C_Get_Rep_From_ORB
     (Self : in OmniObject'class)
      return Strings.chars_ptr;

   pragma Import
     (CPP, C_Get_Rep_From_ORB, "getRepositoryID__14Ada_OmniObject");
   --  Corresponds to Ada_OmniObject::getRepositoryID

   function Get_Rep_From_ORB
     (Self : in OmniObject'Class)
      return CORBA.String
   is
      C_Result : Strings.chars_ptr;
   begin
      C_Result := C_Get_Rep_From_ORB (Self);
      return To_CORBA_String (Strings.Value (C_Result));
   end Get_Rep_From_ORB;

   ----------------------
   -- Get_Profile_List --
   ----------------------

   function C_Get_Profile_List
     (Self : in OmniObject'Class)
      return System.Address;

   pragma Import (CPP, C_Get_Profile_List, "iopProfiles__14Ada_OmniObject");
   --  Return the Profile list of an object wrapper around C function
   --  Ada_OmniObject::iopProfiles() (see Ada_OmniObject.hh)

   function Get_Profile_List
     (Self : in OmniObject'Class)
      return IOP.Tagged_Profile_List
   is
      Result : System.Address;
   begin
      Result := C_Get_Profile_List (Self);
      return IOP.Tagged_Profile_List (Result);
   end Get_Profile_List;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out ImplObject;
      RepoID : in CORBA.String)
   is
   begin
      pragma Debug (O ("initialize : enter with RepoID"));
      Initialize (Self);
      Self.OmniObj.Interface := Rep_To_Id (RepoID);
      pragma Debug (O ("initialize : leave with RepoID"));
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out ImplObject)
   is
      Tmp : OmniObject_Ptr := OmniObject_Ptr_Constructor;
   begin
      pragma Debug (O ("initialize : enter"));

      Tmp.ImplObj   := Self'Unchecked_Access;
      Self.OmniObj  := Tmp;

      pragma Debug (O ("initialize : leave"));
   end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out ImplObject) is
   begin
      null;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out ImplObject)
   is
   begin
      pragma Debug (O ("finalize : enter"));

      if Self.OmniObj /= null then
         Destruct_OmniObject (Self.OmniObj);
         Self.OmniObj  := null;
      end if;

      pragma Debug (O ("finalize : leave"));
   end Finalize;

   function C_Is_Proxy (Self : in OmniObject'Class) return Sysdep.Bool;
   pragma Import (CPP, C_Is_Proxy, "is_proxy__14Ada_OmniObject");

   --------------
   -- Is_Proxy --
   --------------

   function Is_Proxy
     (Self : in OmniObject'Class)
      return Boolean is
   begin
      return Sysdep.To_Boolean (C_Is_Proxy (Self));
   end Is_Proxy;

   procedure C_Set_Rope_And_Key
     (Self    : in out OmniObject'Class;
      L       : in out OmniRopeAndKey.Object;
      KeepIOP : in Sysdep.Bool);
   pragma Import
     (CPP, C_Set_Rope_And_Key,
      "setRopeAndKey__14Ada_OmniObjectRC18Ada_OmniRopeAndKeyb");
   --  Wrapper around Ada_OmniObject function setRopeAndKey (see
   --  Ada_OmniObject.hh)

   ----------------------
   -- Set_Rope_And_Key --
   ----------------------

   procedure Set_Rope_And_Key
     (Self    : in out OmniObject'Class;
      L       : in out OmniRopeAndKey.Object;
      KeepIOP : in Boolean := True)
   is
      C_KeepIOP : Sysdep.Bool;
   begin
      --  Transform the arguments into a C type ...
      C_KeepIOP := Sysdep.To_Bool (KeepIOP);

      --  Call the C procedure
      C_Set_Rope_And_Key (Self, L, C_KeepIOP);
   end Set_Rope_And_Key;

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch
     (Self                  : in OmniObject'Class;
      Orls                  : in out GIOP_S.Object;
      Orl_Op                : in Standard.String;
      Orl_Response_Expected : in CORBA.Boolean;
      Success               : out CORBA.Boolean)
   is

      procedure Marshall_System_Exception
        (Repoid : in Constants.Exception_Id;
         Exbd   : in CORBA.System_Exception_Members'Class;
         Orls   : in out GIOP_S.Object);

      -------------------------------
      -- Marshall_System_Exception --
      -------------------------------

      procedure Marshall_System_Exception
        (Repoid : in Constants.Exception_Id;
         Exbd   : in CORBA.System_Exception_Members'Class;
         Orls   : in out GIOP_S.Object)
      is
         Msgsize : CORBA.Unsigned_Long := 0;
      begin
         pragma Debug (O ("Dispatch : Marshalling System Exception : " &
                          Constants.To_Standard_String (Repoid)));
         pragma Debug (O ("Dispatch : Marshalling System Exception : " &
                          " msgsize = " & Msgsize'Img));

         Msgsize := GIOP_S.Reply_Header_Size;

         pragma Debug (O ("Dispatch : Marshalling System Exception : " &
                          " msgsize = " & Msgsize'Img));

         Msgsize := NetBufferedStream.Align_Size
           (To_CORBA_String (Standard.String (Repoid)), Msgsize);

         pragma Debug (O ("Dispatch : Marshalling System Exception : " &
                          " msgsize = " & Msgsize'Img));

         Msgsize := NetBufferedStream.Align_Size (Exbd, Msgsize);

         pragma Debug (O ("Dispatch : Marshalling System Exception : " &
                          " msgsize = " & Msgsize'Img));

         GIOP_S.Initialize_Reply (Orls, GIOP.System_Exception, Msgsize);

         pragma Debug (O ("Dispatch : Marshalling System Exception : " &
                          " Initialize reply OK"));

         NetBufferedStream.Marshall
           (To_CORBA_String (Standard.String (Repoid)), Orls);

         pragma Debug (O ("Dispatch : Marshalling System Exception : " &
                          " repoid marhshalled OK"));

         NetBufferedStream.Marshall (Exbd, Orls);

         pragma Debug (O ("Dispatch : Marshalling System Exception : " &
                          " System_Exception_Members marshalled OK"));

         GIOP_S.Reply_Completed (Orls);

         pragma Debug (O ("Dispatch : Marshalling System Exception : " &
                          " reply completed OK"));
      end Marshall_System_Exception;

   begin
      pragma Debug (O ("Dispatch : about to make dispatching call"));
      --  Check there is no error

      if Self.ImplObj = null then
         pragma Debug (O ("Dispatch : raise Fatal error"));

         Ada.Exceptions.Raise_Exception
           (CORBA.AdaBroker_Fatal_Error'Identity,
            "Omniobject.Dispatch should not be called on a proxy object");
      end if;
      declare
         Dispatch : Dispatch_Procedure;
      begin
         pragma Debug (O ("Dispatch : making dispatching call"));

         Dispatch := Id_To_Dispatch (Self.Interface);
         Dispatch
           (Self.ImplObj,
            Orls,
            Orl_Op,
            Orl_Response_Expected,
            Success);

         pragma Debug (O ("Dispatch : finished dispatching call"));
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

         when E : CORBA.Bad_Param =>
            declare
               Exbd : CORBA.Bad_Param_Members;
            begin
               CORBA.Get_Members (E, Exbd);
               Marshall_System_Exception
                 (Constants.Bad_Param_Repoid, Exbd, Orls);
            end;

         when E : CORBA.No_Memory =>
            declare
                  Exbd : CORBA.No_Memory_Members;
            begin
               CORBA.Get_Members (E, Exbd);
               Marshall_System_Exception
                 (Constants.No_Memory_Repoid, Exbd, Orls);
            end;

         when E : CORBA.Imp_Limit =>
            declare
               Exbd : CORBA.Imp_Limit_Members;
            begin
               CORBA.Get_Members (E, Exbd);
               Marshall_System_Exception
                 (Constants.Imp_Limit_Repoid, Exbd, Orls);
            end;

         when E : CORBA.Comm_Failure =>
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
                 (Constants.Inv_ObjRef_Repoid, Exbd, Orls);
            end;

         when E : CORBA.No_Permission =>
            declare
               Exbd : CORBA.No_Permission_Members;
               begin
                  CORBA.Get_Members (E, Exbd);
                  Marshall_System_Exception
                    (Constants.No_Permission_Repoid, Exbd, Orls);
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
                 (Constants.Marshal_Repoid, Exbd, Orls);
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
               Exbd : CORBA.Bad_Operation_Members;
            begin
               CORBA.Get_Members (E, Exbd);
               Marshall_System_Exception
                 (Constants.Bad_Operation_Repoid, Exbd, Orls);
            end;

         when E : CORBA.No_Response =>
            declare
               Exbd : CORBA.No_Response_Members;
            begin
               CORBA.Get_Members (E, Exbd);
               Marshall_System_Exception
                 (Constants.No_Response_Repoid, Exbd, Orls);
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
               Exbd : CORBA.Persist_Store_Members;
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

         when E : CORBA.Intf_Repos =>
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

         when E : CORBA.Object_Not_Exist =>
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

         when E : CORBA.AdaBroker_Fatal_Error |
           CORBA.Not_Initialized_Yet          |
           CORBA.C_Out_Of_Range               |
           CORBA.OmniORB_Fatal_Error          |
           CORBA.Wrong_Union_Case             =>
            pragma Debug (O ("Dispatch : caught a serious error : " &
                             Ada.Exceptions.Exception_Name (E)));
            raise;

         when others =>
            declare
               Exbd : CORBA.Unknown_Members;
            begin
               pragma Debug (O ("Dispatch : caught other exception"));
               Exbd := (0, CORBA.Completed_Maybe);
               Marshall_System_Exception
                 (Constants.Unknown_Repoid, Exbd, Orls);
            end;
      end;
      Success := True;
   end Dispatch;

   ----------------
   -- C_Dispatch --
   ----------------
   procedure C_Dispatch
     (Self                  : in OmniObject'Class;
      Orls                  : in out GIOP_S.Object;
      Orl_Op                : in Strings.chars_ptr;
      Orl_Response_Expected : in Sysdep.Bool;
      Success               : out Sysdep.Bool)
   is
      Ada_Orl_Op : Standard.String := Strings.Value (Orl_Op);
      Ada_Orl_Response_Expected : CORBA.Boolean;
      Ada_Success : CORBA.Boolean;
   begin
      pragma Debug (O ("C_Dispatch : enter"));
      pragma Debug (O ("C_Dispatch : arriving in Ada code"));

      Ada_Orl_Response_Expected :=
        Sysdep.To_Boolean (Orl_Response_Expected);

      Dispatch
        (Self, Orls, Ada_Orl_Op, Ada_Orl_Response_Expected, Ada_Success);

      Success := Sysdep.To_Bool (Ada_Success);
      pragma Debug (O ("C_Dispatch : leave successfully"));
   end C_Dispatch;

   ------------
   -- C_Is_A --
   ------------

   function C_Is_A
     (Self   : in OmniObject'Class;
      Repoid : in Strings.chars_ptr)
      return  Sysdep.Bool
   is
      Rep : CORBA.String;
   begin
      --  Never called, never used
      Rep := To_CORBA_String (Strings.Value (Repoid));
      return Sysdep.To_Bool (Is_A (Id_To_Ref (Self.Interface).all, Rep));
   end C_Is_A;

   ------------------------------
   -- C_OmniObject_Ptr_Constructor --
   ------------------------------

   function C_OmniObject_Ptr_Constructor return System.Address;
   pragma Import
     (CPP, C_OmniObject_Ptr_Constructor, "Constructor__14Ada_OmniObject");
   --  This is a workaround for gnat 3.11p we cannot write toto :
   --  Object_Ptr := new Object we have to call the C++ constructor to
   --  create objects this constructor is only used in
   --  Initialize(ImplObject) it must not be ued otherwise, because
   --  it creates non-initialized objects which are initialized afrewards
   --  in Initialize(ImplObject) see Create_Omniobject : another
   --  way of creatins omniobject.Object

   ----------------------------
   -- Object_Ptr_Constructor --
   ----------------------------

   function OmniObject_Ptr_Constructor return OmniObject_Ptr
   is
      C_Result : System.Address;
   begin
      C_Result := C_OmniObject_Ptr_Constructor;
      return To_OmniObject_Ptr (Address_To_OmniObject.To_Pointer (C_Result));
   end OmniObject_Ptr_Constructor;

   -----------------------
   -- Set_Interface_Rep --
   -----------------------

   procedure Set_Interface_Rep
     (Self : in out OmniObject'Class;
      Rep  : in CORBA.String)
   is
   begin
      Self.Interface := Rep_To_Id (Rep);
   end Set_Interface_Rep;

   ----------------------------------
   -- C_Resolve_Initial_References --
   ----------------------------------

   --  function C_Resolve_Initial_References
   --    (Identifier : in Strings.chars_ptr)
   --     return System.Address;
   --  pragma Import
   --    (CPP, C_Resolve_Initial_References,
   --     "string_to_ada_object__14Ada_OmniObjectPCc");
   --  Corresponds to Ada_OmniObject::resolve_initial_references

   --  function Resolve_Initial_References
   --    (Identifier : in CORBA.String)
   --    return OmniObject_Ptr
   --  is
   --     C_Identifier : Strings.char_ptr;
   --     C_Result     : System.Address;
   --     Result       : OmniObject_Ptr;
   --  begin
   --     C_Identifier := Strings.New_String (To_Standard_String (Identifier));

   --     C_Result := C_Resolve_Initial_References (C_Identifier);

   --     Strings.Free (C_Identifier);

   --     if C_Result = System.Null_Address then
   --        return null;

   --     else
   --        Result := To_OmniObject_Ptr
   --          (Address_To_OmniObject.To_Pointer (C_Result));
   --        Result.ImplObj := null;
   --        return Result;
   --     end if;
   --  end Resolve_Initial_References;

end AdaBroker.OmniORB;
