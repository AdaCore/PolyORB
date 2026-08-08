pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/DynamicAny.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with PolyORB.Exceptions;
with DynamicAny.DynAny.Impl;

package body DynamicAny.DynAny is

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynAny.InvalidValue_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members
        (From,
         To);
   end Get_Members;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynAny.TypeMismatch_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members
        (From,
         To);
   end Get_Members;

   --------------
   -- IDL_type --
   --------------

   function IDL_type
     (Self : Local_Ref)
     return CORBA.TypeCode.Object
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.IDL_type
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end IDL_type;

   ------------
   -- assign --
   ------------

   procedure assign
     (Self : Local_Ref;
      dyn_any : DynamicAny.DynAny.Local_Ref'Class)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.assign
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         DynamicAny.DynAny.Local_Ref
           (dyn_any));
   end assign;

   --------------
   -- from_any --
   --------------

   procedure from_any
     (Self : Local_Ref;
      value : CORBA.Any)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.from_any
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end from_any;

   ------------
   -- to_any --
   ------------

   function to_any
     (Self : Local_Ref)
     return CORBA.Any
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.to_any
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end to_any;

   -----------
   -- equal --
   -----------

   function equal
     (Self : Local_Ref;
      dyn_any : DynamicAny.DynAny.Local_Ref'Class)
     return CORBA.Boolean
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.equal
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         DynamicAny.DynAny.Local_Ref
           (dyn_any));
   end equal;

   -------------
   -- destroy --
   -------------

   procedure destroy
     (Self : Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.destroy
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end destroy;

   ----------
   -- copy --
   ----------

   function copy
     (Self : Local_Ref)
     return DynamicAny.DynAny.Local_Ref'Class
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.copy
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end copy;

   --------------------
   -- insert_boolean --
   --------------------

   procedure insert_boolean
     (Self : Local_Ref;
      value : CORBA.Boolean)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_boolean
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_boolean;

   ------------------
   -- insert_octet --
   ------------------

   procedure insert_octet
     (Self : Local_Ref;
      value : CORBA.Octet)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_octet
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_octet;

   -----------------
   -- insert_char --
   -----------------

   procedure insert_char
     (Self : Local_Ref;
      value : CORBA.Char)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_char
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_char;

   ------------------
   -- insert_short --
   ------------------

   procedure insert_short
     (Self : Local_Ref;
      value : CORBA.Short)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_short
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_short;

   -------------------
   -- insert_ushort --
   -------------------

   procedure insert_ushort
     (Self : Local_Ref;
      value : CORBA.Unsigned_Short)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_ushort
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_ushort;

   -----------------
   -- insert_long --
   -----------------

   procedure insert_long
     (Self : Local_Ref;
      value : CORBA.Long)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_long
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_long;

   ------------------
   -- insert_ulong --
   ------------------

   procedure insert_ulong
     (Self : Local_Ref;
      value : CORBA.Unsigned_Long)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_ulong
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_ulong;

   ------------------
   -- insert_float --
   ------------------

   procedure insert_float
     (Self : Local_Ref;
      value : CORBA.Float)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_float
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_float;

   -------------------
   -- insert_double --
   -------------------

   procedure insert_double
     (Self : Local_Ref;
      value : CORBA.Double)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_double
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_double;

   -------------------
   -- insert_string --
   -------------------

   procedure insert_string
     (Self : Local_Ref;
      value : CORBA.String)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_string
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_string;

   ---------------------
   -- insert_typecode --
   ---------------------

   procedure insert_typecode
     (Self : Local_Ref;
      value : CORBA.TypeCode.Object)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_typecode
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_typecode;

   ---------------------
   -- insert_longlong --
   ---------------------

   procedure insert_longlong
     (Self : Local_Ref;
      value : CORBA.Long_Long)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_longlong
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_longlong;

   ----------------------
   -- insert_ulonglong --
   ----------------------

   procedure insert_ulonglong
     (Self : Local_Ref;
      value : CORBA.Unsigned_Long_Long)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_ulonglong
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_ulonglong;

   -----------------------
   -- insert_longdouble --
   -----------------------

   procedure insert_longdouble
     (Self : Local_Ref;
      value : CORBA.Long_Double)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_longdouble
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_longdouble;

   ------------------
   -- insert_wchar --
   ------------------

   procedure insert_wchar
     (Self : Local_Ref;
      value : CORBA.Wchar)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_wchar
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_wchar;

   --------------------
   -- insert_wstring --
   --------------------

   procedure insert_wstring
     (Self : Local_Ref;
      value : CORBA.Wide_String)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_wstring
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_wstring;

   ----------------
   -- insert_any --
   ----------------

   procedure insert_any
     (Self : Local_Ref;
      value : CORBA.Any)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_any
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end insert_any;

   --------------------
   -- insert_dyn_any --
   --------------------

   procedure insert_dyn_any
     (Self : Local_Ref;
      value : DynamicAny.DynAny.Local_Ref'Class)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.insert_dyn_any
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         DynamicAny.DynAny.Local_Ref
           (value));
   end insert_dyn_any;

   -----------------
   -- get_boolean --
   -----------------

   function get_boolean
     (Self : Local_Ref)
     return CORBA.Boolean
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_boolean
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_boolean;

   ---------------
   -- get_octet --
   ---------------

   function get_octet
     (Self : Local_Ref)
     return CORBA.Octet
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_octet
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_octet;

   --------------
   -- get_char --
   --------------

   function get_char
     (Self : Local_Ref)
     return CORBA.Char
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_char
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_char;

   ---------------
   -- get_short --
   ---------------

   function get_short
     (Self : Local_Ref)
     return CORBA.Short
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_short
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_short;

   ----------------
   -- get_ushort --
   ----------------

   function get_ushort
     (Self : Local_Ref)
     return CORBA.Unsigned_Short
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_ushort
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_ushort;

   --------------
   -- get_long --
   --------------

   function get_long
     (Self : Local_Ref)
     return CORBA.Long
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_long
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_long;

   ---------------
   -- get_ulong --
   ---------------

   function get_ulong
     (Self : Local_Ref)
     return CORBA.Unsigned_Long
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_ulong
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_ulong;

   ---------------
   -- get_float --
   ---------------

   function get_float
     (Self : Local_Ref)
     return CORBA.Float
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_float
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_float;

   ----------------
   -- get_double --
   ----------------

   function get_double
     (Self : Local_Ref)
     return CORBA.Double
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_double
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_double;

   ----------------
   -- get_string --
   ----------------

   function get_string
     (Self : Local_Ref)
     return CORBA.String
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_string
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_string;

   ------------------
   -- get_typecode --
   ------------------

   function get_typecode
     (Self : Local_Ref)
     return CORBA.TypeCode.Object
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_typecode
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_typecode;

   ------------------
   -- get_longlong --
   ------------------

   function get_longlong
     (Self : Local_Ref)
     return CORBA.Long_Long
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_longlong
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_longlong;

   -------------------
   -- get_ulonglong --
   -------------------

   function get_ulonglong
     (Self : Local_Ref)
     return CORBA.Unsigned_Long_Long
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_ulonglong
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_ulonglong;

   --------------------
   -- get_longdouble --
   --------------------

   function get_longdouble
     (Self : Local_Ref)
     return CORBA.Long_Double
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_longdouble
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_longdouble;

   ---------------
   -- get_wchar --
   ---------------

   function get_wchar
     (Self : Local_Ref)
     return CORBA.Wchar
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_wchar
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_wchar;

   -----------------
   -- get_wstring --
   -----------------

   function get_wstring
     (Self : Local_Ref)
     return CORBA.Wide_String
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_wstring
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_wstring;

   -------------
   -- get_any --
   -------------

   function get_any
     (Self : Local_Ref)
     return CORBA.Any
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_any
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_any;

   -----------------
   -- get_dyn_any --
   -----------------

   function get_dyn_any
     (Self : Local_Ref)
     return DynamicAny.DynAny.Local_Ref'Class
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.get_dyn_any
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_dyn_any;

   ----------
   -- seek --
   ----------

   function seek
     (Self : Local_Ref;
      index : CORBA.Long)
     return CORBA.Boolean
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.seek
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         index);
   end seek;

   ------------
   -- rewind --
   ------------

   procedure rewind
     (Self : Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynAny.Impl.rewind
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end rewind;

   ----------
   -- next --
   ----------

   function next
     (Self : Local_Ref)
     return CORBA.Boolean
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.next
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end next;

   ---------------------
   -- component_count --
   ---------------------

   function component_count
     (Self : Local_Ref)
     return CORBA.Unsigned_Long
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.component_count
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end component_count;

   -----------------------
   -- current_component --
   -----------------------

   function current_component
     (Self : Local_Ref)
     return DynamicAny.DynAny.Local_Ref'Class
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynAny.Impl.current_component
        (DynamicAny.DynAny.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end current_component;

end DynamicAny.DynAny;
