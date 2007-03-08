------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  CORBA.REPOSITORY_ROOT.REPOSITORY.IMPL                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with CORBA.Object;
with PortableServer;

with CORBA.Repository_Root.FixedDef;
with CORBA.Repository_Root.FixedDef.Impl;
with CORBA.Repository_Root.ArrayDef.Impl;
with CORBA.Repository_Root.ArrayDef;
with CORBA.Repository_Root.SequenceDef;
with CORBA.Repository_Root.SequenceDef.Impl;
with CORBA.Repository_Root.WstringDef;
with CORBA.Repository_Root.WstringDef.Impl;
with CORBA.Repository_Root.StringDef;
with CORBA.Repository_Root.StringDef.Impl;
with CORBA.Repository_Root.PrimitiveDef;
with CORBA.Repository_Root.PrimitiveDef.Impl;
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Repository.Skel;
pragma Warnings (Off, CORBA.Repository_Root.Repository.Skel);

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.CORBA_P.Server_Tools;

package body CORBA.Repository_Root.Repository.Impl is

   -----------
   -- Debug --
   -----------

   use PolyORB.Log;

--    package L is new PolyORB.Log.Facility_Log ("repository.impl");
--    procedure O (Message : Standard.String; Level : Log_Level := Debug)
--      renames L.Output;
--   function C (Level : Log_Level := Debug) return Boolean
--     renames L.Enabled;
--   pragma Unreferenced (C); --  For conditional pragma Debug

   package L2 is new PolyORB.Log.Facility_Log ("repository.impl_method_trace");
   procedure O2 (Message : Standard.String; Level : Log_Level := Debug)
     renames L2.Output;
   function C2 (Level : Log_Level := Debug) return Boolean
     renames L2.Enabled;
   pragma Unreferenced (C2); --  For conditional pragma Debug

   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : Repository_Forward.Ref)
     return Object_Ptr is
      Result : PortableServer.Servant;
   begin
      pragma Debug (O2 ("to_object (repository)"));
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant
        (Repository.Convert_Forward.To_Ref (Fw_Ref),
         Result);
      return Object_Ptr (Result);
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------

   function To_Forward
     (Obj : Object_Ptr)
     return Repository_Forward.Ref is
      Ref : Repository.Ref;
   begin
      pragma Debug (O2 ("to_forward (repository)"));
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Ref);
      return Repository.Convert_Forward.To_Forward (Ref);
   end To_Forward;

   function lookup_id
     (Self : access Object;
      search_id : CORBA.RepositoryId)
     return CORBA.Repository_Root.Contained.Ref
   is
      Result_Object : Contained.Impl.Object_Ptr;
      Nil_Ref : CORBA.Repository_Root.Contained.Ref;
      pragma Warnings (Off, Nil_Ref);
      --  Not initialized explicitly.
      use Contained.Impl;
   begin
      Result_Object := Contained.Impl.Lookup_Id (Get_Contents (Self),
                                                 search_id);

      --  Return a nil_ref if not found
      if Result_Object = null then
         return Nil_Ref;
      end if;

      return Contained.Convert_Forward.To_Ref
        (Contained.Impl.To_Forward (Result_Object));

   end lookup_id;

   function get_canonical_typecode
     (Self : access Object;
      tc : CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, tc);
      pragma Warnings (On);

      Result : CORBA.TypeCode.Object;
   begin

      --  Insert implementation of get_canonical_typecode

      return Result;
   end get_canonical_typecode;

   ---------------------
   --  get_primitive  --
   ---------------------
   function get_primitive
     (Self : access Object;
      kind : CORBA.Repository_Root.PrimitiveKind)
     return CORBA.Repository_Root.PrimitiveDef_Forward.Ref
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      Result : CORBA.Repository_Root.PrimitiveDef.Ref;
      Obj : constant PrimitiveDef.Impl.Object_Ptr
        := new PrimitiveDef.Impl.Object;
      IDL_Type : CORBA.TypeCode.Object;
   begin
      --  Create the appropriate TypeCode
      case kind is
         when pk_null =>
            IDL_Type := TC_Null;
         when pk_void =>
            IDL_Type := TC_Void;
         when pk_short =>
            IDL_Type := TC_Short;
         when pk_long =>
            IDL_Type := TC_Long;
         when pk_ushort =>
            IDL_Type := TC_Unsigned_Short;
         when pk_ulong =>
            IDL_Type := TC_Unsigned_Long;
         when pk_float =>
            IDL_Type := TC_Float;
         when pk_double =>
            IDL_Type := TC_Double;
         when pk_boolean =>
            IDL_Type := TC_Boolean;
         when pk_char =>
            IDL_Type := TC_Char;
         when pk_octet =>
            IDL_Type := TC_Octet;
         when pk_any =>
            IDL_Type := TC_Any;
         when pk_TypeCode =>
            IDL_Type := TC_TypeCode;
         when pk_Principal =>
            IDL_Type := TC_Principal;
         when pk_string =>
            IDL_Type := TC_String;
         when pk_objref =>
            IDL_Type := CORBA.Object.TC_Object;
         when pk_longlong =>
            IDL_Type := TC_Long_Long;
         when pk_ulonglong =>
            IDL_Type := TC_Unsigned_Long_Long;
         when pk_longdouble =>
            IDL_Type := TC_Long_Double;
         when pk_wchar =>
            IDL_Type := TC_Wchar;
         when pk_wstring =>
            IDL_Type := TC_Wide_String;
         when pk_value_base =>
            IDL_Type := TC_Value;
      end case;

      --  initialize the object
      PrimitiveDef.Impl.Init (Obj,
                              IRObject.Impl.Object_Ptr (Obj),
                              dk_Primitive,
                              IDL_Type,
                              kind);

      --  activate it
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Result);

      return PrimitiveDef.Convert_Forward.To_Forward (Result);
   end get_primitive;

   function create_string
     (Self : access Object;
      bound : CORBA.Unsigned_Long)
     return CORBA.Repository_Root.StringDef_Forward.Ref
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      Result : StringDef.Ref;
      Obj : constant StringDef.Impl.Object_Ptr
        := new StringDef.Impl.Object;
   begin
      --  initialization of the string
      StringDef.Impl.Init (Obj,
                           IRObject.Impl.Object_Ptr (Obj),
                           dk_String,
                           bound);

      --  activate it
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Result);

      return StringDef.Convert_Forward.To_Forward (Result);
   end create_string;

   function create_wstring
     (Self : access Object;
      bound : CORBA.Unsigned_Long)
     return CORBA.Repository_Root.WstringDef_Forward.Ref
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      Result : CORBA.Repository_Root.WstringDef.Ref;
      Obj : constant WstringDef.Impl.Object_Ptr
        := new WstringDef.Impl.Object;
   begin
      --  initialization of the wstring
      WstringDef.Impl.Init (Obj,
                            IRObject.Impl.Object_Ptr (Obj),
                            dk_Wstring,
                            bound);

      --  activate it
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Result);

      return WstringDef.Convert_Forward.To_Forward (Result);
   end create_wstring;

   function create_sequence
     (Self : access Object;
      bound : CORBA.Unsigned_Long;
      element_type : CORBA.Repository_Root.IDLType.Ref)
     return CORBA.Repository_Root.SequenceDef_Forward.Ref
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      Result : CORBA.Repository_Root.SequenceDef.Ref;
      Elem_Obj : PortableServer.Servant;
      Obj : constant SequenceDef.Impl.Object_Ptr
        := new SequenceDef.Impl.Object;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant (element_type,
                                               Elem_Obj);
      --  initialization of the Sequence
      SequenceDef.Impl.Init (Obj,
                             IRObject.Impl.Object_Ptr (Obj),
                             dk_Sequence,
                             bound,
                             element_type);

      --  activate it
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Result);

      return SequenceDef.Convert_Forward.To_Forward (Result);
   end create_sequence;

   function create_array
     (Self : access Object;
      length : CORBA.Unsigned_Long;
      element_type : CORBA.Repository_Root.IDLType.Ref)
     return CORBA.Repository_Root.ArrayDef_Forward.Ref
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      Result : CORBA.Repository_Root.ArrayDef.Ref;
      Obj : constant ArrayDef.Impl.Object_Ptr := new ArrayDef.Impl.Object;
      Elem_Obj : PortableServer.Servant;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant (element_type,
                                               Elem_Obj);
      --  initialization of the Array
      ArrayDef.Impl.Init (Obj,
                          IRObject.Impl.Object_Ptr (Obj),
                          dk_Array,
                          length,
                          element_type);

      --  activate it
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Result);

      return ArrayDef.Convert_Forward.To_Forward (Result);
   end create_array;

   function create_fixed
     (Self : access Object;
      IDL_digits : CORBA.Unsigned_Short;
      scale : CORBA.Short)
     return CORBA.Repository_Root.FixedDef_Forward.Ref
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      Result : CORBA.Repository_Root.FixedDef.Ref;
      Obj : constant FixedDef.Impl.Object_Ptr := new FixedDef.Impl.Object;
   begin
      --  initialization of the Fixed
      FixedDef.Impl.Init (Obj,
                          IRObject.Impl.Object_Ptr (Obj),
                          dk_Fixed,
                          IDL_digits,
                          scale);

      --  Activate it
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Result);

      return FixedDef.Convert_Forward.To_Forward (Result);
   end create_fixed;

end CORBA.Repository_Root.Repository.Impl;
