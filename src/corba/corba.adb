------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                C O R B A                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id: //droopi/main/src/corba/corba.adb#27 $

with Ada.Characters.Handling;

with PolyORB.CORBA_P.Exceptions;

with PolyORB.Exceptions;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Types;
with PolyORB.Utils.Strings;

package body CORBA is

   function To_PolyORB_NV (NV : NamedValue) return PolyORB.Any.NamedValue;

   function To_PolyORB_NV (NV : NamedValue) return PolyORB.Any.NamedValue is
   begin
      return PolyORB.Any.NamedValue'
        (Name      => PolyORB.Types.Identifier (NV.Name),
         Argument  => Internals.To_PolyORB_Any (NV.Argument),
         Arg_Modes => PolyORB.Any.Flags (NV.Arg_Modes));
   end To_PolyORB_NV;

   function To_CORBA_NV (NV : PolyORB.Any.NamedValue) return NamedValue is
   begin
      return CORBA.NamedValue'
        (Name      => CORBA.Identifier (NV.Name),
         Argument  => CORBA.Any'(The_Any => NV.Argument),
         Arg_Modes => Flags (NV.Arg_Modes));
   end To_CORBA_NV;

   --------------------------
   -- TC_Completion_Status --
   --------------------------

   function TC_Completion_Status
     return CORBA.TypeCode.Object;
   --  The typecode for standard enumeration type completion_status.

   TC_Completion_Status_Cache : CORBA.TypeCode.Object;

   function TC_Completion_Status
     return CORBA.TypeCode.Object
   is
      use type PolyORB.Types.Unsigned_Long;

      TC : CORBA.TypeCode.Object renames TC_Completion_Status_Cache;

   begin
      if PolyORB.Any.TypeCode.Parameter_Count
        (CORBA.TypeCode.Internals.To_PolyORB_Object (TC)) /= 0
      then
         return TC_Completion_Status_Cache;
      end if;

      TC := CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Enum);
      TypeCode.Internals.Add_Parameter
        (TC, To_Any (To_PolyORB_String ("completion_status")));
      TypeCode.Internals.Add_Parameter
        (TC, To_Any (To_PolyORB_String
                     ("IDL:omg.org/CORBA/completion_status:1.0")));

      for C in Completion_Status'Range loop
         TypeCode.Internals.Add_Parameter
           (TC, To_Any (To_PolyORB_String (Completion_Status'Image (C))));
      end loop;

      return TC;
   end TC_Completion_Status;

   ---------------------------------
   -- String conversion functions --
   ---------------------------------

   function To_CORBA_String
     (Source : Standard.String)
     return CORBA.String is
   begin
      return To_PolyORB_String (Source);
   end To_CORBA_String;

   function To_CORBA_String
     (Source : Standard.String)
     return CORBA.Identifier is
   begin
      return To_PolyORB_String (Source);
   end To_CORBA_String;

   function To_CORBA_Wide_String
     (Source : Standard.Wide_String)
     return CORBA.Wide_String is
   begin
      return CORBA.Wide_String
        (Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String
         (Source));
   end To_CORBA_Wide_String;

   function To_Standard_Wide_String
     (Source : CORBA.Wide_String)
     return Standard.Wide_String is
   begin
      return Ada.Strings.Wide_Unbounded.To_Wide_String
        (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String
         (Source));
   end To_Standard_Wide_String;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out System_Exception_Members)
   is
      Str : constant Standard.String :=
        Ada.Exceptions.Exception_Message (From);
      Val : Unsigned_Long;
   begin
      --  Check length.
      if Str'Length /= 5 then
         Raise_Bad_Param (Default_Sys_Member);
      end if;

      --  Unmarshall completion status.
      --  This can raise constraint_error.
      To.Completed := Completion_Status'Val (Character'Pos (Str (Str'Last)));

      --  Unmarshall minor.
      Val := 0;
      for J in Str'First .. Str'Last - 1 loop
         Val := Val * 256 + Character'Pos (Str (J));
      end loop;
      To.Minor := Val;

   end Get_Members;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidName_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= InvalidName'Identity then
         Raise_Bad_Param (Default_Sys_Member);
      end if;

      To := InvalidName_Members'
        (IDL_Exception_Members with null record);
   end Get_Members;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out InconsistentTypeCode_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= InconsistentTypeCode'Identity then
         Raise_Bad_Param (Default_Sys_Member);
      end if;

      To := InconsistentTypeCode_Members'
        (IDL_Exception_Members with null record);
   end Get_Members;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out PolicyError_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= PolicyError'Identity then
         Raise_Bad_Param (Default_Sys_Member);
      end if;

      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out UnknownUserException_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= UnknownUserException'Identity then
         Raise_Bad_Param (Default_Sys_Member);
      end if;

      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   ----------------------------
   -- Raise_System_Exception --
   ----------------------------

   procedure Raise_System_Exception
     (Excp      : in Ada.Exceptions.Exception_Id;
      Excp_Memb : in System_Exception_Members)
   is
      Str : Standard.String (1 .. 5);
      Val : CORBA.Unsigned_Long;
   begin
      --  Marshall Minor and Completed fields of EXCP_MEMB into a string.
      --  A trivial marshalling is used:
      --  Str (1 .. 4)   Minor (MSB first)
      --  Str (5)        Completed

      Str (5) := Character'Val (Completion_Status'Pos (Excp_Memb.Completed));
      Val := Excp_Memb.Minor;

      for J in 1 .. 4 loop
         Str (J) := Character'Val (Val / 2 ** 24);
         Val := (Val mod 2 ** 24) * 256;
      end loop;

      --  Raise the exception.
      Ada.Exceptions.Raise_Exception (Excp, Str);

      --  'Excp' cannot be null.

      raise Program_Error;
   end Raise_System_Exception;

   -------------------
   -- Raise_Unknown --
   -------------------

   procedure Raise_Unknown
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Unknown'Identity, Excp_Memb);
   end Raise_Unknown;

   ---------------------
   -- Raise_Bad_Param --
   ---------------------

   procedure Raise_Bad_Param
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Bad_Param'Identity, Excp_Memb);
   end Raise_Bad_Param;

   ---------------------
   -- Raise_No_Memory --
   ---------------------

   procedure Raise_No_Memory
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (No_Memory'Identity, Excp_Memb);
   end Raise_No_Memory;

   ---------------------
   -- Raise_Imp_Limit --
   ---------------------

   procedure Raise_Imp_Limit
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Imp_Limit'Identity, Excp_Memb);
   end Raise_Imp_Limit;

   ------------------------
   -- Raise_Comm_Failure --
   ------------------------

   procedure Raise_Comm_Failure
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Comm_Failure'Identity, Excp_Memb);
   end Raise_Comm_Failure;

   ----------------------
   -- Raise_Inv_Objref --
   ----------------------

   procedure Raise_Inv_Objref
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Inv_Objref'Identity, Excp_Memb);
   end Raise_Inv_Objref;

   -------------------------
   -- Raise_No_Permission --
   -------------------------

   procedure Raise_No_Permission
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (No_Permission'Identity, Excp_Memb);
   end Raise_No_Permission;

   --------------------
   -- Raise_Internal --
   --------------------

   procedure Raise_Internal
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Internal'Identity, Excp_Memb);
   end Raise_Internal;

   -------------------
   -- Raise_Marshal --
   -------------------

   procedure Raise_Marshal
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Marshal'Identity, Excp_Memb);
   end Raise_Marshal;

   ----------------------
   -- Raise_Initialize --
   ----------------------

   procedure Raise_Initialize
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Initialize'Identity, Excp_Memb);
   end Raise_Initialize;

   ------------------------
   -- Raise_No_Implement --
   ------------------------

   procedure Raise_No_Implement
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (No_Implement'Identity, Excp_Memb);
   end Raise_No_Implement;

   ------------------------
   -- Raise_Bad_TypeCode --
   ------------------------

   procedure Raise_Bad_TypeCode
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Bad_TypeCode'Identity, Excp_Memb);
   end Raise_Bad_TypeCode;

   -------------------------
   -- Raise_Bad_Operation --
   -------------------------

   procedure Raise_Bad_Operation
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Bad_Operation'Identity, Excp_Memb);
   end Raise_Bad_Operation;

   ------------------------
   -- Raise_No_Resources --
   ------------------------

   procedure Raise_No_Resources
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (No_Resources'Identity, Excp_Memb);
   end Raise_No_Resources;

   -----------------------
   -- Raise_No_Response --
   -----------------------

   procedure Raise_No_Response
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (No_Response'Identity, Excp_Memb);
   end Raise_No_Response;

   -------------------------
   -- Raise_Persist_Store --
   -------------------------

   procedure Raise_Persist_Store
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Persist_Store'Identity, Excp_Memb);
   end Raise_Persist_Store;

   -------------------------
   -- Raise_Bad_Inv_Order --
   -------------------------

   procedure Raise_Bad_Inv_Order
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Bad_Inv_Order'Identity, Excp_Memb);
   end Raise_Bad_Inv_Order;

   ---------------------
   -- Raise_Transient --
   ---------------------

   procedure Raise_Transient
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Transient'Identity, Excp_Memb);
   end Raise_Transient;

   --------------------
   -- Raise_Free_Mem --
   --------------------

   procedure Raise_Free_Mem
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Free_Mem'Identity, Excp_Memb);
   end Raise_Free_Mem;

   ---------------------
   -- Raise_Inv_Ident --
   ---------------------

   procedure Raise_Inv_Ident
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Inv_Ident'Identity, Excp_Memb);
   end Raise_Inv_Ident;

   --------------------
   -- Raise_Inv_Flag --
   --------------------

   procedure Raise_Inv_Flag
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Inv_Flag'Identity, Excp_Memb);
   end Raise_Inv_Flag;

   ---------------------
   -- Raise_Intf_Repos --
   ---------------------

   procedure Raise_Intf_Repos
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Intf_Repos'Identity, Excp_Memb);
   end Raise_Intf_Repos;

   -----------------------
   -- Raise_Bad_Context --
   -----------------------

   procedure Raise_Bad_Context
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Bad_Context'Identity, Excp_Memb);
   end Raise_Bad_Context;

   -----------------------
   -- Raise_Obj_Adapter --
   -----------------------

   procedure Raise_Obj_Adapter
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Obj_Adapter'Identity, Excp_Memb);
   end Raise_Obj_Adapter;

   ---------------------------
   -- Raise_Data_Conversion --
   ---------------------------

   procedure Raise_Data_Conversion
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Data_Conversion'Identity, Excp_Memb);
   end Raise_Data_Conversion;

   ----------------------------
   -- Raise_Object_Not_Exist --
   ----------------------------

   procedure Raise_Object_Not_Exist
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Object_Not_Exist'Identity, Excp_Memb);
   end Raise_Object_Not_Exist;

   --------------------------------
   -- Raise_Transaction_Required --
   --------------------------------

   procedure Raise_Transaction_Required
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Transaction_Required'Identity, Excp_Memb);
   end Raise_Transaction_Required;

   ----------------------------------
   -- Raise_Transaction_Rolledback --
   ----------------------------------

   procedure Raise_Transaction_Rolledback
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Transaction_Rolledback'Identity, Excp_Memb);
   end Raise_Transaction_Rolledback;

   -------------------------------
   -- Raise_Invalid_Transaction --
   -------------------------------

   procedure Raise_Invalid_Transaction
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Invalid_Transaction'Identity, Excp_Memb);
   end Raise_Invalid_Transaction;

   ----------------------
   -- Raise_Inv_Policy --
   ----------------------

   procedure Raise_Inv_Policy
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Inv_Policy'Identity, Excp_Memb);
   end Raise_Inv_Policy;

   --------------------------------
   -- Raise_Codeset_Incompatible --
   --------------------------------

   procedure Raise_Codeset_Incompatible
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Codeset_Incompatible'Identity, Excp_Memb);
   end Raise_Codeset_Incompatible;

   -------------------
   -- Raise_Rebind --
   -------------------

   procedure Raise_Rebind
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Rebind'Identity, Excp_Memb);
   end Raise_Rebind;

   -------------------
   -- Raise_Timeout --
   -------------------

   procedure Raise_Timeout
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Timeout'Identity, Excp_Memb);
   end Raise_Timeout;

   -----------------------------------
   -- Raise_Transaction_Unavailable --
   -----------------------------------

   procedure Raise_Transaction_Unavailable
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Transaction_Unavailable'Identity, Excp_Memb);
   end Raise_Transaction_Unavailable;

   ----------------------------
   -- Raise_Transaction_Mode --
   ----------------------------

   procedure Raise_Transaction_Mode
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Transaction_Mode'Identity, Excp_Memb);
   end Raise_Transaction_Mode;

   -------------------
   -- Raise_Bad_Qos --
   -------------------

   procedure Raise_Bad_Qos
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Bad_Qos'Identity, Excp_Memb);
   end Raise_Bad_Qos;

   package body TypeCode is

      -----------------
      -- Get_Members --
      -----------------

      procedure Get_Members
        (From : in     Ada.Exceptions.Exception_Occurrence;
         To   :    out Bounds_Members)
      is
         use Ada.Exceptions;

      begin
         if Exception_Identity (From) /= InvalidName'Identity then
            Raise_Bad_Param (Default_Sys_Member);
         end if;

         To := Bounds_Members'
           (IDL_Exception_Members with null record);
      end Get_Members;

      procedure Get_Members
        (From : in     Ada.Exceptions.Exception_Occurrence;
         To   :    out BadKind_Members)
      is
         use Ada.Exceptions;

      begin
         if Exception_Identity (From) /= InvalidName'Identity then
            Raise_Bad_Param (Default_Sys_Member);
         end if;

         To := BadKind_Members'
           (IDL_Exception_Members with null record);
      end Get_Members;

      ---------
      -- "=" --
      ---------

      function "=" (Left, Right : in Object) return Boolean is
      begin
         return PolyORB.Any.TypeCode."="
           (Internals.To_PolyORB_Object (Left),
            Internals.To_PolyORB_Object (Right));
      end "=";

      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (Left, Right : in Object) return Boolean is
      begin
         return PolyORB.Any.TypeCode.Equivalent
           (Internals.To_PolyORB_Object (Left),
            Internals.To_PolyORB_Object (Right));
      end Equivalent;

      -------------------------
      -- Get_Compact_TypeCod --
      -------------------------

      function Get_Compact_TypeCode (Self : in Object) return Object is
      begin
         return CORBA.TypeCode.Object
           (PolyORB.Any.TypeCode.Get_Compact_TypeCode
            (Internals.To_PolyORB_Object (Self)));
      end Get_Compact_TypeCode;

      ----------
      -- Kind --
      ----------

      function Kind (Self : in Object) return TCKind is
      begin
         return PolyORB.Any.TypeCode.Kind (Internals.To_PolyORB_Object (Self));
      end Kind;

      --------
      -- Id --
      --------

      function Id (Self : in Object) return RepositoryId is
      begin
         return CORBA.RepositoryId
           (PolyORB.Any.TypeCode.Id (Internals.To_PolyORB_Object (Self)));
      end Id;

      ----------
      -- Name --
      ----------

      function Name (Self : in Object) return Identifier is
      begin
         return CORBA.Identifier
           (PolyORB.Any.TypeCode.Name (Internals.To_PolyORB_Object (Self)));
      end Name;

      ------------------
      -- Member_Count --
      ------------------

      function Member_Count (Self : in Object) return Unsigned_Long is
      begin
         return CORBA.Unsigned_Long
           (PolyORB.Any.TypeCode.Member_Count
            (Internals.To_PolyORB_Object (Self)));
      end Member_Count;

      -----------------
      -- Member_Name --
      -----------------

      function Member_Name
        (Self  : in Object;
         Index : in Unsigned_Long)
        return Identifier
      is
      begin
         return CORBA.Identifier
           (PolyORB.Any.TypeCode.Member_Name
            (Internals.To_PolyORB_Object (Self),
             PolyORB.Types.Unsigned_Long (Index)));
      end Member_Name;

      -----------------
      -- Member_Type --
      -----------------

      function Member_Type
        (Self  : in Object;
         Index : in Unsigned_Long)
        return Object
      is
      begin
         return CORBA.TypeCode.Object
           (PolyORB.Any.TypeCode.Member_Type
            (Internals.To_PolyORB_Object (Self),
             PolyORB.Types.Unsigned_Long (Index)));
      end Member_Type;

      ------------------
      -- Member_Label --
      ------------------

      function Member_Label
        (Self  : in Object;
         Index : in Unsigned_Long)
        return Any
      is
      begin
         return CORBA.Any'
           (The_Any => PolyORB.Any.TypeCode.Member_Label
            (Internals.To_PolyORB_Object (Self),
             PolyORB.Types.Unsigned_Long (Index)));
      end Member_Label;

      ------------------------
      -- Discriminator_Type --
      ------------------------

      function Discriminator_Type (Self : in Object) return Object is
      begin
         return CORBA.TypeCode.Object
           (PolyORB.Any.TypeCode.Discriminator_Type
            (Internals.To_PolyORB_Object (Self)));
      end Discriminator_Type;

      -------------------
      -- Default_Index --
      -------------------

      function Default_Index (Self : in Object) return Long is
      begin
         return CORBA.Long
           (PolyORB.Any.TypeCode.Default_Index
            (Internals.To_PolyORB_Object (Self)));
      end Default_Index;

      ------------
      -- Length --
      ------------

      function Length (Self : in Object) return Unsigned_Long is
      begin
         return CORBA.Unsigned_Long
           (PolyORB.Any.TypeCode.Length
            (Internals.To_PolyORB_Object (Self)));
      end Length;

      ------------------
      -- Content_Type --
      ------------------

      function Content_Type (Self : in Object) return Object is
      begin
         return CORBA.TypeCode.Object
           (PolyORB.Any.TypeCode.Content_Type
            (Internals.To_PolyORB_Object (Self)));
      end Content_Type;

      ------------------
      -- Fixed_Digits --
      ------------------

      function Fixed_Digits (Self : in Object) return Unsigned_Short is
      begin
         return CORBA.Unsigned_Short
           (PolyORB.Any.TypeCode.Fixed_Digits
            (Internals.To_PolyORB_Object (Self)));
      end Fixed_Digits;

      -----------------
      -- Fixed_Scale --
      -----------------

      function Fixed_Scale (Self : in Object) return Short is
      begin
         return CORBA.Short
           (PolyORB.Any.TypeCode.Fixed_Scale
            (Internals.To_PolyORB_Object (Self)));
      end Fixed_Scale;

      -----------------------
      -- Member_Visibility --
      -----------------------

      function Member_Visibility
        (Self  : in Object;
         Index : in Unsigned_Long)
        return Visibility
      is
      begin
         return CORBA.Visibility
           (PolyORB.Any.TypeCode.Member_Visibility
            (Internals.To_PolyORB_Object (Self),
             PolyORB.Types.Unsigned_Long (Index)));
      end Member_Visibility;

      -------------------
      -- Type_Modifier --
      -------------------

      function Type_Modifier (Self : in Object) return ValueModifier is
      begin
         return PolyORB.Any.TypeCode.Type_Modifier
           (Internals.To_PolyORB_Object (Self));
      end Type_Modifier;

      ------------------------
      -- Concrete_Base_Type --
      ------------------------

      function Concrete_Base_Type (Self : in Object) return Object is
      begin
         return CORBA.TypeCode.Object
           (PolyORB.Any.TypeCode.Concrete_Base_Type
            (Internals.To_PolyORB_Object (Self)));
      end Concrete_Base_Type;

      package body Internals is

         --------------
         -- Set_Kind --
         --------------

         procedure Set_Kind (Self : out Object; Kind : in TCKind) is
            P_Self : PolyORB.Any.TypeCode.Object;
         begin
            PolyORB.Any.TypeCode.Set_Kind (P_Self, Kind);
            Self := Internals.To_CORBA_Object (P_Self);
         end Set_Kind;

         -------------------
         -- Add_Parameter --
         -------------------

         procedure Add_Parameter (Self : in out Object; Param : in Any) is
            P_Self : PolyORB.Any.TypeCode.Object := To_PolyORB_Object (Self);
         begin
            PolyORB.Any.TypeCode.Add_Parameter
              (P_Self, CORBA.Internals.To_PolyORB_Any (Param));
            Self := To_CORBA_Object (P_Self);
         end Add_Parameter;

         -----------------------
         -- To_PolyORB_Object --
         -----------------------

         function To_PolyORB_Object
           (Self : in CORBA.TypeCode.Object)
           return PolyORB.Any.TypeCode.Object
         is
         begin
            return PolyORB.Any.TypeCode.Object (Self);
         end To_PolyORB_Object;

         ---------------------
         -- To_CORBA_Object --
         ---------------------

         function To_CORBA_Object
           (Self : in PolyORB.Any.TypeCode.Object)
           return CORBA.TypeCode.Object
         is
         begin
            return CORBA.TypeCode.Object (Self);
         end To_CORBA_Object;

      end Internals;

   end TypeCode;

   -------------
   -- TC_Null --
   -------------

   function TC_Null return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Null);
   end TC_Null;

   -------------
   -- TC_Void --
   -------------

   function TC_Void return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Void);
   end TC_Void;

   --------------
   -- TC_Short --
   --------------

   function TC_Short return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Short);
   end TC_Short;

   -------------
   -- TC_Long --
   -------------

   function TC_Long return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Long);
   end TC_Long;

   ------------------
   -- TC_Long_Long --
   ------------------

   function TC_Long_Long return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Long_Long);
   end TC_Long_Long;

   -----------------------
   -- TC_Unsigned_Short --
   -----------------------

   function TC_Unsigned_Short return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Unsigned_Short);
   end TC_Unsigned_Short;

   ----------------------
   -- TC_Unsigned_Long --
   ----------------------

   function TC_Unsigned_Long return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Unsigned_Long);
   end TC_Unsigned_Long;

   ---------------------------
   -- TC_Unsigned_Long_Long --
   ---------------------------

   function TC_Unsigned_Long_Long return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Unsigned_Long_Long);
   end TC_Unsigned_Long_Long;

   --------------
   -- TC_Float --
   --------------

   function TC_Float return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Float);
   end TC_Float;

   ---------------
   -- TC_Double --
   ---------------

   function TC_Double return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Double);
   end TC_Double;

   --------------------
   -- TC_Long_Double --
   --------------------

   function TC_Long_Double return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Long_Double);
   end TC_Long_Double;

   ----------------
   -- TC_Boolean --
   ----------------

   function TC_Boolean return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Boolean);
   end TC_Boolean;

   -------------
   -- TC_Char --
   -------------

   function TC_Char return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Char);
   end TC_Char;

   --------------
   -- TC_Wchar --
   --------------

   function TC_Wchar return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Wchar);
   end TC_Wchar;

   --------------
   -- TC_Octet --
   --------------

   function TC_Octet return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Octet);
   end TC_Octet;

   ------------
   -- TC_Any --
   ------------

   function TC_Any return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Any);
   end TC_Any;

   -----------------
   -- TC_TypeCode --
   -----------------

   function TC_TypeCode return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_TypeCode);
   end TC_TypeCode;

   ---------------
   -- TC_String --
   ---------------

   function TC_String return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_String);
   end TC_String;

   --------------------
   -- TC_Wide_String --
   --------------------

   function TC_Wide_String  return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Wide_String);
   end TC_Wide_String;

   ------------------
   -- TC_Principal --
   ------------------

   function TC_Principal return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Principal);
   end TC_Principal;

   --------------
   -- TC_Value --
   --------------

   function TC_Value return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Value);
   end TC_Value;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : in Any) return Boolean is
   begin
      return PolyORB.Any."="
        (Internals.To_PolyORB_Any (Left),
         Internals.To_PolyORB_Any (Right));
   end "=";

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in Short) return CORBA.Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.To_Any (PolyORB.Types.Short (Item)));
   end To_Any;

   function To_Any (Item : in Long) return Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.To_Any (PolyORB.Types.Long (Item)));
   end To_Any;

   function To_Any (Item : in Long_Long) return Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.To_Any (PolyORB.Types.Long_Long (Item)));
   end To_Any;

   function To_Any (Item : in Unsigned_Short) return Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Short (Item)));
   end To_Any;

   function To_Any (Item : in Unsigned_Long) return Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Long (Item)));
   end To_Any;

   function To_Any (Item : in Unsigned_Long_Long) return Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.To_Any
         (PolyORB.Types.Unsigned_Long_Long (Item)));
   end To_Any;

   function To_Any (Item : in CORBA.Float) return Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.To_Any (PolyORB.Types.Float (Item)));

   end To_Any;

   function To_Any (Item : in Double) return Any is
   begin
      return CORBA.Any '
        (The_Any => PolyORB.Any.To_Any (PolyORB.Types.Double (Item)));
   end To_Any;

   function To_Any (Item : in Long_Double) return Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.To_Any (PolyORB.Types.Long_Double (Item)));
   end To_Any;

   function To_Any (Item : in Boolean) return Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.To_Any (PolyORB.Types.Boolean (Item)));
   end To_Any;

   function To_Any (Item : in Char) return Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.To_Any (PolyORB.Types.Char (Item)));
   end To_Any;

   function To_Any (Item : in Wchar) return Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.To_Any (PolyORB.Types.Wchar (Item)));
   end To_Any;

   function To_Any (Item : in Octet) return Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.To_Any (PolyORB.Types.Octet (Item)));
   end To_Any;

   function To_Any (Item : in Any) return Any is
   begin
      return CORBA.Any'(The_Any => PolyORB.Any.To_Any
                        (Internals.To_PolyORB_Any (Item)));
   end To_Any;

   function To_Any (Item : in TypeCode.Object) return Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.To_Any
         (CORBA.TypeCode.Internals.To_PolyORB_Object (Item)));
   end To_Any;

   function To_Any (Item : in CORBA.String) return Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.To_Any (PolyORB.Types.String (Item)));
   end To_Any;

   function To_Any (Item : in CORBA.Wide_String) return Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.To_Any (PolyORB.Types.Wide_String (Item)));
   end To_Any;

   function To_Any
     (Item : Completion_Status)
     return CORBA.Any
   is
      Result : CORBA.Any := Get_Empty_Any_Aggregate (TC_Completion_Status);

   begin
      Add_Aggregate_Element
        (Result, To_Any (Unsigned_Long (Completion_Status'Pos (Item))));

      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in Any) return Short is
   begin
      return Short
        (PolyORB.Types.Short'(PolyORB.Any.From_Any
                              (Internals.To_PolyORB_Any (Item))));
   end From_Any;

   function From_Any (Item : in Any) return Long is
   begin
      return Long
        (PolyORB.Types.Long'(PolyORB.Any.From_Any
                             (Internals.To_PolyORB_Any (Item))));
   end From_Any;

   function From_Any (Item : in Any) return Long_Long is
   begin
      return Long_Long
        (PolyORB.Types.Long_Long'(PolyORB.Any.From_Any
                                  (Internals.To_PolyORB_Any (Item))));
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Short is
   begin
      return Unsigned_Short
        (PolyORB.Types.Unsigned_Short'(PolyORB.Any.From_Any
                                       (Internals.To_PolyORB_Any (Item))));
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Long is
   begin
      return Unsigned_Long
        (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any
                                      (Internals.To_PolyORB_Any (Item))));
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Long_Long is
   begin
      return Unsigned_Long_Long
        (PolyORB.Types.Unsigned_Long_Long'(PolyORB.Any.From_Any
                                           (Internals.To_PolyORB_Any (Item))));
   end From_Any;

   function From_Any (Item : in Any) return CORBA.Float is
   begin
      return CORBA.Float
        (PolyORB.Types.Float'(PolyORB.Any.From_Any
                              (Internals.To_PolyORB_Any (Item))));
   end From_Any;

   function From_Any (Item : in Any) return Double is
   begin
      return Double
        (PolyORB.Types.Double'(PolyORB.Any.From_Any
                               (Internals.To_PolyORB_Any (Item))));
   end From_Any;

   function From_Any (Item : in Any) return Long_Double is
   begin
      return Long_Double
        (PolyORB.Types.Long_Double'(PolyORB.Any.From_Any
                                    (Internals.To_PolyORB_Any (Item))));
   end From_Any;

   function From_Any (Item : in Any) return Boolean is
   begin
      return Boolean
        (PolyORB.Types.Boolean'(PolyORB.Any.From_Any
                                (Internals.To_PolyORB_Any (Item))));
   end From_Any;

   function From_Any (Item : in Any) return Char is
   begin
      return Char
        (PolyORB.Types.Char'(PolyORB.Any.From_Any
                             (Internals.To_PolyORB_Any (Item))));
   end From_Any;

   function From_Any (Item : in Any) return Wchar is
   begin
      return Wchar
        (PolyORB.Types.Wchar'(PolyORB.Any.From_Any
                              (Internals.To_PolyORB_Any (Item))));
   end From_Any;

   function From_Any (Item : in Any) return Octet is
   begin
      return Octet
        (PolyORB.Types.Octet'(PolyORB.Any.From_Any
                              (Internals.To_PolyORB_Any (Item))));
   end From_Any;

   function From_Any (Item : in Any) return Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.From_Any (Internals.To_PolyORB_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return TypeCode.Object is
   begin
      return CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.From_Any (Internals.To_PolyORB_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return CORBA.String is
   begin
      return CORBA.String
        (PolyORB.Types.String'(PolyORB.Any.From_Any
                               (Internals.To_PolyORB_Any (Item))));
   end From_Any;

   function From_Any (Item : in Any) return CORBA.Wide_String is
   begin
      return CORBA.Wide_String
        (PolyORB.Types.Wide_String'(PolyORB.Any.From_Any
                                    (Internals.To_PolyORB_Any (Item))));
   end From_Any;

   function From_Any (Item : CORBA.Any) return Completion_Status is
   begin
      return From_Any (PolyORB.Any.Get_Aggregate_Element
                       (Internals.To_PolyORB_Any (Item),
                        PolyORB.Any.TypeCode.TC_Unsigned_Long, 0));
   end From_Any;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (The_Any : in Any) return TypeCode.Object is
   begin
      return CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.Get_Type (Internals.To_PolyORB_Any (The_Any)));
   end Get_Type;

   ----------------------
   -- Get_Unwound_Type --
   ----------------------

   function Get_Unwound_Type (The_Any : in Any) return TypeCode.Object is
   begin
      return CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.Get_Unwound_Type (Internals.To_PolyORB_Any (The_Any)));
   end Get_Unwound_Type;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type
     (The_Any  : in out Any;
      The_Type : in     TypeCode.Object)
   is
   begin
      PolyORB.Any.Set_Type
        (The_Any.The_Any,
         CORBA.TypeCode.Internals.To_PolyORB_Object (The_Type));
   end Set_Type;

   -------------------------------
   -- Iterate_Over_Any_Elements --
   -------------------------------

   procedure Iterate_Over_Any_Elements (In_Any : in Any) is
   begin
      --  null;
      raise PolyORB.Not_Implemented;
   end Iterate_Over_Any_Elements;

   -------------------
   -- Get_Empty_Any --
   -------------------

   function Get_Empty_Any (Tc : TypeCode.Object) return Any is
   begin
      return CORBA.Any'
        (The_Any => PolyORB.Any.Get_Empty_Any
         (CORBA.TypeCode.Internals.To_PolyORB_Object (Tc)));
   end Get_Empty_Any;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Any_Value : in Any) return Boolean is
   begin
      return PolyORB.Any.Is_Empty (Internals.To_PolyORB_Any (Any_Value));
   end Is_Empty;

   -------------------
   -- Set_Any_Value --
   -------------------

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value     :        Short) is
--     begin
--        PolyORB.Any.Set_Any_Value
--          (Any_Value, PolyORB.Types.Short (Value));
--     end Set_Any_Value;

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value : Long) is
--     begin
--        PolyORB.Any.Set_Any_Value
--          (Any_Value, PolyORB.Types.Long (Value));
--     end Set_Any_Value;

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value : Long_Long) is
--     begin
--        PolyORB.Any.Set_Any_Value
--          (Any_Value, PolyORB.Types.Long_Long (Value));
--     end Set_Any_Value;

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value : Unsigned_Short) is
--     begin
--        PolyORB.Any.Set_Any_Value
--          (Any_Value, PolyORB.Types.Unsigned_Short (Value));
--     end Set_Any_Value;

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value : Unsigned_Long) is
--     begin
--        PolyORB.Any.Set_Any_Value
--          (Any_Value, PolyORB.Types.Unsigned_Long (Value));
--     end Set_Any_Value;

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value : Unsigned_Long_Long) is
--     begin
--        PolyORB.Any.Set_Any_Value
--          (Any_Value, PolyORB.Types.Unsigned_Long_Long (Value));
--     end Set_Any_Value;

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value : CORBA.Float) is
--     begin
--        PolyORB.Any.Set_Any_Value
--          (Any_Value, PolyORB.Types.Float (Value));
--     end Set_Any_Value;

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value : Double) is
--     begin
--        PolyORB.Any.Set_Any_Value
--          (Any_Value, PolyORB.Types.Double (Value));
--     end Set_Any_Value;

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value : Long_Double) is
--     begin
--        PolyORB.Any.Set_Any_Value
--          (Any_Value, PolyORB.Types.Long_Double (Value));
--     end Set_Any_Value;

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value : Boolean) is
--     begin
--        PolyORB.Any.Set_Any_Value
--          (Any_Value, PolyORB.Types.Boolean (Value));
--     end Set_Any_Value;

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value : Char) is
--     begin
--        PolyORB.Any.Set_Any_Value
--          (Any_Value, PolyORB.Types.Char (Value));
--     end Set_Any_Value;

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value : Wchar) is
--     begin
--        PolyORB.Any.Set_Any_Value
--          (Any_Value, PolyORB.Types.Wchar (Value));
--     end Set_Any_Value;

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value : Octet) is
--     begin
--        PolyORB.Any.Set_Any_Value
--          (Any_Value, PolyORB.Types.Octet (Value));
--     end Set_Any_Value;

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value : Any)
--       renames PolyORB.Any.Set_Any_Value;

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value : TypeCode.Object)
--       renames PolyORB.Any.Set_Any_Value;

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value : CORBA.String) is
--     begin
--        PolyORB.Any.Set_Any_Value
--          (Any_Value, PolyORB.Types.String (Value));
--     end Set_Any_Value;

--     procedure Set_Any_Value
--       (Any_Value : in out CORBA.Any;
--        Value : CORBA.Wide_String) is
--     begin
--        PolyORB.Any.Set_Any_Value
--          (Any_Value, PolyORB.Types.Wide_String (Value));
--     end Set_Any_Value;

   -----------------------------
   -- Set_Any_Aggregate_Value --
   -----------------------------

   procedure Set_Any_Aggregate_Value (Any_Value : in out CORBA.Any) is
   begin
      PolyORB.Any.Set_Any_Aggregate_Value (Any_Value.The_Any);
   end Set_Any_Aggregate_Value;

   -------------------------
   -- Get_Aggregate_Count --
   -------------------------

   function Get_Aggregate_Count (Value : Any) return Unsigned_Long is
   begin
      return Unsigned_Long (PolyORB.Any.Get_Aggregate_Count
                            (Internals.To_PolyORB_Any (Value)));
   end Get_Aggregate_Count;

   ---------------------------
   -- Add_Aggregate_Element --
   ---------------------------

   procedure Add_Aggregate_Element (Value : in out Any; Element : in Any) is
   begin
      PolyORB.Any.Add_Aggregate_Element
        (Value.The_Any,
         Internals.To_PolyORB_Any (Element));
   end Add_Aggregate_Element;

   ---------------------------
   -- Get_Aggregate_Element --
   ---------------------------

   function Get_Aggregate_Element
     (Value : Any;
      Tc    : CORBA.TypeCode.Object;
      Index : CORBA.Unsigned_Long)
     return Any is
   begin
      return CORBA.Any'(The_Any => PolyORB.Any.Get_Aggregate_Element
                        (Internals.To_PolyORB_Any (Value),
                         CORBA.TypeCode.Internals.To_PolyORB_Object (Tc),
                         PolyORB.Types.Unsigned_Long (Index)));
   end Get_Aggregate_Element;

   -----------------------------
   -- Get_Empty_Any_Aggregate --
   -----------------------------

   function Get_Empty_Any_Aggregate (Tc : CORBA.TypeCode.Object) return Any is
   begin
      return CORBA.Any'(The_Any => PolyORB.Any.Get_Empty_Any_Aggregate
                        (CORBA.TypeCode.Internals.To_PolyORB_Object (Tc)));
   end Get_Empty_Any_Aggregate;

   -----------
   -- Image --
   -----------

   function Image (NV : NamedValue) return Standard.String is
   begin
      return PolyORB.Any.Image (To_PolyORB_NV (NV));
   end Image;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent (RI1, RI2 : RepositoryId) return Boolean is
   begin
      return Is_Equivalent
        (To_Standard_String (RI1),
         To_Standard_String (RI2));
   end Is_Equivalent;

   function Is_Equivalent (RI1, RI2 : Standard.String) return Boolean
   is
      use Ada.Characters.Handling;
   begin
      return To_Lower (RI1) = To_Lower (RI2);
   end Is_Equivalent;

   ----------------------
   -- Raise_From_Error --
   ----------------------

   procedure Raise_From_Error
     (Error : in out PolyORB.Exceptions.Error_Container)
   is
      use PolyORB.Exceptions;

   begin
      pragma Assert (Error.Kind in ORB_System_Error);

      declare
         Member : constant PolyORB.Exceptions.System_Exception_Members
           := PolyORB.Exceptions.System_Exception_Members (Error.Member.all);

         CORBA_Member : constant CORBA.System_Exception_Members
           := System_Exception_Members'
           (Minor =>  CORBA.Unsigned_Long (Member.Minor),
            Completed => CORBA.Completion_Status (Member.Completed));

      begin

         Free (Error.Member);

         --  One to one mapping of PolyORB Error_Id to CORBA System exceptions.

         case Error.Kind is
            when Unknown_E =>
               Raise_Unknown (CORBA_Member);

            when Bad_Param_E =>
               Raise_Bad_Param (CORBA_Member);

            when No_Memory_E =>
               Raise_No_Memory (CORBA_Member);

            when Imp_Limit_E =>
               Raise_Imp_Limit (CORBA_Member);

            when Comm_Failure_E =>
               Raise_Comm_Failure (CORBA_Member);

            when Inv_Objref_E =>
               Raise_Inv_Objref (CORBA_Member);

            when No_Permission_E =>
               Raise_No_Permission (CORBA_Member);

            when Internal_E =>
               Raise_Internal (CORBA_Member);

            when Marshal_E =>
               Raise_Marshal (CORBA_Member);

            when Initialize_E =>
               Raise_Initialize (CORBA_Member);

            when No_Implement_E =>
            Raise_No_Implement (CORBA_Member);

            when Bad_TypeCode_E =>
               Raise_Bad_TypeCode (CORBA_Member);

            when Bad_Operation_E =>
               Raise_Bad_Operation (CORBA_Member);

            when No_Resources_E =>
               Raise_No_Resources (CORBA_Member);

            when No_Response_E =>
            Raise_No_Response (CORBA_Member);

            when Persist_Store_E =>
               Raise_Persist_Store (CORBA_Member);

            when Bad_Inv_Order_E =>
               Raise_Bad_Inv_Order (CORBA_Member);

            when Transient_E =>
               Raise_Transient (CORBA_Member);

            when Free_Mem_E =>
               Raise_Free_Mem (CORBA_Member);

            when Inv_Ident_E =>
               Raise_Inv_Ident (CORBA_Member);

            when Inv_Flag_E =>
               Raise_Inv_Flag (CORBA_Member);

            when Intf_Repos_E =>
               Raise_Intf_Repos (CORBA_Member);

            when Bad_Context_E =>
               Raise_Bad_Context (CORBA_Member);

            when Obj_Adapter_E =>
               Raise_Obj_Adapter (CORBA_Member);

            when Data_Conversion_E =>
               Raise_Data_Conversion (CORBA_Member);

            when Object_Not_Exist_E =>
               Raise_Object_Not_Exist (CORBA_Member);

            when Transaction_Required_E =>
               Raise_Transaction_Required (CORBA_Member);

            when Transaction_Rolledback_E =>
               Raise_Transaction_Rolledback (CORBA_Member);

            when Invalid_Transaction_E =>
               Raise_Invalid_Transaction (CORBA_Member);

            when Inv_Policy_E =>
               Raise_Inv_Policy (CORBA_Member);

            when Codeset_Incompatible_E =>
               Raise_Codeset_Incompatible (CORBA_Member);

            when Rebind_E =>
               Raise_Rebind (CORBA_Member);

            when Timeout_E =>
               Raise_Timeout (CORBA_Member);

            when Transaction_Unavailable_E =>
               Raise_Transaction_Unavailable (CORBA_Member);

            when Transaction_Mode_E =>
               Raise_Transaction_Mode (CORBA_Member);

            when Bad_Qos_E =>
               Raise_Bad_Qos (CORBA_Member);

            when others =>
               raise Program_Error;

         end case;
      end;
   end Raise_From_Error;

   package body Internals is

      --------------------
      -- To_PolyORB_Any --
      --------------------

      function To_PolyORB_Any (Self : in CORBA.Any) return PolyORB.Any.Any is
      begin
         return Self.The_Any;
      end To_PolyORB_Any;

      ------------------
      -- To_CORBA_Any --
      ------------------

      function To_CORBA_Any (Self : in PolyORB.Any.Any) return CORBA.Any is
      begin
         return CORBA.Any'(The_Any => Self);
      end To_CORBA_Any;

      --------------------
      -- Copy_Any_Value --
      --------------------

      procedure Copy_Any_Value (Dest : in Any; Src : in Any) is
      begin
         PolyORB.Any.Copy_Any_Value (Dest.The_Any, Src.The_Any);
      end Copy_Any_Value;

   end Internals;

   ------------------------
   -- Initialize_Package --
   ------------------------

   procedure Initialize_Package;

   procedure Initialize_Package is
   begin
      PolyORB.CORBA_P.Exceptions.CORBA_Raise_From_Error
        := Raise_From_Error'Access;
   end Initialize_Package;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"corba",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize_Package'Access));
end CORBA;
