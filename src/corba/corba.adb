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

--  $Id: //droopi/main/src/corba/corba.adb#22 $

with Ada.Characters.Handling;

with PolyORB.CORBA_P.Exceptions;

with PolyORB.Exceptions;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Types;
with PolyORB.Utils.Strings;

package body CORBA is

   use PolyORB.Any;

   --------------------------
   -- TC_Completion_Status --
   --------------------------

   function TC_Completion_Status
     return PolyORB.Any.TypeCode.Object;
   --  The typecode for standard enumeration type completion_status.

   TC_Completion_Status_Cache : TypeCode.Object;

   function TC_Completion_Status
     return PolyORB.Any.TypeCode.Object
   is
      use type PolyORB.Types.Unsigned_Long;

      TC : TypeCode.Object renames TC_Completion_Status_Cache;

   begin
      if TypeCode.Parameter_Count (TC) /= 0 then
         return TC_Completion_Status_Cache;
      end if;

      TC := TypeCode.TC_Enum;
      TypeCode.Add_Parameter
        (TC, To_Any (To_PolyORB_String ("completion_status")));
      TypeCode.Add_Parameter
        (TC, To_Any (To_PolyORB_String
                     ("IDL:omg.org/CORBA/completion_status:1.0")));

      for C in Completion_Status'Range loop
         TypeCode.Add_Parameter
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

   ----------------------------------
   -- Raise_Initialization_Failure --
   ----------------------------------

   procedure Raise_Initialization_Failure
     (Excp_Memb : in System_Exception_Members) is
   begin
      Raise_System_Exception
        (Initialization_Failure'Identity, Excp_Memb);
   end Raise_Initialization_Failure;

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

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : in Any) return Boolean
     renames PolyORB.Any."=";

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in Short) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Short (Item));
   end To_Any;

   function To_Any (Item : in Long) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Long (Item));
   end To_Any;

   function To_Any (Item : in Long_Long) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Long_Long (Item));
   end To_Any;

   function To_Any (Item : in Unsigned_Short) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Short (Item));
   end To_Any;

   function To_Any (Item : in Unsigned_Long) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Long (Item));
   end To_Any;

   function To_Any (Item : in Unsigned_Long_Long) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Long_Long (Item));
   end To_Any;

   function To_Any (Item : in CORBA.Float) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Float (Item));
   end To_Any;

   function To_Any (Item : in Double) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Double (Item));
   end To_Any;

   function To_Any (Item : in Long_Double) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Long_Double (Item));
   end To_Any;

   function To_Any (Item : in Boolean) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Boolean (Item));
   end To_Any;

   function To_Any (Item : in Char) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Char (Item));
   end To_Any;

   function To_Any (Item : in Wchar) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Wchar (Item));
   end To_Any;

   function To_Any (Item : in Octet) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Octet (Item));
   end To_Any;

   function To_Any (Item : in Any) return Any
     renames PolyORB.Any.To_Any;

   function To_Any (Item : in TypeCode.Object) return Any
     renames PolyORB.Any.To_Any;

   function To_Any (Item : in CORBA.String) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.String (Item));
   end To_Any;

   function To_Any (Item : in CORBA.Wide_String) return Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.Wide_String (Item));
   end To_Any;

   function To_Any
     (Item : Completion_Status)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any
        := Get_Empty_Any_Aggregate (TC_Completion_Status);
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
        (PolyORB.Types.Short'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Long is
   begin
      return Long
        (PolyORB.Types.Long'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Long_Long is
   begin
      return Long_Long
        (PolyORB.Types.Long_Long'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Short is
   begin
      return Unsigned_Short
        (PolyORB.Types.Unsigned_Short'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Long is
   begin
      return Unsigned_Long
        (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Long_Long is
   begin
      return Unsigned_Long_Long
        (PolyORB.Types.Unsigned_Long_Long'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return CORBA.Float is
   begin
      return CORBA.Float
        (PolyORB.Types.Float'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Double is
   begin
      return Double
        (PolyORB.Types.Double'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Long_Double is
   begin
      return Long_Double
        (PolyORB.Types.Long_Double'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Boolean is
   begin
      return Boolean
        (PolyORB.Types.Boolean'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Char is
   begin
      return Char
        (PolyORB.Types.Char'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Wchar is
   begin
      return Wchar
        (PolyORB.Types.Wchar'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Octet is
   begin
      return Octet
        (PolyORB.Types.Octet'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Any
     renames PolyORB.Any.From_Any;

   function From_Any (Item : in Any) return TypeCode.Object
     renames PolyORB.Any.From_Any;

   function From_Any (Item : in Any) return CORBA.String is
   begin
      return CORBA.String
        (PolyORB.Types.String'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return CORBA.Wide_String is
   begin
      return CORBA.Wide_String
        (PolyORB.Types.Wide_String'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any
     (Item : PolyORB.Any.Any)
     return Completion_Status is
   begin
      return Completion_Status'Val
        (Unsigned_Long'
         (From_Any (PolyORB.Any.Get_Aggregate_Element
                    (Item, TC_Unsigned_Long, 0))));
   end From_Any;

   ----------------
   --  Get_Type  --
   ----------------

   function Get_Type (The_Any : in Any) return  TypeCode.Object
     renames PolyORB.Any.Get_Type;

   ------------------------
   --  Get_Unwound_Type  --
   ------------------------

   function Get_Unwound_Type (The_Any : in Any) return  TypeCode.Object
     renames PolyORB.Any.Get_Unwound_Type;

   ----------------
   --  Set_Type  --
   ----------------

   procedure Set_Type
     (The_Any  : in out Any;
      The_Type : in     TypeCode.Object)
     renames PolyORB.Any.Set_Type;

   ---------------------------------
   --  Iterate_Over_Any_Elements  --
   ---------------------------------
   procedure Iterate_Over_Any_Elements (In_Any : in Any) is
   begin
      --  null;
      raise PolyORB.Not_Implemented;
   end Iterate_Over_Any_Elements;

   ---------------------
   --  Get_Empty_Any  --
   ---------------------

   function Get_Empty_Any (Tc : TypeCode.Object) return Any
     renames PolyORB.Any.Get_Empty_Any;

   ----------------
   --  Is_Empty  --
   ----------------

   function Is_Empty (Any_Value : in Any) return Boolean
     renames PolyORB.Any.Is_Empty;

   ---------------------
   --  Set_Any_Value  --
   ---------------------

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value     :        Short) is
   begin
      PolyORB.Any.Set_Any_Value
        (Any_Value, PolyORB.Types.Short (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Long) is
   begin
      PolyORB.Any.Set_Any_Value
        (Any_Value, PolyORB.Types.Long (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Long_Long) is
   begin
      PolyORB.Any.Set_Any_Value
        (Any_Value, PolyORB.Types.Long_Long (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Unsigned_Short) is
   begin
      PolyORB.Any.Set_Any_Value
        (Any_Value, PolyORB.Types.Unsigned_Short (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Unsigned_Long) is
   begin
      PolyORB.Any.Set_Any_Value
        (Any_Value, PolyORB.Types.Unsigned_Long (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Unsigned_Long_Long) is
   begin
      PolyORB.Any.Set_Any_Value
        (Any_Value, PolyORB.Types.Unsigned_Long_Long (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : CORBA.Float) is
   begin
      PolyORB.Any.Set_Any_Value
        (Any_Value, PolyORB.Types.Float (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Double) is
   begin
      PolyORB.Any.Set_Any_Value
        (Any_Value, PolyORB.Types.Double (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Long_Double) is
   begin
      PolyORB.Any.Set_Any_Value
        (Any_Value, PolyORB.Types.Long_Double (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Boolean) is
   begin
      PolyORB.Any.Set_Any_Value
        (Any_Value, PolyORB.Types.Boolean (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Char) is
   begin
      PolyORB.Any.Set_Any_Value
        (Any_Value, PolyORB.Types.Char (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Wchar) is
   begin
      PolyORB.Any.Set_Any_Value
        (Any_Value, PolyORB.Types.Wchar (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Octet) is
   begin
      PolyORB.Any.Set_Any_Value
        (Any_Value, PolyORB.Types.Octet (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Any)
     renames PolyORB.Any.Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : TypeCode.Object)
     renames PolyORB.Any.Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : CORBA.String) is
   begin
      PolyORB.Any.Set_Any_Value
        (Any_Value, PolyORB.Types.String (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : CORBA.Wide_String) is
   begin
      PolyORB.Any.Set_Any_Value
        (Any_Value, PolyORB.Types.Wide_String (Value));
   end Set_Any_Value;

   -------------------------------
   --  Set_Any_Aggregate_Value  --
   -------------------------------

   procedure Set_Any_Aggregate_Value
     (Any_Value : in out CORBA.Any)
     renames PolyORB.Any.Set_Any_Aggregate_Value;

   ---------------------------
   --  Get_Aggregate_Count  --
   ---------------------------

   function Get_Aggregate_Count (Value : Any) return Unsigned_Long is
   begin
      return Unsigned_Long (PolyORB.Any.Get_Aggregate_Count (Value));
   end Get_Aggregate_Count;

   -----------------------------
   --  Add_Aggregate_Element  --
   -----------------------------

   procedure Add_Aggregate_Element
     (Value   : in out Any;
      Element : in     Any)
     renames PolyORB.Any.Add_Aggregate_Element;

   -----------------------------
   --  Get_Aggregate_Element  --
   -----------------------------

   function Get_Aggregate_Element
     (Value : Any;
      Tc : CORBA.TypeCode.Object;
      Index : CORBA.Unsigned_Long)
     return Any is
   begin
      return PolyORB.Any.Get_Aggregate_Element
        (Value, Tc, PolyORB.Types.Unsigned_Long (Index));
   end Get_Aggregate_Element;

   -------------------------------
   --  Get_Empty_Any_Aggregate  --
   -------------------------------

   function Get_Empty_Any_Aggregate
     (Tc : CORBA.TypeCode.Object)
     return Any
     renames PolyORB.Any.Get_Empty_Any_Aggregate;

   -----------
   -- Image --
   -----------

   function Image (NV : NamedValue) return Standard.String is
   begin
      return Image (To_PolyORB_NV (NV));
   end Image;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent (RI1, RI2 : RepositoryId)
     return Boolean is
   begin
      return Is_Equivalent
        (To_Standard_String (RI1),
         To_Standard_String (RI2));
   end Is_Equivalent;

   function Is_Equivalent (RI1, RI2 : Standard.String)
     return Boolean is
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
      pragma Assert (Is_Error (Error));

      declare
         Member : constant CORBA.System_Exception_Members
           := CORBA.System_Exception_Members (Error.Member.all);
      begin

         Free (Error.Member);

         --  One to one mapping of PolyORB Error_Id to CORBA System exceptions.

         case Error.Kind is
            when Unknown_E =>
               Raise_Unknown (Member);

            when Bad_Param_E =>
               Raise_Bad_Param (Member);

            when No_Memory_E =>
               Raise_No_Memory (Member);

            when Imp_Limit_E =>
               Raise_Imp_Limit (Member);

            when Comm_Failure_E =>
               Raise_Comm_Failure (Member);

            when Inv_Objref_E =>
               Raise_Inv_Objref (Member);

            when No_Permission_E =>
               Raise_No_Permission (Member);

            when Internal_E =>
               Raise_Internal (Member);

            when Marshal_E =>
               Raise_Marshal (Member);

            when Initialization_Failure_E =>
               Raise_Internal (Member);

            when No_Implement_E =>
            Raise_No_Implement (Member);

            when Bad_TypeCode_E =>
               Raise_Bad_TypeCode (Member);

            when Bad_Operation_E =>
               Raise_Bad_Operation (Member);

            when No_Resources_E =>
               Raise_No_Resources (Member);

            when No_Response_E =>
            Raise_No_Response (Member);

            when Persist_Store_E =>
               Raise_Persist_Store (Member);

            when Bad_Inv_Order_E =>
               Raise_Bad_Inv_Order (Member);

            when Transient_E =>
               Raise_Transient (Member);

            when Free_Mem_E =>
               Raise_Free_Mem (Member);

            when Inv_Ident_E =>
               Raise_Inv_Ident (Member);

            when Inv_Flag_E =>
               Raise_Inv_Flag (Member);

            when Intf_Repos_E =>
               Raise_Intf_Repos (Member);

            when Bad_Context_E =>
               Raise_Bad_Context (Member);

            when Obj_Adapter_E =>
               Raise_Obj_Adapter (Member);

            when Data_Conversion_E =>
               Raise_Data_Conversion (Member);

            when Object_Not_Exist_E =>
               Raise_Object_Not_Exist (Member);

            when Transaction_Required_E =>
               Raise_Transaction_Required (Member);

            when Transaction_Rolledback_E =>
               Raise_Transaction_Rolledback (Member);

            when Invalid_Transaction_E =>
               Raise_Invalid_Transaction (Member);

            when Inv_Policy_E =>
               Raise_Inv_Policy (Member);

            when Codeset_Incompatible_E =>
               Raise_Codeset_Incompatible (Member);

            when Rebind_E =>
               Raise_Rebind (Member);

            when Timeout_E =>
               Raise_Timeout (Member);

            when Transaction_Unavailable_E =>
               Raise_Transaction_Unavailable (Member);

            when Transaction_Mode_E =>
               Raise_Transaction_Mode (Member);

            when Bad_Qos_E =>
               Raise_Bad_Qos (Member);

            when others =>
               raise Program_Error;

         end case;
      end;
   end Raise_From_Error;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.CORBA_P.Exceptions.CORBA_Raise_From_Error
        := Raise_From_Error'Access;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"corba",
       Conflicts => Empty,
       Depends => Empty,
       Provides => Empty,
       Init => Initialize'Access));

end CORBA;
