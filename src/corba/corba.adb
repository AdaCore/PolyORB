------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                C O R B A                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2007, Free Software Foundation, Inc.          --
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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;

with PolyORB.CORBA_P.Exceptions;
with PolyORB.Errors.Helper;
with PolyORB.Exceptions;
with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body CORBA is

   function To_PolyORB_NV (NV : NamedValue) return PolyORB.Any.NamedValue;

   --  The standard PolyORB's exception annotation mechanism is not
   --  used for CORBA system exceptions: all CORBA system exceptions
   --  have the same exception members structure. Exception members
   --  for such exceptions are represented as first 9 characters of
   --  exception occurrence message. If an optional message associated
   --  with exception occurrence then it added after exception members
   --  representation and separated by the LF character.

   To_Hex : constant array (Natural range 0 .. 15) of Standard.Character
     := "0123456789ABCDEF";

   -------------------
   -- To_PolyORB_NV --
   -------------------

   function To_PolyORB_NV (NV : NamedValue) return PolyORB.Any.NamedValue is
   begin
      return PolyORB.Any.NamedValue'
        (Name      => PolyORB.Types.Identifier (NV.Name),
         Argument  => PolyORB.Any.Any (NV.Argument),
         Arg_Modes => PolyORB.Any.Flags (NV.Arg_Modes));
   end To_PolyORB_NV;

   function To_CORBA_NV (NV : PolyORB.Any.NamedValue) return NamedValue is
   begin
      return CORBA.NamedValue'
        (Name      => CORBA.Identifier (NV.Name),
         Argument  => CORBA.Any (NV.Argument),
         Arg_Modes => Flags (NV.Arg_Modes));
   end To_CORBA_NV;

   --------------------------
   -- TC_Completion_Status --
   --------------------------

   function TC_Completion_Status return CORBA.TypeCode.Object;
   --  The typecode for standard enumeration type completion_status.

   TC_Completion_Status_Cache : CORBA.TypeCode.Object;

   function TC_Completion_Status return CORBA.TypeCode.Object is
      use type PolyORB.Types.Unsigned_Long;

      TC : CORBA.TypeCode.Object renames TC_Completion_Status_Cache;

   begin
      if not TypeCode.Internals.Is_Nil (TC_Completion_Status_Cache) then
         return TC_Completion_Status_Cache;
      end if;

      TC := TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TC_Enum);
      Internals.Add_Parameter
        (TC, To_Any (To_PolyORB_String ("completion_status")));
      Internals.Add_Parameter
        (TC, To_Any (To_PolyORB_String
                     ("IDL:omg.org/CORBA/completion_status:1.0")));

      for C in Completion_Status'Range loop
         Internals.Add_Parameter
           (TC, To_Any (To_PolyORB_String (Completion_Status'Image (C))));
      end loop;

      return TC;
   end TC_Completion_Status;

   procedure Raise_From_Error
     (Error   : in out PolyORB.Errors.Error_Container;
      Message : Standard.String);
   --  Raise the exception associated with the current state of Error.
   --  If Error is an empty Error Container, no exception is raised.

   procedure Raise_System_Exception
     (Excp       : Ada.Exceptions.Exception_Id;
      Excp_Memb  : System_Exception_Members;
      Or_OMGVMCD : Boolean := False;
      Message    : Standard.String := "");
   pragma No_Return (Raise_System_Exception);
   --  Raise any system exception

   ---------------------------------
   -- String conversion functions --
   ---------------------------------

   ---------------------
   -- To_CORBA_String --
   ---------------------

   function To_CORBA_String (Source : Standard.String) return CORBA.String
   is
   begin
      return To_PolyORB_String (Source);
   end To_CORBA_String;

   function To_CORBA_String (Source : Standard.String) return CORBA.Identifier
   is
   begin
      return To_PolyORB_String (Source);
   end To_CORBA_String;

   --------------------------
   -- To_CORBA_Wide_String --
   --------------------------

   function To_CORBA_Wide_String
     (Source : Standard.Wide_String) return CORBA.Wide_String
   is
   begin
      return CORBA.Wide_String
        (Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String
         (Source));
   end To_CORBA_Wide_String;

   -----------------------------
   -- To_Standard_Wide_String --
   -----------------------------

   function To_Standard_Wide_String
     (Source : CORBA.Wide_String) return Standard.Wide_String
   is
   begin
      return Ada.Strings.Wide_Unbounded.To_Wide_String
        (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Source));
   end To_Standard_Wide_String;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out System_Exception_Members)
   is
      Str : constant Standard.String :=
        Ada.Exceptions.Exception_Message (From);
      Val : Unsigned_Long;
   begin
      --  Check length

      if Str'Length < 9
        or else (Str'Length > 9
                   and then Str (Str'First + 9) /= Ada.Characters.Latin_1.LF)
      then
         Raise_Bad_Param (Default_Sys_Member);
      end if;

      --  Unmarshall completion status

      case Str (Str'First + 8) is
         when 'N' =>
            To.Completed := Completed_No;

         when 'Y' =>
            To.Completed := Completed_Yes;

         when 'M' =>
            To.Completed := Completed_Maybe;

         when others =>
            raise Constraint_Error;
      end case;

      --  Unmarshall minor

      Val := 0;

      for J in Str'First .. Str'First + 7 loop
         case Str (J) is
            when '0' .. '9' =>
               Val := Val * 16 + Character'Pos (Str (J)) - Character'Pos ('0');

            when 'A' .. 'F' =>
               Val :=
                 Val * 16 + Character'Pos (Str (J)) - Character'Pos ('A') + 10;

            when others =>
               raise Constraint_Error;
         end case;
      end loop;

      To.Minor := Val;
   end Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidName_Members)
   is
      use Ada.Exceptions;
   begin
      if Exception_Identity (From) /= InvalidName'Identity then
         Raise_Bad_Param (Default_Sys_Member);
      end if;

      To := InvalidName_Members'(IDL_Exception_Members with null record);
   end Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
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
     (From : Ada.Exceptions.Exception_Occurrence;
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
     (From : Ada.Exceptions.Exception_Occurrence;
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
     (Excp       : Ada.Exceptions.Exception_Id;
      Excp_Memb  : System_Exception_Members;
      Or_OMGVMCD : Boolean := False;
      Message    : Standard.String := "")
   is
      Str : Standard.String (1 .. 9);
      Val : CORBA.Unsigned_Long;

   begin
      --  Marshall Minor and Completed fields of EXCP_MEMB into a string.

      case Excp_Memb.Completed is
         when Completed_Yes =>
            Str (9) := 'Y';

         when Completed_No =>
            Str (9) := 'N';

         when Completed_Maybe =>
            Str (9) := 'M';
      end case;

      Val := Excp_Memb.Minor;

      if Or_OMGVMCD then
         Val := Val or OMGVMCID;
      end if;

      for J in 1 .. 8 loop
         Str (J) := To_Hex (Integer (Val / 2 ** 28));
         Val := (Val mod 2 ** 28) * 16;
      end loop;

      --  Raise the exception

      if Message = "" then
         Ada.Exceptions.Raise_Exception (Excp, Str);

      else
         Ada.Exceptions.Raise_Exception
           (Excp, Str & Ada.Characters.Latin_1.LF & Message);
      end if;

      --  This point is never reached (Excp cannot be null)

      raise Program_Error;
   end Raise_System_Exception;

   -------------------
   -- Raise_Unknown --
   -------------------

   procedure Raise_Unknown
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 3;

   begin
      Raise_System_Exception
        (Unknown'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Unknown;

   ---------------------
   -- Raise_Bad_Param --
   ---------------------

   procedure Raise_Bad_Param
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 41;

   begin
      Raise_System_Exception
        (Bad_Param'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Bad_Param;

   ---------------------
   -- Raise_No_Memory --
   ---------------------

   procedure Raise_No_Memory
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception (No_Memory'Identity, Excp_Memb, False, Message);
   end Raise_No_Memory;

   ---------------------
   -- Raise_Imp_Limit --
   ---------------------

   procedure Raise_Imp_Limit
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor = 1;

   begin
      Raise_System_Exception
        (Imp_Limit'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Imp_Limit;

   ------------------------
   -- Raise_Comm_Failure --
   ------------------------

   procedure Raise_Comm_Failure
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception
        (Comm_Failure'Identity, Excp_Memb, False, Message);
   end Raise_Comm_Failure;

   ----------------------
   -- Raise_Inv_Objref --
   ----------------------

   procedure Raise_Inv_Objref
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception (Inv_Objref'Identity, Excp_Memb, False, Message);
   end Raise_Inv_Objref;

   -------------------------
   -- Raise_No_Permission --
   -------------------------

   procedure Raise_No_Permission
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception
        (No_Permission'Identity, Excp_Memb, False, Message);
   end Raise_No_Permission;

   --------------------
   -- Raise_Internal --
   --------------------

   procedure Raise_Internal
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 2;

   begin
      Raise_System_Exception
        (Internal'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Internal;

   -------------------
   -- Raise_Marshal --
   -------------------

   procedure Raise_Marshal
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 7;

   begin
      Raise_System_Exception
        (Marshal'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Marshal;

   ----------------------
   -- Raise_Initialize --
   ----------------------

   procedure Raise_Initialize
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor = 1;

   begin
      Raise_System_Exception
        (Initialize'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Initialize;

   ------------------------
   -- Raise_No_Implement --
   ------------------------

   procedure Raise_No_Implement
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 7;

   begin
      Raise_System_Exception
        (No_Implement'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_No_Implement;

   ------------------------
   -- Raise_Bad_TypeCode --
   ------------------------

   procedure Raise_Bad_TypeCode
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 3;

   begin
      Raise_System_Exception
        (Bad_TypeCode'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Bad_TypeCode;

   -------------------------
   -- Raise_Bad_Operation --
   -------------------------

   procedure Raise_Bad_Operation
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 2;

   begin
      Raise_System_Exception
        (Bad_Operation'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Bad_Operation;

   ------------------------
   -- Raise_No_Resources --
   ------------------------

   procedure Raise_No_Resources
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 2;

   begin
      Raise_System_Exception
        (No_Resources'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_No_Resources;

   -----------------------
   -- Raise_No_Response --
   -----------------------

   procedure Raise_No_Response
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception (No_Response'Identity, Excp_Memb, False, Message);
   end Raise_No_Response;

   -------------------------
   -- Raise_Persist_Store --
   -------------------------

   procedure Raise_Persist_Store
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception
        (Persist_Store'Identity, Excp_Memb, False, Message);
   end Raise_Persist_Store;

   -------------------------
   -- Raise_Bad_Inv_Order --
   -------------------------

   procedure Raise_Bad_Inv_Order
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 20;

   begin
      Raise_System_Exception
        (Bad_Inv_Order'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Bad_Inv_Order;

   ---------------------
   -- Raise_Transient --
   ---------------------

   procedure Raise_Transient
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 4;

   begin
      Raise_System_Exception
        (Transient'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Transient;

   --------------------
   -- Raise_Free_Mem --
   --------------------

   procedure Raise_Free_Mem
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception (Free_Mem'Identity, Excp_Memb, False, Message);
   end Raise_Free_Mem;

   ---------------------
   -- Raise_Inv_Ident --
   ---------------------

   procedure Raise_Inv_Ident
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception (Inv_Ident'Identity, Excp_Memb, False, Message);
   end Raise_Inv_Ident;

   --------------------
   -- Raise_Inv_Flag --
   --------------------

   procedure Raise_Inv_Flag
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception (Inv_Flag'Identity, Excp_Memb, False, Message);
   end Raise_Inv_Flag;

   ----------------------
   -- Raise_Intf_Repos --
   ----------------------

   procedure Raise_Intf_Repos
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 2;

   begin
      Raise_System_Exception
        (Intf_Repos'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Intf_Repos;

   -----------------------
   -- Raise_Bad_Context --
   -----------------------

   procedure Raise_Bad_Context
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 2;

   begin
      Raise_System_Exception
        (Bad_Context'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Bad_Context;

   -----------------------
   -- Raise_Obj_Adapter --
   -----------------------

   procedure Raise_Obj_Adapter
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 7;

   begin
      Raise_System_Exception
        (Obj_Adapter'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Obj_Adapter;

   ---------------------------
   -- Raise_Data_Conversion --
   ---------------------------

   procedure Raise_Data_Conversion
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 2;

   begin
      Raise_System_Exception
        (Data_Conversion'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Data_Conversion;

   ----------------------------
   -- Raise_Object_Not_Exist --
   ----------------------------

   procedure Raise_Object_Not_Exist
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 4;

   begin
      Raise_System_Exception
        (Object_Not_Exist'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Object_Not_Exist;

   --------------------------------
   -- Raise_Transaction_Required --
   --------------------------------

   procedure Raise_Transaction_Required
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception
        (Transaction_Required'Identity, Excp_Memb, False, Message);
   end Raise_Transaction_Required;

   ----------------------------------
   -- Raise_Transaction_Rolledback --
   ----------------------------------

   procedure Raise_Transaction_Rolledback
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 3;

   begin
      Raise_System_Exception
        (Transaction_Rolledback'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Transaction_Rolledback;

   -------------------------------
   -- Raise_Invalid_Transaction --
   -------------------------------

   procedure Raise_Invalid_Transaction
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception
        (Invalid_Transaction'Identity, Excp_Memb, False, Message);
   end Raise_Invalid_Transaction;

   ----------------------
   -- Raise_Inv_Policy --
   ----------------------

   procedure Raise_Inv_Policy
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
      Or_OMGVMCD : constant Boolean := Excp_Memb.Minor in 1 .. 3;

   begin
      Raise_System_Exception
        (Inv_Policy'Identity, Excp_Memb, Or_OMGVMCD, Message);
   end Raise_Inv_Policy;

   --------------------------------
   -- Raise_Codeset_Incompatible --
   --------------------------------

   procedure Raise_Codeset_Incompatible
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception
        (Codeset_Incompatible'Identity, Excp_Memb, False, Message);
   end Raise_Codeset_Incompatible;

   -------------------
   -- Raise_Rebind --
   -------------------

   procedure Raise_Rebind
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception (Rebind'Identity, Excp_Memb, False, Message);
   end Raise_Rebind;

   -------------------
   -- Raise_Timeout --
   -------------------

   procedure Raise_Timeout
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception (Timeout'Identity, Excp_Memb, False, Message);
   end Raise_Timeout;

   -----------------------------------
   -- Raise_Transaction_Unavailable --
   -----------------------------------

   procedure Raise_Transaction_Unavailable
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception
        (Transaction_Unavailable'Identity, Excp_Memb, False, Message);
   end Raise_Transaction_Unavailable;

   ----------------------------
   -- Raise_Transaction_Mode --
   ----------------------------

   procedure Raise_Transaction_Mode
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception
        (Transaction_Mode'Identity, Excp_Memb, False, Message);
   end Raise_Transaction_Mode;

   -------------------
   -- Raise_Bad_Qos --
   -------------------

   procedure Raise_Bad_Qos
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
   is
   begin
      Raise_System_Exception (Bad_Qos'Identity, Excp_Memb, False, Message);
   end Raise_Bad_Qos;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : Short) return CORBA.Any is
   begin
      return CORBA.Any (PolyORB.Any.To_Any (PolyORB.Types.Short (Item)));
   end To_Any;

   function To_Any (Item : Long) return Any is
   begin
      return CORBA.Any (PolyORB.Any.To_Any (PolyORB.Types.Long (Item)));
   end To_Any;

   function To_Any (Item : Long_Long) return Any is
   begin
      return CORBA.Any (PolyORB.Any.To_Any (PolyORB.Types.Long_Long (Item)));
   end To_Any;

   function To_Any (Item : Unsigned_Short) return Any is
   begin
      return CORBA.Any
        (PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Short (Item)));
   end To_Any;

   function To_Any (Item : Unsigned_Long) return Any is
   begin
      return CORBA.Any
        (PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Long (Item)));
   end To_Any;

   function To_Any (Item : Unsigned_Long_Long) return Any is
   begin
      return CORBA.Any
        (PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Long_Long (Item)));
   end To_Any;

   function To_Any (Item : CORBA.Float) return Any is
   begin
      return CORBA.Any (PolyORB.Any.To_Any (PolyORB.Types.Float (Item)));
   end To_Any;

   function To_Any (Item : Double) return Any is
   begin
      return CORBA.Any (PolyORB.Any.To_Any (PolyORB.Types.Double (Item)));
   end To_Any;

   function To_Any (Item : Long_Double) return Any is
   begin
      return CORBA.Any (PolyORB.Any.To_Any (PolyORB.Types.Long_Double (Item)));
   end To_Any;

   function To_Any (Item : Octet) return Any is
   begin
      return CORBA.Any (PolyORB.Any.To_Any (PolyORB.Types.Octet (Item)));
   end To_Any;

   function To_Any (Item : CORBA.String) return Any is
   begin
      return CORBA.Any (PolyORB.Any.To_Any (PolyORB.Types.String (Item)));
   end To_Any;

   function To_Any (Item : CORBA.Wide_String) return Any is
   begin
      return CORBA.Any (PolyORB.Any.To_Any (PolyORB.Types.Wide_String (Item)));
   end To_Any;

   function To_Any (Item : TypeCode.Object) return CORBA.Any is
   begin
      return CORBA.Any
        (PolyORB.Any.To_Any (TypeCode.Internals.To_PolyORB_Object (Item)));
   end To_Any;

   function To_Any (Item : Completion_Status) return CORBA.Any
   is
      Result : CORBA.Any :=
                 Internals.Get_Empty_Any_Aggregate (TC_Completion_Status);
   begin
      CORBA.Internals.Add_Aggregate_Element
        (Result, To_Any (Unsigned_Long (Completion_Status'Pos (Item))));
      return Result;
   end To_Any;

   --------------------
   -- From_Any (Any) --
   --------------------

   function From_Any (Item : Any) return Short is
   begin
      return Short (PolyORB.Types.Short'(From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return Long is
   begin
      return Long (PolyORB.Types.Long'(From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return Long_Long is
   begin
      return Long_Long (PolyORB.Types.Long_Long'(From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return Unsigned_Short is
   begin
      return Unsigned_Short
        (PolyORB.Types.Unsigned_Short'(From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return Unsigned_Long is
   begin
      return Unsigned_Long
        (PolyORB.Types.Unsigned_Long'(From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return Unsigned_Long_Long is
   begin
      return Unsigned_Long_Long
        (PolyORB.Types.Unsigned_Long_Long'(From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return CORBA.Float is
   begin
      return CORBA.Float (PolyORB.Types.Float'(From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return Double is
   begin
      return Double (PolyORB.Types.Double'(From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return Long_Double is
   begin
      return Long_Double
        (PolyORB.Types.Long_Double'(From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return Octet is
   begin
      return Octet (PolyORB.Types.Octet'(From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return CORBA.String is
   begin
      return CORBA.String (PolyORB.Types.String'(From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return CORBA.Wide_String is
   begin
      return CORBA.Wide_String (PolyORB.Types.Wide_String'(From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any) return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object (From_Any (Item));
   end From_Any;

   function From_Any (Item : CORBA.Any) return Completion_Status is
      Result : constant PolyORB.Errors.Completion_Status :=
        PolyORB.Errors.Helper.From_Any
                   (PolyORB.Any.Any (Get_Aggregate_Element
                     (Item, PolyORB.Any.TypeCode.TC_Unsigned_Long, 0)));
   begin
      return CORBA.Completion_Status (Result);
   end From_Any;

   ------------------------------
   -- From_Any (Any_Container) --
   ------------------------------

   function From_Any (Item : Any_Container'Class) return Short is
   begin
      return Short
        (PolyORB.Types.Short'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any_Container'Class) return Long is
   begin
      return Long
        (PolyORB.Types.Long'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any_Container'Class) return Long_Long is
   begin
      return Long_Long
        (PolyORB.Types.Long_Long'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any_Container'Class) return Unsigned_Short is
   begin
      return Unsigned_Short
        (PolyORB.Types.Unsigned_Short'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any_Container'Class) return Unsigned_Long is
   begin
      return Unsigned_Long
        (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any_Container'Class) return Unsigned_Long_Long is
   begin
      return Unsigned_Long_Long
        (PolyORB.Types.Unsigned_Long_Long'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any_Container'Class) return CORBA.Float is
   begin
      return CORBA.Float
        (PolyORB.Types.Float'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any_Container'Class) return Double is
   begin
      return Double
        (PolyORB.Types.Double'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any_Container'Class) return Long_Double is
   begin
      return Long_Double
        (PolyORB.Types.Long_Double'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any_Container'Class) return Boolean is
   begin
      return Boolean
        (PolyORB.Types.Boolean'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any_Container'Class) return Char is
   begin
      return Char
        (PolyORB.Types.Char'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any_Container'Class) return Wchar is
   begin
      return Wchar
        (PolyORB.Types.Wchar'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any_Container'Class) return Octet is
   begin
      return Octet
        (PolyORB.Types.Octet'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any_Container'Class) return TypeCode.Object is
   begin
      return TypeCode.Internals.To_CORBA_Object (PolyORB.Any.From_Any (Item));
   end From_Any;

   function From_Any (Item : Any_Container'Class) return CORBA.String is
   begin
      return CORBA.String
        (PolyORB.Types.String'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : Any_Container'Class) return CORBA.Wide_String is
   begin
      return CORBA.Wide_String
        (PolyORB.Types.Wide_String'(PolyORB.Any.From_Any (Item)));
   end From_Any;

   ----------
   -- Wrap --
   ----------

   function Wrap (X : access Short)              return Content'Class is
   begin
      return PolyORB.Any.Wrap
        (PolyORB.Types.Short (X.all)'Unrestricted_Access);
   end Wrap;

   function Wrap (X : access Long)               return Content'Class is
   begin
      return PolyORB.Any.Wrap (PolyORB.Types.Long (X.all)'Unrestricted_Access);
   end Wrap;

   function Wrap (X : access Long_Long)          return Content'Class is
   begin
      return PolyORB.Any.Wrap
        (PolyORB.Types.Long_Long (X.all)'Unrestricted_Access);
   end Wrap;

   function Wrap (X : access Unsigned_Short)     return Content'Class is
   begin
      return PolyORB.Any.Wrap
        (PolyORB.Types.Unsigned_Short (X.all)'Unrestricted_Access);
   end Wrap;

   function Wrap (X : access Unsigned_Long)      return Content'Class is
   begin
      return PolyORB.Any.Wrap
        (PolyORB.Types.Unsigned_Long (X.all)'Unrestricted_Access);
   end Wrap;

   function Wrap (X : access Unsigned_Long_Long) return Content'Class is
   begin
      return PolyORB.Any.Wrap
        (PolyORB.Types.Unsigned_Long_Long (X.all)'Unrestricted_Access);
   end Wrap;

   function Wrap (X : access CORBA.Float)        return Content'Class is
   begin
      return PolyORB.Any.Wrap
        (PolyORB.Types.Float (X.all)'Unrestricted_Access);
   end Wrap;

   function Wrap (X : access Double)             return Content'Class is
   begin
      return PolyORB.Any.Wrap
        (PolyORB.Types.Double (X.all)'Unrestricted_Access);
   end Wrap;

   function Wrap (X : access Long_Double)        return Content'Class is
   begin
      return PolyORB.Any.Wrap
        (PolyORB.Types.Long_Double (X.all)'Unrestricted_Access);
   end Wrap;

   function Wrap (X : access Boolean)            return Content'Class is
   begin
      return PolyORB.Any.Wrap
        (PolyORB.Types.Boolean (X.all)'Unrestricted_Access);
   end Wrap;

   function Wrap (X : access Char)               return Content'Class is
   begin
      return PolyORB.Any.Wrap (PolyORB.Types.Char (X.all)'Unrestricted_Access);
   end Wrap;

   function Wrap (X : access Wchar)              return Content'Class is
   begin
      return PolyORB.Any.Wrap
        (PolyORB.Types.Wchar (X.all)'Unrestricted_Access);
   end Wrap;

   function Wrap (X : access Octet)              return Content'Class is
   begin
      return PolyORB.Any.Wrap
        (PolyORB.Types.Octet (X.all)'Unrestricted_Access);
   end Wrap;

   function Wrap (X : access TypeCode.Object) return Content'Class is
   begin
      return TypeCode.Internals.Wrap (X);
   end Wrap;

   function Wrap (X : access CORBA.String)       return Content'Class is
   begin
      return PolyORB.Any.Wrap
        (PolyORB.Types.String (X.all)'Unrestricted_Access);
   end Wrap;

   function Wrap (X : access CORBA.Wide_String)  return Content'Class is
   begin
      return PolyORB.Any.Wrap
        (PolyORB.Types.Wide_String (X.all)'Unrestricted_Access);
   end Wrap;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (The_Any : Any) return CORBA.TypeCode.Object is
   begin
      return CORBA.TypeCode.Internals.To_CORBA_Object (Get_Type (The_Any));
   end Get_Type;

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
     (Error   : in out PolyORB.Errors.Error_Container;
      Message : Standard.String)
   is
      use PolyORB.Errors;

   begin
      pragma Assert (Error.Kind in ORB_System_Error);

      declare
         Member : constant PolyORB.Errors.System_Exception_Members
           := PolyORB.Errors.System_Exception_Members (Error.Member.all);

         CORBA_Member : constant CORBA.System_Exception_Members
           := System_Exception_Members'
           (Minor =>  CORBA.Unsigned_Long (Member.Minor),
            Completed => CORBA.Completion_Status (Member.Completed));

      begin

         Free (Error.Member);

         --  One to one mapping of PolyORB Error_Id to CORBA System exceptions

         case Error.Kind is
            when Unknown_E =>
               Raise_Unknown (CORBA_Member, Message);

            when Bad_Param_E =>
               Raise_Bad_Param (CORBA_Member, Message);

            when No_Memory_E =>
               Raise_No_Memory (CORBA_Member, Message);

            when Imp_Limit_E =>
               Raise_Imp_Limit (CORBA_Member, Message);

            when Comm_Failure_E =>
               Raise_Comm_Failure (CORBA_Member, Message);

            when Inv_Objref_E =>
               Raise_Inv_Objref (CORBA_Member, Message);

            when No_Permission_E =>
               Raise_No_Permission (CORBA_Member, Message);

            when Internal_E =>
               Raise_Internal (CORBA_Member, Message);

            when Marshal_E =>
               Raise_Marshal (CORBA_Member, Message);

            when Initialize_E =>
               Raise_Initialize (CORBA_Member, Message);

            when No_Implement_E =>
               Raise_No_Implement (CORBA_Member, Message);

            when Bad_TypeCode_E =>
               Raise_Bad_TypeCode (CORBA_Member, Message);

            when Bad_Operation_E =>
               Raise_Bad_Operation (CORBA_Member, Message);

            when No_Resources_E =>
               Raise_No_Resources (CORBA_Member, Message);

            when No_Response_E =>
               Raise_No_Response (CORBA_Member, Message);

            when Persist_Store_E =>
               Raise_Persist_Store (CORBA_Member, Message);

            when Bad_Inv_Order_E =>
               Raise_Bad_Inv_Order (CORBA_Member, Message);

            when Transient_E =>
               Raise_Transient (CORBA_Member, Message);

            when Free_Mem_E =>
               Raise_Free_Mem (CORBA_Member, Message);

            when Inv_Ident_E =>
               Raise_Inv_Ident (CORBA_Member, Message);

            when Inv_Flag_E =>
               Raise_Inv_Flag (CORBA_Member, Message);

            when Intf_Repos_E =>
               Raise_Intf_Repos (CORBA_Member, Message);

            when Bad_Context_E =>
               Raise_Bad_Context (CORBA_Member, Message);

            when Obj_Adapter_E =>
               Raise_Obj_Adapter (CORBA_Member, Message);

            when Data_Conversion_E =>
               Raise_Data_Conversion (CORBA_Member, Message);

            when Object_Not_Exist_E =>
               Raise_Object_Not_Exist (CORBA_Member, Message);

            when Transaction_Required_E =>
               Raise_Transaction_Required (CORBA_Member, Message);

            when Transaction_Rolledback_E =>
               Raise_Transaction_Rolledback (CORBA_Member, Message);

            when Invalid_Transaction_E =>
               Raise_Invalid_Transaction (CORBA_Member, Message);

            when Inv_Policy_E =>
               Raise_Inv_Policy (CORBA_Member, Message);

            when Codeset_Incompatible_E =>
               Raise_Codeset_Incompatible (CORBA_Member, Message);

            when Rebind_E =>
               Raise_Rebind (CORBA_Member, Message);

            when Timeout_E =>
               Raise_Timeout (CORBA_Member, Message);

            when Transaction_Unavailable_E =>
               Raise_Transaction_Unavailable (CORBA_Member, Message);

            when Transaction_Mode_E =>
               Raise_Transaction_Mode (CORBA_Member, Message);

            when Bad_Qos_E =>
               Raise_Bad_Qos (CORBA_Member, Message);

            when others =>
               raise Program_Error;

         end case;
      end;
   end Raise_From_Error;

   package body Internals is

      ---------------------------
      -- Add_Aggregate_Element --
      ---------------------------

      procedure Add_Aggregate_Element
        (Value   : in out CORBA.Any;
         Element : CORBA.Any) renames CORBA.Add_Aggregate_Element;

      -------------------
      -- Add_Parameter --
      -------------------

      procedure Add_Parameter (TC : TypeCode.Object; Param : Any) is
      begin
         PolyORB.Any.TypeCode.Add_Parameter
           (CORBA.TypeCode.Internals.To_PolyORB_Object (TC),
            PolyORB.Any.Any (Param));
      end Add_Parameter;

      -------------------------
      -- Get_Aggregate_Count --
      -------------------------

      function Get_Aggregate_Count (Value : Any) return CORBA.Unsigned_Long is
      begin
         return CORBA.Unsigned_Long
           (PolyORB.Types.Unsigned_Long'(Get_Aggregate_Count (Value)));
      end Get_Aggregate_Count;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Value : Any;
         TC    : CORBA.TypeCode.Object;
         Index : CORBA.Unsigned_Long) return Any
      is
      begin
         return Get_Aggregate_Element
           (Value,
            TypeCode.Internals.To_PolyORB_Object (TC),
            PolyORB.Types.Unsigned_Long (Index));
      end Get_Aggregate_Element;

      -------------------
      -- Get_Empty_Any --
      -------------------

      function Get_Empty_Any (TC : TypeCode.Object) return PolyORB.Any.Any is
      begin
         return PolyORB.Any.Get_Empty_Any
                  (TypeCode.Internals.To_PolyORB_Object (TC));
      end Get_Empty_Any;

      function Get_Empty_Any (TC : TypeCode.Object) return Any is
      begin
         return Get_Empty_Any (TypeCode.Internals.To_PolyORB_Object (TC));
      end Get_Empty_Any;

      -----------------------------
      -- Get_Empty_Any_Aggregate --
      -----------------------------

      function Get_Empty_Any_Aggregate (TC : TypeCode.Object) return Any is
      begin
         return Get_Empty_Any_Aggregate
           (TypeCode.Internals.To_PolyORB_Object (TC));
      end Get_Empty_Any_Aggregate;

      ----------------------
      -- Get_Unwound_Type --
      ----------------------

      function Get_Unwound_Type (The_Any : Any) return TypeCode.Object is
      begin
         return TypeCode.Internals.To_CORBA_Object
                  (Get_Unwound_Type (The_Any));
      end Get_Unwound_Type;

      ---------------------
      -- Get_Wrapper_Any --
      ---------------------

      function Get_Wrapper_Any
        (TC : TypeCode.Object;
         CC : access PolyORB.Any.Content'Class) return Any
      is
         Result : Any := Get_Empty_Any (TC);
         pragma Suppress (Accessibility_Check);
      begin
         PolyORB.Any.Set_Value (Get_Container (Result).all,
                                PolyORB.Any.Content_Ptr (CC));
         return Result;
      end Get_Wrapper_Any;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty (Any_Value : CORBA.Any) return Boolean
        renames CORBA.Is_Empty;

      --------------------
      -- Move_Any_Value --
      --------------------

      procedure Move_Any_Value (Dest : Any; Src : Any)
        renames CORBA.Move_Any_Value;

      --------------
      -- Set_Type --
      --------------

      procedure Set_Type
        (The_Any  : in out Any;
         The_Type : TypeCode.Object)
      is
      begin
         Set_Type (The_Any, TypeCode.Internals.To_PolyORB_Object (The_Type));
      end Set_Type;

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

   package body TypeCode is

      -----------------
      -- Get_Members --
      -----------------

      procedure Get_Members
        (From : Ada.Exceptions.Exception_Occurrence;
         To   : out Bounds_Members)
      is
         use Ada.Exceptions;
      begin
         if Exception_Identity (From) /= InvalidName'Identity then
            Raise_Bad_Param (Default_Sys_Member);
         end if;

         To := Bounds_Members'(IDL_Exception_Members with null record);
      end Get_Members;

      procedure Get_Members
        (From : Ada.Exceptions.Exception_Occurrence;
         To   : out BadKind_Members)
      is
         use Ada.Exceptions;
      begin
         if Exception_Identity (From) /= InvalidName'Identity then
            Raise_Bad_Param (Default_Sys_Member);
         end if;

         To := BadKind_Members'(IDL_Exception_Members with null record);
      end Get_Members;

      ---------
      -- "=" --
      ---------

      function "=" (Left, Right : Object) return Boolean is
      begin
         return PolyORB.Any.TypeCode."="
           (Internals.To_PolyORB_Object (Left),
            Internals.To_PolyORB_Object (Right));
      end "=";

      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (Left, Right : Object) return Boolean
        renames "=";

      --------------------------
      -- Get_Compact_TypeCode --
      --------------------------

      function Get_Compact_TypeCode (Self : Object) return Object is
      begin
         --  XXX not implemented
         raise Program_Error;
         return Get_Compact_TypeCode (Self);
      end Get_Compact_TypeCode;

      ----------
      -- Kind --
      ----------

      function Kind (Self : Object) return TCKind is
      begin
         return PolyORB.Any.TypeCode.Kind (Internals.To_PolyORB_Object (Self));
      end Kind;

      --------
      -- Id --
      --------

      function Id (Self : Object) return RepositoryId is
      begin
         return CORBA.RepositoryId
           (PolyORB.Any.TypeCode.Id (Internals.To_PolyORB_Object (Self)));
      end Id;

      ----------
      -- Name --
      ----------

      function Name (Self : Object) return Identifier is
      begin
         return CORBA.Identifier
           (PolyORB.Any.TypeCode.Name (Internals.To_PolyORB_Object (Self)));
      end Name;

      ------------------
      -- Member_Count --
      ------------------

      function Member_Count (Self : Object) return Unsigned_Long is
      begin
         return CORBA.Unsigned_Long
           (PolyORB.Any.TypeCode.Member_Count
            (Internals.To_PolyORB_Object (Self)));
      end Member_Count;

      -----------------
      -- Member_Name --
      -----------------

      function Member_Name
        (Self  : Object;
         Index : Unsigned_Long)
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
        (Self  : Object;
         Index : Unsigned_Long)
        return Object
      is
      begin
         return Internals.To_CORBA_Object
           (PolyORB.Any.TypeCode.Member_Type
            (Internals.To_PolyORB_Object (Self),
             PolyORB.Types.Unsigned_Long (Index)));
      end Member_Type;

      ------------------
      -- Member_Label --
      ------------------

      function Member_Label
        (Self  : Object;
         Index : Unsigned_Long)
        return Any
      is
      begin
         return CORBA.Any (PolyORB.Any.Any'(PolyORB.Any.TypeCode.Member_Label
            (Internals.To_PolyORB_Object (Self),
             PolyORB.Types.Unsigned_Long (Index))));
      end Member_Label;

      ------------------------
      -- Discriminator_Type --
      ------------------------

      function Discriminator_Type (Self : Object) return Object is
      begin
         return Internals.To_CORBA_Object
           (PolyORB.Any.TypeCode.Discriminator_Type
            (Internals.To_PolyORB_Object (Self)));
      end Discriminator_Type;

      -------------------
      -- Default_Index --
      -------------------

      function Default_Index (Self : Object) return Long is
      begin
         return CORBA.Long
           (PolyORB.Any.TypeCode.Default_Index
            (Internals.To_PolyORB_Object (Self)));
      end Default_Index;

      ------------
      -- Length --
      ------------

      function Length (Self : Object) return Unsigned_Long is
      begin
         return CORBA.Unsigned_Long
           (PolyORB.Any.TypeCode.Length
            (Internals.To_PolyORB_Object (Self)));
      end Length;

      ------------------
      -- Content_Type --
      ------------------

      function Content_Type (Self : Object) return Object is
      begin
         return Internals.To_CORBA_Object
           (PolyORB.Any.TypeCode.Content_Type
            (Internals.To_PolyORB_Object (Self)));
      end Content_Type;

      ------------------
      -- Fixed_Digits --
      ------------------

      function Fixed_Digits (Self : Object) return Unsigned_Short is
      begin
         return CORBA.Unsigned_Short
           (PolyORB.Any.TypeCode.Fixed_Digits
            (Internals.To_PolyORB_Object (Self)));
      end Fixed_Digits;

      -----------------
      -- Fixed_Scale --
      -----------------

      function Fixed_Scale (Self : Object) return Short is
      begin
         return CORBA.Short
           (PolyORB.Any.TypeCode.Fixed_Scale
            (Internals.To_PolyORB_Object (Self)));
      end Fixed_Scale;

      -----------------------
      -- Member_Visibility --
      -----------------------

      function Member_Visibility
        (Self  : Object;
         Index : Unsigned_Long)
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

      function Type_Modifier (Self : Object) return ValueModifier is
      begin
         return PolyORB.Any.TypeCode.Type_Modifier
           (Internals.To_PolyORB_Object (Self));
      end Type_Modifier;

      ------------------------
      -- Concrete_Base_Type --
      ------------------------

      function Concrete_Base_Type (Self : Object) return Object is
      begin
         return Internals.To_CORBA_Object
           (PolyORB.Any.TypeCode.Concrete_Base_Type
            (Internals.To_PolyORB_Object (Self)));
      end Concrete_Base_Type;

      package body Internals is

         -------------------
         -- Add_Parameter --
         -------------------

         procedure Add_Parameter (Self : in out Object; Param : Any) is
         begin
            PolyORB.Any.TypeCode.Add_Parameter
              (To_PolyORB_Object (Self), PolyORB.Any.Any (Param));
         end Add_Parameter;

         --------------------
         -- Build_Alias_TC --
         --------------------

         function Build_Alias_TC
           (Name, Id : CORBA.String;
            Parent   : Object) return Object
         is
            Res : constant PolyORB.Any.TypeCode.Local_Ref :=
                    PolyORB.Any.TypeCode.TC_Alias;
         begin
            PolyORB.Any.TypeCode.Add_Parameter (Res, PolyORB.Any.Any
              (To_Any (Name)));
            PolyORB.Any.TypeCode.Add_Parameter (Res, PolyORB.Any.Any
              (To_Any (Id)));
            PolyORB.Any.TypeCode.Add_Parameter (Res, PolyORB.Any.Any
              (To_Any (Parent)));
            return To_CORBA_Object (Res);
         end Build_Alias_TC;

         -----------------------
         -- Build_Sequence_TC --
         -----------------------

         function Build_Sequence_TC (Element_TC : Object; Max : Natural)
           return Object is
         begin
            return To_CORBA_Object (
              PolyORB.Any.TypeCode.Build_Sequence_TC (
                To_PolyORB_Object (Element_TC), Max));
         end Build_Sequence_TC;

         ---------------------
         -- Build_String_TC --
         ---------------------

         function Build_String_TC (Max : CORBA.Unsigned_Long) return Object is
         begin
            return To_CORBA_Object
              (PolyORB.Any.TypeCode.Build_String_TC
               (PolyORB.Types.Unsigned_Long (Max)));
         end Build_String_TC;

         ----------------------
         -- Build_Wstring_TC --
         ----------------------

         function Build_Wstring_TC (Max : CORBA.Unsigned_Long) return Object is
         begin
            return To_CORBA_Object
              (PolyORB.Any.TypeCode.Build_Wstring_TC
               (PolyORB.Types.Unsigned_Long (Max)));
         end Build_Wstring_TC;

         --------------------------------
         -- Disable_Reference_Counting --
         --------------------------------

         procedure Disable_Reference_Counting (Self : CORBA.TypeCode.Object) is
         begin
            PolyORB.Any.TypeCode.Disable_Reference_Counting
              (Object_Of (Self).all);
         end Disable_Reference_Counting;

         ------------
         -- Is_Nil --
         ------------

         function Is_Nil (Self : CORBA.TypeCode.Object) return Boolean is
         begin
            return TypeCode.Is_Nil (Self);
         end Is_Nil;

         --------------
         -- Set_Kind --
         --------------

         procedure Set_Kind (Self : out Object; Kind : TCKind) is
            Empty  : PolyORB.Any.TypeCode.Any_Array (1 .. 0);
            P_Self : PolyORB.Any.TypeCode.Local_Ref;
         begin
            P_Self := PolyORB.Any.TypeCode.Build_Complex_TC (Kind, Empty);
            Self := To_CORBA_Object (P_Self);
         end Set_Kind;

         -----------------------
         -- To_PolyORB_Object --
         -----------------------

         function To_PolyORB_Object
           (Self : CORBA.TypeCode.Object) return PolyORB.Any.TypeCode.Local_Ref
         is
         begin
            return PolyORB.Any.TypeCode.Local_Ref (Self);
         end To_PolyORB_Object;

         ---------------------
         -- To_CORBA_Object --
         ---------------------

         function To_CORBA_Object
           (Self : PolyORB.Any.TypeCode.Local_Ref) return CORBA.TypeCode.Object
         is
         begin
            return To_Ref (PolyORB.Any.TypeCode.Object_Of (Self));
         end To_CORBA_Object;

         ----------
         -- Wrap --
         ----------

         function Wrap (X : access Object) return Content'Class is
         begin
            return PolyORB.Any.Wrap
              (PolyORB.Any.TypeCode.Local_Ref (X.all)'Unrestricted_Access);
         end Wrap;

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
       Init      => Initialize_Package'Access,
       Shutdown  => null));
end CORBA;
