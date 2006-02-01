------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    D Y N A M I C A N Y . D Y N A N Y                     --
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

with PolyORB.Exceptions;

with DynamicAny.DynAny.Impl;

package body DynamicAny.DynAny is

   ------------
   -- Assign --
   ------------

   procedure Assign
     (Self    : Local_Ref;
      Dyn_Any : Local_Ref'Class)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Assign (Impl.Object_Ptr (Entity_Of (Self)), Dyn_Any);
   end Assign;

   ---------------------
   -- Component_Count --
   ---------------------

   function Component_Count (Self : Local_Ref) return CORBA.Unsigned_Long is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Component_Count (Impl.Object_Ptr (Entity_Of (Self)));
   end Component_Count;

   ----------
   -- Copy --
   ----------

   function Copy (Self : Local_Ref) return Local_Ref'Class
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Copy (Impl.Object_Ptr (Entity_Of (Self)));
   end Copy;

   -----------------------
   -- Current_Component --
   -----------------------

   function Current_Component (Self : Local_Ref) return Local_Ref'Class is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Current_Component (Impl.Object_Ptr (Entity_Of (Self)));
   end Current_Component;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : Local_Ref) is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Destroy (Impl.Object_Ptr (Entity_Of (Self)));
   end Destroy;

   -----------
   -- Equal --
   -----------

   function Equal
     (Self    : Local_Ref;
      Dyn_Any : Local_Ref'Class)
      return CORBA.Boolean
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Equal (Impl.Object_Ptr (Entity_Of (Self)), Dyn_Any);
   end Equal;

   --------------
   -- From_Any --
   --------------

   procedure From_Any
     (Self  : Local_Ref;
      Value : CORBA.Any)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.From_Any (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end From_Any;

   ------------------
   -- Get_Abstract --
   ------------------

   function Get_Abstract (Self : Local_Ref) return CORBA.AbstractBase.Ref is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Abstract (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Abstract;

   -------------
   -- Get_Any --
   -------------

   function Get_Any (Self : Local_Ref) return CORBA.Any is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Any (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Any;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean (Self : Local_Ref) return CORBA.Boolean is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Boolean (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Boolean;

   ---------------------
   -- Get_Boolean_Seq --
   ---------------------

   function Get_Boolean_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.BooleanSeq
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Boolean_Seq (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Boolean_Seq;

   --------------
   -- Get_Char --
   --------------

   function Get_Char (Self : Local_Ref) return CORBA.Char is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Char (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Char;

   ------------------
   -- Get_Char_Seq --
   ------------------

   function Get_Char_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.CharSeq
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Char_Seq (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Char_Seq;

   ----------------
   -- Get_Double --
   ----------------

   function Get_Double (Self : Local_Ref) return CORBA.Double is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Double (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Double;

   --------------------
   -- Get_Double_Seq --
   --------------------

   function Get_Double_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.DoubleSeq
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Double_Seq (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Double_Seq;

   -----------------
   -- Get_Dyn_Any --
   -----------------

   function Get_Dyn_Any (Self : Local_Ref) return Local_Ref'Class is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Dyn_Any (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Dyn_Any;

   ---------------
   -- Get_Float --
   ---------------

   function Get_Float (Self : Local_Ref) return CORBA.Float is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Float (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Float;

   -------------------
   -- Get_Float_Seq --
   -------------------

   function Get_Float_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.FloatSeq
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Float_Seq (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Float_Seq;

   --------------
   -- Get_Long --
   --------------

   function Get_Long (Self : Local_Ref) return CORBA.Long is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Long (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Long;

   ------------------
   -- Get_Long_Seq --
   ------------------

   function Get_Long_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.LongSeq
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Long_Seq (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Long_Seq;

   --------------------
   -- Get_LongDouble --
   --------------------

   function Get_LongDouble (Self : Local_Ref) return CORBA.Long_Double is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_LongDouble (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_LongDouble;

   ------------------------
   -- Get_LongDouble_Seq --
   ------------------------

   function Get_LongDouble_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.LongDoubleSeq
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_LongDouble_Seq (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_LongDouble_Seq;

   ------------------
   -- Get_LongLong --
   ------------------

   function Get_LongLong (Self : Local_Ref) return CORBA.Long_Long is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_LongLong (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_LongLong;

   ----------------------
   -- Get_LongLong_Seq --
   ----------------------

   function Get_LongLong_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.LongLongSeq
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_LongLong_Seq (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_LongLong_Seq;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   :    out InvalidValue_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   :    out TypeMismatch_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   ---------------
   -- Get_Octet --
   ---------------

   function Get_Octet (Self : Local_Ref) return CORBA.Octet is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Octet (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Octet;

   -------------------
   -- Get_Octet_Seq --
   -------------------

   function Get_Octet_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.OctetSeq
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Octet_Seq (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Octet_Seq;

   -------------------
   -- Get_Reference --
   -------------------

   function Get_Reference (Self : Local_Ref) return CORBA.Object.Ref is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Reference (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Reference;

   ---------------
   -- Get_Short --
   ---------------

   function Get_Short (Self : Local_Ref) return CORBA.Short is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Short (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Short;

   -------------------
   -- Get_Short_Seq --
   -------------------

   function Get_Short_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.ShortSeq
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Short_Seq (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Short_Seq;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Self : Local_Ref) return CORBA.String is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_String (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_String;

   ------------------
   -- Get_TypeCode --
   ------------------

   function Get_TypeCode (Self : Local_Ref) return CORBA.TypeCode.Object is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_TypeCode (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_TypeCode;

   ---------------
   -- Get_ULong --
   ---------------

   function Get_ULong (Self : Local_Ref) return CORBA.Unsigned_Long is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_ULong (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_ULong;

   -------------------
   -- Get_ULong_Seq --
   -------------------

   function Get_ULong_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.ULongSeq
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_ULong_Seq (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_ULong_Seq;

   -------------------
   -- Get_ULongLong --
   -------------------

   function Get_ULongLong
     (Self : Local_Ref)
      return CORBA.Unsigned_Long_Long
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_ULongLong (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_ULongLong;

   -----------------------
   -- Get_ULongLong_Seq --
   -----------------------

   function Get_ULongLong_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.ULongLongSeq
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_ULongLong_Seq (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_ULongLong_Seq;

   ----------------
   -- Get_UShort --
   ----------------

   function Get_UShort (Self : Local_Ref) return CORBA.Unsigned_Short is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_UShort (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_UShort;

   --------------------
   -- Get_UShort_Seq --
   --------------------

   function Get_UShort_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.UShortSeq
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_UShort_Seq (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_UShort_Seq;

   ---------------
   -- Get_WChar --
   ---------------

   function Get_WChar (Self : Local_Ref) return CORBA.Wchar is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_WChar (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_WChar;

   -------------------
   -- Get_WChar_Seq --
   -------------------

   function Get_WChar_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.WCharSeq
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_WChar_Seq (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_WChar_Seq;

   -----------------
   -- Get_WString --
   -----------------

   function Get_WString (Self : Local_Ref) return CORBA.Wide_String is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_WString (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_WString;

   --------------
   -- IDL_Type --
   --------------

   function IDL_Type (Self : Local_Ref) return CORBA.TypeCode.Object is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.IDL_Type (Impl.Object_Ptr (Entity_Of (Self)));
   end IDL_Type;

   ---------------------
   -- Insert_Abstract --
   ---------------------

   procedure Insert_Abstract
     (Self  : Local_Ref;
      Value : CORBA.AbstractBase.Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Abstract (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Abstract;

   ----------------
   -- Insert_Any --
   ----------------

   procedure Insert_Any
     (Self  : Local_Ref;
      Value : CORBA.Any)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Any (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Any;

   --------------------
   -- Insert_Boolean --
   --------------------

   procedure Insert_Boolean
     (Self  : Local_Ref;
      Value : CORBA.Boolean)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Boolean (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Boolean;

   ------------------------
   -- Insert_Boolean_Seq --
   ------------------------

   procedure Insert_Boolean_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.BooleanSeq)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Boolean_Seq (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Boolean_Seq;

   -----------------
   -- Insert_Char --
   -----------------

   procedure Insert_Char
     (Self  : Local_Ref;
      Value : CORBA.Char)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Char (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Char;

   ---------------------
   -- Insert_Char_Seq --
   ---------------------

   procedure Insert_Char_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.CharSeq)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Char_Seq (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Char_Seq;

   -------------------
   -- Insert_Double --
   -------------------

   procedure Insert_Double
     (Self  : Local_Ref;
      Value : CORBA.Double)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Double (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Double;

   -----------------------
   -- Insert_Double_Seq --
   -----------------------

   procedure Insert_Double_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.DoubleSeq)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Double_Seq (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Double_Seq;

   --------------------
   -- Insert_Dyn_Any --
   --------------------

   procedure Insert_Dyn_Any
     (Self  : Local_Ref;
      Value : Local_Ref'Class)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Dyn_Any (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Dyn_Any;

   ------------------
   -- Insert_Float --
   ------------------

   procedure Insert_Float
     (Self  : Local_Ref;
      Value : CORBA.Float)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Float (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Float;

   ----------------------
   -- Insert_Float_Seq --
   ----------------------

   procedure Insert_Float_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.FloatSeq)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Float_Seq (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Float_Seq;

   -----------------
   -- Insert_Long --
   -----------------

   procedure Insert_Long
     (Self  : Local_Ref;
      Value : CORBA.Long)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Long (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Long;

   ---------------------
   -- Insert_Long_Seq --
   ---------------------

   procedure Insert_Long_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.LongSeq)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Long_Seq (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Long_Seq;

   -----------------------
   -- Insert_LongDouble --
   -----------------------

   procedure Insert_LongDouble
     (Self  : Local_Ref;
      Value : CORBA.Long_Double)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_LongDouble (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_LongDouble;

   ---------------------------
   -- Insert_LongDouble_Seq --
   ---------------------------

   procedure Insert_LongDouble_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.LongDoubleSeq)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_LongDouble_Seq (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_LongDouble_Seq;

   ---------------------
   -- Insert_LongLong --
   ---------------------

   procedure Insert_LongLong
     (Self  : Local_Ref;
      Value : CORBA.Long_Long)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_LongLong (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_LongLong;

   -------------------------
   -- Insert_LongLong_Seq --
   -------------------------

   procedure Insert_LongLong_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.LongLongSeq)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_LongLong_Seq (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_LongLong_Seq;

   ------------------
   -- Insert_Octet --
   ------------------

   procedure Insert_Octet
     (Self  : Local_Ref;
      Value : CORBA.Octet)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Octet (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Octet;

   ----------------------
   -- Insert_Octet_Seq --
   ----------------------

   procedure Insert_Octet_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.OctetSeq)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Octet_Seq (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Octet_Seq;

   ----------------------
   -- Insert_Reference --
   ----------------------

   procedure Insert_Reference
     (Self  : Local_Ref;
      Value : CORBA.Object.Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Reference (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Reference;

   ------------------
   -- Insert_Short --
   ------------------

   procedure Insert_Short
     (Self  : Local_Ref;
      Value : CORBA.Short)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Short (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Short;

   ----------------------
   -- Insert_Short_Seq --
   ----------------------

   procedure Insert_Short_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.ShortSeq)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_Short_Seq (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_Short_Seq;

   -------------------
   -- Insert_String --
   -------------------

   procedure Insert_String
     (Self  : Local_Ref;
      Value : CORBA.String)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_String (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_String;

   ---------------------
   -- Insert_TypeCode --
   ---------------------

   procedure Insert_TypeCode
     (Self  : Local_Ref;
      Value : CORBA.TypeCode.Object)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_TypeCode (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_TypeCode;

   ------------------
   -- Insert_ULong --
   ------------------

   procedure Insert_ULong
     (Self  : Local_Ref;
      Value : CORBA.Unsigned_Long)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_ULong (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_ULong;

   ----------------------
   -- Insert_ULong_Seq --
   ----------------------

   procedure Insert_ULong_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.ULongSeq)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_ULong_Seq (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_ULong_Seq;

   ----------------------
   -- Insert_ULongLong --
   ----------------------

   procedure Insert_ULongLong
     (Self  : Local_Ref;
      Value : CORBA.Unsigned_Long_Long)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_ULongLong (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_ULongLong;

   --------------------------
   -- Insert_ULongLong_Seq --
   --------------------------

   procedure Insert_ULongLong_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.ULongLongSeq)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_ULongLong_Seq (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_ULongLong_Seq;

   -------------------
   -- Insert_UShort --
   -------------------

   procedure Insert_UShort
     (Self  : Local_Ref;
      Value : CORBA.Unsigned_Short)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_UShort (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_UShort;

   -----------------------
   -- Insert_UShort_Seq --
   -----------------------

   procedure Insert_UShort_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.UShortSeq)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_UShort_Seq (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_UShort_Seq;

   ------------------
   -- Insert_WChar --
   ------------------

   procedure Insert_WChar
     (Self  : Local_Ref;
      Value : CORBA.Wchar)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_WChar (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_WChar;

   ----------------------
   -- Insert_WChar_Seq --
   ----------------------

   procedure Insert_WChar_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.WCharSeq)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_WChar_Seq (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_WChar_Seq;

   --------------------
   -- Insert_WString --
   --------------------

   procedure Insert_WString
     (Self  : Local_Ref;
      Value : CORBA.Wide_String)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Insert_WString (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Insert_WString;

   ----------
   -- Next --
   ----------

   function Next (Self : Local_Ref) return CORBA.Boolean is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Next (Impl.Object_Ptr (Entity_Of (Self)));
   end Next;

   ------------
   -- Rewind --
   ------------

   procedure Rewind (Self : Local_Ref) is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Rewind (Impl.Object_Ptr (Entity_Of (Self)));
   end Rewind;

   ----------
   -- Seek --
   ----------

   function Seek
     (Self  : Local_Ref;
      Index : CORBA.Long)
      return CORBA.Boolean
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Seek (Impl.Object_Ptr (Entity_Of (Self)), Index);
   end Seek;

   ------------
   -- To_Any --
   ------------

   function To_Any (Self : Local_Ref) return CORBA.Any is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.To_Any (Impl.Object_Ptr (Entity_Of (Self)));
   end To_Any;

end DynamicAny.DynAny;
