------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                                C O R B A                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
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

with Droopi.CORBA_P.Exceptions;

with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

with Droopi.Types;

package body CORBA is

   use Droopi.Log;
   use Droopi.Any;

   package L is new Droopi.Log.Facility_Log ("corba");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   package L2 is new Droopi.Log.Facility_Log ("corba_refcnt");
   procedure O2 (Message : in Standard.String; Level : Log_Level := Debug)
     renames L2.Output;

   ---------------------------------
   -- String conversion functions --
   ---------------------------------

   function To_CORBA_String
     (Source : Standard.String)
     return CORBA.String
     renames Droopi.Types.To_Droopi_String;

   function To_Standard_String
     (Source : CORBA.String)
     return Standard.String
     renames Droopi.Types.To_Standard_String;

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

   -----------------------------------------
   -- Derived string conversion functions --
   -----------------------------------------

   function To_CORBA_String
     (S : Standard.String)
     return CORBA.RepositoryId
     renames Droopi.Types.To_Droopi_String;

   function To_Standard_String
     (S : CORBA.RepositoryId)
     return Standard.String
     renames Droopi.Types.To_Standard_String;

   function To_Standard_String
     (S : ScopedName)
     return Standard.String is
   begin
      return To_Standard_String (CORBA.String (S));
   end To_Standard_String;

   function To_CORBA_String
     (S : Standard.String)
     return ScopedName is
   begin
      return ScopedName (CORBA.String'(To_CORBA_String (S)));
   end To_CORBA_String;

   ----------------------------------------
   --  Get_Members for system exceptions --
   ----------------------------------------
   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out System_Exception_Members)
     renames Droopi.CORBA_P.Exceptions.Get_Members;


   ----------------------
   -- other exceptions --
   ----------------------

   ------------------
   --  Get_Members --
   ------------------
   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out InvalidName_Members) is
   begin
      To := InvalidName_Members'(IDL_Exception_Members with null record);
   end Get_Members;

   ------------------
   --  Get_Members --
   ------------------
   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out InconsistentTypeCode_Members) is
   begin
      To := InconsistentTypeCode_Members'
        (IDL_Exception_Members with null record);
   end Get_Members;

   ------------------
   --  Get_Members --
   ------------------
   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out PolicyError_Members)
   is
   begin
      Droopi.CORBA_P.Exceptions.User_Get_Members (From, To);
   end Get_Members;



   -------------------
   --  Get_Members  --
   -------------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out UnknownUserException_Members) is
   begin
      Droopi.CORBA_P.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   function "=" (Left, Right : in Any) return Boolean
     renames Droopi.Any."=";

   function To_Any (Item : in Short) return Any is
   begin
      return Droopi.Any.To_Any (Droopi.Types.Short (Item));
   end To_Any;

   function To_Any (Item : in Long) return Any is
   begin
      return Droopi.Any.To_Any (Droopi.Types.Long (Item));
   end To_Any;

   function To_Any (Item : in Long_Long) return Any is
   begin
      return Droopi.Any.To_Any (Droopi.Types.Long_Long (Item));
   end To_Any;

   function To_Any (Item : in Unsigned_Short) return Any is
   begin
      return Droopi.Any.To_Any (Droopi.Types.Unsigned_Short (Item));
   end To_Any;

   function To_Any (Item : in Unsigned_Long) return Any is
   begin
      return Droopi.Any.To_Any (Droopi.Types.Unsigned_Long (Item));
   end To_Any;

   function To_Any (Item : in Unsigned_Long_Long) return Any is
   begin
      return Droopi.Any.To_Any (Droopi.Types.Unsigned_Long_Long (Item));
   end To_Any;

   function To_Any (Item : in CORBA.Float) return Any is
   begin
      return Droopi.Any.To_Any (Droopi.Types.Float (Item));
   end To_Any;

   function To_Any (Item : in Double) return Any is
   begin
      return Droopi.Any.To_Any (Droopi.Types.Double (Item));
   end To_Any;

   function To_Any (Item : in Long_Double) return Any is
   begin
      return Droopi.Any.To_Any (Droopi.Types.Long_Double (Item));
   end To_Any;

   function To_Any (Item : in Boolean) return Any is
   begin
      return Droopi.Any.To_Any (Droopi.Types.Boolean (Item));
   end To_Any;

   function To_Any (Item : in Char) return Any is
   begin
      return Droopi.Any.To_Any (Droopi.Types.Char (Item));
   end To_Any;

   function To_Any (Item : in Wchar) return Any is
   begin
      return Droopi.Any.To_Any (Droopi.Types.Wchar (Item));
   end To_Any;

   function To_Any (Item : in Octet) return Any is
   begin
      return Droopi.Any.To_Any (Droopi.Types.Octet (Item));
   end To_Any;

   function To_Any (Item : in Any) return Any
     renames Droopi.Any.To_Any;

   function To_Any (Item : in TypeCode.Object) return Any
     renames Droopi.Any.To_Any;

--    begin
--       return Droopi.Any.To_Any (Item);
--    end To_Any;

   function To_Any (Item : in CORBA.String) return Any is
   begin
      return Droopi.Any.To_Any (Droopi.Types.String (Item));
   end To_Any;

   function To_Any (Item : in CORBA.Wide_String) return Any is
   begin
      return Droopi.Any.To_Any (Droopi.Types.Wide_String (Item));
   end To_Any;


   function From_Any (Item : in Any) return Short is
   begin
      return Short
        (Droopi.Types.Short'(Droopi.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Long is
   begin
      return Long
        (Droopi.Types.Long'(Droopi.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Long_Long is
   begin
      return Long_Long
        (Droopi.Types.Long_Long'(Droopi.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Short is
   begin
      return Unsigned_Short
        (Droopi.Types.Unsigned_Short'(Droopi.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Long is
   begin
      return Unsigned_Long
        (Droopi.Types.Unsigned_Long'(Droopi.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Long_Long is
   begin
      return Unsigned_Long_Long
        (Droopi.Types.Unsigned_Long_Long'(Droopi.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return CORBA.Float is
   begin
      return CORBA.Float
        (Droopi.Types.Float'(Droopi.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Double is
   begin
      return Double
        (Droopi.Types.Double'(Droopi.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Long_Double is
   begin
      return Long_Double
        (Droopi.Types.Long_Double'(Droopi.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Boolean is
   begin
      return Boolean
        (Droopi.Types.Boolean'(Droopi.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Char is
   begin
      return Char
        (Droopi.Types.Char'(Droopi.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Wchar is
   begin
      return Wchar
        (Droopi.Types.Wchar'(Droopi.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Octet is
   begin
      return Octet
        (Droopi.Types.Octet'(Droopi.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return Any
     renames Droopi.Any.From_Any;

   function From_Any (Item : in Any) return TypeCode.Object
     renames Droopi.Any.From_Any;

   function From_Any (Item : in Any) return CORBA.String is
   begin
      return CORBA.String
        (Droopi.Types.String'(Droopi.Any.From_Any (Item)));
   end From_Any;

   function From_Any (Item : in Any) return CORBA.Wide_String is
   begin
      return CORBA.Wide_String
        (Droopi.Types.Wide_String'(Droopi.Any.From_Any (Item)));
   end From_Any;

   ----------------
   --  Get_Type  --
   ----------------
   function Get_Type (The_Any : in Any) return  TypeCode.Object
     renames Droopi.Any.Get_Type;
--    is
--    begin
--       pragma Debug (O ("Get_Type : enter & end"));
--       return The_Any.The_Type;
--    end Get_Type;

   ------------------------
   --  Get_Precise_Type  --
   ------------------------
   function Get_Precise_Type (The_Any : in Any) return  TypeCode.Object
     renames Droopi.Any.Get_Precise_Type;
--    is
--       Result : TypeCode.Object := Get_Type (The_Any);
--    begin
--       while CORBA.TypeCode.Kind (Result) = Tk_Alias loop
--          Result := CORBA.TypeCode.Content_Type (Result);
--       end loop;
--       return Result;
--    end Get_Precise_Type;

   ----------------
   --  Set_Type  --
   ----------------
   procedure Set_Type
     (The_Any  : in out Any;
      The_Type : in     TypeCode.Object)
     renames Droopi.Any.Set_Type;
--    begin
--       The_Any.The_Type := The_Type;
--    end Set_Type;

   ---------------------------------
   --  Iterate_Over_Any_Elements  --
   ---------------------------------
   procedure Iterate_Over_Any_Elements (In_Any : in Any) is
   begin
      --  null;
      raise Droopi.Not_Implemented;
   end Iterate_Over_Any_Elements;

   ---------------------
   --  Get_Empty_Any  --
   ---------------------

   function Get_Empty_Any (Tc : TypeCode.Object) return Any
     renames Droopi.Any.Get_Empty_Any;

   ----------------
   --  Is_Empty  --
   ----------------

   function Is_Empty (Any_Value : in Any) return Boolean
     renames Droopi.Any.Is_Empty;

   ---------------------
   --  Set_Any_Value  --
   ---------------------

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value     :        Short) is
   begin
      Droopi.Any.Set_Any_Value
        (Any_Value, Droopi.Types.Short (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Long) is
   begin
      Droopi.Any.Set_Any_Value
        (Any_Value, Droopi.Types.Long (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Long_Long) is
   begin
      Droopi.Any.Set_Any_Value
        (Any_Value, Droopi.Types.Long_Long (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Unsigned_Short) is
   begin
      Droopi.Any.Set_Any_Value
        (Any_Value, Droopi.Types.Unsigned_Short (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Unsigned_Long) is
   begin
      Droopi.Any.Set_Any_Value
        (Any_Value, Droopi.Types.Unsigned_Long (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Unsigned_Long_Long) is
   begin
      Droopi.Any.Set_Any_Value
        (Any_Value, Droopi.Types.Unsigned_Long_Long (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : CORBA.Float) is
   begin
      Droopi.Any.Set_Any_Value
        (Any_Value, Droopi.Types.Float (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Double) is
   begin
      Droopi.Any.Set_Any_Value
        (Any_Value, Droopi.Types.Double (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Long_Double) is
   begin
      Droopi.Any.Set_Any_Value
        (Any_Value, Droopi.Types.Long_Double (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Boolean) is
   begin
      Droopi.Any.Set_Any_Value
        (Any_Value, Droopi.Types.Boolean (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Char) is
   begin
      Droopi.Any.Set_Any_Value
        (Any_Value, Droopi.Types.Char (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Wchar) is
   begin
      Droopi.Any.Set_Any_Value
        (Any_Value, Droopi.Types.Wchar (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Octet) is
   begin
      Droopi.Any.Set_Any_Value
        (Any_Value, Droopi.Types.Octet (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : Any)
     renames Droopi.Any.Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : TypeCode.Object)
     renames Droopi.Any.Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : CORBA.String) is
   begin
      Droopi.Any.Set_Any_Value
        (Any_Value, Droopi.Types.String (Value));
   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : CORBA.Wide_String) is
   begin
      Droopi.Any.Set_Any_Value
        (Any_Value, Droopi.Types.Wide_String (Value));
   end Set_Any_Value;


   -------------------------------
   --  Set_Any_Aggregate_Value  --
   -------------------------------

   procedure Set_Any_Aggregate_Value
     (Any_Value : in out CORBA.Any)
     renames Droopi.Any.Set_Any_Aggregate_Value;

   ---------------------------
   --  Get_Aggregate_Count  --
   ---------------------------

   function Get_Aggregate_Count (Value : Any) return Unsigned_Long is
   begin
      return Unsigned_Long (Droopi.Any.Get_Aggregate_Count (Value));
   end Get_Aggregate_Count;

   -----------------------------
   --  Add_Aggregate_Element  --
   -----------------------------

   procedure Add_Aggregate_Element
     (Value   : in out Any;
      Element : in     Any)
     renames Droopi.Any.Add_Aggregate_Element;

   -----------------------------
   --  Get_Aggregate_Element  --
   -----------------------------

   function Get_Aggregate_Element
     (Value : Any;
      Tc : CORBA.TypeCode.Object;
      Index : CORBA.Unsigned_Long)
     return Any is
   begin
      return Droopi.Any.Get_Aggregate_Element
        (Value, Tc, Droopi.Types.Unsigned_Long (Index));
   end Get_Aggregate_Element;

   -------------------------------
   --  Get_Empty_Any_Aggregate  --
   -------------------------------

   function Get_Empty_Any_Aggregate
     (Tc : CORBA.TypeCode.Object)
     return Any
     renames Droopi.Any.Get_Empty_Any_Aggregate;

   function Image (NV : NamedValue) return Standard.String
     renames Droopi.Any.Image;

end CORBA;

