package Backend.BE_Types.Utils is

   --  This is the enumeration of all the types
   --  that you can found in the .idl files.
   type TCKind is
      (Tk_Null,
       Tk_Void,
       Tk_Short,
       Tk_Long,
       Tk_Ushort,
       Tk_Ulong,
       Tk_Float,
       Tk_Double,
       Tk_Boolean,
       Tk_Char,
       Tk_Octet,
       Tk_Any,
       Tk_TypeCode,
       Tk_Principal,
       Tk_Objref,
       Tk_Struct,
       Tk_Union,
       Tk_Enum,
       Tk_Sequence,
       Tk_Array,
       Tk_Except,
       Tk_Fixed,
       Tk_String,
       Tk_Alias,
       Tk_Longlong,
       Tk_Ulonglong,
       Tk_Longdouble,
       Tk_Widechar,
       Tk_Wstring,
       Tk_Value,
       Tk_Valuebox,
       Tk_Native,
       Tk_Abstract_Interface,
       Tk_Local_Interface,
       Tk_Component,
       Tk_Home,
       Tk_Event);

   type List is private;
   type Iterator is private;

   --  This List will be contain all the TCKind associated
   --  to the types present in the .idl files.
   --  List_Of_Types : List;

   function First (L : List) return Iterator;
   --  Return an iterator on L positioned at L's first element

   function Last (L : List) return Iterator;
   --  Return an iterator position at the end of L (i.e. immediately
   --  after the last element in L; this iterator has no associated
   --  value).

   function Last (I : Iterator) return Boolean;
   --  True when I is positioned at the end of L (i.e. after the
   --  last element).

   procedure Next (I : in out Iterator);
   --  Move I to the next element in the list

   procedure Insert (L : in out List; T : TCKind);
   --  Prepend the TCKind to the list only if the TCKind
   --  is not present yet.

   procedure Print_List (L : List);
   --  Only for debug.

private

   type Cell;
   type Cell_Access is access all Cell;

   type Cell is record
      Typ  : TCKind;
      --  TCKind of a type present in the .idl file.

      Next : Cell_Access;
      --  Next Cell
   end record;

   type Iterator is record
      The_Cell : Cell_Access;
   end record;

   type List is record
      First, Last : Cell_Access;
   end record;

end Backend.BE_Types.Utils;
