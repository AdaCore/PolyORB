--  A generic package to manage dictionnary (a map of associations between
--  keys and values).

--  $Id$

with Sequences.Unbounded;

generic

   type Key is private;
   type Value is private;

   with function "=" (K1 : Key; K2 : Key) return Boolean is <>;
   with function "=" (V1 : Value; K2 : Value) return Boolean is <>;

package PolyORB.Dict is

   Bad_Element : exception;
   Duplicate_Key : exception;

   Null_Index : constant Natural;

   type Dict is private;
   type Dict_Access is access all Dict;

   procedure Set
     (D : in out Dict; K : Key; V : Value; Index : out Natural);

   function Find_Key (D : Dict; K : Key) return Natural;
   function Find_Value (D : Dict; V : Value) return Natural;

   function Get (D : Dict; K : Key) return Value;
   function Get_By_Index (D : Dict; Index : Natural) return Value;

   procedure Remove (D : in out Dict; K : Key; V : out Value);
   procedure Remove_By_Index
     (D : in out Dict; Index : Natural; V : out Value);

private

   Null_Index : constant Natural := 0;

   type Dict_Entry is record
      K : Key;
      V : Value;
   end record;

   package Dict_Entry_Seqs is new Sequences.Unbounded (Dict_Entry);

   type Dict is record
      Entries : Dict_Entry_Seqs.Sequence;
   end record;

end PolyORB.Dict;

