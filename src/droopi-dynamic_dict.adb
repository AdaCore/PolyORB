--  A dynamic dictionnary of objects, indexed by Strings.

--  $Id$

with Ada.Unchecked_Deallocation;
with GNAT.HTable;

package body Droopi.Dynamic_Dict is

   --------------------------------------------------------
   -- A hash table that stores the Value associated with --
   -- a String key.                                      --
   --------------------------------------------------------

   type String_Ptr is access all String;
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Ptr);

   type Hash_Val is new Integer range 0 .. 32;

   function Hash (S : String_Ptr) return Hash_Val;
   function Equal (S1, S2 : String_Ptr) return Boolean;
   --  Simple {hash,equality} functions operating on a string access.
   --  Used in instanciation of GNAT.HTable.

   function Hash (S : String_Ptr) return Hash_Val
   is
      function Hash_String is new GNAT.HTable.Hash (Hash_Val);
   begin
      pragma Assert (S /= null);
      return Hash_String (S.all);
   end Hash;

   function Equal (S1, S2 : String_Ptr) return Boolean is
   begin
      pragma Assert (S1 /= null and then S2 /= null);
      return S1.all = S2.all;
   end Equal;

   type Dict_Entry is record
      Key_Ptr   : String_Ptr;
      The_Value : Value;
   end record;

   pragma Warnings (Off);
   Null_Dict_Entry : Dict_Entry;
   pragma Warnings (On);
   --  No explicit initialisation.

   package HT is new GNAT.HTable.Simple_HTable
     (Header_Num => Hash_Val,
      Element    => Dict_Entry,
      No_Element => Null_Dict_Entry,
      Key        => String_Ptr,
      Hash       => Hash,
      Equal      => Equal);

   ------------
   -- Lookup --
   ------------

   function Lookup
      (K : String)
     return Value
   is
      KK : aliased String := K;
      E  : constant Dict_Entry := HT.Get (KK'Unchecked_Access);
   begin
      if E.Key_Ptr = null then
         raise Key_Not_Found;
      end if;
      return E.The_Value;
   end Lookup;

   procedure Register
     (K : String;
      V : Value)
   is
      KK : String_Ptr := new String'(K);
      E  : Dict_Entry := HT.Get (KK);
   begin
      if E.Key_Ptr = null then
         E.Key_Ptr := KK;
      else
         Free (KK);
      end if;

      E.The_Value := V;
      HT.Set (E.Key_Ptr, E);
   end Register;

   procedure Unregister
     (K : String)
   is
   begin
      raise Not_Implemented;
   end Unregister;

end Droopi.Dynamic_Dict;
