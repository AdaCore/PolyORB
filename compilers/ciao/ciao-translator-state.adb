----------------------------------------
--                                    --
--       ----  ---     --  ----       --
--       -      -     - -  -  -       --
--       -      -    ----  -  -       --
--       ----  ---  -   -  ----       --
--                                    --
----------------------------------------
--  CORBA                             --
--  Interface for                     --
--  Ada'95 distributed systems annex  --
--  Objects                           --
----------------------------------------
--  Copyright (c) 1999                --
--  École nationale supérieure des    --
--  télécommunications                --
----------------------------------------

--  The internal state of the translator.
--  $Id: //droopi/main/compilers/ciao/ciao-translator-state.adb#8 $

with Asis.Elements; use Asis.Elements;

with GNAT.HTable;

with Idl_Fe.Types; use Idl_Fe.Types;

package body CIAO.Translator.State is

   procedure Initialize_Translator_State
     (Category   : in     Unit_Categories;
      Unit       : in     Asis.Compilation_Unit;
      Repository : in     Node_Id;
      State      :    out Translator_State)
   is
   begin
      State.Repository    := Repository;
      State.Unit_Category := Category;
      State.Current_Node  := Repository;
   end Initialize_Translator_State;

   -----------------------------------------------------------
   -- Map_Info                                              --
   -- All the information recorded about the mapping        --
   -- of an Ada element.                                    --
   --                                                       --
   -- For an element that is part of the current library    --
   -- unit, the translation is defined by the Mapping       --
   -- Specification Document.                               --
   -- Additionally, for each withed unit, a translation     --
   -- for the unit declaration is registered. It designates --
   -- the corresponding N_Preprocessor_Include node.        --
   -----------------------------------------------------------

   type Map_Info is record
      Translation           : Node_Id := No_Node;
      Previous_Current_Node : Node_Id := No_Node;
   end record;
   --  All information we want to keep about the mapping
   --  of an Ada Element is stored as a Map_Info record.

   Nil_Map_Info : constant Map_Info :=
     (Translation           => No_Node,
      Previous_Current_Node => No_Node);

   -------------------------------------
   -- Elementary hash table accessors --
   -------------------------------------

   procedure Set_Map_Info (Element : Asis.Element; Info : Map_Info);
   pragma Inline (Set_Map_Info);
   --  Set the Map_Info for the Element.

   function Get_Map_Info  (Element : Asis.Element)
     return Map_Info;
   pragma Inline (Get_Map_Info);
   --  Retrieve the mapping information for an Element.
   --  If no information was set, Nil_Map_Info is returned.

   procedure Set_Origin (Node : Node_Id; Element : Asis.Element);
   pragma Inline (Set_Origin);
   --  Record the original ASIS element at the origin of Node.

   --------------------------------
   -- High-level state accessors --
   --------------------------------

   procedure Set_Translation
     (Element     : Asis.Element;
      Translation : Node_Id) is
      Info : Map_Info := Get_Map_Info (Element);
   begin
      Info.Translation := Translation;
      Set_Map_Info (Element, Info);
      Set_Origin (Translation, Element);
   end Set_Translation;

   procedure Set_Previous_Current_Node
     (Element               : Asis.Element;
      Previous_Current_Node : Node_Id) is
      Info : Map_Info := Get_Map_Info (Element);
   begin
      Info.Previous_Current_Node := Previous_Current_Node;
      Set_Map_Info (Element, Info);
   end Set_Previous_Current_Node;

   function Get_Translation (Element : Asis.Element)
     return Node_Id is
   begin
      return Get_Map_Info (Element).Translation;
   end Get_Translation;

   function Get_Previous_Current_Node (Element : Asis.Element)
     return Node_Id is
   begin
      return Get_Map_Info (Element).Previous_Current_Node;
   end Get_Previous_Current_Node;

   -------------------------------------------------------------------
   -- Implementation of the hash functions and hash table accessors --
   -------------------------------------------------------------------

   type Map_HTable_Header_Num is range 1 .. 256;

   function Hash_Element (E : Asis.Element)
     return Map_HTable_Header_Num;
   --  Hash function for an ASIS Element.

   function Hash_Element (E : Asis.Element)
     return Map_HTable_Header_Num is
   begin
      return Map_HTable_Header_Num
        (Integer (Map_HTable_Header_Num'First)
         + Asis.Elements.Hash (E)
         mod Asis.ASIS_Integer (Map_HTable_Header_Num'Last -
                           Map_HTable_Header_Num'First + 1));
   end Hash_Element;

   package Map_HTable is new GNAT.HTable.Simple_HTable
     (Header_Num => Map_HTable_Header_Num,
      Element    => Map_Info,
      No_Element => Nil_Map_Info,
      Key        => Asis.Element,
      Hash       => Hash_Element,
      Equal      => Asis.Elements.Is_Identical);
   --  A table that records a reference of the corresponding
   --  IDL node for any given Ada element (represented by an
   --  Ids.Id value). The Map_HTable is notionally part of the
   --  translator's state.

   procedure Set_Map_Info (Element : Asis.Element; Info : Map_Info) is
   begin
      Map_HTable.Set (Element, Info);
   end Set_Map_Info;

   function Get_Map_Info (Element : Asis.Element) return Map_Info is
   begin
      return Map_HTable.Get (Element);
   end Get_Map_Info;

   function Hash_Node_Id (N : Node_Id)
     return Map_HTable_Header_Num;
   --  Hash function for a node id.

   function Hash_Node_Id (N : Node_Id)
     return Map_HTable_Header_Num
   is
   begin
      return Map_HTable_Header_Num
        (Integer (Map_HTable_Header_Num'First)
         + Integer (N)
         mod Asis.ASIS_Integer (Map_HTable_Header_Num'Last -
                                Map_HTable_Header_Num'First + 1));
   end Hash_Node_Id;

   package Origin_HTable is new GNAT.HTable.Simple_HTable
     (Header_Num => Map_HTable_Header_Num,
      Element    => Asis.Element,
      No_Element => Asis.Nil_Element,
      Key        => Node_Id,
      Hash       => Hash_Node_Id,
      Equal      => "=");
   --  A table that records the original ASIS element corresponding
   --  to an IDL node. The Origin_HTable is notionally part of the
   --  translator's state.

   procedure Set_Origin (Node : Node_Id; Element : Asis.Element) is
   begin
      Origin_HTable.Set (Node, Element);
   end Set_Origin;

   function Get_Origin (Node : Node_Id) return Asis.Element is
   begin
      return Origin_HTable.Get (Node);
   end Get_Origin;

end CIAO.Translator.State;
