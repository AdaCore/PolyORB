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
--  $Id: //droopi/main/compilers/ciao/ciao-translator-state.adb#3 $

with Asis;
with Asis.Elements;

with GNAT.HTable;

with Idl_Fe.Tree;  use Idl_Fe.Tree;
with Idl_Fe.Types; use Idl_Fe.Types;
with Errors;       use Errors;

package body CIAO.Translator.State is

   procedure Initialize_Translator_State
     (Category : in Unit_Categories;
      State    : out Translator_State) is
   begin
      State.Unit_Category := Category;
      State.IDL_Tree      := Make_Repository (No_Location);
      State.Current_Node  := State.IDL_Tree;
      State.Pass          := Normal;
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

   procedure Set_Map_Info (Element : Asis.Element; Info : Map_Info);
   pragma Inline (Set_Map_Info);
   --  Set the Map_Info for the Element.

   function Get_Map_Info  (Element : Asis.Element)
     return Map_Info;
   pragma Inline (Get_Map_Info);
   --  Retrieve the mapping information for an Element.
   --  If no information was set, Nil_Map_Info is returned.

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

   procedure Set_Translation
     (Element     : Asis.Element;
      Translation : Node_Id) is
      Info : Map_Info := Get_Map_Info (Element);
   begin
      Info.Translation := Translation;
      Set_Map_Info (Element, Info);
      --  Set_Origin (Translation, Element);
      --  XXX might be needed later when generating code.
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

   procedure Set_Map_Info (Element : Asis.Element; Info : Map_Info) is
   begin
      Map_HTable.Set (Element, Info);
   end Set_Map_Info;

   function Get_Map_Info (Element : Asis.Element) return Map_Info is
   begin
      return Map_HTable.Get (Element);
   end Get_Map_Info;

end CIAO.Translator.State;


