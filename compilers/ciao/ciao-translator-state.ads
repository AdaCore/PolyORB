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
--  $Id: //droopi/main/compilers/ciao/ciao-translator-state.ads#4 $

package CIAO.Translator.State is

   ---------------------------------------------------
   -- Translator_State                              --
   -- Actual for the State_Information parameter in --
   -- Iterator.Traverse_Element.                    --
   ---------------------------------------------------

   type Translation_Pass is
     (Normal,
      --  We are translating declarations in order of
      --  appearance.

--      Deferred_Discriminant_Part,
      --  We are translating a discriminant part which was
      --  precedently left over into a set of <member>s.

--      Type_Definition,
      --  We are translating a type_definition into a <type_spec>.

--       Translate_Subtype_Mark,
      --  We are translating a subtype_mark.

      Normal_Formal_Parameter,
      --  An ordinary formal parameter of a subprogram

      Self_Formal_Parameter
      --  The first controlling formal parameter of a dispatching
      --  operation.
      );

   type Translator_State is record
      Unit_Category : Unit_Categories := Other;
      --  The category (Pure, Remote_Types or Remote_Call_Interface)
      --  of the library unit being translated.

      IDL_Tree     : Node_Id := No_Node;
      --  The root of the translated IDL tree.

      Current_Node : Node_Id := No_Node;
      --  The IDL node which is being constructed.

      Pass         : Translation_Pass := Normal;
      --  The current translation pass.
   end record;

   procedure Initialize_Translator_State
     (Category : in Unit_Categories;
      State    : out Translator_State);
   --  Set the inital values of a Translator_State record.

   procedure Set_Translation
     (Element     : Asis.Element;
      Translation : Node_Id);
   pragma Inline (Set_Translation);
   --  Record the IDL translation of an Element.

   procedure Set_Previous_Current_Node
     (Element               : Asis.Element;
      Previous_Current_Node : Node_Id);
   pragma Inline (Set_Previous_Current_Node);
   --  Record the IDL node that was Current_Node when
   --  Element started being processed.

   function Get_Translation (Element : Asis.Element)
     return Node_Id;
   pragma Inline (Get_Translation);
   function Get_Previous_Current_Node (Element : Asis.Element)
     return Node_Id;
   pragma Inline (Get_Previous_Current_Node);

end CIAO.Translator.State;
