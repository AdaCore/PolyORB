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
--  $Id: //droopi/main/compilers/ciao/ciao-translator-state.ads#7 $

with Asis;

package CIAO.Translator.State is

   ---------------------------------------------------
   -- Translator_State                              --
   -- Actual for the State_Information parameter in --
   -- Iterator.Traverse_Element.                    --
   ---------------------------------------------------

   type Translator_State is record
      Unit_Category : Unit_Categories := Other;
      --  The category (Pure, Remote_Types or Remote_Call_Interface)
      --  of the library unit being translated.

      Repository   : Node_Id := No_Node;
      --  The topmost IDL node (a container for all others).

      Current_Node : Node_Id := No_Node;
      --  The IDL node which is being constructed.
   end record;

   procedure Initialize_Translator_State
     (Category   : in     Unit_Categories;
      Unit       : in     Asis.Compilation_Unit;
      Repository : in     Node_Id;
      State      :    out Translator_State);
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
   --  Element started being processed. This must be called
   --  before returning from a Pre_Translate_Element operation
   --  in an ASIS recursive iterator when State.Current_Node has
   --  been changed and the children of that node are processed
   --  using the implicit recursive traversal. In that case,
   --  the Post_Translate_Element operation must restore
   --  State.Current_Node to its recorded previous value when
   --  the element and all its children have been processed.

   function Get_Translation (Element : Asis.Element)
     return Node_Id;
   pragma Inline (Get_Translation);
   function Get_Previous_Current_Node (Element : Asis.Element)
     return Node_Id;
   pragma Inline (Get_Previous_Current_Node);

end CIAO.Translator.State;
