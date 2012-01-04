------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                C I A O . T R A N S L A T O R . S T A T E                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1999-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  The internal state of the translator.
with Asis;
with CIAO.ASIS_Queries;

package CIAO.Translator.State is

   use CIAO.ASIS_Queries;

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
   function Get_Origin (Node : Node_Id) return Asis.Element;
   pragma Inline (Get_Origin);
   --  Return the original ASIS element at the origin of Node.
   --  If no such information was recorded (by a previous call
   --  to Set_Translation), return Nil_Element.

end CIAO.Translator.State;
