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

--  Various mapping functions for CIAO.Translator.
--  $Id: //droopi/main/compilers/ciao/ciao-translator-maps.ads#5 $

with Asis;       use Asis;

with Idl_Fe.Types; use Idl_Fe.Types;

package CIAO.Translator.Maps is

   -------------------------------------------
   -- IDL_Module_Name                       --
   -- The name of the IDL module that maps  --
   -- the given library unit.               --
   -------------------------------------------

   function IDL_Module_Name (Library_Unit : Compilation_Unit)
     return String;

   ---------------------------------------------------------
   -- Relative_Scoped_Name                                --
   -- A <scoped_name> that denotes the translation of the --
   -- Denoted_Definition with respect to a declaration    --
   -- wchich makes reference to it.                       --
   ---------------------------------------------------------

--    function Relative_Scoped_Name (Denoted_Definition : Definition;
--                                   Referer            : Declaration)
--      return Node_Id;

   ----------------------------------------------
   -- Operator_Symbol_Identifier               --
   -- Return an <identifier> from the image of --
   -- a defining operator symbol.              --
   ----------------------------------------------

   function Operator_Symbol_Identifier (Op : Asis.Defining_Name)
     return String;

   ----------------------------------------------
   -- Character_Literal_Identifier             --
   -- Return an <identifier> from the image of --
   -- a defining character literal.            --
   ----------------------------------------------

   function Character_Literal_Identifier (Ch : Program_Text)
     return String;

   ---------------------------------------------------
   -- Base_Type                                     --
   -- Return the base type kind that represents the --
   -- given class of Ada types.                     --
   ---------------------------------------------------

   type Root_Type is
     (Root_Integer,
      Root_Modular,
      Root_Real,
      Root_Boolean,
      Root_Char,
      Root_String);

   function Base_Type (T : Root_Type) return Node_Id;
   pragma Inline (Base_Type);

end CIAO.Translator.Maps;
