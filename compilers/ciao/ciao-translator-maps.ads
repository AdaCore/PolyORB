------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 C I A O . T R A N S L A T O R . M A P S                  --
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

--  Various mapping functions for CIAO.Translator.
with Asis;       use Asis;

with Errors;
with Idl_Fe.Types; use Idl_Fe.Types;

package CIAO.Translator.Maps is

--    -------------------------------------------
--    -- IDL_Module_Name                       --
--    -- The name of the IDL module that maps  --
--    -- the given library unit.               --
--    -------------------------------------------

--    function IDL_Module_Name (Library_Unit : Compilation_Unit)
--      return String;

   --------------------------------------------------------
   -- Map_Loc                                            --
   -- Map a source code location from ASIS location info --
   -- to Errors.Location.                                --
   --------------------------------------------------------

   function Map_Loc (Element : Asis.Element)
     return Errors.Location;

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
