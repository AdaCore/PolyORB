------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      C I A O . T R A N S L A T O R                       --
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

--  This unit generates a decorated IDL tree by traversing
--  the ASIS tree of a DSA package specification.
with Asis;

with Idl_Fe.Types; use Idl_Fe.Types;

package CIAO.Translator is

   Translation_Error : exception;
   --  An error occured, and the library unit could
   --  not be translated.

   Not_Implemented : exception;
   --  A construct was encountered whose translation is not implemented
   --  in this version of the CIAO translator.

   -----------------------------------------------
   -- Translate                                 --
   -- Produce the IDL tree corresponding to the --
   -- translation of the libray unit.           --
   -----------------------------------------------

   procedure Translate
     (LU : in Asis.Compilation_Unit;
      Repository : in out Node_Id);

end CIAO.Translator;
