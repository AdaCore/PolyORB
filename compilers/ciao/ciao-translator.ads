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

--  This unit generates a decorated IDL tree by traversing
--  the ASIS tree of a DSA package specification.
--  $Id: //depot/ciao/main/ciao-translator.ads#5 $

with Asis;
with CIAO.Types; use CIAO.Types;

package CIAO.Translator is

   Translation_Error : exception;
   --  An error occured, and the library unit could
   --  not be translated.

   type Unit_Categories is
     (Pure, Remote_Types, Remote_Call_Interface, Other);
   --  A type used to denote the category of the unit
   --  being translated.

   -----------------------------------------------
   -- Translate                                 --
   -- Produce the IDL tree corresponding to the --
   -- translation of the libray unit.           --
   -----------------------------------------------

   function Translate (LU : in Asis.Compilation_Unit) return Node_Id;

end CIAO.Translator;
