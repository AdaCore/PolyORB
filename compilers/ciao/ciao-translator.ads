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
--  Copyright (c) 1999-2002           --
--  École nationale supérieure des    --
--  télécommunications                --
----------------------------------------

--  This unit generates a decorated IDL tree by traversing
--  the ASIS tree of a DSA package specification.
--  $Id: //droopi/main/compilers/ciao/ciao-translator.ads#4 $

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
