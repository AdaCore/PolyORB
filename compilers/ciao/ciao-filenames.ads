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

--  Mapping of file names
--  $Id: //depot/ciao/main/ciao-filenames.ads#1 $

with Asis;

package CIAO.Filenames is

   --  The name of the IDL file that contains the mapping of the
   --  given Ada file.
   function IDL_File_Name (Ada_File_Name : String)
     return String;

   --  The name of the source file that contains the declaration
   --  or body of the library unit whose full name is given.
   type Unit_Part is (Unit_Declaration, Unit_Body);

   function Ada_File_Name (Full_Name : Asis.Program_Text;
                           Part      : Unit_Part := Unit_Declaration)
     return String;

end CIAO.Filenames;
