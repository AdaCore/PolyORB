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
--  $Id: //depot/ciao/main/ciao-filenames.adb#1 $

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body CIAO.Filenames is

   function IDL_File_Name (Ada_File_Name : String)
     return String is
      Result : String (Ada_File_Name'Range)
        := Ada_File_Name;
   begin
      Result (Result'Last - 2 .. Result'Last) := "idl";
      return Result;
   end IDL_File_Name;

   function Ada_File_Name (Full_Name : Asis.Program_Text;
                           Part      : Unit_Part := Unit_Declaration)
     return String is
      Extension : constant array (Unit_Part) of Character
        := (Unit_Declaration => 's',
            Unit_Body        => 'b');
      Result : String := To_String (Full_Name) & ".ad?";
   begin
      for I in Result'First .. Result'Last - 4 loop
         if Result (I) = '.' then
            Result (I) := '-';
         else
            Result (I) := To_Lower (Result (I));
         end if;
      end loop;

      Result (Result'Last) := Extension (Part);
      return Result;
   end Ada_File_Name;

end CIAO.Filenames;
