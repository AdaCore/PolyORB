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
--  �cole nationale sup�rieure des    --
--  t�l�communications                --
----------------------------------------

--  The interface description (IDL) generator.
--  Produces an IDL specification from an IDL
--  tree obtained as output of the translator.
with Ada.Text_Io;

with CIAO.Types; use CIAO.Types;

package CIAO.Generator.IDL is

   procedure Generate
     (Tree : in Node_Id;
      File : in Ada.Text_Io.File_Type);
   --  Generate an OMG IDL interface description for
   --  the remotely callable entitites (remote procedures,
   --  remote accesses to subprogram and remote
   --  accesses to class-wide type).

end CIAO.Generator.IDL;
