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

--  The proxy package generator.
--  Produces a CORBA servant implementation from an
--  annotated IDL tree obtained as output of the translator.
with CIAO.Generator.ORB_Deps_G;
with CIAO.Types; use CIAO.Types;

generic
   with package ORB_Deps is new CIAO.Generator.ORB_Deps_G (<>);
package CIAO.Generator.Proxy is

   procedure Generate
     (Tree : in Node_Id);
   --  Generate a CORBA servant implementation for
   --  the remotely callable entitites (remote procedures,
   --  remote accesses to subprogram and remote
   --  accesses to class-wide type).

end CIAO.Generator.Proxy;
