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

--  ORB-specific matter, Broca version.
--  $Id: //depot/ciao/main/ciao-generator-broca.ads#2 $

with CIAO.Generator.ORB_Deps_G;
with CIAO.Types;

package CIAO.Generator.Broca is

   Broca_ObjectId_Sequences_Package : constant Wide_String
     := "Broca.Sequences.Octet_Sequences";

   Broca_ObjectId_Sequences_Dependency : constant Wide_String
     := "Broca.Sequences";

   function Broca_Sequences_Package (N : CIAO.Types.Node_Id)
     return Wide_String;

   package ORB_Deps is new CIAO.Generator.ORB_Deps_G
     (ObjectId_Sequences_Package    => Broca_ObjectId_Sequences_Package,
      ObjectId_Sequences_Dependency => Broca_ObjectId_Sequences_Dependency,
      Sequences_Package             => Broca_Sequences_Package);

end CIAO.Generator.Broca;
