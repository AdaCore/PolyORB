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

--  Generic template for ORB-specific matter.
--  $Id: //depot/ciao/main/ciao-generator-orb_deps_g.ads#3 $

with CIAO.Types;

generic

   ObjectId_Sequences_Package : in Wide_String;
   --  The name of the instanciation of CORBA.Sequences (Octet)
   --  used for PortableServer.ObjectId.

   ObjectId_Sequences_Dependency : in Wide_String;
   --  The name of the library unit that encloses
   --  that instanciation.

   with function Sequences_Package (N : CIAO.Types.Node_Id)
     return Wide_String
   is <>;
   --  The name of the instanciation of CORBA.Sequences (Octet)
   --  used for N_Sequence_Type node N.

package CIAO.Generator.ORB_Deps_G is
end CIAO.Generator.ORB_Deps_G;
