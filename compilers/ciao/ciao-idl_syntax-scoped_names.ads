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

--  IDL syntactic information.
--  Helper subprograms for the construction of
--  <scoped_name> nodes.
--  $Id: //depot/ciao/main/ciao-idl_syntax-scoped_names.ads#1 $

with CIAO.Types; use CIAO.Types;

package CIAO.IDL_Syntax.Scoped_Names is

   --  While constructing a <scoped_name>, it is convenient
   --  to extend it towards the left by prepending prefixes.
   --  This is done by making appending the prefixes to List3
   --  using Add_Prefix/Add_Absolute, then calling Chain_Prefixes
   --  to properly chain the nodes using their Prefix (Node2)
   --  fields.

   function Prefixes        (N : in Node_Id)
     return List_Id;                 --  List3

   procedure Add_Prefix              --  List3
     (N   : Node_Id;
      Val : Node_Id);

   procedure Add_Absolute            --  List3
     (N   : Node_Id);

   procedure Chain_Prefixes
     (N : in Node_Id);

end CIAO.IDL_Syntax.Scoped_Names;
