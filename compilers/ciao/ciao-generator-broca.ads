------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 C I A O . G E N E R A T O R . B R O C A                  --
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

--  ORB-specific matter, Broca version.
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
