------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            C I A O . G E N E R A T O R . O R B _ D E P S _ G             --
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

--  Generic template for ORB-specific matter.
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
