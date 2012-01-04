------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   C I A O . G E N E R A T O R . I D L                    --
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
