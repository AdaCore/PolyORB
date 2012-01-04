------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         I D L _ F E . D E B U G                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  This is a debugging package for IDL-Ada Compiler.
--
--  with Idl_Fe.Debug;
--  pragma Elaborate(Idl_Fe.Debug);
--
--  Flag : constant Natural := Idl_Fe.Debug.Is_Active ("specific_name");
--  procedure O is new Idl_Fe.Debug.Output (Flag);
--
--  and then :
--
--  pragma Debug (O ("debugging info"));
--
--  The output will be done if "idl_fe.opt" file contains
--  a line with "specific_name"

package Idl_Fe.Debug is

   pragma Elaborate_Body;

   function Is_Active (Flag : String) return Natural;
   --  Returns 0 when not active

   generic
      Flag : Natural;
   procedure Output (Message : String);
   --  Prints Message on standard output when Flag is not 0

end Idl_Fe.Debug;
