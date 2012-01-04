------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       B A C K E N D . C O N F I G                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with Backend.BE_CORBA_Ada;
with Backend.BE_IDL;
with Backend.BE_Types;

package body Backend.Config is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register_Language
        (BE_IDL.Generate'Access,
         BE_IDL.Usage'Access,
         "idl",
         "Dump parsed IDL file");
      Register_Language
        (BE_CORBA_Ada.Generate'Access,
         BE_CORBA_Ada.Usage'Access,
         "ada",
         "(default) Generate Ada source code");
      Register_Language
        (BE_Types.Generate'Access,
         BE_Types.Usage'Access,
         "types",
         "Generate a list of all types present in the IDL file");

      --  Now set the current language to the default

      Set_Current_Language ("ada");
   end Initialize;

end Backend.Config;
