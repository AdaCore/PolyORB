------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--                       B A C K E N D . C O N F I G                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                        Copyright (c) 2005 - 2006                         --
--            Ecole Nationale Superieure des Telecommunications             --
--                                                                          --
-- IAC is free software; you  can  redistribute  it and/or modify it under  --
-- terms of the GNU General Public License  as published by the  Free Soft- --
-- ware  Foundation;  either version 2 of the liscence or (at your option)  --
-- any  later version.                                                      --
-- IAC is distributed  in the hope that it will be  useful, but WITHOUT ANY --
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or        --
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for --
-- more details.                                                            --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; if not, write to the Free Software Foundation, Inc.,  --
-- 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.            --
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
      Register
        (BE_IDL.Generate'Access,
         BE_IDL.Usage'Access,
         "idl",
         "Dump parsed IDL file");
      Register
        (BE_CORBA_Ada.Generate'Access,
         BE_CORBA_Ada.Usage'Access,
         "ada",
         "Produce Ada files");
      Register
        (BE_Types.Generate'Access,
         BE_Types.Usage'Access,
         "types",
         "Produce a list of all present types in the idl file");
   end Initialize;

end Backend.Config;
