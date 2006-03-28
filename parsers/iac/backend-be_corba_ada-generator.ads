------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--             B A C K E N D . B E _ A D A . G E N E R A T O R              --
--                                                                          --
--                                 S p e c                                  --
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

package Backend.BE_CORBA_Ada.Generator is

   Var_Name_Len    : Natural := 0;
   Generate_Specs  : Boolean := True;
   Generate_Bodies : Boolean := True;

   procedure Generate (N : Node_Id);

end Backend.BE_CORBA_Ada.Generator;
