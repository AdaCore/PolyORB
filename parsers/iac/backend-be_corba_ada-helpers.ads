------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--         B A C K E N D . B E _ C O R B A _ A D A . H E L P E R S          --
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

package Backend.BE_CORBA_Ada.Helpers is

   package Package_Spec is

      procedure Visit (E : Node_Id);

   end Package_Spec;

   package Package_Body is

      procedure Visit (E : Node_Id);

      procedure Add_Dependency (Dep : Node_Id);
      --  When a Helper package is initialized by the PolyORB
      --  Initialization Manager, all the packages this package
      --  depends on must be initialized. For exemple, When we create
      --  a complex TypeCode, the TypeCodes used must be already
      --  created. This procedure add the dependency 'Dep' to the
      --  current Helper package.

   end Package_Body;

end Backend.BE_CORBA_Ada.Helpers;
