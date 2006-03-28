------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--                 B A C K E N D . B E _ A D A . S T U B S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                           Copyright (c) 2005                             --
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

package Backend.BE_CORBA_Ada.Stubs is

   --  The function below is used by the Impls package in the case of local
   --  interfaces the difference between the two functions are very tiny and
   --  dont justify the creation of a new "Is_A_Body" in the Impls package
   function Local_Is_A_Body
     (E        : Node_Id;
      Spec     : Node_Id := No_Node)
     return Node_Id;

   package Package_Spec is

      procedure Visit (E : Node_Id);

   end Package_Spec;

   package Package_Body is

      procedure Visit (E : Node_Id);

   end Package_Body;

end Backend.BE_CORBA_Ada.Stubs;
