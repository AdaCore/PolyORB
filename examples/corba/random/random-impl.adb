------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                          R A N D O M . I M P L                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $LastChangedRevision$
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Numerics.Discrete_Random;
pragma Elaborate (Ada.Numerics.Discrete_Random);

with Random.Skel;
pragma Elaborate (Random.Skel);
pragma Warnings (Off, Random.Skel);
--  No entity from Random.Skel is referenced.

package body Random.Impl is

   type l48 is range 0 .. 2 ** 31 - 1;
   package l48_Random is new Ada.Numerics.Discrete_Random (l48);
   l48_Gen : l48_Random.Generator;

   function lrand48 (Self : access Object)
     return CORBA.Long is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      return CORBA.Long (l48_Random.Random (l48_Gen));
   end lrand48;

   type m48 is range -2 ** 31 .. 2 ** 31 - 1;
   package m48_Random is new Ada.Numerics.Discrete_Random (m48);
   m48_Gen : m48_Random.Generator;

   function mrand48 (Self : access Object)
     return CORBA.Long is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      return CORBA.Long (m48_Random.Random (m48_Gen));
   end mrand48;

begin
   l48_Random.Reset (l48_Gen);
   m48_Random.Reset (m48_Gen);
end Random.Impl;

