------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . Q O S . T A G G E D _ C O M P O N E N T S         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body PolyORB.QoS.Tagged_Components is

   use PolyORB.Representations.CDR.Common;
   use GIOP_Tagged_Component_Lists;

   procedure Free is
     new Ada.Unchecked_Deallocation (Encapsulation, Encapsulation_Access);

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents
     (QoS : access QoS_GIOP_Tagged_Components_Parameter)
   is
      Iter : Iterator := First (QoS.Components);

   begin
      while not Last (Iter) loop
         Free (Value (Iter).Data);
         Next (Iter);
      end loop;

      Deallocate (QoS.Components);
   end Release_Contents;

end PolyORB.QoS.Tagged_Components;
