------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . Q O S . T A G G E D _ C O M P O N E N T S         --
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
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

pragma Ada_2012;

with PolyORB.Utils.Unchecked_Deallocation;

package body PolyORB.QoS.Tagged_Components is

   use PolyORB.Representations.CDR.Common;
   use GIOP_Tagged_Component_Lists;

   procedure Free is
     new PolyORB.Utils.Unchecked_Deallocation.Free


     (Object => Encapsulation,


      Name   => Encapsulation_Access);

   ----------------------
   -- Release_Contents --
   ----------------------

   overriding procedure Release_Contents
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
