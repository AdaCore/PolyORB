------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              D H B . W O R K E R _ F A C T O R Y . I M P L               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--        Copyright (C) 2006-2008, Free Software Foundation, Inc.           --
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

with DHB.Worker;

with PortableServer;
with RTPortableServer.POA;

package DHB.Worker_Factory.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function Create (Self : access Object) return DHB.Worker.Ref;

   procedure Destroy
     (Self       : access Object;
      The_Worker : DHB.Worker.Ref);

   procedure Initialize
     (Self   : access Object;
      RT_POA : RTPortableServer.POA.Local_Ref);

private

   type Object is new PortableServer.Servant_Base with record
      RT_POA : RTPortableServer.POA.Local_Ref;
   end record;

end DHB.Worker_Factory.Impl;
