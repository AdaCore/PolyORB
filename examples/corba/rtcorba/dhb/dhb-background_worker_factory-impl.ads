------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   D H B . B A C K G R O U N D _ W O R K E R _ F A C T O R Y . I M P L    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

with DHB.Background_Worker;

with PortableServer;
with RTPortableServer.POA;

package DHB.Background_Worker_Factory.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function Create (Self : access Object) return DHB.Background_Worker.Ref;

   procedure Destroy
     (Self       : access Object;
      The_Worker : DHB.Background_Worker.Ref);

   procedure Initialize
     (Self   : access Object;
      RT_POA : RTPortableServer.POA.Local_Ref);

private

   type Object is new PortableServer.Servant_Base with record
      RT_POA : RTPortableServer.POA.Local_Ref;
   end record;

end DHB.Background_Worker_Factory.Impl;
