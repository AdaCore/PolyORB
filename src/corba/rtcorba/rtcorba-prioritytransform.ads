------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            R T C O R B A . P R I O R I T Y T R A N S F O R M             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

with PortableServer;

package RTCORBA.PriorityTransform is

   type Object is tagged private;

   --  Implementation Note: RT-CORBA specifications (formal/03-11-01)
   --  is unclear and does not state default behavior for this
   --  object. By default, these functions will always set Returns
   --  parameter to False.
   --
   --  Other implementations will provide a functionnal mapping.

   procedure Inbound
     (Self         : Object;
      The_Priority : in out RTCORBA.Priority;
      Oid          : PortableServer.ObjectId;
      Returns      :    out CORBA.Boolean);

   procedure Outbound
     (Self         : Object;
      The_Priority : in out RTCORBA.Priority;
      Oid          : PortableServer.ObjectId;
      Returns      :    out CORBA.Boolean);

private

   type Object is tagged null record;

end RTCORBA.PriorityTransform;
