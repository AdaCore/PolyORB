------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O R T A B L E S E R V E R . S E R V A N T L O C A T O R . I M P L    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id: //droopi/main/src/corba/portableserver-servantlocator-impl.ads#2 $

with PortableServer.ServantManager.Impl;

package PortableServer.ServantLocator.Impl is

   type Object is abstract new PortableServer.ServantManager.Impl.Object
     with private;
   type Object_Ptr is access all Object'Class;

   procedure Preinvoke
     (Self       : in out Object;
      Oid        : in     ObjectId;
      Adapter    : in     PortableServer.POA.Ref;
      Operation  : in     CORBA.Identifier;
      The_Cookie :    out Cookie;
      Returns    :    out Servant)
      is abstract;

   procedure Postinvoke
     (Self        : in out Object;
      Oid         : in     ObjectId;
      Adapter     : in     PortableServer.POA.Ref;
      Operation   : in     CORBA.Identifier;
      The_Cookie  : in     Cookie;
      The_Servant : in     Servant)
      is abstract;

private

   type Object is abstract new PortableServer.ServantManager.Impl.Object with
     null record;

end PortableServer.ServantLocator.Impl;
