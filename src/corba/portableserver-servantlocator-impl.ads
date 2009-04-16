------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O R T A B L E S E R V E R . S E R V A N T L O C A T O R . I M P L    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

with PortableServer.ServantManager.Impl;

package PortableServer.ServantLocator.Impl is

   type Object is new PortableServer.ServantManager.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   procedure Preinvoke
     (Self       : access Object;
      Oid        :        PortableServer.ObjectId;
      Adapter    :        PortableServer.POA_Forward.Ref;
      Operation  :        CORBA.Identifier;
      The_Cookie :    out PortableServer.ServantLocator.Cookie;
      Returns    :    out PortableServer.Servant);

   procedure Postinvoke
     (Self        : access Object;
      Oid         :        PortableServer.ObjectId;
      Adapter     :        PortableServer.POA_Forward.Ref;
      Operation   :        CORBA.Identifier;
      The_Cookie  :        PortableServer.ServantLocator.Cookie;
      The_Servant :        PortableServer.Servant);

private

   type Object is
     new PortableServer.ServantManager.Impl.Object with null record;

   function Is_A
     (Self            : access Object;
      Logical_Type_Id :        Standard.String) return Boolean;

end PortableServer.ServantLocator.Impl;
