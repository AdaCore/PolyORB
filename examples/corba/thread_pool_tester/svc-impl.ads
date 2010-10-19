------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             S V C . I M P L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2008, Free Software Foundation, Inc.             --
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

pragma Style_Checks ("NM32766");
---------------------------------------------------
--  This file has been generated automatically from
--  svc.idl
--  by IAC (IDL to Ada Compiler) 2.5.0w (rev. 127820).
---------------------------------------------------
with PortableServer;
with CORBA;
pragma Elaborate_All (CORBA);

package Svc.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is
     access all Object'Class;

   procedure Wait
     (Self : not null access Object;
      Cond_Id : CORBA.Short);

private
   type Object is
     new PortableServer.Servant_Base with record
         --  Insert components to hold the state of the implementation object
         null;
      end record;

end Svc.Impl;
