------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O R T A B L E S E R V E R . S E R V A N T A C T I V A T O R       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

package body PortableServer.ServantActivator is

   ---------------
   -- Incarnate --
   ---------------

   function Incarnate
     (Self    : in Ref;
      Oid     : in ObjectId;
      Adapter : in PortableServer.POA_Forward.Ref)
     return PortableServer.Servant
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (Oid);
      pragma Unreferenced (Adapter);
      pragma Warnings (On);

   begin
      return null;
   end Incarnate;

   -----------------
   -- Etherealize --
   -----------------

   procedure Etherealize
     (Self                  : in Ref;
      Oid                   : in PortableServer.ObjectId;
      Adapter               : in PortableServer.POA_Forward.Ref;
      Serv                  : in PortableServer.Servant;
      Cleanup_In_Progress   : in CORBA.Boolean;
      Remaining_Activations : in CORBA.Boolean)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (Oid);
      pragma Unreferenced (Adapter);
      pragma Unreferenced (Serv);
      pragma Unreferenced (Cleanup_In_Progress);
      pragma Unreferenced (Remaining_Activations);
      pragma Warnings (On);

   begin
      null;
   end Etherealize;

end PortableServer.ServantActivator;
