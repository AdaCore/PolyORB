------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . C O R B A _ P . S E R V A N T A C T I V A T O R      --
--                                                                          --
--                                 S p e c                                  --
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

--  This package provides glue codee between PolyORB's
--  ServantActivator and CORBA specific ServantActivator.

--  $Id$

with PortableServer.ServantActivator;

with PolyORB.POA_Types;
with PolyORB.Servants;

package PolyORB.CORBA_P.ServantActivator is

   package PPT renames PolyORB.POA_Types;

   type CORBA_ServantActivator is new PPT.ServantActivator with private;

   procedure Create
     (Self :    out PPT.ServantActivator_Access;
      SA   : access PortableServer.ServantActivator.Ref'Class);

   function Get_Servant_Manager
     (Self : CORBA_ServantActivator)
     return PortableServer.ServantActivator.Ref'Class;

   function Incarnate
     (Self    : access CORBA_ServantActivator;
      Oid     : in     PPT.Object_Id;
      Adapter : access PPT.Obj_Adapter'Class)
     return PolyORB.Servants.Servant_Access;

   procedure Etherealize
     (Self                  : access CORBA_ServantActivator;
      Oid                   : in     PPT.Object_Id;
      Adapter               : access PPT.Obj_Adapter'Class;
      Serv                  : in     PolyORB.Servants.Servant_Access;
      Cleanup_In_Progress   : in     Boolean;
      Remaining_Activations : in     Boolean);

private

   type SA_Ptr is access all PortableServer.ServantActivator.Ref'Class;

   type CORBA_ServantActivator is new PPT.ServantActivator with record
      SA : SA_Ptr;
   end record;

end PolyORB.CORBA_P.ServantActivator;
