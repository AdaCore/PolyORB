------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                T E S T _ S E R V A N T A C T I V A T O R                 --
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

with CORBA;
with PortableServer.ServantActivator;

package Test_ServantActivator is

   --  Activator that does nothing

   type Null_Activator_Ref is new PortableServer.ServantActivator.Ref
     with null record;

   type Null_Activator_Access is access all Null_Activator_Ref;

   function Incarnate
     (Self    : in Null_Activator_Ref;
      Oid     : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA_Forward.Ref)
     return PortableServer.Servant;

   procedure Etherealize
     (Self                  : in Null_Activator_Ref;
      Oid                   : in PortableServer.ObjectId;
      Adapter               : in PortableServer.POA_Forward.Ref;
      Serv                  : in PortableServer.Servant;
      Cleanup_In_Progress   : in CORBA.Boolean;
      Remaining_Activations : in CORBA.Boolean);

   --  Simple activator that creates a servant on demand

   type Simple_Activator_Ref is new PortableServer.ServantActivator.Ref
     with null record;

   type Simple_Activator_Access is access all Simple_Activator_Ref;

   function Incarnate
     (Self    : in Simple_Activator_Ref;
      Oid     : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA_Forward.Ref)
     return PortableServer.Servant;

   procedure Etherealize
     (Self                  : in Simple_Activator_Ref;
      Oid                   : in PortableServer.ObjectId;
      Adapter               : in PortableServer.POA_Forward.Ref;
      Serv                  : in PortableServer.Servant;
      Cleanup_In_Progress   : in CORBA.Boolean;
      Remaining_Activations : in CORBA.Boolean);

   procedure Run_Test_ServantActivator;

end Test_ServantActivator;
