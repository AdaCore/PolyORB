------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . C O R B A _ P . A D A P T E R A C T I V A T O R      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

--  This package provides glue codee between PolyORB's
--  AdapterActivator and CORBA specific AdapterActivator.

with PortableServer.AdapterActivator;

with PolyORB.Errors;
with PolyORB.POA_Types;
with PolyORB.Smart_Pointers;

package PolyORB.CORBA_P.AdapterActivator is

   package PPT renames PolyORB.POA_Types;

   type CORBA_AdapterActivator is new PPT.AdapterActivator with private;

   procedure Create
     (Self :    out PPT.AdapterActivator_Access;
      AA   : access PortableServer.AdapterActivator.Ref'Class);

   function Get_Adapter_Activator
     (Self : CORBA_AdapterActivator)
     return PortableServer.AdapterActivator.Ref'Class;

   procedure Unknown_Adapter
     (Self   : access CORBA_AdapterActivator;
      Parent : access PPT.Obj_Adapter'Class;
      Name   : String;
      Result :    out Boolean;
      Error  : in out PolyORB.Errors.Error_Container);

private

   type CORBA_AdapterActivator is new PPT.AdapterActivator with null record;

   type AA_Ptr is access all PortableServer.AdapterActivator.Ref'Class;

   type Object is new PolyORB.Smart_Pointers.Non_Controlled_Entity with record
      AA : AA_Ptr;
   end record;

   type Object_Ptr is access all Object;

end PolyORB.CORBA_P.AdapterActivator;
