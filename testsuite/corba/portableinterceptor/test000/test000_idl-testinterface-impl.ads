------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       T E S T 0 0 0 _ I D L . T E S T I N T E R F A C E . I M P L        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

with PortableServer;

package Test000_Idl.TestInterface.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure Proc (Self : access Object);

   procedure Process_Normal (Self : access Object);

   procedure Raise_Exception (Self : access Object);

   procedure Init
     (Self : access Object;
      Name : in     String);

private

   type Process_State is (Normal, Raise_Exception);

   type Object is
     new PortableServer.Servant_Base with
   record
      Name  : CORBA.String;
      State : Process_State := Normal;
   end record;

end Test000_Idl.TestInterface.Impl;
