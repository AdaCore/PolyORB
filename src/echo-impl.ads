------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            E C H O . I M P L                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

--  $Id: //droopi/main/src/echo-impl.ads#4 $

with PortableServer.POA;
with CORBA.ServerRequest;
with CORBA;
with PortableServer;

package Echo.Impl is

   type Object is
     new PortableServer.DynamicImplementation with private;

   type Object_Ptr is access all Object'Class;

   function echoString
     (Self : access Object;
      Mesg : in CORBA.String)
     return CORBA.String;

   procedure Invoke
     (Self : access Object;
      Request : in CORBA.ServerRequest.Object_ptr);

   function Primary_Interface (Self : access Object; -- ....
      POA_Ptr : PortableServer.POA.Ref) return String;

private

   type Object is
     new PortableServer.DynamicImplementation with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end Echo.Impl;
