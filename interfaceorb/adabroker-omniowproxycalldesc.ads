------------------------------------------------------------------------------
--                                                                          --
--                        ADABROKER COMPONENTS                              --
--                                                                          --
--        A D A B R O K E R . O M N I O W P R O X Y C A L L D E S C         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This is a root class. For each subprogram of an IDL interface which is
--  declared "one way", a descendant of this class has to be provided.  It
--  contains all the information to make the remote call : arguments,
--  results, exceptions, and how to send them on/ reveive them from a giop.
--  ( see proxyCall.h)

with CORBA;
with AdaBroker.GIOP_C;

package AdaBroker.OmniOWProxyCallDesc is

   type Object is abstract tagged limited private;
   --  Type of an omniowProxyCallDesc object

   type Object_Ptr is access all Object;
   --  Type pointer on type Object

   function Aligned_Size
     (Self    : in Object;
      Size_In : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long is abstract;
   --  This function computes the size needed to marshall the arguments of
   --  the subprogram

   procedure Marshal_Arguments
     (Self        : in Object;
      Giop_Client : in out AdaBroker.GIOP_C.Object) is abstract;
   --  Marshalls the arguments of the subprogram into a Giop_C object

   function Operation
     (Self : in Object)
      return CORBA.String is abstract;
   --  Returns the name of the subprogram

private

   type Object is abstract tagged limited null record;
   --  Implementation of the private type Object

end AdaBroker.OmniOWProxyCallDesc;

