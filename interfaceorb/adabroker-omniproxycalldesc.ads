------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--          A D A B R O K E R . O M N I P R O X Y C A L L D E S C           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.3 $
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

--  This is a root class. For each subprogram of an IDL interface, which is
--  not declared "one way", a descendant of this class has to be provided.
--  It contains al the information to make the remote call : arguments,
--  results, exceptions, and how to send them on/ reveive them from a giop.
--  ( see proxyCall.h)

with Ada.Finalization;

with CORBA;

with AdaBroker; use AdaBroker;
with AdaBroker.GIOP_C;

package AdaBroker.OmniProxyCallDesc is

   type Object is
     abstract new Ada.Finalization.Limited_Controlled with private;
   --  Type of an omniProxyCallDesc object

   procedure Set_User_Exceptions
     (Self           : in out Object;
      Has_Exceptions : CORBA.Boolean);
   --  Set the boolean Pd_Has_User_Exception


   function Operation
     (Self : in Object)
      return CORBA.String is abstract;
   --  Returns the name of the subprogram

   function Align_Size
     (Self    : in Object;
      Size_In : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long;
   --  This function computes the size needed to marshall the arguments of
   --  the subprogram

   procedure Marshal_Arguments
     (Self        : in Object;
      GIOP_Client : in out GIOP_C.Object);
   --  Marshals the arguments of the subprogram into a Giop_C object

   procedure Unmarshal_Returned_Values
     (Self        : in out Object;
      GIOP_Client : in out GIOP_C.Object);
   --  Unmarshalls the returned values of the subprogram from a Giop_C
   --  object

   procedure User_Exception
     (Self        : in Object;
      GIOP_Client : in out GIOP_C.Object;
      Repoid      : in CORBA.String);
   --  Must be overloaded by call descs which have exceptions

   function Has_User_Exceptions
     (Self : in Object)
      return CORBA.Boolean;
   --  Returns Pd_Has_User_Exception

private

   type Object is
     abstract new Ada.Finalization.Limited_Controlled with record
        Pd_Has_User_Exception : CORBA.Boolean;
     end record;
   --  Implementation of the private type Object

end AdaBroker.OmniProxyCallDesc;

