------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       P O R T A B L E S E R V E R                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
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

with Droopi;

package body PortableServer is

   procedure Invoke
     (Self : DynamicImplementation;
      Request : CORBA.ServerRequest.Object) is
   begin
      raise Droopi.Not_Implemented;
   end Invoke;

   function Get_Default_POA
     (For_Servant : Servant_Base)
     return POA_Forward.Ref is
   begin
      raise Droopi.Not_Implemented;
      return Get_Default_POA (For_Servant);
   end Get_Default_POA;

   procedure Get_Members
     (From : in CORBA.Exception_Occurrence;
      To   : out ForwardRequest_Members) is
   begin
      raise Droopi.Not_Implemented;
   end Get_Members;

end PortableServer;
