------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--        P O R T A B L E S E R V E R . S E R V A N T L O C A T O R         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.4 $
--                                                                          --
--         Copyright (C) 1999, 2000 ENST Paris University, France.          --
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

with PortableServer.ServantManager;
with PortableServer.POA;
with PortableServer.ServantLocator.Impl;
with Broca.POA;

package body PortableServer.ServantLocator is

   procedure Preinvoke
     (Self : in Ref;
      Oid : in ObjectId;
      Adapter : in PortableServer.POA.Ref;
      Operation : in CORBA.Identifier;
      The_Cookie : out Cookie;
      Returns : out Servant) is
   begin
      PortableServer.ServantLocator.Impl.Preinvoke
        (Impl.Object'Class
         (Broca.POA.To_Internal_Skeleton (Self).P_Servant.all),
         Oid, Adapter, Operation, The_Cookie, Returns);
   end Preinvoke;

   procedure Postinvoke
     (Self : in Ref;
      Oid : in ObjectId;
      Adapter : in PortableServer.POA.Ref;
      Operation : in CORBA.Identifier;
      The_Cookie : in Cookie;
      The_Servant : in Servant) is
   begin
      PortableServer.ServantLocator.Impl.Postinvoke
        (Impl.Object'Class
         (Broca.POA.To_Internal_Skeleton (Self).P_Servant.all),
         Oid, Adapter, Operation, The_Cookie, The_Servant);
   end Postinvoke;
end PortableServer.ServantLocator;
