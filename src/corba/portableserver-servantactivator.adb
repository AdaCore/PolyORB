------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O R T A B L E S E R V E R . S E R V A N T A C T I V A T O R       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PortableServer.ServantActivator.Impl;

package body PortableServer.ServantActivator is

   ---------------
   -- Incarnate --
   ---------------

   function Incarnate
     (Self    : Local_Ref;
      Oid     : PortableServer.ObjectId;
      Adapter : PortableServer.POA_Forward.Ref)
      return PortableServer.Servant
   is
   begin
      if CORBA.Object.Is_Nil (CORBA.Object.Ref (Self)) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Incarnate (Impl.Object_Ptr (Entity_Of (Self)), Oid, Adapter);
   end Incarnate;

   -----------------
   -- Etherealize --
   -----------------

   procedure Etherealize
     (Self                  : Local_Ref;
      Oid                   : PortableServer.ObjectId;
      Adapter               : PortableServer.POA_Forward.Ref;
      Serv                  : PortableServer.Servant;
      Cleanup_In_Progress   : CORBA.Boolean;
      Remaining_Activations : CORBA.Boolean)
   is
   begin
      if CORBA.Object.Is_Nil (CORBA.Object.Ref (Self)) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Etherealize
        (Impl.Object_Ptr (Entity_Of (Self)),
         Oid,
         Adapter,
         Serv,
         Cleanup_In_Progress,
         Remaining_Activations);
   end Etherealize;

end PortableServer.ServantActivator;
