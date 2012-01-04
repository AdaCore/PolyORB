------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O R T A B L E S E R V E R . S E R V A N T A C T I V A T O R       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
