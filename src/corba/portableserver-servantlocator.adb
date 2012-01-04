------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O R T A B L E S E R V E R . S E R V A N T L O C A T O R         --
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

with PortableServer.ServantLocator.Impl;

package body PortableServer.ServantLocator is

   ---------------
   -- Preinvoke --
   ---------------

   procedure Preinvoke
     (Self       :     Local_Ref;
      Oid        :     ObjectId;
      Adapter    :     PortableServer.POA_Forward.Ref;
      Operation  :     CORBA.Identifier;
      The_Cookie : out Cookie;
      Returns    : out Servant)
   is
   begin
      if CORBA.Object.Is_Nil (CORBA.Object.Ref (Self)) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Preinvoke
       (Impl.Object_Ptr (Entity_Of (Self)),
        Oid,
        Adapter,
        Operation,
        The_Cookie,
        Returns);
   end Preinvoke;

   ----------------
   -- Postinvoke --
   ----------------

   procedure Postinvoke
     (Self        : Local_Ref;
      Oid         : ObjectId;
      Adapter     : PortableServer.POA_Forward.Ref;
      Operation   : CORBA.Identifier;
      The_Cookie  : Cookie;
      The_Servant : Servant)
   is
   begin
      if CORBA.Object.Is_Nil (CORBA.Object.Ref (Self)) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Impl.Postinvoke
       (Impl.Object_Ptr (Entity_Of (Self)),
        Oid,
        Adapter,
        Operation,
        The_Cookie,
        The_Servant);
   end Postinvoke;

end PortableServer.ServantLocator;
