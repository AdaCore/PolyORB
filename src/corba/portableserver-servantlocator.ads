------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O R T A B L E S E R V E R . S E R V A N T L O C A T O R         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

with CORBA;

with PortableServer.ServantManager;

with PolyORB.POA_Types;

package PortableServer.ServantLocator is

   type Local_Ref is new PortableServer.ServantManager.Local_Ref with private;

   type Root_Cookie is new PolyORB.POA_Types.Cookie_Base with private;

   type Cookie_Base is new Root_Cookie with private;

   type Cookie is access all Cookie_Base'Class;

   procedure Preinvoke
     (Self       :     Local_Ref;
      Oid        :     ObjectId;
      Adapter    :     PortableServer.POA_Forward.Ref;
      Operation  :     CORBA.Identifier;
      The_Cookie : out Cookie;
      Returns    : out Servant);

   procedure Postinvoke
     (Self        : Local_Ref;
      Oid         : ObjectId;
      Adapter     : PortableServer.POA_Forward.Ref;
      Operation   : CORBA.Identifier;
      The_Cookie  : Cookie;
      The_Servant : Servant);

   Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableServer/ServantLocator:1.0";

private

   type Local_Ref is
     new PortableServer.ServantManager.Local_Ref with null record;

   type Root_Cookie is new PolyORB.POA_Types.Cookie_Base with null record;

   type Cookie_Base is new Root_Cookie with null record;

end PortableServer.ServantLocator;
