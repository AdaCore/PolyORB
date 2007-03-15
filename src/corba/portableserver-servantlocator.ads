------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O R T A B L E S E R V E R . S E R V A N T L O C A T O R         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
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
