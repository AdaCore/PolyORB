------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O R T A B L E S E R V E R . S E R V A N T L O C A T O R         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

package body PortableServer.ServantLocator is

   ---------------
   -- Preinvoke --
   ---------------

   procedure Preinvoke
     (Self       : in  Ref;
      Oid        : in  ObjectId;
      Adapter    : in  PortableServer.POA_Forward.Ref;
      Operation  : in  CORBA.Identifier;
      The_Cookie : out Cookie;
      Returns    : out Servant)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self);
      pragma Unreferenced (Oid);
      pragma Unreferenced (Adapter);
      pragma Unreferenced (Operation);
      pragma Warnings (On); --  WAG:3.15

      Result_Cookie : constant Cookie_Base :=
        Cookie_Base'(Root_Cookie with null record);

   begin
      The_Cookie := new Cookie_Base'(Result_Cookie);
      Returns := null;

   end Preinvoke;

   ----------------
   -- Postinvoke --
   ----------------

   procedure Postinvoke
     (Self        : in Ref;
      Oid         : in ObjectId;
      Adapter     : in PortableServer.POA_Forward.Ref;
      Operation   : in CORBA.Identifier;
      The_Cookie  : in Cookie;
      The_Servant : in Servant)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self);
      pragma Unreferenced (Oid);
      pragma Unreferenced (Adapter);
      pragma Unreferenced (Operation);
      pragma Unreferenced (The_Cookie);
      pragma Unreferenced (The_Servant);
      pragma Warnings (On); --  WAG:3.15

   begin
      null;
   end Postinvoke;

end PortableServer.ServantLocator;
