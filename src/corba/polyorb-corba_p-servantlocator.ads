------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . C O R B A _ P . S E R V A N T L O C A T O R        --
--                                                                          --
--                                 S p e c                                  --
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

--  This package provides glue codee between PolyORB's
--  ServantLocator and CORBA specific ServantLocator.

with PortableServer.ServantLocator;

with PolyORB.POA_Types;
with PolyORB.Servants;
with PolyORB.Types;

package PolyORB.CORBA_P.ServantLocator is

   package PPT renames PolyORB.POA_Types;

   type CORBA_ServantLocator is new PPT.ServantLocator with private;

   procedure Create
     (Self :    out PPT.ServantLocator_Access;
      SL   : access PortableServer.ServantLocator.Ref'Class);

   function Get_Servant_Manager
     (Self : CORBA_ServantLocator)
     return PortableServer.ServantLocator.Ref'Class;

   procedure Preinvoke
     (Self       : access CORBA_ServantLocator;
      Oid        : in     PPT.Object_Id;
      Adapter    : access PPT.Obj_Adapter'Class;
      Operation  : in     PolyORB.Types.Identifier;
      The_Cookie :    out PPT.Cookie;
      Returns    :    out PolyORB.Servants.Servant_Access);

   procedure Postinvoke
     (Self        : access CORBA_ServantLocator;
      Oid         : in     PPT.Object_Id;
      Adapter     : access PPT.Obj_Adapter'Class;
      Operation   : in     PolyORB.Types.Identifier;
      The_Cookie  : in     PPT.Cookie;
      The_Servant : in     PolyORB.Servants.Servant_Access);

private

   type SL_Ptr is access all PortableServer.ServantLocator.Ref'Class;

   type CORBA_ServantLocator is new PPT.ServantLocator with record
      SL : SL_Ptr;
   end record;

end PolyORB.CORBA_P.ServantLocator;
