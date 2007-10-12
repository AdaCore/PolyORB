------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . C O R B A _ P . S E R V A N T L O C A T O R        --
--                                                                          --
--                                 S p e c                                  --
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

--  This package provides glue codee between PolyORB's
--  ServantLocator and CORBA specific ServantLocator.

with PortableServer.ServantLocator;

with PolyORB.Errors;
with PolyORB.POA_Types;
with PolyORB.Servants;
with PolyORB.Smart_Pointers;
with PolyORB.Types;

package PolyORB.CORBA_P.ServantLocator is

   package PPT renames PolyORB.POA_Types;

   type CORBA_ServantLocator is new PPT.ServantLocator with private;

   procedure Create
     (Self : out PPT.ServantLocator_Access;
      SL   :     PortableServer.ServantLocator.Local_Ref'Class);

   function Get_Servant_Manager
     (Self : CORBA_ServantLocator)
     return PortableServer.ServantLocator.Local_Ref'Class;

   procedure Preinvoke
     (Self       : access CORBA_ServantLocator;
      Oid        :        PPT.Object_Id;
      Adapter    : access PPT.Obj_Adapter'Class;
      Operation  :        PolyORB.Types.Identifier;
      The_Cookie :    out PPT.Cookie;
      Returns    :    out PolyORB.Servants.Servant_Access;
      Error      : in out PolyORB.Errors.Error_Container);

   procedure Postinvoke
     (Self        : access CORBA_ServantLocator;
      Oid         :        PPT.Object_Id;
      Adapter     : access PPT.Obj_Adapter'Class;
      Operation   :        PolyORB.Types.Identifier;
      The_Cookie  :        PPT.Cookie;
      The_Servant :        PolyORB.Servants.Servant_Access);

private

   type CORBA_ServantLocator is new PPT.ServantLocator with null record;

   type Object is new PolyORB.Smart_Pointers.Non_Controlled_Entity with record
      SL : PortableServer.ServantLocator.Local_Ref;
   end record;

   type Object_Ptr is access all Object;

end PolyORB.CORBA_P.ServantLocator;
