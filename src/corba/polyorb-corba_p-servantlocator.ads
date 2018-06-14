------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . C O R B A _ P . S E R V A N T L O C A T O R        --
--                                                                          --
--                                 S p e c                                  --
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

pragma Ada_2012;

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

   overriding procedure Preinvoke
     (Self       : access CORBA_ServantLocator;
      Oid        :        PPT.Object_Id;
      Adapter    : access PPT.Obj_Adapter'Class;
      Operation  :        PolyORB.Types.Identifier;
      The_Cookie :    out PPT.Cookie;
      Returns    :    out PolyORB.Servants.Servant_Access;
      Error      : in out PolyORB.Errors.Error_Container);

   overriding procedure Postinvoke
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
