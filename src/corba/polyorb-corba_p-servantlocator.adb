------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . C O R B A _ P . S E R V A N T L O C A T O R        --
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

with CORBA.Impl;

package body PolyORB.CORBA_P.ServantLocator is

   ------------
   -- Create --
   ------------

   procedure Create
     (Self :    out PPT.ServantLocator_Access;
      SL   : access PortableServer.ServantLocator.Ref'Class)
   is
      Locator : Object_Ptr := new Object;

   begin
      Self := new CORBA_ServantLocator;
      Locator.SL := SL_Ptr (SL);

      Set (CORBA_ServantLocator (Self.all),
           PolyORB.Smart_Pointers.Entity_Ptr (Locator));
   end Create;

   -------------------------
   -- Get_Servant_Manager --
   -------------------------

   function Get_Servant_Manager
     (Self : CORBA_ServantLocator)
     return PortableServer.ServantLocator.Ref'Class
   is
      Locator : constant Object_Ptr := Object_Ptr (Entity_Of (Self));

   begin
      return Locator.SL.all;
   end Get_Servant_Manager;

   ---------------
   -- Preinvoke --
   ---------------

   procedure Preinvoke
     (Self       : access CORBA_ServantLocator;
      Oid        : in     PPT.Object_Id;
      Adapter    : access PPT.Obj_Adapter'Class;
      Operation  : in     PolyORB.Types.Identifier;
      The_Cookie :    out PPT.Cookie;
      Returns    :    out PolyORB.Servants.Servant_Access)
   is
      CORBA_POA     : PortableServer.POA_Forward.Ref;

      CORBA_Servant : PortableServer.Servant;

      Locator : PortableServer.ServantLocator.Ref'Class
        := PortableServer.ServantLocator.Ref'Class
        (Get_Servant_Manager (Self.all));

   begin
      PortableServer.POA_Forward.Set
        (CORBA_POA,
         PolyORB.Smart_Pointers.Entity_Ptr (Adapter));

      PortableServer.ServantLocator.Preinvoke
        (Locator,
         PortableServer.ObjectId (Oid),
         CORBA_POA,
         CORBA.Identifier (Operation),
         PortableServer.ServantLocator.Cookie (The_Cookie),
         CORBA_Servant);

      Returns := PolyORB.Servants.Servant_Access
        (PortableServer.To_PolyORB_Servant (CORBA_Servant));
   end Preinvoke;

   ----------------
   -- Postinvoke --
   ----------------

   procedure Postinvoke
     (Self        : access CORBA_ServantLocator;
      Oid         : in     PPT.Object_Id;
      Adapter     : access PPT.Obj_Adapter'Class;
      Operation   : in     PolyORB.Types.Identifier;
      The_Cookie  : in     PPT.Cookie;
      The_Servant : in     PolyORB.Servants.Servant_Access)
   is
      CORBA_POA     : PortableServer.POA_Forward.Ref;

      CORBA_Servant : constant PortableServer.Servant :=
        PortableServer.Servant (CORBA.Impl.To_CORBA_Servant (The_Servant));

      Locator : PortableServer.ServantLocator.Ref'Class
        := PortableServer.ServantLocator.Ref'Class
        (Get_Servant_Manager (Self.all));

   begin
      PortableServer.POA_Forward.Set
        (CORBA_POA,
         PolyORB.Smart_Pointers.Entity_Ptr (Adapter));

      PortableServer.ServantLocator.Postinvoke
        (Locator,
         PortableServer.ObjectId (Oid),
         CORBA_POA,
         CORBA.Identifier (Operation),
         PortableServer.ServantLocator.Cookie (The_Cookie),
         CORBA_Servant);
   end Postinvoke;

end PolyORB.CORBA_P.ServantLocator;
