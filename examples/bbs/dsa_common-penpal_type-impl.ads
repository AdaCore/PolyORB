------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          D S A _ C O M M O N . P E N P A L _ T Y P E . I M P L           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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
with PortableServer;

package DSA_Common.Penpal_Type.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure Initialize
     (Self : access Object;
      Name : CORBA.String);

   function Name_Of
     (Self : access Object)
     return CORBA.String;

   procedure New_Message
     (Self : access Object;
      Sender : CORBA.String;
      Message : CORBA.String);

private

   type Object is new PortableServer.Servant_Base with record
      Name : CORBA.String;
   end record;

end DSA_Common.Penpal_Type.Impl;
