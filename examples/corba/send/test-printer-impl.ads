------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    T E S T . P R I N T E R . I M P L                     --
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

package Test.Printer.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Acc is access Object;

   procedure PrintString (Self : access Object; Mesg : CORBA.String);
   procedure PrintLong (Self : access Object; K : CORBA.Long);

   function EchoString (Self : access Object; Mesg : CORBA.String)
     return CORBA.String;

   function EchoLong (Self : access Object; K : CORBA.Long)
     return CORBA.Long;

   function PrintString_Called return Natural;

   function PrintLong_Called return Natural;

private

   type Object is new PortableServer.Servant_Base with null record;

end Test.Printer.Impl;
