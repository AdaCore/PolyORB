------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            E C H O . I M P L                             --
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

with CORBA.ORB;

with Echo.Skel;
pragma Warnings (Off, Echo.Skel);
--  No entity from Echo.Skel is referenced.

package body Echo.Impl is

   ----------------
   -- EchoString --
   ----------------

   function EchoString
     (Self : access Object;
      Mesg : in CORBA.String)
     return CORBA.String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      return Mesg;
   end EchoString;

   ---------------------
   -- EchoString_Wait --
   ---------------------

   function EchoString_Wait
     (Self : access Object;
      Mesg : in CORBA.String)
     return CORBA.String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      delay 3.0;
      return Mesg;
   end EchoString_Wait;

   --------------------------
   -- EchoString_Reentrant --
   --------------------------

   function EchoString_Reentrant
     (Self : access Object;
      Mesg : in CORBA.String)
     return CORBA.String
   is
      pragma Unreferenced (Self);
      Ref : Echo.Ref;
   begin
      CORBA.ORB.String_To_Object (Mesg, Ref);
      return Echo.echoString (Ref, Mesg);
   end EchoString_Reentrant;

end Echo.Impl;
