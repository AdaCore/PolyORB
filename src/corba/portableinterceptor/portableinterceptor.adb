------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O R T A B L E I N T E R C E P T O R                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

with PolyORB.Exceptions;

package body PortableInterceptor is

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in     Ada.Exceptions.Exception_Occurrence;
      To   :    out ForwardRequest_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in     Ada.Exceptions.Exception_Occurrence;
      To   :    out InvalidSlot_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

end PortableInterceptor;
