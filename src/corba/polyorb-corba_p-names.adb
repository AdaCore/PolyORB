------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . C O R B A _ P . N A M E S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2002 Free Software Foundation, Inc.           --
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

--  String constants defined by OMG specifications.

package body PolyORB.CORBA_P.Names is

   Prefix  : constant String := "omg.org";
   Version : constant String := "1.0";

   ----------------
   -- OMG_Prefix --
   ----------------

   function OMG_Prefix
     return String is
   begin
      return Prefix;
   end OMG_Prefix;

   ----------------------
   -- OMG_RepositoryId --
   ----------------------

   function OMG_RepositoryId
     (Name : String)
     return String is
   begin
      return "IDL:" & Prefix & "/" & Name & ":" & Version;
   end OMG_RepositoryId;

   -----------------
   -- OMG_Version --
   -----------------

   function OMG_Version
     return String is
   begin
      return Version;
   end OMG_Version;

end PolyORB.CORBA_P.Names;