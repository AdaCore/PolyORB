------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            M O M A . C O N F I G U R A T I O N . S E R V E R             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2002 Free Software Foundation, Inc.             --
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

--  This package provides utility functions to setup a MOMA server node.

--  $Id$

with MOMA.Types;

with PolyORB.References;

package MOMA.Configuration.Server is

   procedure Create_Message_Pool
     (Pool :     MOMA.Types.Message_Pool;
      Ref  : out PolyORB.References.Ref);
   --  Create a message pool and return its reference.

   procedure Create_Router
     (Id         :     MOMA.Types.String;
      Ref        : out PolyORB.References.Ref;
      Router_Ref :     PolyORB.References.Ref := PolyORB.References.Nil_Ref);
   --  Create a router and return its reference.
   --  If Router_Ref is specified, it's a reference to another router on the
   --  network.

end MOMA.Configuration.Server;
