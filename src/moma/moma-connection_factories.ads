------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            M O M A . C O N N E C T I O N _ F A C T O R I E S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2004 Free Software Foundation, Inc.           --
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

--  A Connection_Factory contains all information to create a connection to
--  the MOMA provider.

--  XXX need to clarify the notion of provider.

with MOMA.Types;

package MOMA.Connection_Factories is

   type Connection_Factory is private;

   procedure Create
     (Self     : out Connection_Factory;
      Remote   :     MOMA.Types.Ref);
   --  Create a new connection factory, with the provider Remote.

   --  Accessors to the Connection_Factory internals.

   procedure Set_Ref
     (Self    : in out Connection_Factory;
      Remote  :        MOMA.Types.Ref);

   function Get_Ref
     (Self    : Connection_Factory)
     return MOMA.Types.Ref;

private

   type Connection_Factory is record
      Remote : MOMA.Types.Ref;
      --  The access point to the MOMA domain.
      --  XXX : this is a concept to clarify.
   end record;

end MOMA.Connection_Factories;
