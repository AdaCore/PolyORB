------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     M O M A . C O N N E C T I O N _ F A C T O R I E S . Q U E U E S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Implementation of Connection_Factory for Message Queues.

--  $Id$

package MOMA.Connection_Factories.Queues is

   type Connection_Factory_Queue is new Connection_Factory with null record;

   procedure Create (Self     : out Connection_Factory_Queue;
                     Remote   : PolyORB.References.Ref);
   --  Implementation of 'Create' for the 'Queues' child.

   function Create_Connection (Self   : Connection_Factory_Queue)
                               return MOMA.Connections.Connection'Class;
   --  Implementation of 'Create_Connection' for the 'Queues' child.

   function Create_Connection (Self      : Connection_Factory_Queue;
                               Username  : String;
                               Password  : String)
                               return MOMA.Connections.Connection'Class;
   --  Implementation of 'Create_Connection' for the 'Queues' child.

end MOMA.Connection_Factories.Queues;
