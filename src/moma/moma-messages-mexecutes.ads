------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              M O M A . M E S S A G E S . M E X E C U T E S               --
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

--  MExecute message type.
--
--  A MExecute message derives directly from MMap message.
--  Its payload contains a 'map' type.
--
--  It should be filled the following way :
--
--   element             : (name => "method",
--                          value => any(<method_name>))
--   element             : (name => "return" ,
--                          value => any(<return_value>))
--   element i in 1 .. n : (name => "arg_i" ,
--                          value => any(<ith_argument>))
--
--  Warning : for this mapping to work, the method _must_ only accept in
--  parameters.
--
--  Given the destination of this messages, the payload can be interpreted
--  either as
--
--  - a normal payload, if the destination is a message pool.
--  - request parameters if the destination is an ORB object.
--    ORB objects are objects created using a PolyORB ORB application
--    personality (e.g. CORBA, DSA ..)

--  $Id$

package MOMA.Messages.MExecutes is

   type MExecute is new Message with private;

   function Create_Execute_Message return Messages.MExecutes.MExecute;
   --  Create a MExecute message.

   function Image (Self : MExecute) return String;
   --  Image function for MExecute type.

   --  Accessors to MExecute payload.

   function Get_Parameter (Self : MExecute)
                           return MOMA.Types.Map;

   procedure Set_Parameter (Self : in out MExecute;
                            Value : MOMA.Types.Map);

private
   type MExecute is new Message with null record;

end MOMA.Messages.MExecutes;
