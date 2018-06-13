------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              M O M A . M E S S A G E S . M E X E C U T E S               --
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
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

--  MExecute message type.
--
--  A MExecute message derives directly from MMap message.
--  Its payload contains a map type.
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

package MOMA.Messages.MExecutes is

   type MExecute is new Message with private;

   function Create_Execute_Message return Messages.MExecutes.MExecute;
   --  Create a MExecute message.

   overriding function Image (Self : MExecute) return String;
   --  Image function for MExecute type.

   --  Accessors to MExecute payload.

   function Get_Parameter (Self : MExecute) return MOMA.Types.Map;

   procedure Set_Parameter (Self : in out MExecute; Value : MOMA.Types.Map);

private

   type MExecute is new Message with null record;

end MOMA.Messages.MExecutes;
