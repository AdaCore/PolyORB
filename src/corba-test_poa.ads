------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       C O R B A . T E S T _ P O A                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

--  $Id$

with PolyORB.POA_Types;
with PolyORB.Components;

package CORBA.Test_POA is

   pragma Elaborate_Body;

   Incorrect_Execution : exception;
   Correct_Execution   : exception;

   type My_Servant is new PolyORB.POA_Types.Servant with
     record
        Nb   : Integer;
        Name : String;
     end record;
   type My_Servant_Access is access all My_Servant;

   function Handle_Message
     (S   : access My_Servant;
      Msg : PolyORB.Components.Message'Class)

     return PolyORB.Components.Message'Class;

   function "=" (Left, Right : My_Servant)
     return Standard.Boolean;

   procedure Test_The_POA;

end CORBA.Test_POA;
