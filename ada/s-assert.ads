------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUNTIME COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . A S S E R T I O N S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                              --
--                                                                          --
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This module contains the definition of the exception that is raised
--  when an assertion made using pragma Assert fails (i.e. the given
--  expression evaluates to False. It also contains the routines used
--  to raise this assertion with an associated message.

with System.Standard_Library;

package System.Assertions is

   Assert_Msg : String (1 .. System.Standard_Library.Exception_Msg_Max);
   Assert_Msg_Length : Natural := 0;
   --  Characters and length of message passed to Raise_Assert_Failure
   --  The initial length of zero indicates that no message has been set
   --  yet (and Assert_Message will return the null string in such cases)

   Assert_Failure : exception;
   --  Exception raised when assertion fails

   procedure Raise_Assert_Failure (Msg : String);
   --  Called to raise Assert_Failure with given message

end System.Assertions;
