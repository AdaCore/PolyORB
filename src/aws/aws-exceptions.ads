------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       A W S . E X C E P T I O N S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;

with AWS.Log;
with AWS.Response;
with AWS.Status;

package AWS.Exceptions is

   type Data is record
      Fatal   : Boolean;
      --  If True it means that we go a fatal error. The slot will be
      --  terminated so AWS will loose one of it's simultaneous connection.
      --  This is clearly an AWS internal error that should be fixed in AWS.

      Slot    : Positive;
      --  The failing slot number

      Request : Status.Data;
      --  The complete request information that was served when the slot has
      --  failed. This variable is set only when Fatal is False.
   end record;

   type Unexpected_Exception_Handler is access
     procedure (E           : Ada.Exceptions.Exception_Occurrence;
                Log         : in out AWS.Log.Object;
                Error       : Data;
                Answer      : in out Response.Data);
   --  Unexpected exception handler can be set to monitor server errors.
   --  Answer can be set with the answer to send back to the client's
   --  browser. Note that this is possible only for non fatal error
   --  (i.e. Error.Fatal is False).
   --  Log is the error log object for the failing server, it can be used
   --  to log user's information (if error log is activated for this
   --  server). Note that the server will have already logged information
   --  about the problem.

end AWS.Exceptions;
