------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . T E S T _ O B J E C T _ P O A               --
--                                                                          --
--                                 B o d y                                  --
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

--  $Id$

with Ada.Exceptions;
with Ada.Text_IO;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Components;
with PolyORB.Requests;
with PolyORB.Servants.Interface;
with PolyORB.Types;
with PolyORB.Exceptions;

package body Ping_Object is

   use Ada.Text_IO;

   use PolyORB.Any;
   use PolyORB.Requests;
   use PolyORB.Servants.Interface;

   Count : Natural := 0;
   --  Count number of invocations

   ---------------------
   -- Execute_Servant --
   ---------------------

   function Execute_Servant
     (Obj : access My_Object;
      Msg : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class
   is
      pragma Unreferenced (Obj);

      use PolyORB.Any.NVList;
      use PolyORB.Types;

   begin
      Put_Line ("Handle Message : enter");

      if Msg in Execute_Request then
         declare
            use PolyORB.Any.NVList.Internals;
            use PolyORB.Any.NVList.Internals.NV_Lists;
            use PolyORB.Exceptions;

            Req   : constant Request_Access
              := Execute_Request (Msg).Req;
            Args  : PolyORB.Any.NVList.Ref;
            Error : Error_Container;
         begin
            Put_Line ("The server is executing the request:"
                      & PolyORB.Requests.Image (Req.all));

            Create (Args);
            if Req.all.Operation = To_PolyORB_String ("ping") then
               Add_Item (Args,
                         (Name => To_PolyORB_String ("S"),
                          Argument => Get_Empty_Any (TypeCode.TC_String),
                          Arg_Modes => PolyORB.Any.ARG_IN));
               Arguments (Req, Args, Error);

               if Found (Error) then
                  raise PolyORB.Unknown;
                  --  XXX We should do something more constructive

               end if;

               --  Actual implementation of the echoString function:
               --  simply return the argument
               Count := Count + 1;

               Put_Line ("Pong !" & Count'Img);

               Req.Result.Argument := To_Any
                 (Get_Empty_Any (TypeCode.TC_Void));
            else
               raise PolyORB.Components.Unhandled_Message;
            end if;

            return Executed_Request'(Req => Req);
         end;
      else
         raise PolyORB.Components.Unhandled_Message;
      end if;

   exception
      when E : others =>
         Put_Line ("Handle_Message: Got exception "
                   & Ada.Exceptions.Exception_Information (E));
         raise;
   end Execute_Servant;

end Ping_Object;

