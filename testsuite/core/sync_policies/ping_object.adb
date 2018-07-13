------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P I N G _ O B J E C T                           --
--                                                                          --
--                                 B o d y                                  --
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

pragma Ada_2012;

with Ada.Exceptions;
with Ada.Text_IO;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Types;
with PolyORB.Errors;

package body Ping_Object is

   use Ada.Text_IO;

   use PolyORB.Any;
   use PolyORB.Requests;

   Count : Natural := 0;
   --  Count number of invocations

   ---------------------
   -- Execute_Servant --
   ---------------------

   overriding function Execute_Servant
     (Obj : not null access My_Object;
      Req : PolyORB.Requests.Request_Access) return Boolean
   is
      pragma Unreferenced (Obj);

      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;
      use PolyORB.Errors;
      use PolyORB.Types;

      Args  : PolyORB.Any.NVList.Ref;
      Error : Error_Container;
   begin
      Put_Line ("The server is executing the request:"
                & PolyORB.Requests.Image (Req.all));

      Create (Args);
      if Req.all.Operation.all = "ping" then
         Add_Item (Args,
                   (Name => To_PolyORB_String ("S"),
                    Argument => Get_Empty_Any (TypeCode.TC_String),
                    Arg_Modes => PolyORB.Any.ARG_IN));
         Arguments (Req, Args, Error);

         if Found (Error) then
            raise Program_Error;
            --  XXX We should do something more constructive

         end if;

         --  Actual implementation of the echoString function:
         --  simply return the argument
         Count := Count + 1;

         Put_Line ("Pong !" & Count'Img);

         Req.Result.Argument := To_Any
           (Get_Empty_Any (TypeCode.TC_Void));
      else
         raise Program_Error;
      end if;

      return True;

   exception
      when E : others =>
         Put_Line ("Handle_Message: Got exception "
                   & Ada.Exceptions.Exception_Information (E));
         raise;
   end Execute_Servant;

end Ping_Object;
