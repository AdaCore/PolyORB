------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . T E S T _ O B J E C T _ P O A               --
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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Text_IO;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Errors;

package body PolyORB.Test_Object_POA is

   use Ada.Text_IO;

   use PolyORB.Any;
   use PolyORB.Requests;

   --------------------------------------
   -- Application part of the servant. --
   --------------------------------------

   function echoString
     (O : My_Object;
      S : PolyORB.Types.String)
     return PolyORB.Types.String
   is
      pragma Warnings (Off);
      pragma Unreferenced (O);
      pragma Warnings (On);
   begin
      Put_Line ("echoString is being executed with argument: "
                & PolyORB.Types.To_Standard_String (S));
      return S;
   end echoString;

   function echoInteger
     (O : My_Object;
      I : PolyORB.Types.Long)
     return PolyORB.Types.Long
   is
      pragma Warnings (Off);
      pragma Unreferenced (O);
      pragma Warnings (On);
   begin
      Put_Line ("Echo_Integer is being executed with argument" & I'Img);

      return I;
   end echoInteger;

   --------------------------------------------------------------
   -- "Middleware glue" that should be generated automatically --
   --------------------------------------------------------------

   overriding function Execute_Servant
     (Obj : not null access My_Object;
      Req : Requests.Request_Access) return Boolean
   is
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;
      use PolyORB.Errors;
      use PolyORB.Types;

      Args  : PolyORB.Any.NVList.Ref;
      Error : Error_Container;
   begin
      Put_Line ("Handle Message : enter");
      Put_Line ("The server is executing the request:"
                & PolyORB.Requests.Image (Req.all));

      Create (Args);
      if Req.Operation.all = "echoString" then
         Add_Item (Args,
                   (Name => To_PolyORB_String ("S"),
                    Argument => Get_Empty_Any (TypeCode.TC_String),
                    Arg_Modes => PolyORB.Any.ARG_IN));
         Arguments (Req, Args, Error);

         if Found (Error) then
            raise Program_Error;
            --  XXX We should do something more constructive

         end if;

         Req.Result.Argument := To_Any
           (echoString
            (Obj.all,
             From_Any
             (Value (First (List_Of (Args).all)).Argument)));
         Put_Line ("Result: " & Image (Req.Result));

      elsif Req.Operation.all = "echoInteger" then
         Add_Item (Args, (Name => To_PolyORB_String ("I"),
                          Argument => Get_Empty_Any (TypeCode.TC_Long),
                          Arg_Modes => PolyORB.Any.ARG_IN));
         Arguments (Req, Args, Error);

         if Found (Error) then
            raise Program_Error;
            --  XXX We should do something more constructive

         end if;

         Req.Result.Argument := To_Any
           (echoInteger
            (Obj.all,
             From_Any (Value (First (List_Of (Args).all)).Argument)));
            Put_Line ("Result: " & Image (Req.Result));

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

end PolyORB.Test_Object_POA;
