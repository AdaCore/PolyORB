------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . T E S T _ O B J E C T _ P O A               --
--                                                                          --
--                                 B o d y                                  --
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

--  $Id$

with Ada.Exceptions;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Components;
with PolyORB.Log;
with PolyORB.Objects.Interface;
with PolyORB.Requests;
with PolyORB.Types;

package body PolyORB.Test_Object_POA is

   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.Objects.Interface;
   use PolyORB.Requests;

   package L is new PolyORB.Log.Facility_Log ("corba.test_object");
   procedure Output (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

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
      pragma Debug (Output ("echoString is being executed with argument: "
                            & PolyORB.Types.To_Standard_String (S)));
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
      pragma Debug
        (Output ("Echo_Integer is being executed with argument" & I'Img));
      return I;
   end echoInteger;

   --------------------------------------------------------------
   -- "Middleware glue" that should be generated automatically --
   --------------------------------------------------------------

   function Execute_Servant
     (Obj : access My_Object;
      Msg : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class is
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Types;
   begin
      pragma Debug (Output ("Handle Message : enter"));

      if Msg in Execute_Request then
         declare
            Req : Request_Access
              := Execute_Request (Msg).Req;
            Args : PolyORB.Any.NVList.Ref;
         begin
            pragma Debug (Output ("The server is executing the request:"
                                    & PolyORB.Requests.Image (Req.all)));
            Create (Args);
            if Req.all.Operation = To_PolyORB_String ("echoString") then
               Add_Item (Args,
                         (Name => To_PolyORB_String ("S"),
                          Argument => Get_Empty_Any (TypeCode.TC_String),
                          Arg_Modes => PolyORB.Any.ARG_IN));
               Arguments (Req, Args);

               declare
                  use PolyORB.Any.NVList.Internals;
                  Args_Sequence : constant NV_Sequence_Access
                    := List_Of (Args);
                  echoString_Arg : PolyORB.Types.String :=
                    From_Any (NV_Sequence.Element_Of
                              (Args_Sequence.all, 1).Argument);
               begin
                  Req.Result.Argument := To_Any
                    (echoString (Obj.all, echoString_Arg));
                  pragma Debug (Output ("Result: " & Image (Req.Result)));
               end;
            elsif Req.all.Operation = "echoInteger" then
               Add_Item (Args, (Name => To_PolyORB_String ("I"),
                                Argument => Get_Empty_Any (TypeCode.TC_Long),
                                Arg_Modes => PolyORB.Any.ARG_IN));
               Arguments (Req, Args);
               declare
                  use PolyORB.Any.NVList.Internals;
                  Args_Sequence : constant NV_Sequence_Access
                    := List_Of (Args);
                  echoInteger_Arg : constant PolyORB.Types.Long :=
                    From_Any (NV_Sequence.Element_Of
                              (Args_Sequence.all, 1).Argument);
               begin
                  Req.Result.Argument := To_Any
                    (echoInteger (Obj.all, echoInteger_Arg));
                  pragma Debug (Output ("Result: " & Image (Req.Result)));
               end;
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
         pragma Debug (Output ("Handle_Message: Got exception "
                          & Ada.Exceptions.Exception_Information (E)));
         raise;
   end Execute_Servant;

end PolyORB.Test_Object_POA;

