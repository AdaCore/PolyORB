------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . T E S T _ O B J E C T                   --
--                                                                          --
--                                 B o d y                                  --
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

--  A simple test server object.

--  $Id$

with Ada.Exceptions;

with PolyORB.Any;
with PolyORB.Any.NVList;

with PolyORB.Components;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

with PolyORB.Obj_Adapters;
with PolyORB.Obj_Adapters.Simple;
with PolyORB.Objects.Interface;
with PolyORB.Requests;
with PolyORB.Tasking.Threads;
with PolyORB.Types;

package body PolyORB.Test_Object is

   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.Objects.Interface;
   use PolyORB.Requests;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.test_object");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;


   --------------------------------------
   -- Application part of the servant. --
   --------------------------------------

   function waitAndEchoString
     (O : My_Object;
      S : Types.String;
      T : Types.Long)
     return Types.String
   is
      pragma Warnings (Off);
      pragma Unreferenced (O);
      pragma Warnings (On);
   begin
      pragma Debug
        (L.Output ("waitAndEchoString is being executed with arguments "
                   & To_Standard_String (S)
                   & Integer'Image (Integer (T))));

      --  delay (Duration (T));
      --  XXX Relative delay forbidden under pragma Ravenscar.
      return S;
   end waitAndEchoString;

   function echoString
     (O : My_Object;
      S : Types.String)
     return Types.String
   is
      pragma Warnings (Off);
      pragma Unreferenced (O);
      pragma Warnings (On);
   begin
--  pragma Debug (O ("echoString is being executed with argument: " & S));
      return S;
   end echoString;

   function echoInteger
     (O : My_Object;
      I : Types.Long)
     return Types.Long
   is
      pragma Warnings (Off);
      pragma Unreferenced (O);
      pragma Warnings (On);
   begin
--  pragma Debug (O ("Echo_Integer is being executed with argument"
--                   & Integer'Image (I)));
      return I;
   end echoInteger;

   --------------------------------------------------------------
   -- "Middleware glue" that should be generated automatically --
   --------------------------------------------------------------

   function Execute_Servant
     (Obj : access My_Object;
      Msg : Components.Message'Class)
     return Components.Message'Class is
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
   begin
      pragma Debug (O ("Handle Message : enter"));
      if Msg in Execute_Request then
         declare
            Req : Request_Access
              := Execute_Request (Msg).Req;
            Args_Sequence :
              constant PolyORB.Any.NVList.Internals.NV_Sequence_Access
              := PolyORB.Any.NVList.Internals.List_Of (Req.all.Args);
         begin
            pragma Debug (O ("The server is executing the request:"
                             & PolyORB.Requests.Image (Req.all)));
            if Req.all.Operation = To_PolyORB_String ("echoString") then
               declare
                  echoString_Arg : Types.String :=
                    From_Any (NV_Sequence.Element_Of
                              (Args_Sequence.all, 1).Argument);
               begin
                  pragma Debug (O ("Echoing in task "
                    & PolyORB.Tasking.Threads.Image
                      (PolyORB.Tasking.Threads.Current_Task)));
                  Req.Result.Argument := To_Any
                    (echoString (Obj.all, echoString_Arg));
                  pragma Debug (O ("Result: " & Image (Req.Result)));
               end;
            elsif
              Req.all.Operation = To_PolyORB_String ("waitAndEchoString")
            then
               declare
                  waitAndEchoString_Arg1 : Types.String :=
                    From_Any (NV_Sequence.Element_Of
                              (Args_Sequence.all, 1).Argument);
                  waitAndEchoString_Arg2 : constant Types.Long :=
                    From_Any (NV_Sequence.Element_Of
                              (Args_Sequence.all, 2).Argument);
               begin
                  Req.Result.Argument := To_Any
                    (waitAndEchoString (Obj.all,
                                        waitAndEchoString_Arg1,
                                        waitAndEchoString_Arg2));
                  pragma Debug (O ("Result: " & Image (Req.Result)));
               end;
            elsif Req.all.Operation = "echoInteger" then
               declare
                  echoInteger_Arg : constant Types.Long :=
                    From_Any (NV_Sequence.Element_Of
                              (Args_Sequence.all, 1).Argument);
               begin
                  Req.Result.Argument := To_Any
                    (echoInteger (Obj.all, echoInteger_Arg));
                  pragma Debug (O ("Result: " & Image (Req.Result)));
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
         pragma Debug (O ("Handle_Message: Got exception "
                          & Ada.Exceptions.Exception_Information (E)));
         raise;


   end Execute_Servant;

   function Get_Parameter_Profile
     (Method : String)
     return Any.NVList.Ref;

   function Get_Result_Profile
     (Method : String)
     return Any.Any;

   function Get_Parameter_Profile
     (Method : String)
     return Any.NVList.Ref
   is
      use Any;
      use Any.NVList;
      use Types;

      Result : Any.NVList.Ref;
   begin
      Any.NVList.Create (Result);
      pragma Debug (O ("Parameter profile for " & Method & " requested."));
      if Method = "echoString" then
         Add_Item (Result,
                   (Name => To_PolyORB_String ("S"),
                    Argument => Get_Empty_Any (TypeCode.TC_String),
                    Arg_Modes => ARG_IN));
      elsif Method = "echoInteger" then
         Add_Item (Result, (Name => To_PolyORB_String ("I"),
                            Argument => Get_Empty_Any (TypeCode.TC_Long),
                            Arg_Modes => ARG_IN));
      elsif Method = "waitAndEchoString" then
         Add_Item (Result, (Name => To_PolyORB_String ("S"),
                            Argument => Get_Empty_Any (TypeCode.TC_String),
                            Arg_Modes => ARG_IN));
         Add_Item (Result, (Name => To_PolyORB_String ("I"),
                            Argument => Get_Empty_Any (TypeCode.TC_Long),
                            Arg_Modes => ARG_IN));
      else
         raise Program_Error;
      end if;
      return Result;
   end Get_Parameter_Profile;

   function Get_Result_Profile
     (Method : String)
     return Any.Any
   is
      use Any;

   begin
      pragma Debug (O ("Result profile for " & Method & " requested."));
      if Method = "echoString" then
         return Get_Empty_Any (TypeCode.TC_String);
      elsif Method = "echoInteger" then
         return Get_Empty_Any (TypeCode.TC_Long);
      elsif Method = "waitAndEchoString" then
         return Get_Empty_Any (TypeCode.TC_String);
      else
         raise Program_Error;
      end if;
   end Get_Result_Profile;

   function If_Desc
     return Obj_Adapters.Simple.Interface_Description is
   begin
      return
        (PP_Desc => Get_Parameter_Profile'Access,
         RP_Desc => Get_Result_Profile'Access);
   end If_Desc;

end PolyORB.Test_Object;

