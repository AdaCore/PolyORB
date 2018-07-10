------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . T E S T _ O B J E C T _ S O A               --
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

pragma Ada_2012;

with Ada.Exceptions;
with Ada.Text_IO;

with PolyORB.Any.NVList;
with PolyORB.Tasking.Threads;

package body PolyORB.Test_Object_SOA is

   use Ada.Text_IO;

   use PolyORB.Any;
   use PolyORB.Requests;

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
      Put_Line
        ("waitAndEchoString is being executed with arguments "
         & To_Standard_String (S)
         & Integer'Image (Integer (T)));

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
      Put_Line ("echoString is being executed with argument: "
                & To_Standard_String (S));
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
      Put_Line ("Echo_Integer is being executed with argument"
                & Types.Long'Image (I));
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

      It  : Iterator := First (List_Of (Req.Args).all);
   begin
      Put_Line ("Handle Message : enter");
      Put_Line ("The server is executing the request:"
                & PolyORB.Requests.Image (Req.all));

      if Req.Operation.all = "echoString" then
         declare
            echoString_Arg : constant Types.String
              := From_Any (Value (It).Argument);
         begin
            Put_Line ("Echoing in task "
                      & PolyORB.Tasking.Threads.Image
                      (PolyORB.Tasking.Threads.Current_Task));
            Req.Result.Argument := To_Any
              (echoString (Obj.all, echoString_Arg));
            Put_Line ("Result: " & Image (Req.Result));
         end;

      elsif Req.Operation.all = "waitAndEchoString" then
         declare
            Arg1, Arg2 : Element_Access;
         begin
            Arg1 := Value (It);
            Next (It);
            Arg2 := Value (It);

            Req.Result.Argument := To_Any
              (waitAndEchoString (Obj.all,
                                  From_Any (Arg1.Argument),
                                  From_Any (Arg2.Argument)));
            Put_Line ("Result: " & Image (Req.Result));
         end;

      elsif Req.Operation.all = "echoInteger" then
         declare
            echoInteger_Arg : constant Types.Long
               := From_Any (Value (It).Argument);
         begin
            Req.Result.Argument := To_Any
              (echoInteger (Obj.all, echoInteger_Arg));
            Put_Line ("Result: " & Image (Req.Result));
         end;

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
      use Any.NVList;

      Result : Any.NVList.Ref;
   begin
      Any.NVList.Create (Result);
      Put_Line ("Parameter profile for " & Method & " requested.");
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

   function Get_Result_Profile (Method : String) return Any.Any is
   begin
      Put_Line ("Result profile for " & Method & " requested.");
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

end PolyORB.Test_Object_SOA;
