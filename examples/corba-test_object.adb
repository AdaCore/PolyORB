------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    C O R B A . T E S T _ O B J E C T                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

--  A simple test server object.

--  $Id$

with Ada.Exceptions;

with PolyORB.Any;
with PolyORB.Any.NVList;

with PolyORB.Components;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

with PolyORB.Objects.Interface;
with PolyORB.Requests;
with PolyORB.Types;

with CORBA;
with PolyORB.POA_Types;

package body CORBA.Test_Object is

   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.Objects.Interface;
   use PolyORB.Requests;
   use CORBA;

   package L is new PolyORB.Log.Facility_Log ("corba.test_object");
   procedure Output (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   function "=" (Left, Right : My_Object)
                return Standard.Boolean
   is
      use PolyORB.POA_Types;
   begin
      return (Left.If_Desc = Right.If_Desc);
   end "=";

   --------------------------------------
   -- Application part of the servant. --
   --------------------------------------

   function echoString
     (O : My_Object;
      S : CORBA.String)
     return CORBA.String
   is
   begin
      pragma Debug (Output ("echoString is being executed with argument: "
                            & CORBA.To_Standard_String (S)));
      return S;
   end echoString;

   function echoInteger
     (O : My_Object;
      I : CORBA.Long)
     return CORBA.Long
   is
   begin
      pragma Debug
        (Output ("Echo_Integer is being executed with argument" & I'Img));
      return I;
   end echoInteger;

   --------------------------------------------------------------
   -- "Middleware glue" that should be generated automatically --
   --------------------------------------------------------------

   function Handle_Message
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
            Args_Sequence : PolyORB.Any.NVList.Internals.NV_Sequence_Access
              := PolyORB.Any.NVList.Internals.List_Of (Req.all.Args);
         begin
            pragma Debug (Output ("The server is executing the request:"
                             & PolyORB.Requests.Image (Req.all)));
            if Req.all.Operation = To_PolyORB_String ("echoString") then
               declare
                  echoString_Arg : CORBA.String :=
                    From_Any (NV_Sequence.Element_Of
                              (Args_Sequence.all, 1).Argument);
               begin
                  Req.Result.Argument := To_Any
                    (echoString (Obj.all, echoString_Arg));
                  pragma Debug (Output ("Result: " & Image (Req.Result)));
               end;
            elsif Req.all.Operation = "echoInteger" then
               declare
                  echoInteger_Arg : CORBA.Long :=
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


   end Handle_Message;

   function Get_Parameter_Profile
     (Method : PolyORB.Requests.Operation_Id)
     return PolyORB.Any.NVList.Ref;

   function Get_Result_Profile
     (Method : PolyORB.Requests.Operation_Id)
     return PolyORB.Any.Any;

   function Get_Parameter_Profile
     (Method : PolyORB.Requests.Operation_Id)
     return PolyORB.Any.NVList.Ref
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList;
      use PolyORB.Types;

      Result : PolyORB.Any.NVList.Ref;
   begin
      PolyORB.Any.NVList.Create (Result);
      pragma Debug
        (Output ("Parameter profile for " & Method & " requested."));
      if Method = "echoString" then
         Add_Item (Result,
                   (Name => To_PolyORB_String ("S"),
                    Argument => Get_Empty_Any (TypeCode.TC_String),
                    Arg_Modes => ARG_IN));
      elsif Method = "echoInteger" then
         Add_Item (Result, (Name => To_PolyORB_String ("I"),
                            Argument => Get_Empty_Any (TypeCode.TC_Long),
                            Arg_Modes => ARG_IN));
      else
         raise Program_Error;
      end if;
      return Result;
   end Get_Parameter_Profile;

   function Get_Result_Profile
     (Method : PolyORB.Requests.Operation_Id)
     return PolyORB.Any.Any
   is
      use PolyORB.Any;

   begin
      pragma Debug (Output ("Result profile for " & Method & " requested."));
      if Method = "echoString" then
         return Get_Empty_Any (TypeCode.TC_String);
      elsif Method = "echoInteger" then
         return Get_Empty_Any (TypeCode.TC_Long);
      else
         raise Program_Error;
      end if;
   end Get_Result_Profile;

   procedure Create (O : in out My_Object)
   is
   begin
      O.If_Desc.PP_Desc := Get_Parameter_Profile'Access;
      O.If_Desc.RP_Desc := Get_Result_Profile'Access;
   end Create;

--    function If_Desc
--      return Obj_Adapters.Simple.Interface_Description is
--    begin
--       return
--         (PP_Desc => Get_Parameter_Profile'Access,
--          RP_Desc => Get_Result_Profile'Access);
--    end If_Desc;

end CORBA.Test_Object;

