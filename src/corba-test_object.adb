--  A simple test server object.

--  $Id$

with Ada.Exceptions;

with Droopi.Any;
with Droopi.Any.NVList;

with Droopi.Components;
with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

with Droopi.Objects.Interface;
with Droopi.Requests;
with Droopi.Types;

with CORBA;
with CORBA.POA;
with CORBA.POA_Types;

package body CORBA.Test_Object is

   use Droopi.Any;
   use Droopi.Log;
   use Droopi.Objects.Interface;
   use Droopi.Requests;
   use CORBA;

   package L is new Droopi.Log.Facility_Log ("corba.test_object");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   procedure Output (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   function "=" (Left, Right : My_Object)
                return Standard.Boolean
   is
      use CORBA.POA_Types;
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
      pragma Debug (Output ("Echo_Integer is being executed with argument"
                            & Long'Image (Long (I))));
      return I;
   end echoInteger;

   --------------------------------------------------------------
   -- "Middleware glue" that should be generated automatically --
   --------------------------------------------------------------

   function Handle_Message
     (Obj : access My_Object;
      Msg : Droopi.Components.Message'Class)
     return Droopi.Components.Message'Class is
      use Droopi.Any.NVList;
      use Droopi.Any.NVList.Internals;
      use Droopi.Types;
   begin
      pragma Debug (O ("Handle Message : enter"));
      if Msg in Execute_Request then
         declare
            Req : Request_Access
              := Execute_Request (Msg).Req;
            Args_Sequence : Internals.NV_Sequence_Access :=
              Internals.List_Of (Req.all.Args);
         begin
            pragma Debug (O ("The server is executing the request:"
                             & Droopi.Requests.Image (Req.all)));
            if Req.all.Operation = To_Droopi_String ("echoString") then
               declare
                  echoString_Arg : CORBA.String :=
                    From_Any (NV_Sequence.Element_Of
                              (Args_Sequence.all, 1).Argument);
               begin
                  Req.Result.Argument := To_Any
                    (echoString (Obj.all, echoString_Arg));
                  pragma Debug (O ("Result: " & Image (Req.Result)));
               end;
            elsif Req.all.Operation = "echoInteger" then
               declare
                  echoInteger_Arg : CORBA.Long :=
                    From_Any (NV_Sequence.Element_Of
                              (Args_Sequence.all, 1).Argument);
               begin
                  Req.Result.Argument := To_Any
                    (echoInteger (Obj.all, echoInteger_Arg));
                  pragma Debug (O ("Result: " & Image (Req.Result)));
               end;
            else
               raise Droopi.Components.Unhandled_Message;
            end if;
            return Executed_Request'(Req => Req);
         end;
      else
         raise Droopi.Components.Unhandled_Message;
      end if;

   exception
      when E : others =>
         pragma Debug (O ("Handle_Message: Got exception "
                          & Ada.Exceptions.Exception_Information (E)));
         raise;


   end Handle_Message;

   function Get_Parameter_Profile
     (Method : Droopi.Requests.Operation_Id)
     return Droopi.Any.NVList.Ref;

   function Get_Result_Profile
     (Method : Droopi.Requests.Operation_Id)
     return Droopi.Any.Any;

   function Get_Parameter_Profile
     (Method : Droopi.Requests.Operation_Id)
     return Droopi.Any.NVList.Ref
   is
      use Droopi.Any;
      use Droopi.Any.NVList;
      use Droopi.Types;

      Result : Droopi.Any.NVList.Ref;
   begin
      Droopi.Any.NVList.Create (Result);
      pragma Debug (O ("Parameter profile for " & Method & " requested."));
      if Method = "echoString" then
         Add_Item (Result,
                   (Name => To_Droopi_String ("S"),
                    Argument => Get_Empty_Any (TypeCode.TC_String),
                    Arg_Modes => ARG_IN));
      elsif Method = "echoInteger" then
         Add_Item (Result, (Name => To_Droopi_String ("I"),
                            Argument => Get_Empty_Any (TypeCode.TC_Long),
                            Arg_Modes => ARG_IN));
      else
         raise Program_Error;
      end if;
      return Result;
   end Get_Parameter_Profile;

   function Get_Result_Profile
     (Method : Droopi.Requests.Operation_Id)
     return Droopi.Any.Any
   is
      use Droopi.Any;

   begin
      pragma Debug (O ("Result profile for " & Method & " requested."));
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

