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
     return Types.String is
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
     return Types.String is
   begin
--  pragma Debug (O ("echoString is being executed with argument: " & S));
      return S;
   end echoString;

   function echoInteger
     (O : My_Object;
      I : Types.Long)
     return Types.Long is
   begin
--  pragma Debug (O ("Echo_Integer is being executed with argument"
--                   & Integer'Image (I)));
      return I;
   end echoInteger;

   --------------------------------------------------------------
   -- "Middleware glue" that should be generated automatically --
   --------------------------------------------------------------

   function Handle_Message
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
            Args_Sequence : PolyORB.Any.NVList.Internals.NV_Sequence_Access
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
                  waitAndEchoString_Arg2 : Types.Long :=
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
                  echoInteger_Arg : Types.Long :=
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


   end Handle_Message;

   function Get_Parameter_Profile
     (Method : Requests.Operation_Id)
     return Any.NVList.Ref;

   function Get_Result_Profile
     (Method : Requests.Operation_Id)
     return Any.Any;

   function Get_Parameter_Profile
     (Method : Requests.Operation_Id)
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
     (Method : Requests.Operation_Id)
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

