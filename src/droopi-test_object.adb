--  A simple test server object.

--  $Id$

with Droopi.Any;
with Droopi.Any.NVList;

with Droopi.Components;
with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

with Droopi.Obj_Adapters;
with Droopi.Obj_Adapters.Simple;
with Droopi.Objects.Interface;
with Droopi.Requests;
with Droopi.Types;

package body Droopi.Test_Object is

   use Droopi.Any;
   use Droopi.Log;
   use Droopi.Objects.Interface;
   use Droopi.Requests;
   use Droopi.Types;

   package L is new Droopi.Log.Facility_Log ("droopi.test_object");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;


   --------------------------------------
   -- Application part of the servant. --
   --------------------------------------

   function Echo_String
     (O : My_Object;
      S : Types.String)
     return Types.String is
   begin
--  pragma Debug (O ("Echo_String is being executed with argument: " & S));
      return S;
   end Echo_String;

   function Echo_Integer
     (O : My_Object;
      I : Types.Long)
     return Types.Long is
   begin
--  pragma Debug (O ("Echo_Integer is being executed with argument"
--                   & Integer'Image (I)));
      return I;
   end Echo_Integer;

   --------------------------------------------------------------
   -- "Middleware glue" that should be generated automatically --
   --------------------------------------------------------------

   function Handle_Message
     (Obj : access My_Object;
      Msg : Components.Message'Class)
     return Components.Message'Class is
      use Droopi.Any.NVList;
      use Droopi.Any.NVList.Internals;
   begin
      if Msg in Execute_Request then
         declare
            Req : Request_Access renames Execute_Request (Msg).Req;
            Args_Sequence : Internals.NV_Sequence_Access :=
              Internals.List_Of (Req.all.Args);
         begin
            pragma Debug (O ("The server is executing the request:"
                             & Droopi.Requests.Image (Req.all)));
            if Req.all.Operation = To_Droopi_String ("Echo_String") then
               declare
                  Echo_String_Arg : Types.String :=
                    From_Any (NV_Sequence.Element_Of
                              (Args_Sequence.all, 1).Argument);
                  Result : Types.String_Ptr;
               begin
                  Result :=
                    new Types.String'(Echo_String
                                (Obj.all, Echo_String_Arg));
               end;
            elsif Req.all.Operation = "Echo_Integer" then
--                declare
--                   Echo_Integer_Arg : Types.Short :=
--                     From_Any (NV_Sequence.Element_Of
--                               (Args_Sequence.all, 1).Argument);
--                   Result : Types.Long_Ptr;
--                begin
--                   Result := new Long'(Echo_Integer
--                                       (Obj.all,
--                                        Echo_Integer_Arg));
--                end;
               raise Not_Implemented;
            else
               raise Droopi.Components.Unhandled_Message;
            end if;
            return Executed_Request'(Req => Req);
         end;
      else
         raise Droopi.Components.Unhandled_Message;
      end if;
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
      if Method = "Echo_String" then
         Add_Item (Result,
                   (Name => To_Droopi_String ("S"),
                    Argument => Get_Empty_Any (TypeCode.TC_String),
                    Arg_Modes => ARG_IN));
      elsif Method = "Echo_Integer" then
         Add_Item (Result, (Name => To_Droopi_String ("I"),
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
      if Method = "Echo_String" then
         return Get_Empty_Any (TypeCode.TC_String);
      elsif Method = "Echo_Integer" then
         return Get_Empty_Any (TypeCode.TC_Long);
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

end Droopi.Test_Object;

