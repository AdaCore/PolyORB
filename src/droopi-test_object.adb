--  A simple test server object.

--  $Id$

with CORBA;
--  For Anys and TypeCodes.
with CORBA.NVList;

with Droopi.Components;
with Droopi.Log;
with Droopi.Obj_Adapters;
with Droopi.Obj_Adapters.Simple;
with Droopi.Objects.Interface;
with Droopi.Requests;

package body Droopi.Test_Object is

   use Droopi.Log;
   use Droopi.Objects.Interface;
   use Droopi.Requests;

   package L is new Droopi.Log.Facility_Log ("droopi.test_object");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;


   --------------------------------------
   -- Application part of the servant. --
   --------------------------------------

   function Echo_String
     (O : My_Object;
      S : String)
     return String is
   begin
      return S;
   end Echo_String;

   function Echo_Integer
     (O : My_Object;
      I : Integer)
     return Integer is
   begin
      return I;
   end Echo_Integer;

   --------------------------------------------------------------
   -- "Middleware glue" that should be generated automatically --
   --------------------------------------------------------------

   function Handle_Message
     (Obj : access My_Object;
      Msg : Components.Message'Class)
     return Components.Message'Class is
   begin
      if Msg in Execute_Request then
         declare
            Req : Request_Access renames Execute_Request (Msg).Req;
         begin
            pragma Debug (O ("The server is executing the request:"
                             & Droopi.Requests.Image (Req.all)));
            return Executed_Request'(Req => Req);
         end;
      else
         raise Droopi.Components.Unhandled_Message;
      end if;
   end Handle_Message;

   function Get_Parameter_Profile
     (Method : Requests.Operation_Id)
     return CORBA.NVList.Ref;

   function Get_Result_Profile
     (Method : Requests.Operation_Id)
     return CORBA.Any;

   function Get_Parameter_Profile
     (Method : Requests.Operation_Id)
     return CORBA.NVList.Ref
   is
      use CORBA;
      use CORBA.NVList;

      Result : CORBA.NVList.Ref;
   begin
      CORBA.NVList.Create (Result);
      pragma Debug (O ("Parameter profile for " & Method & " requested."));
      if Method = "Echo_String" then
         Add_Item (Result,
                   (Name => To_CORBA_String ("S"),
                    Argument => Get_Empty_Any (TypeCode.TC_String),
                    Arg_Modes => ARG_IN));
      elsif Method = "Echo_Integer" then
         Add_Item (Result, (Name => To_CORBA_String ("I"),
                            Argument => Get_Empty_Any (TypeCode.TC_Long),
                            Arg_Modes => ARG_IN));
      else
         raise Program_Error;
      end if;
      return Result;
   end Get_Parameter_Profile;

   function Get_Result_Profile
     (Method : Requests.Operation_Id)
     return CORBA.Any
   is
      use CORBA;

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

