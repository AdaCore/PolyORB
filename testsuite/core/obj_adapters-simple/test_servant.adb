with PolyORB.Any;
with PolyORB.Any.NVList;

with PolyORB.Components;

with PolyORB.Obj_Adapters;
with PolyORB.Obj_Adapters.Simple;
with PolyORB.Objects.Interface;
with PolyORB.Requests;

package body Test_Servant is

   use PolyORB.Any;
   use PolyORB.Objects.Interface;
   use PolyORB.Requests;
   use PolyORB.Types;

   function echoInteger
     (O : My_Servant;
      I : PolyORB.Types.Long)
     return PolyORB.Types.Long;
   --  Actual function implemented by the servant.

   function Get_Parameter_Profile
     (Method : String)
     return PolyORB.Any.NVList.Ref;

   function Get_Result_Profile
     (Method : String)
     return PolyORB.Any.Any;
   --  Accessors to servant interface.

   -----------------
   -- echoInteger --
   -----------------

   function echoInteger
     (O : My_Servant;
      I : PolyORB.Types.Long)
     return PolyORB.Types.Long
   is
      pragma Warnings (Off);
      pragma Unreferenced (O);
      pragma Warnings (On);
   begin
      return I;
   end echoInteger;

   ---------------------
   -- Execute_Servant --
   ---------------------

   function Execute_Servant
     (S   : access My_Servant;
      Msg : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class
   is
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
   begin
      if Msg in Execute_Request then
         declare
            Req : Request_Access
              := Execute_Request (Msg).Req;
            Args_Sequence :
              constant PolyORB.Any.NVList.Internals.NV_Sequence_Access
              := PolyORB.Any.NVList.Internals.List_Of (Req.all.Args);
         begin

            if Req.all.Operation = "echoInteger" then
               declare
                  echoInteger_Arg : constant PolyORB.Types.Long :=
                    From_Any (NV_Sequence.Element_Of
                              (Args_Sequence.all, 1).Argument);
               begin
                  Req.Result.Argument := To_Any
                    (echoInteger (S.all, echoInteger_Arg));
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
      when others =>
         raise;

   end Execute_Servant;

   ---------------------------
   -- Get_Parameter_Profile --
   ---------------------------

   function Get_Parameter_Profile
     (Method : String)
     return PolyORB.Any.NVList.Ref
   is
      Result : PolyORB.Any.NVList.Ref;
   begin
      PolyORB.Any.NVList.Create (Result);
      if Method = "echoInteger" then
         PolyORB.Any.NVList.Add_Item
           (Result, (Name => To_PolyORB_String ("I"),
                     Argument => Get_Empty_Any (TypeCode.TC_Long),
                     Arg_Modes => ARG_IN));
      else
         raise Program_Error;
      end if;
      return Result;
   end Get_Parameter_Profile;

   ------------------------
   -- Get_Result_Profile --
   ------------------------

   function Get_Result_Profile
     (Method : String)
     return PolyORB.Any.Any is
   begin
      if Method = "echoInteger" then
         return PolyORB.Any.Get_Empty_Any (TypeCode.TC_Long);
      else
         raise Program_Error;
      end if;
   end Get_Result_Profile;

   -------------
   -- If_Desc --
   -------------

   function If_Desc
     return PolyORB.Obj_Adapters.Simple.Interface_Description is
   begin
      return
        (PP_Desc => Get_Parameter_Profile'Access,
         RP_Desc => Get_Result_Profile'Access);
   end If_Desc;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : My_Servant)
                return Standard.Boolean is
   begin
      if Left.Nb = Right.Nb
        and then Left.Name = Right.Name
      then
         return True;
      end if;
      return False;
   end "=";

end Test_Servant;
