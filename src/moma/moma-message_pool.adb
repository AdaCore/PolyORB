with Ada.Text_IO; use Ada.Text_IO;

with PolyORB.Any; use PolyORB.Any;
with PolyORB.Any.NVList; use PolyORB.Any.NVList;
with PolyORB.Log;
with PolyORB.Types; use PolyORB.Types;
with PolyORB.Requests; use PolyORB.Requests;

with MOMA.Message_Pool.Impl;

package body MOMA.Message_Pool is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("moma.message_pool");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Self : access Object;
      Req  : in     PolyORB.Requests.Request_Access)
   is
      Args : PolyORB.Any.NVList.Ref;
   begin
      Put_Line ("The server is executing the request:"
                & PolyORB.Requests.Image (Req.all));

      Create (Args);

      if Req.all.Operation = To_PolyORB_String ("Publish") then

         --  Publish

         Add_Item (Args,
                   (Name => To_PolyORB_String ("Message"),
                    Argument => Get_Empty_Any (TypeCode.TC_String),
                    Arg_Modes => PolyORB.Any.ARG_IN));
         Arguments (Req, Args);

         declare
            use PolyORB.Any.NVList.Internals;
            Args_Sequence : constant NV_Sequence_Access
              := List_Of (Args);
            Publish_Arg : PolyORB.Types.String :=
              From_Any (NV_Sequence.Element_Of
                        (Args_Sequence.all, 1).Argument);
         begin
            Req.Result.Argument := To_Any
              (MOMA.Message_Pool.Impl.Publish (Publish_Arg));
            pragma Debug (O ("Result: " & Image (Req.Result)));
         end;

      elsif Req.all.Operation = To_PolyORB_String ("Get") then

         --  Get

         Add_Item (Args,
                   (Name => To_PolyORB_String ("Message_Id"),
                    Argument => Get_Empty_Any (TypeCode.TC_String),
                    Arg_Modes => PolyORB.Any.ARG_IN));
         Arguments (Req, Args);

         declare
            use PolyORB.Any.NVList.Internals;
            Args_Sequence : constant NV_Sequence_Access
              := List_Of (Args);
            Get_Arg : PolyORB.Types.String :=
              From_Any (NV_Sequence.Element_Of
                        (Args_Sequence.all, 1).Argument);
         begin
            Req.Result.Argument := To_Any
              (MOMA.Message_Pool.Impl.Get (Get_Arg));
            pragma Debug (O ("Result: " & Image (Req.Result)));
         end;

      end if;
   end Invoke;

   ---------------------------
   -- Get_Parameter_Profile --
   ---------------------------

   function Get_Parameter_Profile
     (Method : String)
     return PolyORB.Any.NVList.Ref;

   function Get_Parameter_Profile
     (Method : String)
     return PolyORB.Any.NVList.Ref
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList;
      use PolyORB.Types;

      Result : PolyORB.Any.NVList.Ref;
   begin
      PolyORB.Any.NVList.Create (Result);
      pragma Debug (O ("Parameter profile for " & Method & " requested."));
      if Method = "Publish" then
         Add_Item (Result,
                   (Name => To_PolyORB_String ("Message"),
                    Argument => Get_Empty_Any (TypeCode.TC_String),
                    Arg_Modes => ARG_IN));
      elsif Method = "Get" then
         Add_Item (Result,
                   (Name => To_PolyORB_String ("Message_Id"),
                    Argument => Get_Empty_Any (TypeCode.TC_String),
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
     return PolyORB.Any.Any;

   function Get_Result_Profile
     (Method : String)
     return PolyORB.Any.Any
   is
      use PolyORB.Any;

   begin
      pragma Debug (O ("Result profile for " & Method & " requested."));
      if Method = "Publish" then
         return Get_Empty_Any (TypeCode.TC_String);
      elsif Method = "Get" then
         return Get_Empty_Any (TypeCode.TC_String);
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

end MOMA.Message_Pool;
