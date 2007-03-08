------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         T E S T _ S E R V A N T                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Obj_Adapters;
with PolyORB.Requests;
with PolyORB.Servants.Iface;

package body Test_Servant is

   use PolyORB.Any;
   use PolyORB.Requests;
   use PolyORB.Servants.Iface;
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
      use PolyORB.Any.NVList.Internals.NV_Lists;
   begin
      if Msg in Execute_Request then
         declare
            Req : Request_Access renames Execute_Request (Msg).Req;
         begin

            if Req.Operation.all = "echoInteger" then
               declare
                  echoInteger_Arg : constant PolyORB.Types.Long
                    := From_Any
                    (Value (First (List_Of (Req.Args).all)).Argument);
               begin
                  Req.Result.Argument := To_Any
                    (echoInteger (S.all, echoInteger_Arg));
               end;

            else
               raise Program_Error;
            end if;

            return Executed_Request'(Req => Req);
         end;
      else
         raise Program_Error;
      end if;

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
