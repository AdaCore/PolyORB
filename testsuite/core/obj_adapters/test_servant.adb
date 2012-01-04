------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         T E S T _ S E R V A N T                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Obj_Adapters;

package body Test_Servant is

   use PolyORB.Any;
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
     (S   : not null access My_Servant;
      Req : PolyORB.Requests.Request_Access) return Boolean
   is
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;
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

      return True;
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
