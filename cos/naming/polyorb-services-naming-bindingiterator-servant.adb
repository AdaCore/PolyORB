------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.SERVICES.NAMING.BINDINGITERATOR.SERVANT              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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

--  $Id$

with Ada.Unchecked_Deallocation;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Exceptions;
with PolyORB.Log;
with PolyORB.Requests;
with PolyORB.Types;

with PolyORB.Services.Naming.Helper;
--  with PolyORB.Services.Naming.BindingIterator.Helper;

with GNAT.Task_Lock; use GNAT.Task_Lock;
--  XXX change for a better abstraction, search PolyORB.Tasking ...

package body PolyORB.Services.Naming.BindingIterator.Servant is

   use PolyORB.Any;
   use PolyORB.Any.NVList;
   use PolyORB.Log;
   use PolyORB.Requests;
   use PolyORB.Types;

   use PolyORB.Services.Naming.Helper;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.services.naming.bindingiterator.servant");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Next_One
     (Self    : access Object;
      B       : out    PolyORB.Services.Naming.Binding;
      Returns : out    PolyORB.Types.Boolean);

   procedure Next_N
     (Self     : access Object;
      How_Many : in     PolyORB.Types.Unsigned_Long;
      BL       : out    PolyORB.Services.Naming.BindingList;
      Returns  : out    PolyORB.Types.Boolean);

   procedure Destroy
     (Self : access Object);
   --  Actual functions implemented by the servant.

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Self    : access Object;
      Request : in     PolyORB.Requests.Request_Access)
   is
      Operation : constant Standard.String
         := PolyORB.Types.To_Standard_String (Request.all.Operation);

      Arg_List    : PolyORB.Any.NVList.Ref;

   begin
      pragma Debug (O ("The server is executing the request:"
                    & PolyORB.Requests.Image (Request.all)));

      Create (Arg_List);

      if Operation = "next_one" then

         declare
            B                : Binding;
            Argument_B       : PolyORB.Any.Any
              := Get_Empty_Any (TC_Binding);
            Returns          : PolyORB.Types.Boolean;
         begin
            --  Call implementation
            Next_One (Self, B, Returns);

            --  Set out arguments.
            PolyORB.Any.Copy_Any_Value (Argument_B, To_Any (B));

            Request.Result.Argument := To_Any (Returns);
            return;
         end;

      elsif Operation = "next_n" then

         declare
            How_Many          : PolyORB.Types.Unsigned_Long;
            Argument_How_Many : PolyORB.Any.Any := Get_Empty_Any
              (TypeCode.TC_Unsigned_Long);

            Bl                : BindingList;
            Argument_Bl       : PolyORB.Any.Any
              := Get_Empty_Any (TC_BindingList);

            Returns           : PolyORB.Types.Boolean;
         begin
            Add_Item (Arg_List,
                      (Name      => To_PolyORB_String ("how_many"),
                       Argument  => Argument_How_Many,
                       Arg_Modes => PolyORB.Any.ARG_IN));
            Add_Item (Arg_List,
                      (Name      => To_PolyORB_String ("bl"),
                       Argument  => Argument_Bl,
                       Arg_Modes => PolyORB.Any.ARG_OUT));

            Arguments (Request, Arg_List);

            --  Convert arguments from their Any
            How_Many := PolyORB.Any.From_Any (Argument_How_Many);
            --  XXX why does GNAT requires this ???

            --  Call implementation
            Next_N (Self, How_Many, Bl, Returns);

            --  Set out arguments.
            PolyORB.Any.Copy_Any_Value (Argument_Bl, To_Any (Bl));

            --  Set Result
            Request.Result.Argument := To_Any (Returns);

            return;
         end;

      elsif Operation = "destroy" then
         --  Call implementation
         Destroy (Self);
         return;

      else
         PolyORB.Exceptions.Raise_Bad_Operation;
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
                    Argument => Get_Empty_Any (TC_Void),
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
         return Get_Empty_Any (TypeCode.TC_Void);

      elsif Method = "Get" then
         --  return Get_Empty_Any (TypeCode.TC_Any);
         return Get_Empty_Any (TC_Void);

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

   ------------------------------
   -- Servant actual functions --
   ------------------------------

   Null_Binding : constant Binding := (To_Sequence (0), Nobject);

   procedure Free is
      new Ada.Unchecked_Deallocation
     (Bindings.Element_Array, Binding_Element_Array_Ptr);

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr is
      Obj : Object_Ptr;

   begin
      Obj := new Object;
      Obj.Self := Obj;
      return Obj;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self : access Object) is
   begin
      Lock;
      if Self.Table /= null then
         Free (Self.Table);
      end if;
      Unlock;
   end Destroy;

   --------------
   -- Next_One --
   --------------

   procedure Next_One
     (Self    : access Object;
      B       : out    PolyORB.Services.Naming.Binding;
      Returns : out    PolyORB.Types.Boolean) is
   begin
      Lock;
      if Self.Index <= Self.Table'Last then
         B := Self.Table (Self.Index);
         Self.Index := Self.Index + 1;
         Returns := True;

      else
         B := Null_Binding;
         Returns := False;
      end if;
      Unlock;
   end Next_One;

   ------------
   -- Next_N --
   ------------

   procedure Next_N
     (Self     : access Object;
      How_Many : in     PolyORB.Types.Unsigned_Long;
      BL       : out    PolyORB.Services.Naming.BindingList;
      Returns  : out    PolyORB.Types.Boolean)
   is
      First : Natural renames Self.Index;
      Last  : Natural;

   begin
      Lock;
      Last := Self.Index + Natural (How_Many) - 1;
      if Last <= Self.Table'Last then
         BL := BindingList (Bindings.To_Sequence (Self.Table (First .. Last)));
         Self.Index := Last + 1;
         Returns := True;

      else
         Returns := False;
      end if;
      Unlock;
   end Next_N;

end PolyORB.Services.Naming.BindingIterator.Servant;
