------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . E R R O R S . H E L P E R                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2008, Free Software Foundation, Inc.          --
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

with PolyORB.Any.ObjRef;
with PolyORB.Log;
with PolyORB.References;

package body PolyORB.Errors.Helper is

   use PolyORB.Any;
   use PolyORB.Any.ObjRef;
   use PolyORB.Log;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.errors.helper");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   ------------------
   -- Error_To_Any --
   ------------------

   function Error_To_Any (Error : Error_Container) return PolyORB.Any.Any is
      Result         : PolyORB.Any.Any;
      Error_Name     : constant String := Error_Id'Image (Error.Kind);
      Exception_Name : constant String :=
                         Error_Name (Error_Name'First .. Error_Name'Last - 2);
      --  Strip "_E" suffix

   begin
      pragma Debug (C, O ("Error_To_Any: enter."));
      pragma Debug (C, O ("Error is: " & Error_Name));
      pragma Debug (C, O ("Exception name is: " & Exception_Name));

      if Error.Kind in ORB_System_Error then
         Result := To_Any (Exception_Name,
                           System_Exception_Members (Error.Member.all));

      elsif Error.Kind = ForwardRequest_E then
         Result := To_Any (ForwardRequest_Members (Error.Member.all));

      elsif Error.Kind = ForwardRequestPerm_E then
         Result := To_Any (ForwardRequestPerm_Members (Error.Member.all));

      elsif Error.Kind = NeedsAddressingMode_E then
         Result := To_Any (NeedsAddressingMode_Members (Error.Member.all));

      elsif Error.Kind in POA_Error then
         Result := To_Any (Exception_Name,
                           Null_Members (Error.Member.all));
      else
         raise Program_Error;
         --  Never happens
      end if;

      pragma Debug (C, O ("Error_To_Any: leave."));
      return Result;
   end Error_To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : PolyORB.Any.Any) return Completion_Status is
   begin
      return Completion_Status'Val
        (Unsigned_Long'(PolyORB.Any.Get_Aggregate_Element (Item, 0)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : Completion_Status) return Any.Any is
      Result : Any.Any := Get_Empty_Any_Aggregate (TC_Completion_Status);

   begin
      Add_Aggregate_Element
        (Result, To_Any (Unsigned_Long (Completion_Status'Pos (Item))));

      return Result;
   end To_Any;

   --------------------------
   -- TC_Completion_Status --
   --------------------------

   TC_Completion_Status_Cache : TypeCode.Local_Ref;

   function TC_Completion_Status return PolyORB.Any.TypeCode.Local_Ref is
      TC : TypeCode.Local_Ref renames TC_Completion_Status_Cache;
   begin
      if not TypeCode.Is_Nil (TC) then
         return TC;
      end if;

      TC := TypeCode.TC_Enum;
      TypeCode.Add_Parameter (TC, To_Any ("completion_status"));
      TypeCode.Add_Parameter
        (TC, To_Any ("IDL:omg.org/CORBA/completion_status:1.0"));

      for C in Completion_Status'Range loop
         TypeCode.Add_Parameter
           (TC, To_Any (Completion_Status'Image (C)));
      end loop;

      return TC;
   end TC_Completion_Status;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Name   : Standard.String;
      Member : Null_Members)
     return PolyORB.Any.Any
   is
      pragma Unreferenced (Member);

      TC    : constant TypeCode.Local_Ref := TypeCode.TC_Except;
      Shift : Natural         := 0;
   begin
      --  Name

      TypeCode.Add_Parameter (TC, To_Any (Name));

      if Name (Name'First .. Name'First + PolyORB_Exc_Root'Length - 1)
        = PolyORB_Exc_Root
      then
         Shift := PolyORB_Exc_Root'Length + 1;
      end if;

      --  RepositoryId : 'INTERNAL:<Name>:1.0'

      declare
         Repository_Id : constant String :=
           PolyORB_Exc_Prefix
           & Name (Name'First + Shift .. Name'Last)
           & PolyORB_Exc_Version;
      begin
         TypeCode.Add_Parameter (TC, To_Any (Repository_Id));
      end;

      return Get_Empty_Any_Aggregate (TC);
   end To_Any;

   ---------------------
   -- TC_Comm_Failure --
   ---------------------

   TC_Comm_Failure_Cache : TypeCode.Local_Ref;

   function TC_Comm_Failure return PolyORB.Any.TypeCode.Local_Ref is
   begin
      if not TypeCode.Is_Nil (TC_Comm_Failure_Cache) then
         return TC_Comm_Failure_Cache;
      end if;

      TC_Comm_Failure_Cache := System_Exception_TypeCode ("COMM_FAILURE");
      return TC_Comm_Failure_Cache;
   end TC_Comm_Failure;

   ------------------
   -- TC_Transient --
   ------------------

   TC_Transient_Cache : TypeCode.Local_Ref;

   function TC_Transient return PolyORB.Any.TypeCode.Local_Ref is
   begin
      if not TypeCode.Is_Nil (TC_Transient_Cache) then
         return TC_Transient_Cache;
      end if;

      TC_Transient_Cache := System_Exception_TypeCode ("TRANSIENT");
      return TC_Transient_Cache;
   end TC_Transient;

   --------------------
   -- TC_No_Response --
   --------------------

   TC_No_Response_Cache : TypeCode.Local_Ref;

   function TC_No_Response return PolyORB.Any.TypeCode.Local_Ref is
   begin
      if not TypeCode.Is_Nil (TC_No_Response_Cache) then
         return TC_No_Response_Cache;
      end if;

      TC_No_Response_Cache := System_Exception_TypeCode ("NO_RESPONSE");
      return TC_No_Response_Cache;
   end TC_No_Response;

   --------------------
   -- TC_Obj_Adapter --
   --------------------

   TC_Obj_Adapter_Cache : TypeCode.Local_Ref;

   function TC_Obj_Adapter return PolyORB.Any.TypeCode.Local_Ref is
   begin
      if not TypeCode.Is_Nil (TC_Obj_Adapter_Cache) then
         return TC_Obj_Adapter_Cache;
      end if;

      TC_Obj_Adapter_Cache := System_Exception_TypeCode ("OBJ_ADAPTER");
      return TC_Obj_Adapter_Cache;
   end TC_Obj_Adapter;

   -----------------------
   -- TC_ForwardRequest --
   -----------------------

   TC_ForwardRequest_Cache : TypeCode.Local_Ref;

   function TC_ForwardRequest
     return PolyORB.Any.TypeCode.Local_Ref
   is
      TC : TypeCode.Local_Ref renames TC_ForwardRequest_Cache;

      Name          : constant String := "ForwardRequest";
      Repository_Id : constant String :=
        PolyORB_Exc_Prefix & Name & PolyORB_Exc_Version;
   begin
      if not TypeCode.Is_Nil (TC) then
         return TC;
      end if;

      TC := TypeCode.TC_Except;

      TypeCode.Add_Parameter (TC, To_Any (Name));
      TypeCode.Add_Parameter (TC, To_Any (Repository_Id));

      TypeCode.Add_Parameter
        (TC, To_Any (TypeCode.TC_Object));
      TypeCode.Add_Parameter
        (TC, To_Any ("forward_reference"));

      return TC;
   end TC_ForwardRequest;

   ---------------------------
   -- TC_ForwardRequestPerm --
   ---------------------------

   TC_ForwardRequestPerm_Cache : TypeCode.Local_Ref;

   function TC_ForwardRequestPerm
     return PolyORB.Any.TypeCode.Local_Ref
   is
      TC : TypeCode.Local_Ref renames TC_ForwardRequestPerm_Cache;

      Name          : constant String := "ForwardRequestPerm";
      Repository_Id : constant String :=
        PolyORB_Exc_Prefix & Name & PolyORB_Exc_Version;

   begin
      if not TypeCode.Is_Nil (TC) then
         return TC;
      end if;

      TC := TypeCode.TC_Except;

      TypeCode.Add_Parameter (TC, To_Any (Name));
      TypeCode.Add_Parameter (TC, To_Any (Repository_Id));

      TypeCode.Add_Parameter
        (TC, To_Any (TypeCode.TC_Object));
      TypeCode.Add_Parameter
        (TC, To_Any ("forward_reference"));

      return TC;
   end TC_ForwardRequestPerm;

   ----------------------------
   -- TC_NeedsAddressingMode --
   ----------------------------

   TC_NeedsAddressingMode_Cache : TypeCode.Local_Ref;

   function TC_NeedsAddressingMode
     return PolyORB.Any.TypeCode.Local_Ref
   is
      TC : TypeCode.Local_Ref renames TC_NeedsAddressingMode_Cache;

      Name          : constant String := "NeedsAddressingMode";
      Repository_Id : constant String :=
        PolyORB_Exc_Prefix & Name & PolyORB_Exc_Version;

   begin
      if not TypeCode.Is_Nil (TC) then
         return TC;
      end if;

      TC := TypeCode.TC_Except;

      TypeCode.Add_Parameter (TC, To_Any (Name));
      TypeCode.Add_Parameter (TC, To_Any (Repository_Id));

      TypeCode.Add_Parameter (TC, To_Any (TC_Short));
      TypeCode.Add_Parameter (TC, To_Any ("mode"));

      return TC;
   end TC_NeedsAddressingMode;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : Any.Any) return ForwardRequest_Members is
      Index          : Any.Any;
      Result_Forward : References.Ref;
   begin
      Index := Get_Aggregate_Element (Item, TypeCode.TC_Object, 0);
      Result_Forward := From_Any (Index);

      return (Forward_Reference => Smart_Pointers.Ref (Result_Forward));
   end From_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : Any.Any) return ForwardRequestPerm_Members is
      Index          : Any.Any;
      Result_Forward : References.Ref;

   begin
      Index := Get_Aggregate_Element (Item, TypeCode.TC_Object, 0);
      Result_Forward := From_Any (Index);

      return (Forward_Reference => Smart_Pointers.Ref (Result_Forward));
   end From_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : PolyORB.Any.Any)
      return NeedsAddressingMode_Members
   is
      Index : Any.Any;
      Mode  : Short;

   begin
      Index := Get_Aggregate_Element (Item, TC_Short, 0);
      Mode  := From_Any (Index);

      case Mode is
         when 0 =>
            return (Mode => Key);

         when 1 =>
            return (Mode => Profile);

         when 2 =>
            return (Mode => Reference);

         when others =>
            raise Program_Error;
            --  Never be happen
      end case;
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : ForwardRequest_Members)
      return PolyORB.Any.Any
   is
      Result : Any.Any := Get_Empty_Any_Aggregate (TC_ForwardRequest);
      Ref    : References.Ref;
   begin
      References.Set (Ref, Smart_Pointers.Entity_Of (Item.Forward_Reference));
      Add_Aggregate_Element (Result, To_Any (Ref));

      return Result;
   end To_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : ForwardRequestPerm_Members)
      return PolyORB.Any.Any
   is
      Result : Any.Any := Get_Empty_Any_Aggregate (TC_ForwardRequestPerm);
      Ref    : References.Ref;

   begin
      References.Set (Ref, Smart_Pointers.Entity_Of (Item.Forward_Reference));
      Add_Aggregate_Element (Result, To_Any (Ref));

      return Result;
   end To_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : NeedsAddressingMode_Members)
      return PolyORB.Any.Any
   is
      Result : Any.Any := Get_Empty_Any_Aggregate (TC_NeedsAddressingMode);
      Mode   : Short;

   begin
      case Item.Mode is
         when Key =>
            Mode := 0;

         when Profile =>
            Mode := 1;

         when Reference =>
            Mode := 2;
      end case;

      Add_Aggregate_Element (Result, To_Any (Mode));

      return Result;
   end To_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Name   : Standard.String;
      Member : System_Exception_Members)
     return PolyORB.Any.Any
   is
      TC : PolyORB.Any.TypeCode.Local_Ref;
      Result : PolyORB.Any.Any;

   begin
      --  Construct exception typecode

      TC := System_Exception_TypeCode (Name);

      Result := Get_Empty_Any_Aggregate (TC);
      Add_Aggregate_Element (Result, To_Any (Member.Minor));
      Add_Aggregate_Element (Result, To_Any (Member.Completed));

      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : Any.Any) return System_Exception_Members is
      Minor     : PolyORB.Types.Unsigned_Long;
      Completed : Completion_Status;

   begin
      Minor :=
        From_Any
        (Get_Aggregate_Element
         (Item, TC_Unsigned_Long, PolyORB.Types.Unsigned_Long (0)));
      Completed :=
        From_Any
        (Get_Aggregate_Element
         (Item, TC_Completion_Status, PolyORB.Types.Unsigned_Long (1)));

      return (Minor => Minor, Completed => Completed);
   end From_Any;

   -------------------------------
   -- System_Exception_TypeCode --
   -------------------------------

   function System_Exception_TypeCode
     (Name : Standard.String)
     return Any.TypeCode.Local_Ref
   is
      TC    : constant TypeCode.Local_Ref := TypeCode.TC_Except;
      Shift : Natural := 0;
   begin
      --  Name

      TypeCode.Add_Parameter (TC, To_Any (Name));

      if Name (Name'First .. Name'First + PolyORB_Exc_Root'Length - 1)
        = PolyORB_Exc_Root then
         Shift := PolyORB_Exc_Root'Length + 1;
      end if;

      --  RepositoryId : 'INTERNAL:<Name>:1.0'

      declare
         Repository_Id : constant String :=
           PolyORB_Exc_Prefix
             & Name (Name'First + Shift .. Name'Last)
             & PolyORB_Exc_Version;
      begin
         TypeCode.Add_Parameter (TC, To_Any (Repository_Id));

         --  Component 'minor'
         TypeCode.Add_Parameter
           (TC, To_Any (TC_Unsigned_Long));
         TypeCode.Add_Parameter (TC, To_Any ("minor"));

         --  Component 'completed'
         TypeCode.Add_Parameter
           (TC, To_Any (TC_Completion_Status));
         TypeCode.Add_Parameter
           (TC, To_Any ("completed"));

         pragma Debug (C, O ("Built Exception TypeCode for: "
                          & Repository_Id));
      end;

      pragma Debug (C, O (" " & PolyORB.Any.Image (TC)));
      return TC;
   end System_Exception_TypeCode;

end PolyORB.Errors.Helper;
