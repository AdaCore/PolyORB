------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O L Y O R B . E R R O R S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Any.ObjRef;

with PolyORB.Log;
with PolyORB.References;

package body PolyORB.Errors is

   use PolyORB.Any;
   use PolyORB.Any.ObjRef;

   use PolyORB.Log;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.errors");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   -----------
   -- Found --
   -----------

   function Found (Error : in Error_Container) return Boolean is
   begin
      return Error.Kind /= No_Error;
   end Found;

   -----------
   -- Throw --
   -----------

   procedure Throw
     (Error  : in out Error_Container;
      Kind   : in     Error_Id;
      Member : in     Exception_Members'Class)
   is
   begin
      if Error.Kind /= No_Error then
         pragma Debug (O ("*** Abort *** "
                          & Error_Id'Image (Error.Kind)));

         Free (Error.Member);
      end if;

      Error.Kind := Kind;
      Error.Member := new Exception_Members'Class'(Member);

      pragma Debug (O ("*** Throw *** " & Error_Id'Image (Error.Kind)));
   end Throw;

   -----------
   -- Catch --
   -----------

   procedure Catch
     (Error : in out Error_Container) is
   begin
      Error.Kind := No_Error;
      Free (Error.Member);
   end Catch;

   --------------
   -- Is_Error --
   --------------

   function Is_Error (Error : in Error_Container) return Boolean is
   begin
      return Error.Kind /= No_Error;
   end Is_Error;

   ------------------
   -- Error_To_Any --
   ------------------

   function Error_To_Any (Error : in Error_Container) return PolyORB.Any.Any is
      Result : PolyORB.Any.Any;
      Error_Name : constant String :=  Error_Id'Image (Error.Kind);
      Exception_Name : constant String
        := Error_Name (Error_Name'First .. Error_Name'Last - 2);

   begin
      pragma Debug (O ("Error_To_Any: enter."));
      pragma Debug (O ("Error is: " & Error_Name));
      pragma Debug (O ("Exception name is: " & Exception_Name));

      if Error.Kind in ORB_System_Error then
         Result := To_Any (Exception_Name,
                           System_Exception_Members (Error.Member.all));

      elsif Error.Kind = ForwardRequest_E then
         Result := To_Any (ForwardRequest_Members (Error.Member.all));

      elsif Error.Kind in POA_Error then
         Result := To_Any (Exception_Name,
                           Null_Members (Error.Member.all));
      else
         raise Program_Error;
         --  Never happens
      end if;

      pragma Debug (O ("Error_To_Any: leave."));
      return Result;
   end Error_To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : PolyORB.Any.Any) return Completion_Status is
   begin
      return Completion_Status'Val
        (Unsigned_Long'
         (From_Any (PolyORB.Any.Get_Aggregate_Element
                    (Item, TC_Unsigned_Long, 0))));
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

   TC_Completion_Status_Cache : TypeCode.Object;

   function TC_Completion_Status
     return PolyORB.Any.TypeCode.Object
   is
      use type PolyORB.Types.Unsigned_Long;

      TC : TypeCode.Object renames TC_Completion_Status_Cache;

   begin
      if TypeCode.Parameter_Count (TC) /= 0 then
         return TC_Completion_Status_Cache;
      end if;

      TC := TypeCode.TC_Enum;
      TypeCode.Add_Parameter
        (TC, To_Any (To_PolyORB_String ("completion_status")));
      TypeCode.Add_Parameter
        (TC, To_Any (To_PolyORB_String
                     ("IDL:omg.org/CORBA/completion_status:1.0")));

      for C in Completion_Status'Range loop
         TypeCode.Add_Parameter
           (TC, To_Any (To_PolyORB_String (Completion_Status'Image (C))));
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
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Member);
      pragma Warnings (On); --  WAG:3.15

      TC    : TypeCode.Object := TypeCode.TC_Except;
      Shift : Natural         := 0;
   begin
      --  Name
      TypeCode.Add_Parameter (TC, To_Any (To_PolyORB_String (Name)));

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
         TypeCode.Add_Parameter (TC,
           To_Any (To_PolyORB_String (Repository_Id)));
      end;

      return Get_Empty_Any_Aggregate (TC);
   end To_Any;

   -----------------------
   -- TC_ForwardRequest --
   -----------------------

   TC_ForwardRequest_Cache : TypeCode.Object;

   function TC_ForwardRequest
     return PolyORB.Any.TypeCode.Object
   is
      TC : TypeCode.Object renames TC_ForwardRequest_Cache;

      Name          : constant String := "ForwardRequest";
      Repository_Id : constant String :=
        PolyORB_Exc_Prefix & Name & PolyORB_Exc_Version;
   begin
      if TypeCode.Parameter_Count (TC) /= 0 then
         return TC;
      end if;

      TC := TypeCode.TC_Except;

      TypeCode.Add_Parameter (TC, To_Any (To_PolyORB_String (Name)));
      TypeCode.Add_Parameter (TC, To_Any (To_PolyORB_String (Repository_Id)));

      TypeCode.Add_Parameter
        (TC, To_Any (TC_Object));
      TypeCode.Add_Parameter
        (TC, To_Any (To_PolyORB_String ("forward_reference")));

      return TC;
   end TC_ForwardRequest;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in Any.Any) return ForwardRequest_Members is
      Index          : Any.Any;
      Result_Forward : References.Ref;
   begin
      Index := Get_Aggregate_Element (Item, TC_Object, 0);
      Result_Forward := From_Any (Index);

      return (Forward_Reference => Smart_Pointers.Ref (Result_Forward));
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

   -------------------------------
   -- System_Exception_TypeCode --
   -------------------------------

   function System_Exception_TypeCode
     (Name : Standard.String)
     return Any.TypeCode.Object
   is
      TC    : TypeCode.Object := TypeCode.TC_Except;
      Shift : Natural := 0;
   begin
      --  Name
      TypeCode.Add_Parameter (TC, To_Any (To_PolyORB_String (Name)));

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
         TypeCode.Add_Parameter (TC,
           To_Any (To_PolyORB_String (Repository_Id)));

         --  Component 'minor'
         TypeCode.Add_Parameter
           (TC, To_Any (TC_Unsigned_Long));
         TypeCode.Add_Parameter
           (TC, To_Any (To_PolyORB_String ("minor")));

         --  Component 'completed'
         TypeCode.Add_Parameter
           (TC, To_Any (TC_Completion_Status));
         TypeCode.Add_Parameter
           (TC, To_Any (To_PolyORB_String ("completed")));

         pragma Debug (O ("Built Exception TypeCode for: "
                          & Repository_Id));
      end;

      pragma Debug (O (" " & PolyORB.Any.Image (TC)));
      return TC;
   end System_Exception_TypeCode;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Name   : Standard.String;
      Member : System_Exception_Members)
     return PolyORB.Any.Any
   is
      TC : PolyORB.Any.TypeCode.Object;
      Result : PolyORB.Any.Any;

   begin
      --  Construct exception typecode

      TC := System_Exception_TypeCode (Name);

      Result := Get_Empty_Any_Aggregate (TC);
      Add_Aggregate_Element (Result, To_Any (Member.Minor));
      Add_Aggregate_Element (Result, To_Any (Member.Completed));

      return Result;
   end To_Any;

end PolyORB.Errors;
