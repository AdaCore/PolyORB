------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.SERVICES.NAMING.NAMINGCONTEXT.HELPER                --
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

with PolyORB.Any;
with PolyORB.Any.ObjRef;
with PolyORB.Exceptions;
with PolyORB.Initialization;
--  with PolyORB.Minimal_Servant;
with PolyORB.Smart_Pointers;
with PolyORB.Types;
with PolyORB.Utils.Strings;

with PolyORB.Services.Naming.Helper;

package body PolyORB.Services.Naming.NamingContext.Helper is

   use PolyORB.Any;
   use PolyORB.Any.ObjRef;
   use PolyORB.Types;

   use PolyORB.Services.Naming.Helper;

   ----------------------
   -- Unchecked_To_Ref --
   ----------------------

   function Unchecked_To_Ref
     (The_Ref : in PolyORB.References.Ref)
      return PolyORB.Services.Naming.NamingContext.Ref
   is
      Result : PolyORB.Services.Naming.NamingContext.Ref;
   begin
      Set (Result,
           PolyORB.Smart_Pointers.Entity_Of (Smart_Pointers.Ref (The_Ref)));
      return Result;
   end Unchecked_To_Ref;

   ------------
   -- To_Ref --
   ------------

   function To_Ref
     (The_Ref : in PolyORB.References.Ref)
     return PolyORB.Services.Naming.NamingContext.Ref is
   begin
      --  if CORBA.Object.Is_Nil (The_Ref)
      --    or else CORBA.Object.Is_A (The_Ref, Repository_Id) then
         return Unchecked_To_Ref (The_Ref);
      --  end if;
      --  PolyORB.Exceptions.Raise_Bad_Param;
   end To_Ref;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in PolyORB.Any.Any)
      return NamingContext.Ref is
   begin
      return To_Ref (PolyORB.Any.ObjRef.From_Any (Item));
   end From_Any;

   function From_Any (Item : in PolyORB.Any.Any)
      return NamingContext.NotFoundReason
   is
      Index : PolyORB.Any.Any :=
         Get_Aggregate_Element (Item,
                                TypeCode.TC_Unsigned_Long,
                                PolyORB.Types.Unsigned_Long (0));
      Position : constant PolyORB.Types.Unsigned_Long
        := From_Any (Index);
   begin
      return NotFoundReason'Val (Position);
   end From_Any;

   function From_Any (Item : in PolyORB.Any.Any)
      return NamingContext.NotFound_Members
   is
      Index : PolyORB.Any.Any;
      Result_why : NamingContext.NotFoundReason;
      Result_rest_of_name : Name;
   begin
      Index := Get_Aggregate_Element (Item,
                                      TC_NotFoundReason,
                                      PolyORB.Types.Unsigned_Long (0));
      Result_why := From_Any (Index);

      Index := Get_Aggregate_Element (Item,
                                      TC_Name,
                                      PolyORB.Types.Unsigned_Long (1));
      Result_rest_of_name := From_Any (Index);

      return
         (why => Result_why,
          rest_of_name => Result_rest_of_name);
   end From_Any;

   function From_Any (Item : in PolyORB.Any.Any)
      return NamingContext.CannotProceed_Members
   is
      Index : PolyORB.Any.Any;
      Result_cxt : NamingContext.Ref;
      Result_rest_of_name : Name;
   begin
      Index := Get_Aggregate_Element (Item,
                                      TC_NamingContext,
                                      PolyORB.Types.Unsigned_Long (0));
      Result_cxt := From_Any (Index);

      Index := Get_Aggregate_Element (Item,
                                      TC_Name,
                                      PolyORB.Types.Unsigned_Long (1));
      Result_rest_of_name := From_Any (Index);

      return
         (cxt => Result_cxt,
          rest_of_name => Result_rest_of_name);
   end From_Any;

   function From_Any (Item : in PolyORB.Any.Any)
      return NamingContext.InvalidName_Members is
      Result : InvalidName_Members;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
      return Result;
   end From_Any;

   function From_Any (Item : in PolyORB.Any.Any)
      return NamingContext.AlreadyBound_Members is
      Result : AlreadyBound_Members;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
      return Result;
   end From_Any;

   function From_Any (Item : in PolyORB.Any.Any)
      return NamingContext.NotEmpty_Members is
      Result : NotEmpty_Members;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
      return Result;
   end From_Any;


   ---------------------------------
   -- Raise_AlreadyBound_From_Any --
   ---------------------------------

   procedure Raise_AlreadyBound_From_Any
     (Item : in PolyORB.Any.Any)
   is
      Members : constant AlreadyBound_Members
        := From_Any (Item);
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (AlreadyBound'Identity,
         Members);
   end Raise_AlreadyBound_From_Any;

   ----------------------------------
   -- Raise_CannotProceed_From_Any --
   ----------------------------------

   procedure Raise_CannotProceed_From_Any
     (Item : in PolyORB.Any.Any)
   is
      Members : constant CannotProceed_Members
        := From_Any (Item);
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (CannotProceed'Identity,
         Members);
   end Raise_CannotProceed_From_Any;

   --------------------------------
   -- Raise_InvalidName_From_Any --
   --------------------------------

   procedure Raise_InvalidName_From_Any
     (Item : in PolyORB.Any.Any)
   is
      Members : constant InvalidName_Members
        := From_Any (Item);
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InvalidName'Identity,
         Members);
   end Raise_InvalidName_From_Any;

   -----------------------------
   -- Raise_NotEmpty_From_Any --
   -----------------------------

   procedure Raise_NotEmpty_From_Any
     (Item : in PolyORB.Any.Any)
   is
      Members : constant NotEmpty_Members
        := From_Any (Item);
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (NotEmpty'Identity,
         Members);
   end Raise_NotEmpty_From_Any;

   -----------------------------
   -- Raise_NotFound_From_Any --
   -----------------------------

   procedure Raise_NotFound_From_Any
     (Item : in PolyORB.Any.Any)
   is
      Members : constant NotFound_Members
        := From_Any (Item);
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (NotFound'Identity,
         Members);
   end Raise_NotFound_From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : in NamingContext.Ref)
     return PolyORB.Any.Any is
      A : PolyORB.Any.Any
        := PolyORB.Any.ObjRef.To_Any (PolyORB.References.Ref (Item));
   begin
      Set_Type (A, TC_NamingContext);
      return A;
   end To_Any;

   function To_Any
     (Item : in NamingContext.NotFoundReason)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
         Get_Empty_Any_Aggregate (TC_NotFoundReason);
   begin
      Add_Aggregate_Element
        (Result,
         To_Any (PolyORB.Types.Unsigned_Long (NotFoundReason'Pos (Item))));
      return Result;
   end To_Any;

   function To_Any
     (Item : in NamingContext.NotFound_Members)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any := Get_Empty_Any_Aggregate (TC_NotFound);
   begin
      Add_Aggregate_Element (Result, To_Any (Item.why));
      Add_Aggregate_Element (Result, To_Any (Item.rest_of_name));

      return Result;
   end To_Any;

   function To_Any
     (Item : in NamingContext.CannotProceed_Members)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
         Get_Empty_Any_Aggregate (TC_CannotProceed);
   begin
      Add_Aggregate_Element (Result, To_Any (Item.cxt));
      Add_Aggregate_Element (Result, To_Any (Item.rest_of_name));
      return Result;
   end To_Any;

   function To_Any
     (Item : in NamingContext.InvalidName_Members)
     return PolyORB.Any.Any is
      Result : PolyORB.Any.Any :=
        Get_Empty_Any_Aggregate (TC_InvalidName);
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
      return Result;
   end To_Any;

   function To_Any
     (Item : in NamingContext.AlreadyBound_Members)
     return PolyORB.Any.Any is
      Result : PolyORB.Any.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_AlreadyBound);
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
      return Result;
   end To_Any;

   function To_Any
     (Item : in NamingContext.NotEmpty_Members)
     return PolyORB.Any.Any is
      Result : PolyORB.Any.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_NotEmpty);
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
      return Result;
   end To_Any;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      declare
         Name : PolyORB.Types.String
           := To_PolyORB_String ("NamingContext");
         Id : PolyORB.Types.String
           := To_PolyORB_String ("IDL:omg.org/CosNaming/NamingContext:1.0");
      begin
         TypeCode.Add_Parameter (TC_NamingContext, To_Any (Name));
         TypeCode.Add_Parameter (TC_NamingContext, To_Any (Id));
      end;
      declare
         Name : PolyORB.Types.String
           := To_PolyORB_String ("NotFoundReason");
         Id : PolyORB.Types.String
           := To_PolyORB_String
           ("IDL:omg.org/CosNaming/NamingContext/NotFoundReason:1.0");
         missing_node_Name : PolyORB.Types.String
           := To_PolyORB_String ("missing_node");
         not_context_Name : PolyORB.Types.String
           := To_PolyORB_String ("not_context");
         not_object_Name : PolyORB.Types.String
           := To_PolyORB_String ("not_object");
      begin
         TypeCode.Add_Parameter (TC_NotFoundReason, To_Any (Name));
         TypeCode.Add_Parameter (TC_NotFoundReason, To_Any (Id));
         TypeCode.Add_Parameter (TC_NotFoundReason,
                                 To_Any (missing_node_Name));
         TypeCode.Add_Parameter (TC_NotFoundReason,
                                 To_Any (not_context_Name));
         TypeCode.Add_Parameter (TC_NotFoundReason,
                                 To_Any (not_object_Name));
      end;

      declare
         Name : PolyORB.Types.String := To_PolyORB_String ("NotFound");
         Id : PolyORB.Types.String
           := To_PolyORB_String
           ("IDL:omg.org/CosNaming/NamingContext/NotFound:1.0");
         Arg_Name_why : PolyORB.Types.String := To_PolyORB_String ("why");
         Arg_Name_rest_of_name : PolyORB.Types.String
           := To_PolyORB_String ("rest_of_name");
      begin
         TypeCode.Add_Parameter (TC_NotFound, To_Any (Name));
         TypeCode.Add_Parameter (TC_NotFound, To_Any (Id));
         TypeCode.Add_Parameter (TC_NotFound,
                                 To_Any (TC_NotFoundReason));
         TypeCode.Add_Parameter (TC_NotFound, To_Any (Arg_Name_why));
         TypeCode.Add_Parameter (TC_NotFound, To_Any (TC_Name));
         TypeCode.Add_Parameter (TC_NotFound,
                                 To_Any (Arg_Name_rest_of_name));
      end;
      PolyORB.Exceptions.Register_Exception
        (TC_NotFound,
         Raise_NotFound_From_Any'Access);

      declare
         Name : PolyORB.Types.String := To_PolyORB_String ("CannotProceed");
         Id : PolyORB.Types.String
           := To_PolyORB_String
           ("IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0");
         Arg_Name_cxt : PolyORB.Types.String := To_PolyORB_String ("cxt");
         Arg_Name_rest_of_name : PolyORB.Types.String
           := To_PolyORB_String ("rest_of_name");
      begin
         TypeCode.Add_Parameter (TC_CannotProceed, To_Any (Name));
         TypeCode.Add_Parameter (TC_CannotProceed, To_Any (Id));
         TypeCode.Add_Parameter (TC_CannotProceed,
                                 To_Any (TC_NamingContext));
         TypeCode.Add_Parameter (TC_CannotProceed, To_Any (Arg_Name_cxt));
         TypeCode.Add_Parameter (TC_CannotProceed, To_Any (TC_Name));
         TypeCode.Add_Parameter (TC_CannotProceed,
                                 To_Any (Arg_Name_rest_of_name));
      end;
      PolyORB.Exceptions.Register_Exception
        (TC_CannotProceed,
         Raise_CannotProceed_From_Any'Access);

      declare
         Name : PolyORB.Types.String
           := To_PolyORB_String ("InvalidName");
         Id : PolyORB.Types.String
           := To_PolyORB_String
           ("IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0");
      begin
         TypeCode.Add_Parameter (TC_InvalidName, To_Any (Name));
         TypeCode.Add_Parameter (TC_InvalidName, To_Any (Id));
      end;
      PolyORB.Exceptions.Register_Exception
        (TC_InvalidName,
         Raise_InvalidName_From_Any'Access);

      declare
         Name : PolyORB.Types.String := To_PolyORB_String ("AlreadyBound");
         Id : PolyORB.Types.String
           := To_PolyORB_String
           ("IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0");
      begin
         TypeCode.Add_Parameter (TC_AlreadyBound, To_Any (Name));
         TypeCode.Add_Parameter (TC_AlreadyBound, To_Any (Id));
      end;
      PolyORB.Exceptions.Register_Exception
        (TC_AlreadyBound,
         Raise_AlreadyBound_From_Any'Access);

      declare
         Name : PolyORB.Types.String := To_PolyORB_String ("NotEmpty");
         Id : PolyORB.Types.String
           := To_PolyORB_String
           ("IDL:omg.org/CosNaming/NamingContext/NotEmpty:1.0");
      begin
         TypeCode.Add_Parameter (TC_NotEmpty, To_Any (Name));
         TypeCode.Add_Parameter (TC_NotEmpty, To_Any (Id));
      end;
      PolyORB.Exceptions.Register_Exception
        (TC_NotEmpty,
         Raise_NotEmpty_From_Any'Access);

   end Initialize;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"Naming.NamingContext.Helper",
          Conflicts => Empty,
          Depends   =>
            +"soft_links"
          ,
          Provides  => Empty,
          Init      => Initialize'Access));
   end;

end PolyORB.Services.Naming.NamingContext.Helper;
