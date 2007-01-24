------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . I F _ D E S C R I P T O R S . C O R B A _ I R       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
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

--  An Interface Descriptor that uses the CORBA Interface Repository.

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.CORBA_P.IR_Tools;
with PolyORB.Log;
with PolyORB.Types;

with CORBA.Repository_Root;
with CORBA.Repository_Root.Helper;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.Contained.Helper;
with CORBA.Repository_Root.InterfaceDef;
with CORBA.Repository_Root.InterfaceDef.Helper;
with CORBA.Repository_Root.Repository;

package body PolyORB.If_Descriptors.CORBA_IR is

   use PolyORB.Log;
   use CORBA.Repository_Root;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.if_descriptors.corba_ir");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   package ContainedSeq_Seq renames
     IDL_SEQUENCE_CORBA_Contained_Forward;
   package InterfaceDefSeq_Seq renames
     IDL_SEQUENCE_CORBA_InterfaceDef_Forward;
   package ParDescriptionSeq_Seq renames
     IDL_SEQUENCE_CORBA_ParameterDescription;

   function Corresponding_InterfaceDef
     (Object : PolyORB.References.Ref)
     return InterfaceDef.Ref;

   function Find_Operation
     (Intf : InterfaceDef.Ref;
      Method : CORBA.Identifier)
     return OperationDescription;

   function Corresponding_InterfaceDef
     (Object : PolyORB.References.Ref)
     return InterfaceDef.Ref
   is
      RId : constant String
        := References.Type_Id_Of (Object);
   begin
--       return InterfaceDef.Helper.To_Ref
--         (CORBA.Object.get_interface (To_CORBA_Ref (Object));
      pragma Debug
        (O ("Corresponding_IfDef for " & RId));
      return InterfaceDef.Helper.To_Ref
        (Repository.lookup_id
         (CORBA_P.IR_Tools.Get_IR_Root,
          CORBA.To_CORBA_String (RId)));
      --  XXX actually we should do a call to get_interface here,
      --  and use Lookup_Id or whatever only on the /server/.
   end Corresponding_InterfaceDef;

   function Find_Operation
     (Intf : InterfaceDef.Ref;
      Method : CORBA.Identifier)
     return OperationDescription
   is
      Contents : constant ContainedSeq_Seq.Element_Array
        := To_Element_Array
        (InterfaceDef.contents (Intf, dk_Operation, True));

      Base_Intfs : constant InterfaceDefSeq_Seq.Element_Array
        := To_Element_Array (InterfaceDef.Get_base_interfaces (Intf));
   begin

      --  First try to find the method in this InterfaceDef...

      for I in Contents'Range loop
         declare
            use type CORBA.Identifier;

            R : constant Contained.Ref
              := Contained.Helper.To_Ref (Contents (I));
         begin
            if Contained.Get_name (R) = Method then
               return Helper.From_Any (Contained.describe (R).value);
            end if;
         end;
      end loop;

      --  Then try ancestors in turn.

      Base_Intfs_Loop :
      for I in Base_Intfs'Range loop
         begin
            return Find_Operation
              (InterfaceDef.Helper.To_Ref (Base_Intfs (I)),
               Method);
         exception
            when CORBA.Bad_Operation =>
               null;
         end;
      end loop Base_Intfs_Loop;
      CORBA.Raise_Bad_Operation (CORBA.Default_Sys_Member);
   end Find_Operation;

   -------------------------------------------
   -- Public primitives of IR_If_Descriptor --
   -------------------------------------------

   Mode_Map : constant array (ParameterMode) of PolyORB.Any.Flags
     := (PARAM_IN    => Any.ARG_IN,
         PARAM_OUT   => Any.ARG_OUT,
         PARAM_INOUT => Any.ARG_INOUT);

   function Get_Empty_Arg_List
     (If_Desc : access IR_If_Descriptor;
      Object  :        PolyORB.References.Ref;
      Method  :        String)
     return Any.NVList.Ref
   is
      pragma Warnings (Off);
      pragma Unreferenced (If_Desc);
      pragma Warnings (On);

      Oper : constant OperationDescription
        := Find_Operation
        (Corresponding_InterfaceDef (Object),
         CORBA.Identifier'(CORBA.To_CORBA_String (Method)));
      Args : constant ParDescriptionSeq_Seq.Element_Array
        := To_Element_Array (Oper.Parameters);
      Result : Any.NVList.Ref;
   begin
      Any.NVList.Create (Result);
      for I in Args'Range loop
         Any.NVList.Add_Item
           (Result, PolyORB.Any.NamedValue'
            (Name      => PolyORB.Types.Identifier (Args (I).Name),
             Argument  => Any.Get_Empty_Any
             (CORBA.TypeCode.Internals.To_PolyORB_Object (Args (I).IDL_Type)),
             Arg_Modes => Mode_Map (Args (I).Mode)));
      end loop;
      return Result;
   end Get_Empty_Arg_List;

   function Get_Empty_Result
     (If_Desc : access IR_If_Descriptor;
      Object  :        PolyORB.References.Ref;
      Method  :        String)
     return Any.Any
   is
      pragma Warnings (Off);
      pragma Unreferenced (If_Desc);
      pragma Warnings (On);

      Oper : constant OperationDescription
        := Find_Operation
        (Corresponding_InterfaceDef (Object),
         CORBA.Identifier'(CORBA.To_CORBA_String (Method)));
   begin
      pragma Debug
        (O ("Get_Empty_Result: TC is of kind "
            & PolyORB.Any.TCKind'Image
            (PolyORB.Any.TypeCode.Kind
             (CORBA.TypeCode.Internals.To_PolyORB_Object (Oper.Result)))));
      return Any.Get_Empty_Any
        (CORBA.TypeCode.Internals.To_PolyORB_Object (Oper.Result));
   end Get_Empty_Result;

end PolyORB.If_Descriptors.CORBA_IR;
