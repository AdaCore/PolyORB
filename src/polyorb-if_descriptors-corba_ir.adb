------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . I F _ D E S C R I P T O R S . C O R B A _ I R       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2002 Free Software Fundation                --
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

--  An Interface Descriptor that uses the CORBA Interface Repository.

--  $Id$

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.CORBA_P.Exceptions;
with PolyORB.Types;

with CORBA.Repository_Root;
with CORBA.Repository_Root.Helper;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.Contained.Helper;
with CORBA.Repository_Root.InterfaceDef;
with CORBA.Repository_Root.InterfaceDef.Helper;

package body PolyORB.If_Descriptors.CORBA_IR is

   use CORBA.Repository_Root;

   package ContainedSeq_Seq renames
     IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward;
   package InterfaceDefSeq_Seq renames
     IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward;
   package ParDescriptionSeq_Seq renames
     IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription;

   function Corresponding_InterfaceDef
     (Object : PolyORB.References.Ref)
     return InterfaceDef.Ref;

   function Find_Operation
     (Intf : InterfaceDef.Ref;
      Method : CORBA.Identifier)
     return OperationDescription;

   function Corresponding_InterfaceDef
     (Object : PolyORB.References.Ref)
     return InterfaceDef.Ref is
   begin
      raise Not_Implemented;
      return Corresponding_InterfaceDef (Object);
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
        := To_Element_Array (InterfaceDef.get_base_interfaces (Intf));
   begin

      --  First try to find the method in this InterfaceDef...

      for I in Contents'Range loop
         declare
            use type CORBA.Identifier;

            R : constant Contained.Ref
              := Contained.Helper.To_Ref (Contents (I));
         begin
            if Contained.get_name (R) = Method then
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
            when others =>
               raise;
         end;
      end loop Base_Intfs_Loop;
      PolyORB.CORBA_P.Exceptions.Raise_Bad_Operation;
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
      Method  :        Requests.Operation_Id)
     return Any.NVList.Ref
   is
      Oper : constant OperationDescription
        := Find_Operation
        (Corresponding_InterfaceDef (Object),
         CORBA.Identifier'(CORBA.To_CORBA_String (Method)));
      Args : constant ParDescriptionSeq_Seq.Element_Array
        := To_Element_Array (Oper.parameters);
      Result : Any.NVList.Ref;
   begin
      Any.NVList.Create (Result);
      for I in Args'Range loop
         Any.NVList.Add_Item
           (Result, PolyORB.Any.NamedValue'
            (Name      => PolyORB.Types.Identifier (Args (I).name),
             Argument  => Any.Get_Empty_Any (Args (I).IDL_type),
             Arg_Modes => Mode_Map (Args (I).mode)));
      end loop;
      return Result;
   end Get_Empty_Arg_List;

   function Get_Empty_Result
     (If_Desc : access IR_If_Descriptor;
      Object  :        PolyORB.References.Ref;
      Method  :        Requests.Operation_Id)
     return Any.Any
   is
      Oper : constant OperationDescription
        := Find_Operation
        (Corresponding_InterfaceDef (Object),
         CORBA.Identifier'(CORBA.To_CORBA_String (Method)));
   begin
      return Any.Get_Empty_Any (Oper.result);
   end Get_Empty_Result;

end PolyORB.If_Descriptors.CORBA_IR;
