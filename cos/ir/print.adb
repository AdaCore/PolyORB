------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                P R I N T                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1999-2004 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Text_IO;

with CORBA.ORB;
with CORBA.Repository_Root;
with CORBA.Repository_Root.Helper;
with CORBA.Repository_Root.Repository;
with CORBA.Repository_Root.Repository.Helper;
with CORBA.Repository_Root.Container;
with CORBA.Repository_Root.Container.Helper;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.UnionDef.Helper;
with CORBA.Repository_Root.StructDef.Helper;
with CORBA.Repository_Root.InterfaceDef.Helper;
with CORBA.Repository_Root.ExceptionDef.Helper;
with CORBA.Repository_Root.ValueDef.Helper;
with CORBA.Repository_Root.ModuleDef.Helper;

with PolyORB.CORBA_P.Naming_Tools;

with PolyORB.Setup.Client;
pragma Elaborate_All (PolyORB.Setup.Client);
pragma Warnings (Off, PolyORB.Setup.Client);

procedure Print is

   use Ada.Text_IO;
   use CORBA;
   use CORBA.Repository_Root;

   procedure Print_TypeCode
     (TC  : CORBA.TypeCode.Object;
      Inc : Standard.String);

   procedure Print_ParDescriptionSeq
     (Des : ParDescriptionSeq;
      Inc : Standard.String);

   procedure Print_Contents
     (In_Seq : ContainedSeq;
      Inc : Standard.String);

   procedure Print_Description
     (Des : Contained.Description;
      Inc : Standard.String);

   procedure Print_TypeCode (TC  : CORBA.TypeCode.Object;
                            Inc : Standard.String)
   is
   begin
      case CORBA.TypeCode.Kind (TC) is
         when Tk_Null =>
            Put ("Null");
         when Tk_Void =>
            Put ("Void");
         when Tk_Short =>
            Put ("Short");
         when Tk_Long =>
            Put ("Long");
         when Tk_Ushort =>
            Put ("Ushort");
         when Tk_Ulong =>
            Put ("Ulong");
         when Tk_Float =>
            Put ("Float");
         when Tk_Double =>
            Put ("Double");
         when Tk_Boolean =>
            Put ("Boolean");
         when Tk_Char =>
            Put ("Char");
         when Tk_Octet =>
            Put ("Octet");
         when Tk_Any =>
            Put ("Any");
         when Tk_TypeCode =>
            Put ("TypeCode");
         when Tk_Principal =>
            Put ("Principal");
         when Tk_Objref =>
            Put ("ObjRef");
         when Tk_Struct =>
            declare
               L : constant CORBA.Unsigned_Long
                 := CORBA.TypeCode.Member_Count (TC);
            begin
               Put ("Struct  :");
               if L /= 0 then
                  for I in 0 .. L - 1 loop
                     Put_Line (" ");
                     Put (Inc & "    ");
                     Print_TypeCode (TypeCode.Member_Type (TC, I),
                                     Inc & "    ");
                  end loop;
               else
                  Put_Line (" null record");
               end if;
            end;
         when Tk_Union =>
            declare
               L : constant Unsigned_Long
                 := TypeCode.Member_Count (TC);
            begin
               Put ("Union  :");
               Put (" discr : ");
               Print_TypeCode (TypeCode.Discriminator_Type (TC),
                               Inc & "        ");
               if L /= 0 then
                  for I in 0 .. L - 1 loop
                     Put_Line (" ");
                     Put (Inc & "    ");
                     Print_TypeCode (TypeCode.Member_Type (TC, I),
                                     Inc & "    ");
                  end loop;
               else
                  Put_Line (" null record");
               end if;
            end;
         when Tk_Enum =>
            Put ("Enum");
         when Tk_String =>
            Put ("String");
         when Tk_Sequence =>
            Put ("Sequence (");
            Print_TypeCode (CORBA.TypeCode.Content_Type (TC),
                            Inc & "    ");
            Put (")");
         when Tk_Array =>
            Put ("Array (");
            Print_TypeCode (CORBA.TypeCode.Content_Type (TC),
                            Inc & "    ");
            Put (")");
         when Tk_Alias =>
            Put ("Alias (");
            Print_TypeCode (CORBA.TypeCode.Content_Type (TC),
                            Inc & "    ");
            Put (")");
         when Tk_Except =>
            declare
               L : constant Unsigned_Long
                 := TypeCode.Member_Count (TC);
            begin
               Put ("Exception  :");
               if L /= 0 then
                  for I in 0 .. L - 1 loop
                     Put_Line (" ");
                     Put (Inc & "    ");
                     Print_TypeCode (TypeCode.Member_Type (TC, I),
                                     Inc & "    ");
                  end loop;
               else
                  Put_Line (" null record");
               end if;
            end;
         when Tk_Longlong =>
            Put ("LongLong");
         when Tk_Ulonglong =>
            Put ("Ulonglonh");
         when Tk_Longdouble =>
            Put ("LongDouble");
         when Tk_Widechar =>
            Put ("Widechar");
         when Tk_Wstring =>
            Put ("Wstring");
         when Tk_Fixed =>
            Put ("Fixed");
         when Tk_Value =>
            Put ("value");
         when Tk_Valuebox =>
            Put ("valueBox");
            Print_TypeCode (CORBA.TypeCode.Content_Type (TC),
                            Inc & "    ");
            Put (")");
         when Tk_Native =>
            Put ("Native");
         when Tk_Abstract_Interface =>
            Put ("Abstr-Ref");
      end case;
   end Print_TypeCode;



   procedure Print_ParDescriptionSeq (Des : ParDescriptionSeq;
                                      Inc : Standard.String) is
      package PDS renames
        IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription;
      A : PDS.Element_Array
        := PDS.To_Element_Array (PDS.Sequence (Des));
   begin
      for I in A'Range loop
         Put_Line (Inc & "Param " & Integer'Image (I) & " : ");
         Put (Inc & "    type   : ");
         Print_TypeCode (A (I).IDL_type, Inc & "        ");
         Put_Line (" ");
         Put_Line (Inc & "    name   : " &
                   CORBA.To_Standard_String (CORBA.String ((A (I).name))));
         Put_Line (Inc & "    mode   : " &
                   ParameterMode'Image (A (I).mode));
      end loop;
   end Print_ParDescriptionSeq;


   procedure Print_Description (Des : Contained.Description;
                                Inc : Standard.String) is
   begin
      case Des.kind is
         when
           dk_Repository |
           dk_Primitive  |
           dk_String     |
           dk_Sequence   |
           dk_Array      |
           dk_Wstring    |
           dk_Fixed      |
           dk_Typedef    |
           dk_all        |
           dk_none       =>
            null;
         when
           dk_Attribute  =>
            declare
               D : constant AttributeDescription :=
                 Helper.From_Any (Des.value);
            begin
               Put (Inc & "Type     :");
               Print_TypeCode (D.IDL_type, Inc & "    ");
               Put_Line (" ");
               Put_Line (Inc & "Mode     :" &
                         AttributeMode'Image (D.mode));
            end;

         when
           dk_Constant   |
           dk_ValueMember =>
            null;
         when
           dk_Operation =>
            declare
               D : constant OperationDescription :=
                 Helper.From_Any (Des.value);
            begin
               Put (Inc & "Result_type : ");
               Print_TypeCode (D.result, Inc & "    ");
               Put_Line (" ");
               Print_ParDescriptionSeq (D.parameters, Inc);
            end;
         when
           dk_Alias      |
           dk_Struct     |
           dk_Union      |
           dk_Enum       |
           dk_ValueBox   |
           dk_Native =>
            declare
               D : constant TypeDescription :=
                 Helper.From_Any (Des.value);
            begin
               Put_Line (Inc & "TC_Type : " &
                         TCKind'Image
                         (TypeCode.Kind (D.IDL_type)));
            end;
--          when
--            dk_Exception  =>
--             declare
--                D : constant ExceptionDescription :=
--                  Helper.From_Any (Des.value);
--             begin
--                null;
--             end;
--          when
--            dk_Module     =>
--             declare
--                D : constant ModuleDescription :=
--                  Helper.From_Any (Des.value);
--             begin
--                null;
--             end;
--          when
--            dk_value      =>
--             declare
--                D : constant valueDescription :=
--                  Helper.From_Any (Des.value);
--             begin
--                null;
--             end;
--          when
--            dk_Interface  =>
--             declare
--                D : constant InterfaceDescription :=
--                  Helper.From_Any (Des.value);
--             begin
--                null;
--             end;
         when others =>
            null;
      end case;
   end Print_Description;

   procedure Print_Contents (In_Seq : ContainedSeq;
                            Inc : Standard.String) is

      package Contained_For_Seq renames
      CORBA.Repository_Root.
        IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward;
      Cont_Array : Contained_For_Seq.Element_Array
        := Contained_For_Seq.To_Element_Array
        (Contained_For_Seq.Sequence (In_Seq));
      use Contained;
   begin
      for I in Cont_Array'Range loop
         declare
            The_Ref : Contained.Ref := Convert_Forward.To_Ref (Cont_Array (I));
         begin
            Put_Line (Inc & "Node     : " &
                      DefinitionKind'Image
                      (get_def_kind (The_Ref)));
            Put_Line (Inc & "Name     : " &
                      CORBA.To_Standard_String
                      (CORBA.String (get_name (The_Ref))));
            Put_Line (Inc & "Id       : " &
                      CORBA.To_Standard_String
                      (CORBA.String (get_id (The_Ref))));
            Put_Line (Inc & "Vers     : " &
                      CORBA.To_Standard_String
                      (CORBA.String (get_version (The_Ref))));
            Put_Line (Inc & "Abs-Name : " &
                      CORBA.To_Standard_String
                      (CORBA.String
                       (get_absolute_name (The_Ref))));
            Print_Description (Contained.describe (The_Ref), Inc);
            Put_Line (" ");

            --  recursivity
            case Contained.get_def_kind (The_Ref) is
               when dk_Module =>
                  declare
                     R : Container.Ref := Container.Helper.To_Ref
                       (ModuleDef.Helper.To_Ref (The_Ref));
                  begin
                     Print_Contents
                       (Container.contents (R, dk_all, True), Inc & "    ");
                  end;
               when dk_Exception =>
                  declare
                     R : Container.Ref := Container.Helper.To_Ref
                       (ExceptionDef.Helper.To_Ref (The_Ref));
                  begin
                     Print_Contents
                       (Container.contents (R, dk_all, True), Inc & "     ");
                  end;
               when dk_Interface =>
                  declare
                     R : Container.Ref := Container.Helper.To_Ref
                       (InterfaceDef.Helper.To_Ref (The_Ref));
                  begin
                     Print_Contents
                       (Container.contents (R, dk_all, True), Inc & "     ");
                  end;
               when dk_Value =>
                  declare
                     R : Container.Ref := Container.Helper.To_Ref
                       (ValueDef.Helper.To_Ref (The_Ref));
                  begin
                     Print_Contents
                       (Container.contents (R, dk_all, True), Inc & "    ");
                  end;
               when dk_Struct =>
                  declare
                     R : Container.Ref := Container.Helper.To_Ref
                       (StructDef.Helper.To_Ref (The_Ref));
                  begin
                     Print_Contents
                       (Container.contents (R, dk_all, True), Inc & "    ");
                  end;
               when dk_Union =>
                  declare
                     R : Container.Ref := Container.Helper.To_Ref
                       (UnionDef.Helper.To_Ref (The_Ref));
                  begin
                     Print_Contents
                       (Container.contents (R, dk_all, True), Inc & "    ");
                  end;
               when others =>
                  null;
            end case;
         end;
      end loop;
   end Print_Contents;

   Myrep : Repository.Ref;

begin
   CORBA.ORB.Initialize ("ORB");
   Myrep := Repository.Helper.To_Ref
     (PolyORB.CORBA_P.Naming_Tools.Locate ("Interface_Repository"));

--  If Ada.Command_Line.Argument_Count < 1 then
--      Put_Line ("usage : client <IOR_string_from_server>");
--      return;
--   end if;

   --  transforms the Ada string into CORBA.String
--   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));

   --  getting the CORBA.Object
--   CORBA.ORB.String_To_Object (IOR, Myrep);

   --  checking if it worked
   if Repository.Is_Nil (Myrep) then
      Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   Put_Line ("Start IR dump");
   Print_Contents (Repository.contents (Myrep, dk_all, True), " ");
   New_Line;
   Put_Line ("End of Print Interface Repository client!");

end Print;
