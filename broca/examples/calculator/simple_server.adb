------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $LastChangedRevision$
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------


with Ada.Tags;
with Ada.Command_Line;
with CORBA; use CORBA;
with CORBA.ORB;
with CORBA.Object;

with PortableServer;
with Broca.Server_Tools; use Broca.Server_Tools;
pragma Elaborate (Broca.Server_Tools);

with Calculators.Simple_Calculator;
with Calculators.Simple_Calculator.Impl;

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.Repository;
with CORBA.Repository_Root.Container;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.InterfaceDef;
with CORBA.Repository_Root.OperationDef;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.ModuleDef;
with CORBA.Repository_Root.ModuleDef.Helper;

with Ada.Text_IO; use Ada.Text_IO;

procedure Simple_Server is

   procedure Create_Operation (Rep : Repository.Ref;
                               Int : in InterfaceDef_Forward.Ref;
                               Id : in RepositoryId;
                               Name : in Identifier;
                               Version : in VersionSpec) is

      Op : OperationDef.Ref;
      Name_Op : Identifier;
      package PDS renames
        IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription;
      package EDS renames
           IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDef_Forward;
      package CIS renames
        IDL_SEQUENCE_CORBA_Repository_Root_ContextIdentifier;
      Mem : ParDescriptionSeq
        := ParDescriptionSeq (PDS.Null_Sequence);
      Exc : ExceptionDefSeq
        := ExceptionDefSeq (EDS.Null_Sequence);
      Con : ContextIdSeq
        := ContextIdSeq (CIS.Null_Sequence);
      Memb : ParameterDescription;
   begin
      --  create the members
      Name_Op := To_CORBA_String ("Left_op");
      Memb :=  (Name => Name_Op,
                IDL_Type => TC_Long,
                Type_Def => IDLType.Convert_Forward.To_Forward
                (IDLType.Ref
                 (Repository.Get_Primitive
                  (Rep,
                   Pk_Long))),
                Mode => PARAM_IN);
      PDS.Append (PDS.Sequence (Mem), Memb);

      Name_Op := To_CORBA_String ("Right_op");
      Memb :=  (Name => Name_Op,
                IDL_Type => TC_Long,
                Type_Def => IDLType.Convert_Forward.To_Forward
                (IDLType.Ref
                 (Repository.Get_Primitive
                  (Rep,
                   Pk_Long))),
                   Mode => PARAM_IN);
      PDS.Append (PDS.Sequence (Mem), Memb);

      --  create the operation
      Op := InterfaceDef.Create_Operation (InterfaceDef.Convert_Forward.
                                           To_Ref (Int),
                                           Id,
                                           Name,
                                           Version,
                                           IDLType.Ref
                                           (Repository.Get_Primitive
                                            (Rep,
                                             Pk_Long)),
                                           OP_NORMAL,
                                           Mem,
                                           Exc,
                                           Con);
   end;

   Ref : CORBA.Object.Ref;
   Obj : PortableServer.Servant;
   Calc : Calculators.Simple_Calculator.Impl.Object_Ptr
     := new Calculators.Simple_Calculator.Impl.Object;
   package IDS renames IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward;

   Myrep : Repository.Ref;
   IOR : CORBA.String;
begin
   --------------------------
   --  Register to the IR  --
   --------------------------

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   --  transforms the Ada string into CORBA.String
   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));

   --  getting the CORBA.Object
   CORBA.ORB.String_To_Object (IOR, Myrep);

   --  checking if it worked
   if Repository.Is_Nil (Myrep) then
      Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   --  get/creating the module calculators
   declare
      Mod1 : ModuleDef_Forward.Ref;
--      Look : Contained.Ref
--        := Repository.Lookup_Id (Myrep,
--                                 To_CORBA_String ("IDL:calculators:1.0"));
   begin
--      if Contained.Is_Nil (Look) then
         declare
            Id : RepositoryId;
            Name : Identifier;
            Version : VersionSpec;
         begin
            Id := To_CORBA_String ("IDL:calculators:1.0");
            Name := To_CORBA_String ("calculators");
            Version := To_CORBA_String ("1.0");
            Mod1 := Repository.Create_Module (Myrep,
                                              Id,
                                              Name,
                                              Version);
         end;
--      else
--         Mod1 := ModuleDef.Convert_Forward.To_Forward
--           (ModuleDef.Helper.To_Ref (Look));
--      end if;

      --  create the interface simple_calculator
      declare
         Int : InterfaceDef_Forward.Ref;
         Id : RepositoryId;
         Name : Identifier;
         Version : VersionSpec;
      begin
         Id := To_CORBA_String (Calculators.Simple_Calculator.Repository_Id_Ü);
         Name := To_CORBA_String ("simple_calculator");
         Version := To_CORBA_String ("1.0");
         Int := ModuleDef.Create_Interface (ModuleDef.Convert_Forward.To_Ref (Mod1),
                                             Id,
                                             Name,
                                             Version,
                                             InterfaceDefSeq (IDS.Null_Sequence),
                                             False);

         --  create the first operation of simple_calculator
         declare
            Id : RepositoryId;
            Name : Identifier;
            Version : VersionSpec;
         begin
            Id := To_CORBA_String ("IDL:calculators/simple_calculator/add:1.0");
            Name := To_CORBA_String ("add");
            Version := To_CORBA_String ("1.0");
            Create_Operation (Myrep, Int, Id, Name, Version);
         end;

         --  create the second operation of simple_calculator
         declare
            Id : RepositoryId;
            Name : Identifier;
            Version : VersionSpec;
         begin
            Id := To_CORBA_String ("IDL:calculators/simple_calculator/subtract:1.0");
            Name := To_CORBA_String ("subtract");
            Version := To_CORBA_String ("1.0");
            Create_Operation (Myrep, Int, Id, Name, Version);
         end;

      end;
   end;


   -----------------------------
   --  Inititate  the server  --
   -----------------------------
   Initiate_Servant (PortableServer.Servant (Calc), Ref);
   Ada.Text_IO.Put_Line
     ("'" & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)) &
      "'");
   Initiate_Server;

exception
   when E : CORBA.Bad_Param =>
         declare
            Memb : System_Exception_Members;
         begin
            Get_Members (E, Memb);
            Put ("received Bad_Param exception, minor");
            Put_Line (Unsigned_Long'Image (Memb.Minor));
         end;

end Simple_Server;

