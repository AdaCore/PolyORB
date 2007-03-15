------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1999-2005 Free Software Foundation, Inc.           --
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

with Ada.Command_Line;
with Ada.Text_IO;

with CORBA.ORB;
with CORBA.Repository_Root;
with CORBA.Repository_Root.PrimitiveDef;
with CORBA.Repository_Root.Repository;
with CORBA.Repository_Root.InterfaceDef;
with CORBA.Repository_Root.OperationDef;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.ModuleDef;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

procedure Client is
   use Ada.Text_IO;

   use CORBA;
   use CORBA.Repository_Root;

   IOR : CORBA.String;
   Myrep : Repository.Ref;

begin
   CORBA.ORB.Initialize ("ORB");
   if Ada.Command_Line.Argument_Count = 1 then
      IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));
      CORBA.ORB.String_To_Object (IOR, Myrep);
   else
      Put_Line ("Usage: client <IOR>");
      return;
   end if;

   --  Checking if it worked

   if Repository.Is_Nil (Myrep) then
      Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   --  Creating a module

   declare
      Mod1 : ModuleDef_Forward.Ref;
      Int1 : InterfaceDef_Forward.Ref;
      Op1 : OperationDef.Ref;
      pragma Warnings (Off, Op1);
      --  Op1 is assigned but never read

      Id : RepositoryId;
      Name : Identifier;
      Version : VersionSpec;

      package IDS renames IDL_SEQUENCE_CORBA_InterfaceDef_Forward;

   begin
      Id := To_CORBA_String ("idl:toto:1.1");
      Name := To_CORBA_String ("toto");
      Version := To_CORBA_String ("1.1");
      Mod1 := Repository.create_module (Myrep,
                                        Id,
                                        Name,
                                        Version);
      Id := To_CORBA_String ("idl:toto/titi:1.0");
      Name := To_CORBA_String ("titi");
      Version := To_CORBA_String ("1.0");
      Int1 := ModuleDef.create_interface
        (ModuleDef.Convert_Forward.To_Ref (Mod1),
         Id,
         Name,
         Version,
         InterfaceDefSeq (IDS.Null_Sequence),
         False);
      declare
         package PDS renames IDL_SEQUENCE_CORBA_ParameterDescription;
         package EDS renames IDL_SEQUENCE_CORBA_ExceptionDef_Forward;
         package CIS renames IDL_SEQUENCE_CORBA_ContextIdentifier;
         Mem : ParDescriptionSeq
           := ParDescriptionSeq (PDS.Null_Sequence);
         Exc : ExceptionDefSeq
           := ExceptionDefSeq (EDS.Null_Sequence);
         Con : ContextIdSeq
           := ContextIdSeq (CIS.Null_Sequence);
         Memb : ParameterDescription;
      begin

         --  Create the members

         Name := To_CORBA_String ("oper1");
         Memb :=  (Name => Name,
                   IDL_Type => TC_Long,
                   Type_Def => IDLType.Convert_Forward.To_Forward
                   (IDLType.Ref
                    (PrimitiveDef.Convert_Forward.To_Ref
                     (Repository.get_primitive
                      (Myrep,
                       pk_long)))),
                   Mode => PARAM_IN);
         PDS.Append (PDS.Sequence (Mem), Memb);

         Name := To_CORBA_String ("oper2");
         Memb :=  (Name => Name,
                   IDL_Type => TC_Long,
                   Type_Def => IDLType.Convert_Forward.To_Forward
                   (IDLType.Ref
                    (PrimitiveDef.Convert_Forward.To_Ref
                     (Repository.get_primitive
                      (Myrep,
                       pk_long)))),
                   Mode => PARAM_IN);
         PDS.Append (PDS.Sequence (Mem), Memb);

         --  Create the operation

         Id := To_CORBA_String ("idl:toto/titi/myop:1.1");
         Name := To_CORBA_String ("myop");
         Version := To_CORBA_String ("1.1");
         Op1 := InterfaceDef.create_operation
           (InterfaceDef.Convert_Forward.
            To_Ref (Int1),
            Id,
            Name,
            Version,
            IDLType.Ref
            (PrimitiveDef.Convert_Forward.To_Ref
             (Repository.get_primitive
              (Myrep,
               pk_long))),
            OP_NORMAL,
            Mem,
            Exc,
            Con);

      end;
   end;

exception
   when E : CORBA.Bad_Param =>
      declare
         Memb : System_Exception_Members;
      begin
         Get_Members (E, Memb);
         Put ("received Bad_Param exception, minor");
         Put_Line (Unsigned_Long'Image (Memb.Minor));
      end;
end Client;
