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
with Ada.Interrupts; use Ada.Interrupts;
with Ada.Interrupts.Names;

with Handler;

with CORBA; use CORBA;
with CORBA.ORB;
with CORBA.Object;

with PortableServer;
with Broca.Server_Tools; use Broca.Server_Tools;
pragma Elaborate (Broca.Server_Tools);

with Calculators.Calculator_Plus;
with Calculators.Calculator_Plus.Impl;

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.Repository;
with CORBA.Repository_Root.Repository.Helper;
with CORBA.Repository_Root.Container;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.InterfaceDef;
with CORBA.Repository_Root.OperationDef;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.ModuleDef;
with CORBA.Repository_Root.ModuleDef.Helper;

with Naming_Tools;
with CosNaming.NamingContext;

with Ada.Text_IO; use Ada.Text_IO;

with StringSeq;
with Interfaces.C;

procedure Server is

   procedure Create_Operation (Rep : Repository.Ref;
                               Int : in InterfaceDef_Forward.Ref;
                               Id : in RepositoryId;
                               Name : in Identifier;
                               Version : in VersionSpec;
                               Parameters : StringSeq.Element_Array) is

      Op : OperationDef.Ref;
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
      for I in Parameters'Range loop

         --  create the members
         Memb :=  (Name => Identifier (Parameters (I)),
                   IDL_Type => TC_Long,
                   Type_Def => IDLType.Convert_Forward.To_Forward
                   (IDLType.Ref
                    (Repository.Get_Primitive
                     (Rep,
                      Pk_Long))),
                   Mode => PARAM_IN);
         PDS.Append (PDS.Sequence (Mem), Memb);
      end loop;

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
   Calc : Calculators.Calculator_Plus.Impl.Object_Ptr
     := new Calculators.Calculator_Plus.Impl.Object;
   package IDS renames IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward;

   --  this task is waiting for a SIGINT and removes
   --  this server from the naming service when it occurs
   task Wait_SIGINT;
   task body Wait_SIGINT is
      procedure Quit (I : in Interfaces.C.int);
      pragma Import (C, quit, "exit");
   begin
      Handler.Handle_Kill.Wait;
      Put_Line ("Removing this server from cosnaming");
      Naming_Tools.Unregister ("calculators/calculator_plus");
      Put_Line ("Thank you for using our servers.");
      Quit (Interfaces.C.Int (2));
   end;

   Myrep : Repository.Ref;
   Look_Is_Nil : Boolean;
begin
   --------------------------
   --  Register to the IR  --
   --------------------------
   Myrep := Repository.Helper.To_Ref
     (Naming_Tools.Locate ("Interface_Repository"));

   --  checking if it worked
   if Repository.Is_Nil (Myrep) then
      Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   --  get/creating the module calculators
   declare
      Mod1 : ModuleDef_Forward.Ref;
      Look : Contained.Ref;
   begin
      begin
         Look := Repository.Lookup_Id (Myrep,
                                       To_CORBA_String ("IDL:calculators:1.0"));
         Look_Is_Nil := Contained.Is_Nil (Look);
         --  This is a work arround to supply a bug of the ORB
      exception
         when others =>
            Look_Is_Nil := True;
      end;
      if Look_Is_Nil then
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
      else
         Mod1 := ModuleDef.Convert_Forward.To_Forward
         (ModuleDef.Helper.To_Ref (Look));
      end if;

      --  create the interface Calculator_Plus
      begin
         Look := Repository.Lookup_Id (Myrep,
                                       To_CORBA_String ("IDL:calculators/calculator_plus:1.0"));
         Look_Is_Nil := Contained.Is_Nil (Look);
         --  This is a work-around to supply a bug of the ORB
      exception
         when others =>
            Look_Is_Nil := True;
      end;
      if Look_Is_Nil then
         declare
            Int : InterfaceDef_Forward.Ref;
            Id : RepositoryId;
            Name : Identifier;
            Version : VersionSpec;
         begin
            Id := To_CORBA_String (Calculators.Calculator_Plus.Repository_Id);
            Name := To_CORBA_String ("calculator_plus");
            Version := To_CORBA_String ("1.0");
            Int := ModuleDef.Create_Interface (ModuleDef.Convert_Forward.To_Ref (Mod1),
                                               Id,
                                               Name,
                                               Version,
                                               InterfaceDefSeq (IDS.Null_Sequence),
                                               False);

            --  create the first operation of Calculator_Plus
            declare
               Id : RepositoryId;
               Name : Identifier;
               Version : VersionSpec;
               Parameters : StringSeq.Element_Array :=
                 (CORBA.To_CORBA_String ("Left_Op"),
                  CORBA.To_CORBA_String ("Right_Op"));
            begin
               Id := To_CORBA_String ("IDL:calculators/calculator_plus/add:1.0");
               Name := To_CORBA_String ("add");
               Version := To_CORBA_String ("1.0");
               Create_Operation (Myrep, Int, Id, Name, Version, Parameters);
            end;

            --  create the second operation of Calculator_Plus
            declare
               Id : RepositoryId;
               Name : Identifier;
               Version : VersionSpec;
               Parameters : StringSeq.Element_Array :=
                 (CORBA.To_CORBA_String ("Left_Op"),
                  CORBA.To_CORBA_String ("Right_Op"));
            begin
               Id := To_CORBA_String ("IDL:calculators/calculator_plus/subtract:1.0");
               Name := To_CORBA_String ("subtract");
               Version := To_CORBA_String ("1.0");
               Create_Operation (Myrep, Int, Id, Name, Version, Parameters);
            end;

            --  create the third operation of Calculator_Plus
            declare
               Id : RepositoryId;
               Name : Identifier;
               Version : VersionSpec;
               Parameters : StringSeq.Element_Array :=
                 (CORBA.To_CORBA_String ("Left_Op"),
                  CORBA.To_CORBA_String ("Right_Op"));
            begin
               Id := To_CORBA_String ("IDL:calculators/calculator_plus/multiply:1.0");
               Name := To_CORBA_String ("multiply");
               Version := To_CORBA_String ("1.0");
               Create_Operation (Myrep, Int, Id, Name, Version, Parameters);
            end;

            --  create the fourth operation of Calculator_Plus
            declare
               Id : RepositoryId;
               Name : Identifier;
               Version : VersionSpec;
               Parameters : StringSeq.Element_Array :=
                 (CORBA.To_CORBA_String ("Left_Op"),
                  CORBA.To_CORBA_String ("Right_Op"));
            begin
               Id := To_CORBA_String ("IDL:calculators/calculator_plus/divide:1.0");
               Name := To_CORBA_String ("divide");
               Version := To_CORBA_String ("1.0");
               Create_Operation (Myrep, Int, Id, Name, Version, Parameters);
            end;

            --  create the fifth operation of Calculator_Plus
            declare
               Id : RepositoryId;
               Name : Identifier;
               Version : VersionSpec;
               Parameters : StringSeq.Element_Array :=
                 (CORBA.To_CORBA_String ("Op1"),
                  CORBA.To_CORBA_String ("Op2"),
                  CORBA.To_CORBA_String ("Op3"));
            begin
               Id := To_CORBA_String ("IDL:calculators/calculator_plus/add3:1.0");
               Name := To_CORBA_String ("add3");
               Version := To_CORBA_String ("1.0");
               Create_Operation (Myrep, Int, Id, Name, Version, Parameters);
            end;

            --  create the sixth operation of Calculator_Plus
            declare
               Id : RepositoryId;
               Name : Identifier;
               Version : VersionSpec;
               Parameters : StringSeq.Element_Array :=
                 (CORBA.To_CORBA_String ("Op1"),
                  CORBA.To_CORBA_String ("Op2"),
                  CORBA.To_CORBA_String ("Op3"),
                  CORBA.To_CORBA_String ("Op4"));
            begin
               Id := To_CORBA_String ("IDL:calculators/calculator_plus/add4:1.0");
               Name := To_CORBA_String ("add4");
               Version := To_CORBA_String ("1.0");
               Create_Operation (Myrep, Int, Id, Name, Version, Parameters);
            end;

            --  create the seventh operation of Calculator_Plus
            declare
               Id : RepositoryId;
               Name : Identifier;
               Version : VersionSpec;
               Parameters : StringSeq.Element_Array (1 .. 1);
            begin
               Parameters (1) := CORBA.To_CORBA_String ("Op");
               Id := To_CORBA_String ("IDL:calculators/calculator_plus/square:1.0");
               Name := To_CORBA_String ("square");
               Version := To_CORBA_String ("1.0");
               Create_Operation (Myrep, Int, Id, Name, Version, Parameters);
            end;

         end;
      end if;
   end;


   -----------------------------
   --  Inititate  the server  --
   -----------------------------
   Initiate_Servant (PortableServer.Servant (Calc), Ref);
--   Ada.Text_IO.Put_Line
--     ("'" & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)) &
--      "'");

   ------------------------------
   --  Register to the Naming  --
   ------------------------------
   begin
      Naming_Tools.Register ("calculators/calculator_plus", Ref);
   exception
      when CosNaming.NamingContext.AlreadyBound =>
         Naming_Tools.Register ("calculators/calculator_plus", Ref, Rebind => True);
   end;

   --------------------------------
   --  Unregister to the naming  --
   --------------------------------
   --  deal with the SIGINT Signal
   Attach_Handler (Handler.Handle_Kill.Response'Access,
                   Names.SIGINT);

   ----------------------
   --  Run the server  --
   ----------------------
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

end Server;
