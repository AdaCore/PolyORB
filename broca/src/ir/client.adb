------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                               C L I E N T                                --
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

--   echo client.
with Ada.Command_Line;
with Text_IO; use Text_IO;
with CORBA; use CORBA;
with CORBA.ORB;
with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.Repository;
with CORBA.Repository_Root.Container;
with CORBA.Repository_Root.Container.Helper;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.Moduledef.Helper;
with CORBA.Repository_Root.UnionDef.Helper;
with CORBA.Repository_Root.StructDef.Helper;
with CORBA.Repository_Root.InterfaceDef.Helper;
with CORBA.Repository_Root.ExceptionDef.Helper;
with CORBA.Repository_Root.ValueDef.Helper;

procedure Client is

   procedure Print_Content (In_Seq : ContainedSeq;
                            Inc : Standard.String) is

      Package Contained_For_Seq renames
        CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward;
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
                      (Get_Def_Kind (The_Ref)));
            Put_Line (Inc & "Name     : " &
                      CORBA.To_Standard_String
                      (CORBA.String (Get_Name (The_Ref))));
            Put_Line (Inc & "Id       : " &
                      CORBA.To_Standard_String
                      (CORBA.String (Get_Id (The_Ref))));
            Put_Line (Inc & "Vers     : " &
                      CORBA.To_Standard_String
                      (CORBA.String (Get_Version (The_Ref))));
            Put_Line (Inc & "Abs-Name : " &
                      CORBA.To_Standard_String
                      (CORBA.String
                       (Get_Absolute_Name (The_Ref))));
            Put_Line (" ");

            --  recursivity
            case Contained.Get_Def_Kind (The_Ref) is
               when Dk_Module =>
                  declare
                     R : Container.Ref := Container.Helper.To_Ref
                       (ModuleDef.Helper.To_Ref (The_Ref));
                  begin
                     Print_Content (Container.Contents (R,
                                                        Dk_All,
                                                        True),
                                    Inc & "          ");
                  end;
               when Dk_Exception =>
                  declare
                     R : Container.Ref := Container.Helper.To_Ref
                       (Exceptiondef.Helper.To_Ref (The_Ref));
                  begin
                     Print_Content (Container.Contents (R,
                                                        Dk_All,
                                                        True),
                                    Inc & "          ");
                  end;
               when Dk_Interface =>
                  declare
                     R : Container.Ref := Container.Helper.To_Ref
                       (InterfaceDef.Helper.To_Ref (The_Ref));
                  begin
                     Print_Content (Container.Contents (R,
                                                        Dk_All,
                                                        True),
                                    Inc & "          ");
                  end;
               when Dk_Value =>
                  declare
                     R : Container.Ref := Container.Helper.To_Ref
                       (ValueDef.Helper.To_Ref (The_Ref));
                  begin
                     Print_Content (Container.Contents (R,
                                                        Dk_All,
                                                        True),
                                    Inc & "          ");
                  end;
               when Dk_Struct =>
                  declare
                     R : Container.Ref := Container.Helper.To_Ref
                       (StructDef.Helper.To_Ref (The_Ref));
                  begin
                     Print_Content (Container.Contents (R,
                                                        Dk_All,
                                                        True),
                                    Inc & "          ");
                  end;
               when Dk_Union =>
                  declare
                     R : Container.Ref := Container.Helper.To_Ref
                       (UnionDef.Helper.To_Ref (The_Ref));
                  begin
                     Print_Content (Container.Contents (R,
                                                        Dk_All,
                                                        True),
                                    Inc & "          ");
                  end;
               when others =>
                  null;
            end case;
         end;
      end loop;

   end;

   Sent_Msg, Rcvd_Msg, IOR : CORBA.String;
   Myrep : Repository.Ref;

begin

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

   --  creating a module
   declare
      Mod1 : ModuleDef_Forward.Ref;
      Int1 : InterfaceDef_Forward.Ref;
      Int2 : InterfaceDef_Forward.Ref;
      Id : RepositoryId;
      Name : Identifier;
      Version : VersionSpec;
      package IDS renames IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward;

   begin
      Id := To_CORBA_String ("idl:tutu:1.0");
      Name := To_CORBA_String ("tutu");
      Version := To_CORBA_String ("1.0");
      Int2 := Repository.Create_Interface (Myrep,
                                           Id,
                                           Name,
                                           Version,
                                           InterfaceDefSeq (IDS.Null_Sequence),
                                           False);
      Id := To_CORBA_String ("idl:toto:1.1");
      Name := To_CORBA_String ("toto");
      Version := To_CORBA_String ("1.1");
      Mod1 := Repository.Create_Module (Myrep,
                                        Id,
                                        Name,
                                        Version);
      Id := To_CORBA_String ("idl:titi:1.0");
      Name := To_CORBA_String ("titi");
      Version := To_CORBA_String ("1.0");
      Int1 := ModuleDef.Create_Interface (ModuleDef.Convert_Forward.To_Ref (Mod1),
                                          Id,
                                          Name,
                                          Version,
                                          InterfaceDefSeq (IDS.Null_Sequence),
                                          False);
   end;

   Print_Content (Repository.Contents (Myrep,
                                       Dk_All,
                                       True),
                  " ");

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
