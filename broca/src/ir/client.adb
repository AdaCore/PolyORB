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
with CORBA.Repository_Root.Repository.Impl;
with CORBA.Repository_Root.Container;
--  with CORBA.Repository_Root.Moduledef;

procedure Client is
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
      Id : RepositoryId;
      Name : Identifier;
      Version : VersionSpec;
   begin
      Id := To_CORBA_String ("idl:toto:1.1");
      Name := To_CORBA_String ("toto");
      Version := To_CORBA_String ("1.1");
      Mod1 := Repository.Create_Module (Myrep,
                                        Id,
                                        Name,
                                        Version);
   end;

exception
   when E : CORBA.Transient =>
      declare
         Memb : System_Exception_Members;
      begin
         Get_Members (E, Memb);
         Put ("received exception transient, minor");
         Put (Unsigned_Long'Image (Memb.Minor));
         Put (", completion status: ");
         Put_Line (Completion_Status'Image (Memb.Completed));
      end;
end Client;
