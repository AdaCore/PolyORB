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

--  Calculator adaptative client

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Io_Exceptions;

with Broca.Server_Tools; use Broca.Server_Tools;
pragma Elaborate (Broca.Server_Tools);

with CORBA; use CORBA;
with CORBA.Object;
with CORBA.Object.Helper;
with CORBA.Context;
with CORBA.Request;
with CORBA.NVList;
with CORBA.ContextList;
with CORBA.ExceptionList;
with CORBA.ORB;

with StringSeq;
with LongSeq;

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.Repository;
with CORBA.Repository_Root.Repository.Helper;
with CORBA.Repository_Root.InterfaceDef;
with CORBA.Repository_Root.ModuleDef;
with CORBA.Repository_Root.ModuleDef.Helper;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.InterfaceDef.Helper;
with CORBA.Repository_Root.Helper;

with Broca.Naming_Tools; use Broca.Naming_Tools;

with Interfaces.C;

procedure Client is
   IOR_RepId : CORBA.String;
   RepId : Repository.Ref;
   package Long_IO is new Integer_IO (CORBA.Long);
   End_Of_Game : exception;
   Prefix : CORBA.String := To_Corba_String ("IDL:");
   Postfix : CORBA.String := To_Corba_String (":1.0");
   Separator : CORBA.String := To_Corba_String ("/");
   Module_name : CORBA.String := To_Corba_String ("calculators");

   procedure Quit (I : in Interfaces.C.int);
   pragma Import (C, quit, "exit");

   function Generic_Function
     (Server : in CORBA.Object.Ref;
      Operation_Name : in CORBA.Identifier;
      Operands : in LongSeq.Sequence;
      Operand_Names : in StringSeq.Sequence)
     return CORBA.Long is
      Request : CORBA.Request.Object;
      Ctx : CORBA.Context.Ref := CORBA.Context.Nil_Ref;
      Argument : CORBA.Any;
      Arg_List : CORBA.NVList.Ref;
      Result_Name : CORBA.String := To_CORBA_String ("Result");
      Result : CORBA.NamedValue;
   begin
      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List);
      for I in 1 .. StringSeq.Length (Operand_Names) loop
         Argument := CORBA.To_Any (LongSeq.Element_Of (Operands, I));
         CORBA.NVList.Add_Item (Arg_List,
                                Identifier (StringSeq.Element_Of (Operand_Names, I)),
                                Argument,
                                CORBA.ARG_IN);
      end loop;
      --  setting the result type
      Result := (Name => Identifier (Result_Name),
                 Argument => Get_Empty_Any (CORBA.TC_Long),
                 Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Server,
                                   Ctx,
                                   Operation_Name,
                                   Arg_List,
                                   Result,
                                   Request,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request, 0);
      --  getting the answer
      return From_Any (Result.Argument);
   end Generic_Function;


   function Get_Parameter_Seq (Op : Contained.Ref) return StringSeq.Sequence is
      Result : StringSeq.Sequence := StringSeq.Null_Sequence;
      --  select all the available operations
      Cont_Desc : Contained.Description := Contained.Describe (Op);
      D : OperationDescription :=
        CORBA.Repository_Root.Helper.From_Any (Cont_Desc.Value);
      DescSeq : ParDescriptionSeq := D.Parameters;
      package PDS renames
        IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription;
      A : PDS.Element_Array
        := PDS.To_Element_Array (PDS.Sequence (DescSeq));
   begin
      for I in A'Range loop
         StringSeq.Append (Result, CORBA.String (A (I).Name));
      end loop;
      return Result;
   end Get_Parameter_Seq;


   function Get_Operation_List (Int : InterfaceDef.Ref) return StringSeq.Sequence is
      Result : StringSeq.Sequence := StringSeq.Null_Sequence;
      --  select all the available operations
      Content : ContainedSeq := InterfaceDef.Contents (Int,
                                                       Dk_Operation,
                                                       True);
      Package CFS renames IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward;
      Cont_Array : CFS.Element_Array := CFS.To_Element_Array (CFS.Sequence (Content));
   begin
      for I in Cont_Array'Range loop
         declare
            The_Ref : Contained.Ref := Contained.Convert_Forward.To_Ref (Cont_Array (I));
         begin
            StringSeq.Append (Result, CORBA.String (Contained.Get_Name (The_Ref)));
         end;
      end loop;
      return Result;
   end Get_Operation_List;

   procedure Print_Operations (Name : CORBA.String) is
      Serv : InterfaceDef.Ref := InterfaceDef.Helper.To_Ref
        (Repository.Lookup_Id (RepId,
                               CORBA.RepositoryId
                               (Prefix & Module_Name & Separator
                                & Name & Postfix)));
      Op_List : StringSeq.Sequence := Get_Operation_List (Serv);
      Op_Array : StringSeq.Element_Array
        := StringSeq.To_Element_Array (Op_List);
   begin
      for I in Op_Array'Range loop
         declare
            Operation_Ref : Contained.Ref
              := Repository.Lookup_Id
              (RepId,
               CORBA.RepositoryId
               (Prefix & Module_Name & Separator &
                Name & Separator & Op_Array (I) & Postfix));
            Parameter_Names : StringSeq.Sequence
              := Get_Parameter_Seq (Operation_Ref);
            Parameter_Array : StringSeq.Element_Array
              := StringSeq.To_Element_Array (Parameter_Names);
            Is_First : Boolean := True;
         begin
            Put ("function " &
                 To_Standard_String (Op_Array (I))
                 & "(");
            for J in Parameter_Array'Range loop
               if Is_First then
                  Is_First := False;
               else
                  Put ("; ");
               end if;
               Put (To_Standard_String (Parameter_Array (J)) &
                    " : Long");
            end loop;
            Put_Line (") return Long;");
         end;
      end loop;
   end;

   function Get_Integer (Prompt : in String) return CORBA.Long is
      Line : String (1..50);
      End_Of_String : Natural;
      Last : Positive;
      Op : CORBA.Long;
      Bad_Arg : Boolean := True;
   begin
      while Bad_Arg loop
         Put (Prompt & " : ");
         Get_Line (Line, End_Of_String);
         if Line (1..End_Of_String) = "quit" then
            raise End_Of_Game;
         end if;
         begin
            Long_IO.Get (Line (1..End_Of_String), Op, Last);
            Bad_Arg := False;
         exception
            when ADA.IO_EXCEPTIONS.DATA_ERROR =>
               Put_Line ("Type an integer please");
               Bad_Arg := True;
         end;
      end loop;
      return Op;
   end Get_Integer;

   function Choose_Server return CORBA.String;

   function Get_Name (Prompt : in String;
                      Name_List : in StringSeq.Sequence;
                      Un_Name_List : in StringSeq.Sequence)
                      return CORBA.String is
      Line : String (1..100);
      End_Of_String : Natural;
      String_Array : StringSeq.Element_Array := StringSeq.To_Element_Array (Name_List);
      Un_String_Array : StringSeq.Element_Array := StringSeq.To_Element_Array (Un_Name_List);
      Index, Un_Index : Natural;
      Put_Available : Boolean := False;
   begin
      if (StringSeq.Length (Name_List) = 0) and
        (StringSeq.Length (Un_Name_List) = 0) then
         Put_Line ("There is nothing to choose!");
         raise End_Of_Game;
      end if;

      Put_Line (Prompt & " :");
      if StringSeq.Length (Un_Name_List) /= 0 then
         Put_Available := True;
         for I in Un_String_Array'Range loop
            Put_line ("     " &
                      To_Standard_String (Un_String_Array (I)) &
                      " (unavailable)");
         end loop;
      end if;
      for I in String_Array'Range loop
         Put ("     " &
              To_Standard_String (String_Array (I)));
         if Put_Available then
            Put_Line (" (available)");
         else
            Put_Line ("");
         end if;
      end loop;
      Put_Line ("");
      Put ("Give your choice : ");
      Get_Line (Line, End_Of_String);
      if Line (1 .. End_Of_String) = "quit" then
         raise End_Of_Game;
      end if;
      declare
         Ar : StringSeq.Element_Array (1 .. 1);
      begin
         Ar (1) := To_Corba_String (Line (1 .. End_Of_String));
         Index := StringSeq.Index (Name_List,
                                   Ar);
         Un_Index := StringSeq.Index (Un_Name_List,
                                      Ar);
         if (Index = 0) and (Un_Index = 0) then
            Put_Line ("This is not a valid entry.");
            return Choose_Server;
         else
            if Un_Index /= 0 then
               Put_Line ("");
               Put_Line ("The available operations are");
               Print_Operations (Un_String_Array (Un_Index));
               Put_Line ("");
               Put_Line ("But this selection is not available,");
               Put_Line ("You will have to choose another one.");
               return Choose_Server;
            end if;
         end if;
      end;
      return String_Array (Index);
   end Get_Name;

   function Choose_Server return CORBA.String is
      Module : Contained.Ref;
      Look_Is_Nil : Boolean;
   begin
      begin
         Module := Repository.Lookup_Id
           (RepId,
            CORBA.RepositoryId (Prefix & Module_Name & Postfix));
         Look_Is_Nil := Contained.Is_Nil (Module);
         --  This is a work-around to supply a bug of the ORB
      exception
         when others =>
            Look_Is_Nil := True;
      end;

      if Look_Is_Nil then
         Put_Line ("There is no server available");
         raise End_Of_Game;
      end if;

      declare
         Servers : StringSeq.Sequence := StringSeq.Null_Sequence;
         Un_Servers : StringSeq.Sequence := StringSeq.Null_Sequence;
         Content : ContainedSeq
           := ModuleDef.Contents (ModuleDef.Helper.To_Ref (Module),
                                  Dk_Interface,
                                  True);
         Package CFS renames IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward;
         Cont_Array : CFS.Element_Array
           := CFS.To_Element_Array (CFS.Sequence (Content));
      begin
         for I in Cont_Array'Range loop
            declare
               The_Ref : Contained.Ref := Contained.Convert_Forward.To_Ref (Cont_Array (I));
               The_Name : CORBA.String := CORBA.String (Contained.Get_Name (The_Ref));
               The_Useless_Ref : CORBA.Object.Ref;
            begin
               begin
                  The_Useless_Ref := Locate
                    (To_Standard_String (Module_Name & Separator & The_Name));
                  StringSeq.Append (Servers, The_Name);
               exception
                  when others =>
                     StringSeq.Append (Un_Servers, The_Name);
               end;
            end;
         end loop;
         return Get_Name ("The servers are",
                          Servers, Un_Servers);
      end;
   end Choose_Server;

   procedure Server_Loop (Serv_Ref : InterfaceDef.Ref;
                          Server_Name : CORBA.String) is
      Operations : StringSeq.Sequence := Get_Operation_List (Serv_Ref);
      Operation_Name : CORBA.String;
      Operation_Ref : Contained.Ref;
      Parameter_Names : StringSeq.Sequence := StringSeq.Null_Sequence;
      Parameters : LongSeq.Sequence := LongSeq.Null_Sequence;
   begin
      Operation_Name := Get_Name ("The available operations are", Operations, StringSeq.Null_Sequence);
      Operation_Ref := Repository.Lookup_Id
        (RepId,
         CORBA.RepositoryId
         (Prefix & Module_Name & Separator &
          Server_Name & Separator & Operation_Name & Postfix));
      Parameter_Names := Get_Parameter_Seq (Operation_Ref);
      declare
         Param_Array : StringSeq.Element_Array
           := StringSeq.To_Element_Array (Parameter_Names);
      begin
         for I in Param_Array'Range loop
            LongSeq.Append (Parameters,
                            Get_Integer ("Please give " &
                                         To_Standard_String (Param_Array (I))));
         end loop;
         Put_Line ("The result of this operation is : " &
                   CORBA.Long'Image (Generic_Function
                                     (Locate
                                      (To_Standard_String
                                       (Module_Name & Separator & Server_Name)),
                                      Identifier (Operation_Name),
                                      Parameters,
                                      Parameter_Names)));
      end;
   end Server_Loop;

   procedure Main_Loop is
      Server_Name : CORBA.String;
      Serv_Ref : InterfaceDef.Ref;
   begin
      Server_Name := Choose_Server;
      Serv_Ref := InterfaceDef.Helper.To_Ref
        (Repository.Lookup_Id (RepId,
                               CORBA.RepositoryId
                               (Prefix & Module_Name & Separator
                                & Server_Name & Postfix)));
      begin
         while True loop
            Server_Loop (Serv_Ref, Server_Name);
         end loop;
      exception
         when End_Of_Game =>
            null;
      end;
   end Main_Loop;

begin
   RepId := Repository.Helper.To_Ref
     (Locate ("Interface_Repository"));

   --  Enter the main loop
   Put_Line ("This is The Calculator Example");
   Put_Line ("You can quit every time by typing ""quit"".");
   Put_Line("");
   Put_Line("############################################");
   Put_Line("");
   while true loop
      Main_Loop;
   end loop;

exception
   when End_Of_Game =>
      --  End of the game
      Put_Line ("Thank you for using our adaptative calculator...");
      Quit (Interfaces.C.Int (2));
end Client;



