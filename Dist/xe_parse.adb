------------------------------------------------------------------------------
--                                                                          --
--                          GNATDIST COMPONENTS                             --
--                                                                          --
--                             X E _ P A R S E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            1.21                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
--                                                                          --
-- GNATDIST is  free software;  you  can redistribute  it and/or  modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version. GNATDIST is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General Public License distributed with  GNATDIST; see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--              GNATDIST is maintained by ACT Europe.                       --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------
with Osint;            use Osint;
with Output;           use Output;
with Namet;            use Namet;
with Types;            use Types;
with XE_Scan;          use XE_Scan;
with XE_Utils;         use XE_Utils;
with XE;               use XE;

package body XE_Parse is

   type Predefined_Object is
      record
         Oname : Name_Id;
         Otype : Predefined_Type;

      end record;

   procedure Legal_Partition_Name (Name : Name_Id);
   function  Get_Defined_PID (Name : Name_Id) return PID_Type;

   --  XXXXX: temporary.
   Pgm_S_Method : Name_Id;
   Pgm_SM_Ada   : Name_Id;
   Pgm_SM_Shell : Name_Id;
   Pgm_SM_None  : Name_Id;

   Max_Line_Length   : constant Natural := 1024;

   procedure Take_Token (T : Token_Type);
   procedure Take_Token (L : Token_List_Type);

   function  Match (L : Token_List_Type) return Boolean;

   procedure No_Match (L : Token_List_Type);
   procedure No_Match (T : Token_Type);

   procedure T_String_Literal;
   procedure T_Identifier;
   procedure T_Dot;
   procedure T_Apostrophe;
   procedure T_Left_Paren;
   procedure T_Right_Paren;
   procedure T_Comma;
   procedure T_Colon_Equal;
   procedure T_Colon;
   procedure T_Configuration;
   procedure T_Pragma;
   procedure T_Procedure;
   procedure T_Is;
   procedure T_In;
   procedure T_For;
   procedure T_Use;
   procedure T_Function;
   procedure T_End;
   procedure T_Arrow;
   procedure T_EOF;
   procedure T_Semicolon;

   procedure P_Configuration_End;
   procedure P_Pragma;
   procedure P_Pragma_Starter;
   procedure P_Main_Subprogram;
   procedure P_Partition_Clause;
   procedure P_Full_Ada_Identifier;
   procedure P_Configuration_Body;
   function  P_Attribute_Name
     return Attribute_Type;
   procedure P_Package_Enumeration
     (Partition : in Name_Id);
   function  P_Object_Declaration
     (Prev : in Name_Id;
      Many : in Int)
      return Predefined_Object;
   procedure Init_Previous_Object
     (Prev : in Name_Id;
      Next : in Predefined_Object);

   --------------
   -- No_Match --
   --------------

   procedure No_Match (L : Token_List_Type) is
   begin
      Write_Location (Get_Token_Location);
      Write_Token (L (L'First));
      for Index in L'First + 1 .. L'Last loop
         Write_Str (" or ");
         Write_Token (L (Index));
      end loop;
      Write_Str (" was expected");
      Write_Eol;
      raise Parsing_Error;
   end No_Match;

   --------------
   -- No_Match --
   --------------

   procedure No_Match (T : Token_Type) is
   begin
      Write_Location (Get_Token_Location);
      Write_Str ("missing ");
      Write_Token (T);
      Write_Eol;
      raise Parsing_Error;
   end No_Match;

   -----------
   -- Match --
   -----------

   function Match (L : Token_List_Type) return Boolean is
   begin
      for Index in L'Range loop
         if L (Index) = Token then
            return True;
         end if;
      end loop;
      return False;
   end Match;

   ----------------
   -- Take_Token --
   ----------------

   procedure Take_Token (T : Token_Type) is
   begin
      Next_Token;
      if T /= Token then
         No_Match (T);
      end if;
   end Take_Token;

   ----------------
   -- Take_Token --
   ----------------

   procedure Take_Token (L : Token_List_Type) is
   begin
      Next_Token;
      if Match (L) then
         return;
      end if;
      No_Match (L);
   end Take_Token;

   procedure T_String_Literal is
   begin
      Take_Token (Tok_String_Literal);
   end T_String_Literal;

   procedure T_Identifier is
   begin
      Take_Token (Tok_Identifier);
   end T_Identifier;

   procedure T_Dot is
   begin
      Take_Token (Tok_Dot);
   end T_Dot;

   procedure T_Apostrophe is
   begin
      Take_Token (Tok_Apostrophe);
   end T_Apostrophe;

   procedure T_Left_Paren is
   begin
      Take_Token (Tok_Left_Paren);
   end T_Left_Paren;

   procedure T_Right_Paren is
   begin
      Take_Token (Tok_Right_Paren);
   end T_Right_Paren;

   procedure T_Comma is
   begin
      Take_Token (Tok_Comma);
   end T_Comma;

   procedure T_Colon_Equal is
   begin
      Take_Token (Tok_Colon_Equal);
   end T_Colon_Equal;

   procedure T_Colon is
   begin
      Take_Token (Tok_Colon);
   end T_Colon;

   procedure T_Configuration is
   begin
      Take_Token (Tok_Configuration);
   end T_Configuration;

   procedure T_Pragma is
   begin
      Take_Token (Tok_Pragma);
   end T_Pragma;

   procedure T_Procedure is
   begin
      Take_Token (Tok_Procedure);
   end T_Procedure;

   procedure T_Is is
   begin
      Take_Token (Tok_Is);
   end T_Is;

   procedure T_In is
   begin
      Take_Token (Tok_In);
   end T_In;

   procedure T_For is
   begin
      Take_Token (Tok_For);
   end T_For;

   procedure T_Use is
   begin
      Take_Token (Tok_Use);
   end T_Use;

   procedure T_Function is
   begin
      Take_Token (Tok_Function);
   end T_Function;

   procedure T_End is
   begin
      Take_Token (Tok_End);
   end T_End;

   procedure T_Arrow is
   begin
      Take_Token (Tok_Arrow);
   end T_Arrow;

   procedure T_EOF is
   begin
      Take_Token (Tok_EOF);
   end T_EOF;

   procedure T_Semicolon is
   begin
      Take_Token (Tok_Semicolon);
   end T_Semicolon;

   ----------------------
   -- P_Attribute_Name --
   ----------------------

   function P_Attribute_Name return Attribute_Type is
      Attribute : Attribute_Type;
   begin

      T_Identifier;

      Attribute := Get_Attribute (Token_Name);
      if Attribute = Att_Unknown then
         Write_Location (Get_Token_Location);
         Write_Str (": host or storage_dir attribute was expected");
         Write_Eol;
         raise Parsing_Error;
      end if;
      return Attribute;

   end P_Attribute_Name;

   -------------------------
   -- P_Configuration_End --
   -------------------------

   procedure P_Configuration_End is
   begin

      Take_Token ((Tok_Identifier, Tok_Semicolon));
      if Token = Tok_Identifier then
         if Token_Name /= Configuration then
            Write_Location (Get_Token_Location);
            Write_Str ("name mismatch");
            Write_Eol;
            raise Parsing_Error;
         end if;
         T_Semicolon;
      end if;

   end P_Configuration_End;

   --------------
   -- P_Pragma --
   --------------

   procedure P_Pragma is
   begin
      T_Identifier;
      if Get_Pragma (Token_Name) /= Pgm_Starter then
         Write_Location (Get_Token_Location);
         Write_Str  (": pragma ");
         Write_Name (Token_Name);
         Write_Str  (" not supported");
         Write_Eol;
      end if;
      P_Pragma_Starter;
      T_Semicolon;
   end P_Pragma;

   ----------------------
   -- P_Pragma_Starter --
   ----------------------

   procedure P_Pragma_Starter is
   begin
      T_Left_Paren;
      T_Identifier;
      if Token_Name = Pgm_S_Method then
         T_Arrow;
         T_Identifier;
      end if;
      if Token_Name = Pgm_SM_Ada then
         Starter_Method := Ada_Starter;
      elsif Token_Name = Pgm_SM_Shell then
         Starter_Method := Shell_Starter;
      elsif Token_Name = Pgm_SM_None  then
         Starter_Method := None_Starter;
      else
         Write_Location (Get_Token_Location);
         Write_Str (": unexpected pragma argument");
         Write_Eol;
         raise Parsing_Error;
      end if;
      T_Right_Paren;
   end P_Pragma_Starter;

   -----------------------
   -- P_Main_subprogram --
   -----------------------

   procedure P_Main_Subprogram is
      Unit      : Unit_Name_Type;
      Partition : PID_Type;
   begin

      --  main_subprogram_declaration
      T_Identifier;

      Unit := Token_Name;

      T_Is;
      T_In;
      T_Identifier;

      Partition := Get_Defined_PID (Token_Name);

      Add_Conf_Unit (Unit, Partition);
      if Partitions.Table (Partition).Main_Subprogram = No_Name then
         Partitions.Table (Partition).Main_Subprogram := Unit;
         Main_Partition  := Partition;
         Main_Subprogram := Unit;
      else
         Write_Location (Get_Token_Location);
         Write_Str ("a partition has at most one main subprogram");
         Write_Eol;
         raise Parsing_Error;
      end if;

      T_Semicolon;

   end P_Main_Subprogram;

   ------------------------
   -- P_Partition_Clause --
   ------------------------

   procedure P_Partition_Clause is
      Attribute : Attribute_Type;
      Partition : PID_Type;
      All_PIDs  : Boolean := False;
      Func      : Boolean := False;
   begin

      T_Identifier;

      if Token_Name = Configuration then
         All_PIDs := True;

      else
         Partition := Get_Defined_PID (Token_Name);

      end if;

      T_Apostrophe;
      Attribute := P_Attribute_Name;
      T_Use;

      Take_Token ((Tok_Function, Tok_Identifier, Tok_String_Literal));

      if Attribute = Att_Host then
         if Token = Tok_Function then
            Func := True;
            Take_Token ((Tok_Identifier, Tok_String_Literal));
         end if;
      end if;

      if Token = Tok_Identifier then
         P_Full_Ada_Identifier;
      end if;

      if Attribute = Att_Host then
         declare
            Host : Host_Type := (Func, Host_Name_Type (Token_Name));
         begin
            if All_PIDs then
               for PID in Partitions.First .. Partitions.Last loop
                  if Partitions.Table (PID).Host = Default_Host then
                     Partitions.Table (PID).Host := Host;
                  end if;
               end loop;
               Default_Host := Host;

            else
               Partitions.Table (Partition).Host := Host;

            end if;

         end;

      else
         declare
            Storage_Dir : Storage_Dir_Name_Type
                        := Storage_Dir_Name_Type (Token_Name);
         begin
            if All_PIDs then
               for PID in Partitions.First .. Partitions.Last loop
                  if Partitions.Table (PID).Storage_Dir =
                     Default_Storage_Dir then
                     Partitions.Table (PID).Storage_Dir := Storage_Dir;
                  end if;
               end loop;
               Default_Storage_Dir := Storage_Dir;

            else
               Partitions.Table (Partition).Storage_Dir := Storage_Dir;

            end if;

         end;

      end if;

      T_Semicolon;

   end P_Partition_Clause;

   ---------------------------
   -- P_Full_Ada_Identifier --
   ---------------------------

   procedure P_Full_Ada_Identifier is
      Identifier : Name_Id := Token_Name;
      Location   : Location_Type;
   begin
      loop
         Location := Get_Token_Location;
         Next_Token;
         if Token = Tok_Dot then
            T_Identifier;
            Identifier := Identifier & Dot_Sep_Id & Token_Name;
         else
            Set_Token_Location (Location);
            Token_Name := Identifier;
            Token      := Tok_Identifier;
            exit;
         end if;
      end loop;
   end P_Full_Ada_Identifier;

   ---------------------------
   -- P_Package_Enumeration --
   ---------------------------

   procedure P_Package_Enumeration
     (Partition : in Partition_Name_Type) is
      PID      : PID_Type;
   begin

      --  partition_definition
      PID := Get_Defined_PID (Partition);

      --  unit name set
      T_Left_Paren;

      loop

         Take_Token ((Tok_Identifier, Tok_Right_Paren));
         exit when Token = Tok_Right_Paren;
         P_Full_Ada_Identifier;
         Add_Conf_Unit (Token_Name, PID);
         Take_Token ((Tok_Comma, Tok_Right_Paren));
         exit when Token = Tok_Right_Paren;

      end loop;

      T_Semicolon;

   end P_Package_Enumeration;

   --------------------------
   -- P_Object_Declaration --
   --------------------------

   function P_Object_Declaration
     (Prev : in Name_Id;
      Many : in Int)
      return Predefined_Object is
      Next   : Name_Id;
      Object : Predefined_Object;
   begin

      Take_Token ((Tok_Comma, Tok_Colon));

      --  New declarations follow
      if Token = Tok_Comma then
         T_Identifier;
         Next   := Token_Name;
         Object := P_Object_Declaration (Next, Many + 1);
         Init_Previous_Object (Next, Object);
         Object.Oname := Next;
         return Object;

      end if;

      --  Predefined type
      T_Identifier;
      Object.Oname := Prev;
      Object.Otype := Get_Predefined_Type (Token_Name);
      case Object.Otype is
         when Pre_Type_Partition =>
            Legal_Partition_Name (Prev);
            Create_Partition (Prev);
         when others =>
            Write_Location (Get_Token_Location);
            Write_Str ("predefined type was expected");
            Write_Eol;
            raise Parsing_Error;
      end case;

      Take_Token ((Tok_Semicolon, Tok_Colon_Equal));

      --  Initialisation
      if Token = Tok_Colon_Equal then
         case Object.Otype is
            when Pre_Type_Partition =>
               P_Package_Enumeration (Prev);
            when others =>
               raise Parsing_Error;
         end case;
      end if;
      if Many > 0 then
         Copy_Partition (Prev, Many);
      end if;
      return Object;

   end P_Object_Declaration;

   --------------------------
   -- P_Configuration_Body --
   --------------------------

   procedure P_Configuration_Body is
   begin
      loop

         Take_Token
           ((Tok_Identifier,
             Tok_Null,
             Tok_End));

         case Token is
            when Tok_Identifier =>
               T_Colon_Equal;
               P_Package_Enumeration (Token_Name);

            when Tok_End        =>
               P_Configuration_End; exit;

            when others         =>  null;
         end case;

      end loop;

   end P_Configuration_Body;

   ----------------------------
   -- Init_Previous_Object --
   ----------------------------

   procedure Init_Previous_Object
     (Prev : in Name_Id;
      Next : in Predefined_Object) is
      PID : PID_Type;
   begin
      if Prev = Next.Oname then
         return;
      end if;
      case Next.Otype is
         when Pre_Type_Partition =>
            PID := Get_Defined_PID (Next.Oname) - 1;
            Legal_Partition_Name (Prev);
            Set_PID (Prev, PID);
            Partitions.Table (PID).Name := Prev;
         when others =>
            Write_Location (Get_Token_Location);
            Write_Str ("predefined type was expected");
            Write_Eol;
            raise Parsing_Error;
      end case;
   end Init_Previous_Object;

   -----------
   -- Parse --
   -----------

   procedure Parse is
      Object : Predefined_Object;
   begin  --  Parse

      Maybe_Most_Recent_Stamp (Source_File_Stamp (Configuration_File));

      Load_File (Configuration_File);

      T_Configuration;
      T_Identifier;
      Configuration := Token_Name;
      T_Is;

      loop

         Take_Token
           ((Tok_Identifier,
             Tok_Procedure,
             Tok_For,
             Tok_Pragma,
             Tok_Begin,
             Tok_End));

         case Token is
            when Tok_Procedure  =>
               P_Main_Subprogram;

            when Tok_For        =>
               P_Partition_Clause;

            when Tok_Pragma     =>
               P_Pragma;

            when Tok_Identifier =>
               declare
                  Name : Name_Id := Token_Name;
               begin
                  Object := P_Object_Declaration (Name, 0);
                  Init_Previous_Object (Name, Object);
               end;

            when Tok_Begin      =>
               P_Configuration_Body;
               exit;

            when Tok_End        =>
               P_Configuration_End;
               exit;

            when others         => null;
         end case;

      end loop;

      T_EOF;

   end Parse;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Set_Attribute ("host",        Att_Host);
      Set_Attribute ("storage_dir", Att_Storage_Dir);

      Set_Predefined_Type ("partition", Pre_Type_Partition);

      Set_Pragma    ("starter",     Pgm_Starter);
      Pgm_S_Method := Register ("method");
      Pgm_SM_Ada   := Register ("ada");
      Pgm_SM_Shell := Register ("shell");
      Pgm_SM_None  := Register ("none");

   end Initialize;

   function Get_Defined_PID (Name : Name_Id) return PID_Type is
      PID : PID_Type;
   begin
      Legal_Partition_Name (Name);
      PID := Get_PID (Name);
      if PID = Null_PID then
         Write_Location (Get_Token_Location);
         Write_Str ("partition ");
         Write_Name (Name);
         Write_Str (" has not been declared");
         Write_Eol;
         raise Parsing_Error;
      end if;
      return PID;
   end Get_Defined_PID;

   procedure Legal_Partition_Name (Name : Name_Id) is
      PID : PID_Type;
   begin
      PID := Get_PID (Name);
      if PID = Wrong_PID then
         Write_Location (Get_Token_Location);
         Write_Name (Name);
         Write_Str  (" is not a legal partition name");
         Write_Eol;
         raise Parsing_Error;
      end if;
   end Legal_Partition_Name;

end XE_Parse;
