package body Ada_Be.Types is

   ------------------------
   --  BUFFER OPERATIONS --
   ------------------------

   procedure Write (B : Buffer;
                    S : String) is
   begin
      null;
   end Write;

   procedure New_Line (B : Buffer) is
   begin
      null;
   end New_Line;

   procedure Write_Comment (B : Buffer;
                            Comment : String) is
   begin
      null;
   end Write_Comment;

   procedure Write_Instruction (B : Buffer;
                                Instr : String) is
   begin
      null;
   end Write_Instruction;

   procedure Append (B : Buffer;
                     Bl : Block) is
   begin
      null;
   end Append;

   procedure Dump (B : Buffer;
                   File : Ada.Text_IO.File_Type) is
   begin
      null;
   end Dump;


   --------------------------
   --  WITH_USE OPERATIONS --
   --------------------------
   procedure Add_With (List : With_Use_List;
                       Package_Name : String) is
   begin
      null;
   end Add_With;

   procedure Add_Use (List : With_Use_List;
                      Package_Name : String) is
   begin
      null;
   end Add_Use;

   procedure Add_With_Use (List : With_Use_List;
                           Package_Name : String) is
   begin
      null;
   end Add_With_Use;

   ------------------------

   procedure Dump_Code (Spec : Package_Spec) is
   begin
      null;
   end Dump_Code;


   procedure Dump_Code (The_Body : Package_Body) is
   begin
      null;
   end Dump_Code;



   procedure Initialize (Scope : in out IDL_Scope) is
   begin
      Scope.Spec.Name := new String' (Scope.Name.all);
      Scope.Marshall_Spec.Name :=
       new String' (Scope.Name.all
                    & Ada_Be.Constants.Marshall_Suffix);
      Scope.Marshall_Body.Name :=
       new String' (Scope.Name.all
                    & Ada_Be.Constants.Marshall_Suffix);
   end Initialize;


   procedure Initialize (Object : in out IDL_Object) is
   begin
      Object.The_Body.Name := new String' (Object.Name.all);
      Object.Skel_Spec.Name :=
       new String' (Object.Name.all
                    & Ada_Be.Constants.Skel_Suffix);
      Object.Skel_Body.Name :=
       new String' (Object.Name.all
                    & Ada_Be.Constants.Skel_Suffix);
      Object.Impl_Spec.Name :=
       new String' (Object.Name.all
                    & Ada_Be.Constants.Impl_Suffix);
      Object.Impl_Body.Name :=
       new String' (Object.Name.all
                    & Ada_Be.Constants.Impl_Suffix);
   end Initialize;


   procedure Dump_Code (Scope : IDL_Scope) is
   begin
      Dump_Code (Scope.Spec);
      Dump_Code (Scope.Marshall_Spec);
      Dump_Code (Scope.Marshall_Body);
   end Dump_Code;

   procedure Dump_Code (Object : IDL_Object) is
   begin
      Dump_Code (IDL_Scope (Object));
      Dump_Code (Object.The_Body);
      Dump_Code (Object.Skel_Spec);
      Dump_Code (Object.Skel_Body);
      Dump_Code (Object.Impl_Spec);
      Dump_Code (Object.Impl_Body);
      if (Object.Is_Forward) then
         --  dump forward code
         null;
      end if;
   end Dump_Code;


end Ada_Be.Types;
