with Ada.Text_IO;
with Ada_Be.Constants;
with Ada.Finalization;

package Ada_Be.Types is

   type String_Ptr is access String;

   --------------
   --  buffers --
   --------------
   type Buffer_Object is private;
   type Buffer is access Buffer_Object;
   procedure Write (B : Buffer;
                    S : String);
   procedure New_Line (B : Buffer);
   procedure Write_Comment (B : Buffer;
                            Comment : String);
   procedure Write_Instruction (B : Buffer;
                                Instr : String);
   procedure Dump (B : Buffer;
                   File : Ada.Text_IO.File_Type);

   ---------------
   --  With_Use --
   ---------------
   --  a structure to store the with
   --  and use statements of a block
   type With_Use_List_Element is private;
   type With_Use_List is access With_Use_List_Element;
   procedure Add_With (List : With_Use_List;
                       Package_Name : String);
   procedure Add_Use (List : With_Use_List;
                      Package_Name : String);
   procedure Add_With_Use (List : With_Use_List;
                           Package_Name : String);


   --------------------------------

   --  an ada block
   type Entity is abstract tagged record
      With_Use : With_Use_List;
      Declare_Buf : Buffer;
   end record;


   type Package_Spec is new Entity with record
      Name : String_Ptr;
      Private_Buf : Buffer;
   end record;

   procedure Dump_Code (Spec : Package_Spec);

   type Exception_Handler;
   type Exception_Handler_Ptr is access Exception_Handler;

   type Block is new Entity with record
      Body_Buf : Buffer;
      Exception_Buf : Exception_Handler_Ptr;
   end record;

   procedure Append (B : Buffer;
                     Bl : Block);

   type Exception_Handler (Variable_Name : String_Ptr;
                           Exception_Name : String_Ptr)
   is record
      Statements : Block;
   end record;

   type Package_Body is new Block with record
      Name : String_Ptr;
   end record;

   procedure Dump_Code (The_Body : Package_Body);

   type Operation_Block is new Block with record
      Signature : Buffer;
   end record;

   --------------------------------

   --  for _IDL_file, module
   type IDL_Scope (Name : String_Ptr) is
     new Ada.Finalization.Controlled with record
        Spec : Package_Spec;
        Marshall_Spec : Package_Spec;
        Marshall_Body : Package_Body;
     end record;

   procedure Initialize (Scope : in out IDL_Scope);
   --  procedure Duplicate (Scope : in out IDL_Scope);
   --  procedure Finalize (Scope : in out IDL_Scope);

   --  creates the files
   procedure Dump_Code (Scope : IDL_Scope);

   --  for interface, value
   type IDL_Object (Name : String_Ptr) is new IDL_Scope (Name) with record
      The_Body : Package_Body;
      Skel_Spec : Package_Spec;
      Skel_Body : Package_Body;
      Impl_Spec : Package_Spec;
      Impl_Body : Package_Body;
      Is_Forward : Boolean;
   end record;

   procedure Initialize (Object : in out IDL_Object);
   --  procedure Duplicate (Object : in out IDL_Object);
   --  procedure Finalize (Object : in out IDL_Object);

   procedure Dump_Code (Object : IDL_Object);


private

   type With_Use_List_Element is record
      Package_Name : String_Ptr;
      Is_Use : Boolean := False;
      Next : With_Use_List;
   end record;

   --  buffers --
   type Buffer_Element_Type is (String_Type, Block_Type);
   type Buffer_Element;
   type Buffer_Element_List is access Buffer_Element;
   type Buffer_Element (Buf_Type : Buffer_Element_Type) is record
      Next : Buffer_Element_List;
      case Buf_Type is
         when String_Type =>
            String_Value : String_Ptr;
         when Block_Type =>
            Block_Value : Block;
      end case;
   end record;

   type Buffer_Object is record
      First : Buffer_Element_List;
      Last : Buffer_Element_List;
   end record;

end Ada_Be.Types;


