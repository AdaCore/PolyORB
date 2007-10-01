with Ada.Text_IO;
with CORBA.ORB;

package body IOR_Utils is

   use Ada.Text_IO;

   -------------
   -- Put_Ref --
   -------------

   procedure Put_Ref (Filename : String; Ref : CORBA.Object.Ref) is
      File : File_Type;

   begin
      Create (File, Out_File, Filename);
      Put_Line
        (File,
         CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)));
      Close (File);
   end Put_Ref;

   -------------
   -- Get_Ref --
   -------------

   procedure Get_Ref
     (Filename :        String;
      Ref      : in out CORBA.Object.Ref'Class)
   is
      File : Ada.Text_IO.File_Type;
      Item : String (1 .. 1024);
      Last : Natural;

   begin
      Open (File, In_File, Filename);
      Get_Line (File, Item, Last);

      --  Getting the CORBA.Object

      CORBA.ORB.String_To_Object
        (CORBA.To_CORBA_String (Item (1 .. Last)), Ref);
   end Get_Ref;

end IOR_Utils;
