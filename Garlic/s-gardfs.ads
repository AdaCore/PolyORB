with Ada.Streams;
with Ada.Streams.Stream_IO;

with GNAT.OS_Lib;

with System.Garlic.Storages;
pragma Elaborate (System.Garlic.Storages);

package System.Garlic.Dfs is

   package SIO renames Ada.Streams.Stream_IO;

   package GSS renames System.Garlic.Storages;

   package OS renames GNAT.OS_Lib;

   type DFS_Data_Type is new GSS.Shared_Data_Type with private;

   --  Management subprograms

   procedure Create_Storage
     (Master   : in out DFS_Data_Type;
      Location : in  String;
      Storage  : out GSS.Shared_Data_Access);

   procedure Create_Package
     (Storage  : in  DFS_Data_Type;
      Pkg_Name : in  String;
      Pkg_Data : out GSS.Shared_Data_Access);

   procedure Create_Variable
     (Pkg_Data : in  DFS_Data_Type;
      Var_Name : in  String;
      Var_Data : out GSS.Shared_Data_Access);

   procedure Initialize (Default : in String);

   procedure Enter_Variable (Var_Data : in out DFS_Data_Type);
   procedure Leave_Variable (Var_Data : in out DFS_Data_Type);

   procedure Set_Access_Mode
     (Var_Data : in out DFS_Data_Type;
      Var_Mode : in  GSS.Access_Mode;
      Failure  : out Boolean);

   procedure Read
     (Data : in out DFS_Data_Type;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Data : in out DFS_Data_Type;
      Item : in Ada.Streams.Stream_Element_Array);

private

   type DFS_Data_Access is access DFS_Data_Type;

   type DFS_Data_Type is
     new GSS.Shared_Data_Type with
      record
         Data_Name  : OS.String_Access;
         Data_File  : SIO.File_Type;
         Lock_Count : Natural;
         Previous   : DFS_Data_Access;
         Next       : DFS_Data_Access;
         Self       : DFS_Data_Access;
      end record;

end System.Garlic.Dfs;
