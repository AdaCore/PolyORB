with Ada.IO_Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with GNAT.Lock_Files;
with GNAT.Task_Lock;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with System;
with System.File_Control_Block;
with System.File_IO;

with System.Garlic.Debug; use System.Garlic.Debug;
pragma Elaborate (System.Garlic.Debug);

with System.Garlic.Options; use System.Garlic.Options;
with System.Garlic.Utils;   use System.Garlic.Utils;

package body System.Garlic.Dfs is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARDFS", "(s-gardfs): ");

   Dfs_Storage_Name : constant String := "dfs";

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use type SIO.File_Mode;
   use type GSS.Access_Mode;

   package IOX renames Ada.IO_Exceptions;

   package FCB renames System.File_Control_Block;

   package SFI renames System.File_IO;

   package TSL renames GNAT.Task_Lock;

   function To_AFCB_Ptr is
      new Ada.Unchecked_Conversion (SIO.File_Type, FCB.AFCB_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation (DFS_Data_Type, DFS_Data_Access);

   Root : DFS_Data_Access;

   LRU_Head : DFS_Data_Access;
   LRU_Tail : DFS_Data_Access;

   ----------------------------------------------
   -- Variables for Shared Memory Access Files --
   ----------------------------------------------

   Max_Shared_Data_Files_Open : constant := 20;
   --  Maximum number of lock files that can be open

   Shared_Data_Files_Open : Natural := 0;
   --  Number of shared memory access files currently open

   To_File_Mode : constant array (GSS.Read .. GSS.Write) of SIO.File_Mode
     := (GSS.Read => SIO.In_File, GSS.Write => SIO.Out_File);

   function  Lock_File_Name
     (Var_Data : DFS_Data_Type)
     return String;

   --------------------
   -- Create_Storage --
   --------------------

   procedure Create_Storage
     (Master   : in out DFS_Data_Type;
      Location : in  String;
      Storage  : out GSS.Shared_Data_Access)
   is
      Result : DFS_Data_Access;

   begin
      Result := new DFS_Data_Type;
      Result.Data_Name := new String'(Location);
      Storage := GSS.Shared_Data_Access (Result);
   end Create_Storage;

   --------------------
   -- Create_Package --
   --------------------

   procedure Create_Package
     (Storage  : in  DFS_Data_Type;
      Pkg_Name : in  String;
      Pkg_Data : out GSS.Shared_Data_Access)
   is
      Result : DFS_Data_Access;

   begin
      pragma Debug (D ("create package file " & Pkg_Name &
                       " on support " & Storage.Data_Name.all));

      Result := new DFS_Data_Type;
      Result.Data_Name := Storage.Data_Name;
      Pkg_Data := GSS.Shared_Data_Access (Result);
   end Create_Package;

   ---------------------
   -- Create_Variable --
   ---------------------

   procedure Create_Variable
     (Pkg_Data : in  DFS_Data_Type;
      Var_Name : in  String;
      Var_Data : out GSS.Shared_Data_Access)
   is
      Var : DFS_Data_Access := new DFS_Data_Type;

   begin
      Var.Data_Name  := new String'(Pkg_Data.Data_Name.all & Var_Name);
      Var.Lock_Count := 0;
      Var.Self       := Var;
      Var_Data       := GSS.Shared_Data_Access (Var);
   end Create_Variable;

   --------------------
   -- Enter_Variable --
   --------------------

   procedure Enter_Variable (Var_Data : in out DFS_Data_Type) is
   begin
      pragma Debug (D ("enter protected variable file " &
                       Lock_File_Name (Var_Data)));

      TSL.Lock;
      if Var_Data.Lock_Count /= 0 then
         Var_Data.Lock_Count := Var_Data.Lock_Count + 1;
         TSL.Unlock;

      else
         Var_Data.Lock_Count := 1;
         TSL.Unlock;
         GNAT.Lock_Files.Lock_File (Lock_File_Name (Var_Data), Wait => 0.1);
      end if;

      pragma Debug (D ("lock count =" & Var_Data.Lock_Count'Img));
   end Enter_Variable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Default : in String) is
   begin
      pragma Debug (D ("initialize DFS"));

      if Root = null then
         Root := new DFS_Data_Type;
         if Data_Location /= null then
            for L in Data_Location'Range loop
               if Data_Location (L).all = Dfs_Storage_Name then
                  Root.Data_Name := OS.String_Access (Data_Location (L));
                  exit;
               end if;
            end loop;
         end if;
         if OS."=" (Root.Data_Name, null) then
            Root.Data_Name := OS.Getenv ("DFS_DATA_DIR");
            if Root.Data_Name'Length = 0 then
               Root.Data_Name := new String'(Default);
            end if;
         end if;
         GSS.Register_Storage
           (Dfs_Storage_Name,
            GSS.Shared_Data_Access (Root));
      end if;
   end Initialize;

   --------------------
   -- Leave_Variable --
   --------------------

   procedure Leave_Variable (Var_Data : in out DFS_Data_Type) is
   begin
      pragma Debug (D ("leave protected variable file " &
                       Lock_File_Name (Var_Data)));

      TSL.Lock;
      Var_Data.Lock_Count := Var_Data.Lock_Count - 1;

      if Var_Data.Lock_Count = 0 then
         GNAT.Lock_Files.Unlock_File (Lock_File_Name (Var_Data));
      end if;
      pragma Debug (D ("lock count =" & Var_Data.Lock_Count'Img));
      TSL.Unlock;
   end Leave_Variable;

   --------------------
   -- Lock_File_Name --
   --------------------

   function Lock_File_Name (Var_Data : DFS_Data_Type) return String is
   begin
      return Var_Data.Data_Name.all & ".entry";
   end Lock_File_Name;

   ----------
   -- Read --
   ----------

   procedure Read
     (Data : in out DFS_Data_Type;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      pragma Debug (D ("read variable file " & Data.Data_Name.all));

      SIO.Read (Data.Data_File, Item, Last);
   end Read;

   ---------------------
   -- Set_Access_Mode --
   ---------------------

   procedure Set_Access_Mode
     (Var_Data : in out DFS_Data_Type;
      Var_Mode : in GSS.Access_Mode;
      Failure  : out Boolean)
   is
      Free : DFS_Data_Access;

   begin
      TSL.Lock;
      Failure := False;

      if Var_Mode in GSS.Read .. GSS.Write then
         declare
            Fname : constant String := Var_Data.Data_Name.all;
            Fmode : SIO.File_Mode   := To_File_Mode (Var_Mode);

         begin
            if not SIO.Is_Open (Var_Data.Data_File) then
               begin
                  SIO.Open (Var_Data.Data_File, Fmode, Name => Fname);
                  SFI.Make_Unbuffered (To_AFCB_Ptr (Var_Data.Data_File));

                  pragma Debug (D ("open variable file " & Fname));

               exception
                  when IOX.Name_Error =>

                     if Var_Mode = GSS.Read then
                        Failure := True;

                     else
                        begin
                           SIO.Create
                             (Var_Data.Data_File, Fmode, Name => Fname);
                           pragma Debug (D ("create variable file " & Fname));

                        exception when others =>
                           Failure := True;
                        end;
                     end if;
               end;

               Shared_Data_Files_Open := Shared_Data_Files_Open + 1;

               --  Here if file is already open, set file for reading

            else
               if SIO.Mode (Var_Data.Data_File) /= Fmode then
                  pragma Debug
                    (D ("reset variable file " &
                        Var_Data.Data_Name.all &
                        " mode to " & Var_Mode'Img));

                  SIO.Set_Mode (Var_Data.Data_File, Fmode);
                  SFI.Make_Unbuffered (To_AFCB_Ptr (Var_Data.Data_File));
               end if;

               SIO.Set_Index (Var_Data.Data_File, 1);
            end if;
         end;
      end if;

      if Failure then
         pragma Debug (D ("reject mode " & Var_Mode'Img &
                          " for variable file " & Var_Data.Data_Name.all));
         TSL.Unlock;
         return;
      end if;

      if Shared_Data_Files_Open = Max_Shared_Data_Files_Open then
         Free := LRU_Head;

         if Free.Next /= null then
            Free.Next.Previous := null;
         end if;

         LRU_Head       := Free.Next;
         Free.Next     := null;
         Free.Previous := null;
         SIO.Close (Free.Data_File);
         pragma Debug (D ("close variable file " & Free.Data_Name.all));
      end if;

      --  Add new entry at end of LRU chain

      if LRU_Head = null then
         LRU_Head := Var_Data.Self;
         LRU_Tail := Var_Data.Self;

      else
         Var_Data.Previous := LRU_Tail;
         LRU_Tail.Next     := Var_Data.Self;
         LRU_Tail          := Var_Data.Self;
      end if;
      TSL.Unlock;
   end Set_Access_Mode;

   -----------
   -- Write --
   -----------

   procedure Write
     (Data : in out DFS_Data_Type;
      Item : in Ada.Streams.Stream_Element_Array) is
   begin
      pragma Debug (D ("write variable " & Data.Data_Name.all));

      SIO.Write (Data.Data_File, Item);
   end Write;

end System.Garlic.Dfs;
