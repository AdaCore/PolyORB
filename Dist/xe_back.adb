------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                              X E _ B A C K                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2005 Free Software Foundation, Inc.           --
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
--                   GLADE  is maintained by AdaCore                        --
--                      (email: sales@adacore.com)                          --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.HTable;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with XE_Flags; use XE_Flags;
with XE_IO; use XE_IO;
with XE_Names; use XE_Names;
with XE_Types; use XE_Types;
with XE_Utils; use XE_Utils;

package body XE_Back is

   type String_Ptr is access all String;
   type Header_Num is range 1 .. 7;
   function Hash (S : String_Ptr) return Header_Num;

   function Eq (S1, S2 : String_Ptr) return Boolean;
   --  Test equality of designated strings

   package All_Backends is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Backend_Access,
      No_Element => null,
      Key        => String_Ptr,
      Hash       => Hash,
      Equal      => Eq);

   --------
   -- Eq --
   --------

   function Eq (S1, S2 : String_Ptr) return Boolean is
   begin
      if S1 = null or else S2 = null then
         return S1 = S2;
      end if;
      return S1.all = S2.all;
   end Eq;

   ------------------
   -- Find_Backend --
   ------------------

   function Find_Backend (PCS_Name : String) return Backend_Access is
      S : aliased String := PCS_Name;
   begin
      return All_Backends.Get (S'Unchecked_Access);
   end Find_Backend;

   -------------------------------------
   -- Generate_Partition_Project_File --
   -------------------------------------

   procedure Generate_Partition_Project_File
     (D : Directory_Name_Type;
      P : Partition_Id := No_Partition_Id)
   is
      Prj_Fname  : File_Name_Type;
      Prj_File   : File_Descriptor;

   begin
      Prj_Fname := Dir (D, Part_Prj_File_Name);
      Create_File (Prj_File, Prj_Fname);
      Set_Output (Prj_File);
      Write_Str  ("project Partition extends """);
      Write_Str  (Project_File_Name.all);
      Write_Line (""" is");
      Write_Line ("   for Object_Dir use ""."";");
      if P /= No_Partition_Id then
         Write_Str  ("   for Exec_Dir use """);
         declare
            Exec_Dir : constant String :=
                         Get_Name_String (Partitions.Table (P).Executable_Dir);
         begin
            if Exec_Dir'Length = 0
              or else not Is_Absolute_Path (Exec_Dir)
            then
               Write_Str ("../../..");
            end if;
            Write_Str (Exec_Dir);
         end;
         Write_Line (""";");
         Write_Line ("   package Builder is");
         Write_Str  ("      for Executable (""partition.adb"") use """);
         Write_Name (Partitions.Table (P).Name);
         Write_Line (""";");
         Write_Line ("   end Builder;");
      end if;
      Write_Line ("end Partition;");
      Close (Prj_File);
      Set_Standard_Output;
   end Generate_Partition_Project_File;

   ----------
   -- Hash --
   ----------

   function Hash (S : String_Ptr) return Header_Num is
      function Hash is new GNAT.HTable.Hash (Header_Num);
   begin
      if S = null then
         return Header_Num'First;
      end if;
      return Hash (S.all);
   end Hash;

   ----------------------
   -- Register_Backend --
   ----------------------

   procedure Register_Backend (PCS_Name : String; The_Backend : Backend_Access)
   is
   begin
      All_Backends.Set (new String'(PCS_Name), The_Backend);
   end Register_Backend;

end XE_Back;
