------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--         S Y S T E M . G A R L I C . S T O R A G E S . D S M              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;

with System.Garlic.Streams;
with System.Garlic.Soft_Links;
with System.Garlic.Utils;

package System.Garlic.Storages.Dsm is

   type DSM_Data_Type is new Shared_Data_Type with private;

   --  Management subprograms

   procedure Create_Storage
     (Master   : in out DSM_Data_Type;
      Location : in     String;
      Storage  : out    Shared_Data_Access);

   procedure Create_Package
     (Storage  : in out DSM_Data_Type;
      Pkg_Name : in     String;
      Pkg_Data : out    Shared_Data_Access);

   procedure Create_Variable
     (Pkg_Data : in out DSM_Data_Type;
      Var_Name : in     String;
      Var_Data : out    Shared_Data_Access);

   procedure Initialize;

   procedure Initiate_Request
     (Var_Data : in out DSM_Data_Type;
      Request  : in     Request_Type;
      Success  : out    Boolean);

   procedure Complete_Request
     (Var_Data : in out DSM_Data_Type);

   procedure Read
     (Data : in out DSM_Data_Type;
      Item : out    Ada.Streams.Stream_Element_Array;
      Last : out    Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Data : in out DSM_Data_Type;
      Item : in     Ada.Streams.Stream_Element_Array);

private

   type Request_Kind is
     (Write_Rqst, Read_Rqst, Cancel_Rqst, Write_Data, Read_Data);

   subtype Copy_Set_Type is Types.Partition_List;
   type Copy_Set_Access is access Copy_Set_Type;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Copy_Set_Access);

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Copy_Set_Access);

   for Copy_Set_Access'Read  use Read;
   for Copy_Set_Access'Write use Write;

   type Request_Record (Kind : Request_Kind := Write_Rqst) is
      record
         case Kind is
            when Write_Rqst | Read_Rqst =>
               Reply_To : Types.Partition_ID;

            when Write_Data | Read_Data =>
               Stream : Streams.Stream_Element_Access;
               Copies : Copy_Set_Access;

            when Cancel_Rqst =>
               Owner : Types.Partition_ID;
         end case;
      end record;

   function Input
     (S : access Ada.Streams.Root_Stream_Type'Class)
     return Request_Record;

   procedure Output
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Request_Record);

   for Request_Record'Input  use Input;
   for Request_Record'Output use Output;

   type Status_Type is (Read, Write, None);

   type DSM_Data_Access is access all DSM_Data_Type'Class;

   type DSM_Data_Type is
     new Shared_Data_Type with
      record
         Name    : Utils.String_Access;
         Status  : Status_Type;
         Owner   : Types.Partition_ID;
         Copies  : Copy_Set_Access;
         Stream  : Streams.Stream_Element_Access;
         Offset  : Ada.Streams.Stream_Element_Offset;
         Mutex   : Soft_Links.Mutex_Access;
         Watcher : Soft_Links.Watcher_Access;
      end record;

end System.Garlic.Storages.Dsm;
