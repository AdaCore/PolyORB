------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--           S Y S T E M . G A R L I C . S T O R A G E S . D S M            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
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

with GNAT.Strings;

with System.Garlic.Exceptions;
with System.Garlic.Streams;
with System.Garlic.Soft_Links;
with System.Garlic.Types;

package System.Garlic.Storages.Dsm is

   type DSM_Data_Type is new Shared_Data_Type with private;

   --  Management subprograms

   procedure Create_Storage
     (Master   : in out DSM_Data_Type;
      Location : in     String;
      Storage  : out    Shared_Data_Access;
      Error    : in out Exceptions.Error_Type);

   procedure Create_Package
     (Storage  : in out DSM_Data_Type;
      Pkg_Name : in     String;
      Pkg_Data : out    Shared_Data_Access;
      Error    : in out Exceptions.Error_Type);

   procedure Create_Variable
     (Pkg_Data : in out DSM_Data_Type;
      Var_Name : in     String;
      Var_Data : out    Shared_Data_Access;
      Error    : in out Exceptions.Error_Type);

   procedure Initialize;

   procedure Initiate_Request
     (Var_Data : access DSM_Data_Type;
      Request  : in     Request_Type;
      Success  : out    Boolean);

   procedure Complete_Request
     (Var_Data : access DSM_Data_Type);

   procedure Shutdown (Storage : DSM_Data_Type);

   procedure Read
     (Data : in out DSM_Data_Type;
      Item : out    Ada.Streams.Stream_Element_Array;
      Last : out    Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Data : in out DSM_Data_Type;
      Item : in     Ada.Streams.Stream_Element_Array);

private

   --  Write_Rqst is used to get a a variable write copy when
   --  Read_Rqst is used to get a variable readonly
   --  copy. Invalidate_Rqst is used to invalidate readonly copies on
   --  partitions when a partition is about to modify the
   --  variable. Write_Data and Read_Data are used to transmit a
   --  variable content.

   type Request_Kind is
     (Write_Rqst, Read_Rqst, Invalidate_Rqst, Write_Data, Read_Data);

   --  Copy_Set_Type is used to track of partitions to which a
   --  readonly copy was sent.

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

   --  A request message can be of five kinds:
   --
   --  * Write_Rqst and Read_Rqst are used to get a copy of a variable
   --  either in write or read mode. The message also includes the
   --  partition to which the partition should reply.
   --
   --  * Invalidate_Rqst is used to invalidate the readonly
   --  copies. The version number is used in debugging purpose just to
   --  check that we invalidate the appropriate copy. We also transmit
   --  the new probable owner as it is indicated in Li & Hudak algorithm.
   --
   --  * Write_Data and Read_Data are used to transmit a copy of a
   --  variable in both write and read mode. These messages also
   --  include the version number used in the invalidation request. In
   --  the case of Write_Data, we also transmit the copy set of the
   --  previous owner of the variable as it is indicated in the algorithm.

   type Request_Message (Kind : Request_Kind := Write_Rqst) is record
      case Kind is
         when Write_Rqst | Read_Rqst =>
            Reply_To : Types.Partition_ID;

         when others =>
            Version : Types.Version_Id;

            case Kind is
               when Invalidate_Rqst =>
                  Owner : Types.Partition_ID;

               when Write_Data | Read_Data =>
                  Stream : Streams.Stream_Element_Access;

                  case Kind is
                     when Write_Data =>
                        Copies : Copy_Set_Access;

                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
      end case;
   end record;

   function Input
     (S : access Ada.Streams.Root_Stream_Type'Class)
     return Request_Message;

   procedure Output
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Request_Message);

   for Request_Message'Input  use Input;
   for Request_Message'Output use Output;

   type Status_Type is (Write, Read, None);

   type DSM_Data_Access is access all DSM_Data_Type'Class;

   --  The variable state is composed of the following attribute :
   --  * Name    : Variable identifier
   --  * Status  : Variable mode (Write, Read or None)
   --  * Owner   : Probable owner (Li & Hudak algorithm)
   --  * Copies  : Copy set (Li & Hudak algorithm)
   --  * Version : Sanity check used for debugging purpose
   --  * Locked  : Indicates whether a variable is used locally or not
   --  * Watcher : Watch when a variable state is modified
   --  * Stream  : Variable content
   --  * Offset  : Position in the stream

   type DSM_Data_Type is new Shared_Data_Type with record
      Name    : GNAT.Strings.String_Access;
      Status  : Status_Type;
      Owner   : Types.Partition_ID;
      Copies  : Copy_Set_Access;
      Version : Types.Version_Id;
      Locked  : Boolean;
      Is_A_PO : Boolean;
      Depth   : Natural;
      Watcher : Soft_Links.Watcher_Access;
      Stream  : Streams.Stream_Element_Access;
      Offset  : Ada.Streams.Stream_Element_Offset;
   end record;

end System.Garlic.Storages.Dsm;
