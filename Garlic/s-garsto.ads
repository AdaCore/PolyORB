------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--               S Y S T E M . G A R L I C . S T O R A G E S                --
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
with System.Garlic.Exceptions;
with System.Garlic.Types;

package System.Garlic.Storages is

   pragma Elaborate_Body;

   type Shared_Data_Type is abstract
     new Ada.Streams.Root_Stream_Type with null record;

   type Shared_Data_Access is access all Shared_Data_Type'Class;

   type Request_Type is (Read, Write, Lock);

   --  Primitives of Shared_Data_Type

   procedure Create_Storage
     (Master   : in out Shared_Data_Type;
      Location : in     String;
      Storage  : out    Shared_Data_Access;
      Error    : in out Exceptions.Error_Type) is abstract;
   --  Provide the Master factory with a location and return another
   --  Storage factory initialized with the data from the location
   --  location. This factory is supposed to be used into the
   --  Create_Package routine.

   procedure Create_Package
     (Storage  : in out Shared_Data_Type;
      Pkg_Name : in     String;
      Pkg_Data : out    Shared_Data_Access;
      Error    : in out Exceptions.Error_Type) is abstract;
   --  Return a Pkg_Data factory which is supposed to be used in the
   --  Create_Variable routine. This intermediate layer between
   --  Create_Storage and Create_Variable is used to configure a
   --  shared passive unit on a storage.

   procedure Create_Variable
     (Pkg_Data : in out Shared_Data_Type;
      Var_Name : in     String;
      Var_Data : out    Shared_Data_Access;
      Error    : in out Exceptions.Error_Type) is abstract;
   --  Return a stream to use when any read or write operation is
   --  performed.

   procedure Initiate_Request
     (Var_Data : access Shared_Data_Type;
      Request  : in     Request_Type;
      Success  : out    Boolean) is abstract;
   --  Initiate an operation on a variable. This routine can be thread
   --  blocking in order to serialize several concurrent requests and
   --  should be protected against abortion. Success returns whether
   --  the request can be performed on the variable. Typically, if
   --  there is no storage available on a read operation, Success is
   --  set to False. Any exception must be caught inside the
   --  routine. Note also that primitives Read and Write must catch
   --  all the potential exceptions.

   procedure Complete_Request
     (Var_Data : access Shared_Data_Type) is abstract;
   --  Complete the request previously initiated by the routine above.

   --  Any storage implementation must provide an Initialize routine.
   --     procedure Initialize;
   --  This routine is supposed to elaborate the package body of the
   --  storage support implementation. It must include the
   --  registration of a master factory dedicated to the storage
   --  support. This must be done using register_storage.

   procedure Shutdown (Storage : Shared_Data_Type) is abstract;
   --  Some storage support are active in the sense that they are running
   --  algorithm and need running tasks. This routine is used to shutdown
   --  these tasks.

   --  General services

   procedure Lookup_Variable
     (Var_Name : String;
      Var_Data : out Shared_Data_Access;
      Error    : in out Exceptions.Error_Type);

   procedure Lookup_Package
     (Pkg_Name : String;
      Pkg_Data : out Shared_Data_Access;
      Error    : in out Exceptions.Error_Type);

   procedure Lookup_Storage
     (Storage_Name : String;
      Storage_Data : out Shared_Data_Access;
      Error        : in out Exceptions.Error_Type);

   procedure Register_Storage
     (Storage_Name : String;
      Storage_Data : Shared_Data_Access);
   --  Register a factory for a storage. This factory is used to
   --  produce another factory each time a shared passive package is
   --  registered. Multiple registrations are ignored. Call it at
   --  elaboration time.

   procedure Register_Package
     (Pkg_Name  : String;
      Partition : Types.Partition_ID;
      Error     : in out Exceptions.Error_Type);
   --  Register a shared passive package on a partition and create a
   --  factory to produce shared variables later on. Multiple
   --  registrations are ignored. Call it at elaboration time.

   procedure Register_Partition
     (Partition : Types.Partition_ID;
      Location  : String;
      Error     : in out Exceptions.Error_Type);
   --  Register a partition and its storage location (support and
   --  data). If the partition has already been registered, ignored
   --  this request. If not, create the factory to produce shared
   --  variables. Call it at elaboration time.

   procedure Shutdown;

end System.Garlic.Storages;
