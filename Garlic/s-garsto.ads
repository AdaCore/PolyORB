with Ada.Streams;
with System.Garlic.Types;

package System.Garlic.Storages is

   pragma Elaborate_Body;

   type Shared_Data_Type is abstract
     new Ada.Streams.Root_Stream_Type with null record;

   type Shared_Data_Access is access all Shared_Data_Type'Class;

   type Access_Mode is (Read, Write, Lock, Unlock);

   --  Management subprograms

   procedure Create_Storage
     (Master   : in out Shared_Data_Type;
      Location : in  String;
      Storage  : out Shared_Data_Access) is abstract;

   procedure Create_Package
     (Storage  : in  Shared_Data_Type;
      Pkg_Name : in  String;
      Pkg_Data : out Shared_Data_Access) is abstract;

   procedure Create_Variable
     (Pkg_Data : in  Shared_Data_Type;
      Var_Name : in  String;
      Var_Data : out Shared_Data_Access) is abstract;

   procedure Initialize (Default : in String) is abstract;

   procedure Set_Access_Mode
     (Var_Data : in out Shared_Data_Type;
      Var_Mode : in  Access_Mode;
      Failure  : out Boolean) is abstract;

   procedure Enter_Variable (Var_Data : in out Shared_Data_Type) is abstract;
   procedure Leave_Variable (Var_Data : in out Shared_Data_Type) is abstract;

   function  Lookup_Variable
     (Var_Name : in String;
      Var_Mode : in Access_Mode)
     return Shared_Data_Access;

   function  Lookup_Storage
     (Storage_Name : in String)
     return Shared_Data_Access;

   procedure Register_Storage
     (Storage_Name : in String;
      Storage_Data : in Shared_Data_Access);
   --  Register a factory for a storage. This factory is used to
   --  produce another factory each time a shared passive package is
   --  registered. Multiple registrations are ignored. Call it at
   --  elaboration time.

   procedure Register_Package
     (Pkg_Name  : in String;
      Partition : in Types.Partition_ID);
   --  Register a shared passive package on a partition and create a
   --  factory to produce shared variables later on. Multiple
   --  registrations are ignored. Call it at elaboration time.

   procedure Register_Partition
     (Partition : in Types.Partition_ID;
      Location  : in String);
   --  Register a passive partition and its storage location (support
   --  and data). If the partition has already been registered,
   --  ignored this request. If not, create the factory to produce
   --  shared variables. Call it at elaboration time.

end System.Garlic.Storages;
