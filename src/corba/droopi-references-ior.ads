------------------------------------------------------------------------------
--                     DROOPI COMPONENTS                                    --
--                       IOR SPEC                                           --
------------------------------------------------------------------------------

with CORBA;
with CORBA.Impl;

with Droopi.Buffers;      use Droopi.Buffers;
with Droopi.Binding_Data;

package Droopi.References.IOR is

   type IOR_Type is record
      Ref : Droopi.References.Ref;
      Type_Id  : CORBA.String;
   end record;

   type IOR_Ptr is access all IOR_Type;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in IOR_Type);

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      Result : out IOR_Type);

   type Marshall_Profile_Body_Type is access procedure
     (Buffer  : access Buffers.Buffer_Type;
      Profile : access Profile_Type'Class);

   type Unmarshall_Profile_Body_Type is access procedure
     (Buffer  : access Buffers.Buffer_Type;
      Profile : out Droopi.Binding_Data.Profile_Access);

   type Profile_Record is record
      Marshall_Profile_Body   : Marshall_Profile_Body_Type;
      Unmarshall_Profile_Body : Unmarshall_Profile_Body_Type;
   end record;

   Callbacks : array (Tag_Internet_IOP .. Tag_Multiple_Components)
     of Profile_Record;

end Droopi.References.IOR;
