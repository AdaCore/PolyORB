--  Representation of object references as typed
--  Interoperable Object References.

--  An IOR aggregates the identification of an interface
--  and a set of profiles designating an object that supports
--  this interface. An IOR can be converted to a stringified
--  representation by marshalling it according to CDR, and converting
--  the resulting stream element array into a string of hexadecimal digits.

--  $Id$

with CORBA;

with Droopi.Buffers;      use Droopi.Buffers;

package Droopi.References.IOR is

   type IOR_Type is record
      Ref : Droopi.References.Ref;
      Type_Id  : CORBA.String;
   end record;

   type IOR_Access is access all IOR_Type;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in IOR_Type);

   function  Unmarshall
     (Buffer : access Buffer_Type)
   return  IOR_Type;

   type Marshall_Profile_Body_Type is access procedure
     (Buffer  : access Buffers.Buffer_Type;
      Profile : access Profile_Type'Class);

   type Unmarshall_Profile_Body_Type is access function
     (Buffer  : access Buffers.Buffer_Type)
     return Profile_Access;

   type Profile_Record is record
      Marshall_Profile_Body   : Marshall_Profile_Body_Type;
      Unmarshall_Profile_Body : Unmarshall_Profile_Body_Type;
   end record;

   procedure Register
     (Profile                 : in Profile_Tag;
      Marshall_Profile_Body   : in Marshall_Profile_Body_Type;
      Unmarshall_Profile_Body : in Unmarshall_Profile_Body_Type);

   Callbacks : array (Tag_Internet_IOP .. Tag_Multiple_Components)
     of Profile_Record;

end Droopi.References.IOR;
