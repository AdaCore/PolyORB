--  Representation of object references as typed
--  Interoperable Object References.

--  An IOR aggregates the identification of an interface (i.e. a type
--  identifier) and a set of profiles designating an object that supports
--  this interface. An IOR can be converted to a stringified
--  representation by marshalling it according to CDR, and converting
--  the resulting stream element array into a string of hexadecimal digits.

--  $Id$

with CORBA;

with Droopi.Buffers;      use Droopi.Buffers;

with Sequences.Unbounded;

package Droopi.References.IOR is


   type Marshall_Profile_Body_Type is access procedure
     (Buffer  : access Buffers.Buffer_Type;
      Profile : Profile_Access);

   type Unmarshall_Profile_Body_Type is access function
     (Buffer  : access Buffers.Buffer_Type)
     return Profile_Access;

   type Profile_Record is record
      Tag                     : Profile_Tag;
      Marshall_Profile_Body   : Marshall_Profile_Body_Type;
      Unmarshall_Profile_Body : Unmarshall_Profile_Body_Type;
   end record;

   package Profile_Record_Seq is
      new Sequences.Unbounded (Profile_Record);

   --  An object reference (whose supported interface is not
   --  reflected by its Ada type) and the associated type information
   --  (within the IDL typing model).

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

   function Object_To_String
     (IOR : IOR_Type)
      return CORBA.String;

   function  String_To_Object
     (Str : CORBA.String)
     return IOR_Type;


   Callbacks : Profile_Record_Seq.Sequence;

   procedure Register
     (Profile                 : in Profile_Tag;
      Marshall_Profile_Body   : in Marshall_Profile_Body_Type;
      Unmarshall_Profile_Body : in Unmarshall_Profile_Body_Type);

   --   Callbacks : array (Tag_Internet_IOP .. Tag_Multiple_Components)
   --   of Profile_Record;

end Droopi.References.IOR;
