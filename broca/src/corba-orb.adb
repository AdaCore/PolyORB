with Broca.Types; use Broca.Types;
with Broca.Ior;
with Broca.Orb;

package body CORBA.Orb is
   procedure String_To_Object (From : in CORBA.String;
                               To   : out CORBA.Object.Ref'Class)
   is
      Buffer : Broca.Types.Buffer_Descriptor;
   begin
      Buffer.Buffer := Broca.Ior.Ior_String_To_Buffer (From);
      Broca.Orb.IOR_To_Object (Buffer, To);
      Unchecked_Deallocation (Buffer.Buffer);
   end String_To_Object;

   function List_Initial_Services return ObjectIdList
     renames Broca.Orb.List_Initial_Services;

   function Resolve_Initial_References (Identifier : ObjectId)
                                        return CORBA.Object.Ref
     renames Broca.Orb.Resolve_Initial_References;

   procedure Run renames Broca.Orb.Run;
end CORBA.Orb;
