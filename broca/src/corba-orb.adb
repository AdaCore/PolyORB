with Broca.Buffers; use Broca.Buffers;
with Broca.Ior;
with Broca.Orb;

with Corba.Object;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body CORBA.Orb is
   Flag : constant Natural := Broca.Debug.Is_Active ("corba.orb");
   procedure O is new Broca.Debug.Output (Flag);

   procedure String_To_Object (From : in CORBA.String;
                               To   : out CORBA.Object.Ref'Class)
   is
      Buffer : Broca.Buffers.Buffer_Descriptor;
   begin
      pragma Debug (O ("String_To_Object : enter"));
      Buffer.Buffer := Broca.Ior.Ior_String_To_Buffer (From);
      Broca.Orb.IOR_To_Object (Buffer, To);
      if Corba.Object.Is_Nil(To) then
         pragma Debug (O ("String_To_Object : null object returned"));
         null;
      end if;
      Free (Buffer.Buffer);
   end String_To_Object;

   function List_Initial_Services return ObjectIdList
     renames Broca.Orb.List_Initial_Services;

   function Resolve_Initial_References (Identifier : ObjectId)
                                        return CORBA.Object.Ref
     renames Broca.Orb.Resolve_Initial_References;

   procedure Run renames Broca.Orb.Run;
end CORBA.Orb;



