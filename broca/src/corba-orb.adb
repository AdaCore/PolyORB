with Broca.Buffers; use Broca.Buffers;
with Broca.IOR;
with Broca.ORB;

with CORBA.Object;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body CORBA.ORB is

   Flag : constant Natural := Broca.Debug.Is_Active ("corba.orb");
   procedure O is new Broca.Debug.Output (Flag);

   ----------------------
   -- String_To_Object --
   ----------------------

   procedure String_To_Object
     (From : in CORBA.String;
      To   : out CORBA.Object.Ref'Class)
   is
      Buffer : Broca.Buffers.Buffer_Descriptor;
   begin
      pragma Debug (O ("String_To_Object : enter"));

      Buffer := Broca.IOR.IOR_String_To_Buffer (From);
      Broca.ORB.IOR_To_Object (Buffer, To);

      if CORBA.Object.Is_Nil (To) then
         pragma Debug (O ("String_To_Object : null object returned"));
         null;
      end if;

      Destroy (Buffer);
   end String_To_Object;

   function List_Initial_Services return ObjectIdList
     renames Broca.ORB.List_Initial_Services;

   function Resolve_Initial_References
     (Identifier : ObjectId)
     return CORBA.Object.Ref
     renames Broca.ORB.Resolve_Initial_References;

   procedure Run renames Broca.ORB.Run;

end CORBA.ORB;



