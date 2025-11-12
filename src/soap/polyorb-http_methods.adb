--  THIS IS A GENERATED FILE, DO NOT EDIT!
with Interfaces; use Interfaces;

with PolyORB.Initialization;

with PolyORB.Utils.Strings;

package body PolyORB.HTTP_Methods is

   type String_Access is access String;

   Table : array (Method) of String_Access;

   function Hash (S : String) return Natural;
   pragma Inline (Hash);

   procedure Initialize;

   procedure Set (N : Method; S : String);

   P : constant array (0 .. 1) of Natural :=
     (1, 2);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (16, 1);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (15, 8);

   G : constant array (0 .. 16) of Unsigned_8 :=
     (0, 0, 0, 0, 0, 2, 0, 0, 5, 0, 2, 0, 5, 0, 2, 1, 3);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 17;
         F2 := (F2 + Natural (T2 (K)) * J) mod 17;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 8;
   end Hash;

   function In_Word_Set (S : String) return Method is
      N : constant Method := Method'Val (Hash (S));
   begin
      if Table (N).all = S then
         return N;
      else
         return Extension_Method;
      end if;
   end In_Word_Set;

   procedure Initialize is
   begin
      Set (OPTIONS, "OPTIONS");
      Set (GET, "GET");
      Set (HEAD, "HEAD");
      Set (POST, "POST");
      Set (PUT, "PUT");
      Set (DELETE, "DELETE");
      Set (TRACE, "TRACE");
      Set (CONNECT, "CONNECT");
   end Initialize;

   procedure Set (N : Method; S : String) is
   begin
      Table (N) := new String'(S);
   end Set;

   function To_String (Id : Method) return String is
   begin
      return Table (Id).all;
   end To_String;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"http_methods",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.HTTP_Methods;
