with Backend.BE_A;
with Backend.BE_Ada;
with Backend.BE_IDL;

package body Backend.Config is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register
        (BE_IDL.Generate'Access,
         BE_IDL.Configure'Access,
         BE_IDL.Usage'Access,
         "idl",
         "Dump parsed IDL file");
      Register
        (BE_A.Generate'Access,
         BE_A.Configure'Access,
         BE_A.Usage'Access,
         "a",
         "Produce Ada files");
      Register
        (BE_Ada.Generate'Access,
         BE_Ada.Configure'Access,
         BE_Ada.Usage'Access,
         "ada",
         "Produce Ada files");
   end Initialize;

end Backend.Config;
