with Backend.BE_Ada;
with Backend.BE_IDL;
with Backend.BE_Types;

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
        (BE_Ada.Generate'Access,
         BE_Ada.Configure'Access,
         BE_Ada.Usage'Access,
         "ada",
         "Produce Ada files");
      Register
        (BE_Types.Generate'Access,
         BE_Types.Configure'Access,
         BE_Types.Usage'Access,
         "types",
         "Produce a list of all present types in the idl file");
   end Initialize;

end Backend.Config;
