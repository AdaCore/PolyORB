--
--  $Id$
--

with System.Garlic.Thin;

private package System.Garlic.TCP.Platform_Specific is

   --  Solaris version of this package.

   package Net renames System.Garlic.Thin;

private

   pragma Linker_Options ("-lnsl");
   pragma Linker_Options ("-lsocket");

end System.Garlic.TCP.Platform_Specific;
