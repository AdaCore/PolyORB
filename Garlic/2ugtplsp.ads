--
--  $Id$
--

with System.Garlic.Non_Blocking;

private package System.Garlic.TCP.Platform_Specific is

   --  SunOS version of this package.

   package Net renames System.Garlic.Non_Blocking;

end System.Garlic.TCP.Platform_Specific;
