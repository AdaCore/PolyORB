--  This package only defines a Key type. It is not wrapped around the C
--  type since it is never used in AdaBroker but just taken from a C
--  function and given to another one.

with System;

package AdaBroker.Key is

   type Object is new System.Address;
   --  This object is never used in Ada (just taken from a C function and
   --  given to another one) so it is not right implemented.  We just keep
   --  the system Address of the object.

end AdaBroker.Key;
