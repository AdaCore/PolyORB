--  A dynamic dictionnary of objects, indexed by Strings.

--  $Id$

generic

   type Value is private;

package Droopi.Dynamic_Dict is

   pragma Elaborate_Body;

   Key_Not_Found : exception;

   function Lookup
      (K : String)
     return Value;

   procedure Register
     (K : String;
      V : Value);

   procedure Unregister
     (K : String);

end Droopi.Dynamic_Dict;
