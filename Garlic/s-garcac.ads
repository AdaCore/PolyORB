--
--  $Id$
--

generic
   type Index_Type is range <>;
   type Data_Type is private;
   Unset : Data_Type;

package System.Garlic.Caching is

   --  This package provides efficient caching for objects which will never
   --  change once they are allocated. It uses a protected type when needed
   --  and regular data when possible.

   function Get (Index : Index_Type) return Data_Type;
   --  This function, which may be blocking, returns the current value. If
   --  it returns Unset, then the calling process has to query the data and
   --  call Set.

   procedure Set
     (Index : in Index_Type;
      Value : in Data_Type);
   --  Set the value and unblock pending Get calls.

   procedure Die;
   --  Cause any pending calls to Get (and next ones) to raise Program_Error.

end System.Garlic.Caching;
