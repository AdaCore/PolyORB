--
--  $Id$
--

package Timing is

   type Milliseconds is new Natural;

   function Current return Milliseconds;
   pragma Inline (Current);

end Timing;
