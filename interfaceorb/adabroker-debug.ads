--  This is a debugging package for AdaBroker.
--
--  with Adabroker.Debug;
--  pragma Elaborate(Adabroker.Debug);
--
--  Flag : constant Natural := Adabroker.Debug.Is_Active ("specific_name");
--  procedure O is new AdaBroker.Debug.Output (Flag);
--
--  and then :
--
--  pragma Debug (O ("debugging info"));
--
--  The output will be done if "adabroker.deb" file contains
--  a line with "specific_name"

package AdaBroker.Debug is

   function Is_Active (Flag : in String) return Natural;
   --  returns 0 when not active

   generic
      Flag : Natural;
   procedure Output (Message : in String);
   --  Prints Message on standard output when Flag is not 0

end AdaBroker.Debug;
