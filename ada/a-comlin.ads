------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                     A D A . C O M M A N D _ L I N E                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                              --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

package Ada.Command_Line is
pragma Preelaborate (Command_Line);

   function Argument_Count return Natural;
   --  If the external execution environment supports passing arguments to a
   --  program, then Argument_Count returns the number of arguments passed to
   --  the program invoking the function. Otherwise it return 0.
   --
   --  In GNAT: Corresponds to (argc - 1) in C.

   function Argument (Number : in Positive) return String;
   --  If the external execution environment supports passing arguments to
   --  a program, then Argument returns an implementation-defined value
   --  corresponding to the argument at relative position Number. If Number
   --  is outside the range 1 .. Argument_Count, then Constraint_Error is
   --  propagated.
   --
   --  in GNAT: Corresponds to argv [n] (for n > 0) in C.

   function Command_Name return String;
   --  If the external execution environment supports passing arguments to
   --  a program, then Command_Name returns an implementation-defined value
   --  corresponding to the name of the command invoking the program.
   --  Otherwise Command_Name returns the null string.
   --
   --  in GNAT: Corresponds to argv [0] in C.

   type Exit_Status is new Integer;

   Success : constant Exit_Status;
   Failure : constant Exit_Status;

   procedure Set_Exit_Status (Code : in Exit_Status);
   pragma Import (C, Set_Exit_Status, "set_gnat_exit_status");

private

   Success : constant Exit_Status := 0;
   Failure : constant Exit_Status := 1;
   --  ??? Later these will be properly handled through Import variables.

end Ada.Command_Line;
