--  Logging support for DROOPI

--  Usage is as follows:
--
--  with Broca.Debug;
--
--  package Broca.Foo is
--
--     Flag : constant Natural := Broca.Debug.Is_Active ("specific_name");
--     procedure O is new Broca.Debug.Output (Flag);
--
--     [...]
--
--     pragma Debug (O ("debugging info"));
--
--  The output will be done if "adabroker.deb" file contains
--  a line with "specific_name"

--  $Id$

package Droopi.Log is

   pragma Elaborate_Body;

   --  Log_Levels are used to classify the importance of messages

   type Log_Level is
     (Debug,
      --  Developer interest only, should never be displayed
      --  in a production environment.

      Info,
      --  Informational message indicating progress of normal
      --  operation.

      Notice,
      --  Notesworthy message in normal operation.

      Warning,
      --  Indication that a condition may be abnormal
      --  and requires attention.

      Error,
      --  Indication that an abnormal condition has been identified.

      Critical
      --  Indication that an abnormal condition has been
      --  identified, and that immediate attention is required
      --  to resume normal operation.
      );

   procedure Initialize;
   --  Initialize the logging subsystem.

   procedure Set_Log_Level (Facility : in String; Level : Log_Level);
   --  Set the log level for Facility to the specified value.

   function Get_Log_Level (Facility : in String) return Log_Level;
   --  Returns the user-requested log level for facility Flag.

   generic
      Facility : String;
   package Facility_Log is

      procedure Output
        (Message : in String;
         Level   : Log_Level := Debug);
      --  Log Message when Level is at least equal to the user-requested
      --  level for Facility.

   end Facility_Log;

end Droopi.Log;
