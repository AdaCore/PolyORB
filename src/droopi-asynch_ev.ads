--  Abstract data type for an asynchrous event source.

--  $Id$

with Sequences.Unbounded;

with Droopi.Annotations;

package Droopi.Asynch_Ev is

   pragma Elaborate_Body;

   --  Some environment components can produce events in an asynchronous
   --  asynchronous fashion, i.e. independently of middleware actions
   --  currently in progress. A typical example of such components is a
   --  connection to the outside world.
   --  outside world.)

   --  Such components are represented within DROOPI as Asynch_Ev_Source
   --  objects. These objects are registered in collections called
   --  Asynch_Ev_Monitors.

   --  Monitors provide an interface for the middleware to check whether
   --  events have occured on any of their member Asynch_Ev_Sources.

   type Asynch_Ev_Monitor is abstract tagged limited private;
   type Asynch_Ev_Monitor_Access is
     access all Asynch_Ev_Monitor'Class;

   type AEM_Factory is access function
     return Asynch_Ev_Monitor_Access;
   --  A function that allocates an instance of a concrete AEM type.

   type Asynch_Ev_Source is abstract tagged limited private;
   type Asynch_Ev_Source_Access is
     access all Asynch_Ev_Source'Class;

   function Notepad_Of (AES : Asynch_Ev_Source_Access)
     return Annotations.Notepad_Access;
   pragma Inline (Notepad_Of);
   --  An Asynch_Ev_Source is an annotable object (cf. Droopi.Annotations),
   --  so clients can associate it with any information that is necessary
   --  to process events that occur on it.
   --  This functions returns an access to AES' Notepad component.

   function AEM_Factory_Of (AES : Asynch_Ev_Source)
     return AEM_Factory is abstract;
   pragma Inline (AEM_Factory_Of);
   --  Return a factory capable of creating an AEM that can
   --  monitor AES.

   procedure Create (AEM : out Asynch_Ev_Monitor)
     is abstract;
   --  Initialize.

   procedure Destroy (AEM : in out Asynch_Ev_Monitor)
      is abstract;
   --  Finalize.

   procedure Register_Source
     (AEM     : in out Asynch_Ev_Monitor;
      AES     : Asynch_Ev_Source_Access;
      Success : out Boolean)
     is abstract;
   --  Try to register AES for monitoring by AEM.
   --  On exit, Success is True iff AEM accepts AES for monitoring.

   procedure Unregister_Source
     (AEM : in out Asynch_Ev_Monitor;
      AES : Asynch_Ev_Source_Access)
     is abstract;
   --  Remove AES from the set of sources monitored by AEM.

   procedure Unregister_Source
     (AES : Asynch_Ev_Source_Access);
   --  Remove AES from any AEM that it is currently in.

   type AES_Array is array (Integer range <>)
     of Asynch_Ev_Source_Access;

   function Check_Sources
     (AEM     : access Asynch_Ev_Monitor;
      Timeout : Duration)
     return AES_Array
      is abstract;
   --  Wait for events on sources monitored by AEM, and prepare Job
   --  structures for their processing.
   --  Return when one event source in AEM has had an event.
   --  If no event happened within Timeout, an empty array is returned.
   --  Note that a Timeout of 0.0 returns immediatly.
   --  A Timeout of Droopi.Constants.Forever means to not return
   --  until an event occurs.

   procedure Abort_Check_Sources
     (AEM : Asynch_Ev_Monitor)
     is abstract;
   --  Send an abort signal to AEM.

private

   type Asynch_Ev_Source is abstract tagged limited record
      Monitor : Asynch_Ev_Monitor_Access;
      Notes   : aliased Annotations.Notepad;
   end record;

   package Source_Seqs is new Sequences.Unbounded
     (Asynch_Ev_Source_Access);
   subtype Source_Seq is Source_Seqs.Sequence;

   type Asynch_Ev_Monitor is abstract tagged limited record
      Sources : Source_Seq;
   end record;

end Droopi.Asynch_Ev;
