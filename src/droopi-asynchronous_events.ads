--  Abstract data type for an asynchrous event source.

--  $Id$

with Sequences.Unbounded;

with Droopi.Annotations;

package Droopi.Asynchronous_Events is

   type Asynchronous_Event_Monitor is abstract tagged limited private;
   type Asynchronous_Event_Monitor_Access is
     access all Asynchronous_Event_Monitor'Class;

   type AEM_Factory is access function
     return Asynchronous_Event_Monitor_Access;
   --  A function that allocates an instance of a concrete AEM type.

   type Asynchronous_Event_Source is abstract tagged limited private;
   type Asynchronous_Event_Source_Access is
     access all Asynchronous_Event_Source'Class;

   function Notepad_Of (AES : Asynchronous_Event_Source_Access)
     return Annotations.Notepad_Access;
   pragma Inline (Notepad_Of);
   --  Return the AES' Notepad.

   function AEM_Factory_Of (AES : Asynchronous_Event_Source)
     return AEM_Factory is abstract;
   pragma Inline (AEM_Factory_Of);
   --  Return a factory capable of creating an AEM that can
   --  monitor AES.

   procedure Create (AEM : out Asynchronous_Event_Monitor)
     is abstract;
   --  Initialize.

   procedure Destroy (AEM : in out Asynchronous_Event_Monitor)
      is abstract;
   --  Finalize.

   procedure Register_Source
     (AEM     : in out Asynchronous_Event_Monitor;
      AES     : Asynchronous_Event_Source_Access;
      Success : out Boolean)
     is abstract;
   --  Try to register AES for monitoring by AEM.
   --  On exit, Success is True iff AEM accepts AES for monitoring.

   procedure Unregister_Source
     (AEM : in out Asynchronous_Event_Monitor;
      AES : Asynchronous_Event_Source_Access)
     is abstract;
   --  Remove AES from the set of sources monitored by AEM.

   procedure Unregister_Source
     (AES : Asynchronous_Event_Source_Access);
   --  Remove AES from any AEM that it is currently in.

   Forever : constant Duration;

   type AES_Array is array (Integer range <>)
     of Asynchronous_Event_Source_Access;

   function Check_Sources
     (AEM     : access Asynchronous_Event_Monitor;
      Timeout : Duration)
     return AES_Array
      is abstract;
   --  Wait for events on sources monitored by AEM, and prepare Job
   --  structures for their processing.
   --  Return when one event source in AEM has had an event.
   --  If no event happened within Timeout, an empty array is returned.
   --  Note that a Timeout of 0.0 returns immediatly.

   procedure Abort_Check_Sources
     (AEM : Asynchronous_Event_Monitor)
     is abstract;
   --  Send an abort signal to AEM.

private

   type Asynchronous_Event_Source is abstract tagged limited record
      Monitor : Asynchronous_Event_Monitor_Access;
      Notes   : aliased Annotations.Notepad;
   end record;

   package Source_Seqs is new Sequences.Unbounded
     (Asynchronous_Event_Source_Access);
   subtype Source_Seq is Source_Seqs.Sequence;

   type Asynchronous_Event_Monitor is abstract tagged limited record
      Sources : Source_Seq;
   end record;

   Forever : constant Duration := Duration'Last;

end Droopi.Asynchronous_Events;
