--  Abstract data type for an asynchrous event source.

--  $Id$

with Sequences.Unbounded;

with Droopi.Jobs;

package Droopi.Asynchronous_Events is

   type Asynchronous_Event_Monitor is abstract tagged limited private;
   type Asynchronous_Event_Monitor_Access is
     access all Asynchronous_Event_Monitor'Class;

   type Asynchronous_Event_Source is abstract tagged limited private;
   type Asynchronous_Event_Source_Access is
     access all Asynchronous_Event_Source'Class;

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

   Forever : constant Duration;

   type Job_Array is array (Integer range <>) of Jobs.Job_Access;

   function Check_Sources
     (AEM     : Asynchronous_Event_Monitor;
      Timeout : Duration)
     return Job_Array
      is abstract;
   --  Wait for events on sources monitored by AEM, and prepare Job
   --  structures for their processing.
   --  Return when one event source in AEM has had an event.
   --  If no event happened within Timeout, an empty array is returned.
   --  The caller is responsible for deallocation the jobs after they
   --  are processed.
   --  Note that a Timeout of 0.0 returns immediatly.

   procedure Abort_Check_Sources
     (AEM : Asynchronous_Event_Monitor)
     is abstract;
   --  Send an abort signal to AEM.

private

   type Asynchronous_Event_Source is abstract tagged limited null record;

   package Source_Seqs is new Sequences.Unbounded
     (Asynchronous_Event_Source_Access);
   subtype Source_Seq is Source_Seqs.Sequence;

   type Asynchronous_Event_Monitor is abstract tagged limited record
      Sources : Source_Seq;
   end record;

   Forever : constant Duration := Duration'Last;

end Droopi.Asynchronous_Events;
