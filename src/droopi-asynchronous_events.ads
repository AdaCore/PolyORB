--  Abstract data type for an asynchrous event source.

--  $Id$

package Droopi.Asynchronous_Events is

   pragma Preelaborate;

   type Asynchronous_Event_Source is abstract tagged limited private;
   type Asynchronous_Event_Source_Access is
     access all Asynchronous_Event_Source'Class;

   procedure Set_Polling
     (AES : access Asynchronous_Event_Source;
      V   : Boolean);
   function Polling
     (AES : access Asynchronous_Event_Source)
     return Boolean;
   --  Accessors to the Polling attribute of an AES.
   --  True if, and only if, a task is currently block in
   --  the AES's Poll primitive.
   --  These accessors must be called only from within a
   --  critical section.

   procedure Poll (AES : access Asynchronous_Event_Source)
     is abstract;
   --  Block until an asynchronous event occurs in AES.
   --  This primitive may block in an operating system call,
   --  and therefore must not be called from within a critical
   --  section.

   procedure Handle_Events (AES : access Asynchronous_Event_Source)
      is abstract;
   --  Process any available event in AES.
   --  This primitive must be called only from within a critical
   --  section, and must not block in an operating system call.

private

   type Asynchronous_Event_Source is abstract tagged limited record
      Polling : Boolean := False;
   end record;

end Droopi.Asynchronous_Events;
