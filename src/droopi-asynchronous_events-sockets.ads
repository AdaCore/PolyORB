--  An asynchrous event source that is a set of socket descriptors.

--  $Id$

with Droopi.Sockets;

package Droopi.Asynchronous_Events.Sockets is

   pragma Elaborate_Body;

   type Socket_Event_Monitor is new Asynchronous_Event_Monitor with private;

   procedure Create (AEM : out Socket_Event_Monitor);
   procedure Destroy (AEM : in out Socket_Event_Monitor);
   
   procedure Register_Source
     (AEM     : in out Socket_Event_Monitor;
      AES     : Asynchronous_Event_Source_Access;
      Success : out Boolean);
   
   procedure Unregister_Source
     (AEM : in out Socket_Event_Monitor;
      AES : Asynchronous_Event_Source_Access);
   
   function Check_Sources
     (AEM     : Socket_Event_Monitor;
      Timeout : Duration)
     return Job_Array;

   procedure Abort_Check_Sources
     (AEM : Socket_Event_Monitor);
   
   type Socket_Kind is (Listening_Sk, Communication_Sk);
   
   function Create_Event_Source
     (Socket : Droopi.Sockets.Socket_Type;
      Kind   : Socket_Kind)
     return Asynchronous_Event_Source_Access;
   
private
   
   type Socket_Event_Source is abstract new Asynchronous_Event_Source 
     with record
	Socket : Droopi.Sockets.Socket_Type;
     end record;
   
   type Comm_Socket_Event_Source is new Socket_Event_Source
     with record
	TE_View : Droopi.Transport.Sockets.Socket_Endpoint
	  (Comm_Socket_Event_Source'Access);
     end record;
   
   type Listen_Socket_Event_Source is new Socket_Event_Source
     with record
	TAP_View : Droopi.Transport.Sockets.Socket_Access_Point
	  (Listen_Socket_Event_Source'Access);
     end record;
   
   type Socket_Event_Monitor is new Asynchronous_Event_Monitor
     with record
	Selector : Droopi.Sockets.Selector_Access;
     end record;
   
end Droopi.Asynchronous_Events.Sockets;
