--  Abstract transport service access points and
--  communication endpoints.

--  $Id$

with Droopi.Filters;
with Droopi.Asynchronous_Events;

package Droopi.Transport is

   pragma Preelaborate;

   type Transport_Access_Point
     (AES_View : access Asynchronous_Event_Source)
      is abstract tagged limited record
         Filter_Factories : Filters.Factory_Chain_Access;
      end record;
   --  A listening transport service access point.

   type Transport_Endpoint
     (AES_View : access Asynchronous_Event_Source)
     is abstract tagged limited private;
   --  An opened transport endpoint.

   procedure Read (TE : Transport_Endpoint) return Data_Unit
     is abstract;
   procedure Write (TE : Transport_Endpoint; DU : Data_Unit)
     is abstract;

private

   type Transport_Endpoint
     (AES_View : access Asynchronous_Event_Source'Class)
      is abstract tagged limited record
         Upper : Filters.Filter_Access;
      end record;

end Droopi.Transport;
