--  Interface for Sessions.

--  $Id$

with Droopi.Any.NVList;
with Droopi.Components;

package Droopi.Protocols.Interface is

   type Unmarshall_Arguments is new Components.Message with record
      Args : Any.NVList.Ref;
   end record;

end Droopi.Protocols.Interface;
