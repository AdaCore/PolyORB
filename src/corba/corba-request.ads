--  $Id$

with Droopi.Requests;

package CORBA.Request is

   type Object is limited private;

private

   type Object is record
      Req : Droopi.Requests.Request;
   end record;

end CORBA.Request;

