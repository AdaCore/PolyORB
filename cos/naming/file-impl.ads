----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;
with PortableServer;

package File.Impl is

   type Object is
     new PortableServer.Servant_Base
     with record
        Image : CORBA.String;
     end record;

   function New_File
     return File.Ref;

   function get_Image
     (Self : access Object)
     return CORBA.String;

   procedure set_Image
     (Self : access Object;
      To : in CORBA.String);

end File.Impl;
