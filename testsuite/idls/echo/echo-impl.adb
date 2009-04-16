-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA;
with Echo.Skel;
pragma Elaborate (Echo.Skel);
pragma Warnings (Off, Echo.Skel);

package body Echo.Impl is


   function echoString
     (Self : access Object;
      Mesg : in CORBA.String)
     return CORBA.String
   is
      Result : CORBA.String;
   begin

      --  Insert implementation of echoString
      Result := CORBA.To_CORBA_String ("Hello Ada!");

      return Result;
   end echoString;

end Echo.Impl;
