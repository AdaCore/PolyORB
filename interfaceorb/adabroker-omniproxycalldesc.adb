
--  This is a root class. For each subprogram of an IDL interface, which is
--  not declared "one way", a descendant of this class has to be provided.
--  It contains al the information to make the remote call : arguments,
--  results, exceptions, and how to send them on/ reveive them from a giop.
--  ( see proxyCall.h )

with Ada.Exceptions;

with AdaBroker; use AdaBroker;

package body AdaBroker.OmniProxyCallDesc is

   -------------------------
   -- Set_User_Exceptions --
   -------------------------

   procedure Set_User_Exceptions
     (Self           : in out Object;
      Has_Exceptions : CORBA.Boolean) is
   begin
      Self.Pd_Has_User_Exception := Has_Exceptions;
   end Set_User_Exceptions;

   ----------------
   -- Align_Size --
   ----------------

   function Align_Size
     (Self    : in Object;
      Size_In : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long is
   begin
      return Size_In;
   end Align_Size;

   -----------------------
   -- Marshal_Arguments --
   -----------------------

   procedure Marshal_Arguments
     (Self        : in Object;
      GIOP_Client : in out GIOP_C.Object) is
   begin
      null;
   end Marshal_Arguments;

   -------------------------------
   -- Unmarshal_Returned_Values --
   -------------------------------

   procedure Unmarshal_Returned_Values
     (Self        : in out Object;
      GIOP_Client : in out GIOP_C.Object) is
   begin
      null;
   end Unmarshal_Returned_Values;

   --------------------
   -- User_Exception --
   --------------------

   procedure User_Exception
     (Self        : in Object;
      GIOP_Client : in out GIOP_C.Object;
      Repoid      : in CORBA.String) is
   begin
      if Self.Pd_Has_User_Exception then
         Ada.Exceptions.Raise_Exception
           (CORBA.AdaBroker_Fatal_Error'Identity,
            "call desc throwing exception not allowed in User_Exception");
      else
         --  Nothing to be done since we do not throw any exception
         null;
      end if;
   end User_Exception;

   -------------------------
   -- Has_User_Exceptions --
   -------------------------

   function Has_User_Exceptions
     (Self : in Object)
      return CORBA.Boolean is
   begin
      return Self.Pd_Has_User_Exception;
   end Has_User_Exceptions;

end AdaBroker.OmniProxyCallDesc;



