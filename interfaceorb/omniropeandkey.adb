-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package body omniRopeAndKey                  ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Corba, Rope ;

package body OmniRopeAndKey is

   procedure Init (Self : in out Object ;
                     R : in Rope.Object ;
                     K : in CORBA.Octet ;
                     Ksize : in CORBA.Unsigned_Long);
   -- wrapper around inline omniRopeAndKey(Rope *r,
   --                              _CORBA_Octet *k, _CORBA_ULong ksize)
   -- in omniInternal.h L 234


   package Address_To_Octet is new System.Address_To_Access_Conversions (CORBA.Octet) ;

   function Key (This : in Object) return CORBA.Octet is
      function C_Key (This : in Object) return Address_To_Octet.Object_Pointer;
      pragma Import (CPP,C_Key,"key__14omniRopeAndKeyPUcUl");
   begin
      return (Address_To_Octet.To_Address(C_Key(This))).all ;
   end;


   function Rope (This : in Object) return Rope.Object is
   begin
      return (This.Pd_R)'all ;
   end;


   function Key_Size (This : in Object) return CORBA.Unsigned_Long is
   begin
      return This.Pd_KeySize ;
   end;


end OmniRopeAndKey ;











