----------------------------------------
--                                    --
--       ----  ---     --  ----       --
--       -      -     - -  -  -       --
--       -      -    ----  -  -       --
--       ----  ---  -   -  ----       --
--                                    --
----------------------------------------
--  CORBA                             --
--  Interface for                     --
--  Ada'95 distributed systems annex  --
--  Objects                           --
----------------------------------------
--  Copyright (c) 1999                --
--  École nationale supérieure des    --
--  télécommunications                --
----------------------------------------

--  The CIAO run-time library.
--  $Id: //depot/ciao/main/ciao_runtime-convert.adb#2 $

package body CIAO_Runtime.Convert is

   function To_Ada   (Val : CORBA.Boolean)          return Boolean is
   begin
      return Val;
   end To_Ada;

   function To_Ada   (Val : CORBA.Short)            return Integer is
   begin
      return Integer (Val);
   end To_Ada;

   function To_Ada   (Val : CORBA.Long)             return Integer is
   begin
      return Integer (Val);
   end To_Ada;

   function To_Ada   (Val : CORBA.Unsigned_Short)   return Integer is
   begin
      return Integer (Val);
   end To_Ada;

   function To_Ada   (Val : CORBA.Unsigned_Long)    return Integer is
   begin
      return Integer (Val);
   end To_Ada;

   function To_Ada   (Val : CORBA.Float)            return Float is
   begin
      return Float (Val);
   end To_Ada;

   function To_Ada   (Val : CORBA.Double)           return Float is
   begin
      return Float (Val);
   end To_Ada;

   function To_Ada   (Val : CORBA.Char)             return Character is
   begin
      return Character (Val);
   end To_Ada;

   function To_Ada   (Val : CORBA.String)           return String is
   begin
      return CORBA.To_Standard_String (Val);
   end To_Ada;

   function To_CORBA (Val : Boolean)                return CORBA.Boolean is
   begin
      return Val;
   end To_CORBA;

   function To_CORBA (Val : Integer)                return CORBA.Short is
   begin
      return CORBA.Short (Val);
   end To_CORBA;

   function To_CORBA (Val : Integer)                return CORBA.Long is
   begin
      return CORBA.Long (Val);
   end To_CORBA;

   function To_CORBA (Val : Integer)                return CORBA.Unsigned_Short is
   begin
      return CORBA.Unsigned_Short (Val);
   end To_CORBA;

   function To_CORBA (Val : Integer)                return CORBA.Unsigned_Long is
   begin
      return CORBA.Unsigned_Long (Val);
   end To_CORBA;

   function To_CORBA (Val : Float)                  return CORBA.Float is
   begin
      return CORBA.Float (Val);
   end To_CORBA;

   function To_CORBA (Val : Float)                  return CORBA.Double is
   begin
      return CORBA.Double (Val);
   end To_CORBA;

   function To_CORBA (Val : Character)              return CORBA.Char is
   begin
      return Val;
   end To_CORBA;

   function To_CORBA (Val : String)                 return CORBA.String is
   begin
      return CORBA.To_CORBA_String (Val);
   end To_CORBA;

end CIAO_Runtime.Convert;
