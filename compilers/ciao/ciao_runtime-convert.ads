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
--  $Id: //depot/ciao/main/ciao_runtime-convert.ads#2 $

with CORBA;

package CIAO_Runtime.Convert is

   function To_Ada   (Val : CORBA.Boolean)          return Boolean;

   function To_Ada   (Val : CORBA.Short)            return Integer;
   function To_Ada   (Val : CORBA.Long)             return Integer;
   function To_Ada   (Val : CORBA.Unsigned_Short)   return Integer;
   function To_Ada   (Val : CORBA.Unsigned_Long)    return Integer;

   function To_Ada   (Val : CORBA.Float)            return Float;
   function To_Ada   (Val : CORBA.Double)           return Float;

   function To_Ada   (Val : CORBA.Char)             return Character;
   --  function To_Ada   (Val : CORBA.Octet)            return ;
   function To_Ada   (Val : CORBA.String)           return String;

   pragma Inline (To_Ada);

   function To_CORBA (Val : Boolean)                return CORBA.Boolean;

   function To_CORBA (Val : Integer)                return CORBA.Short;
   function To_CORBA (Val : Integer)                return CORBA.Long;
   function To_CORBA (Val : Integer)                return CORBA.Unsigned_Short;
   function To_CORBA (Val : Integer)                return CORBA.Unsigned_Long;

   function To_CORBA (Val : Float)                  return CORBA.Float;
   function To_CORBA (Val : Float)                  return CORBA.Double;

   function To_CORBA (Val : Character)              return CORBA.Char;
   function To_CORBA (Val : String)                 return CORBA.String;

   pragma Inline (To_CORBA);

end CIAO_Runtime.Convert;
