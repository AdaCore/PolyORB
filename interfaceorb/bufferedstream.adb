-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package BufferedStream                       ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/22/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Ada.Exceptions ;

package body BufferedStream is

   --------------------------------------------------------
   ----   marshalling objects into buffered streams    ----
   --------------------------------------------------------

   -- Marshal
   ----------
   procedure Marshal (Len : in CORBA.Unsigned_Long ;
                      Buff_Str : in out Object) is
   begin
      Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                     "BufferedStream.Marshal") ;
   end ;

   -- Marshal
   ----------
   procedure Marshal (obj : in CORBA.Char ;
                      Buff_Str : in out Object) is
   begin
      Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                     "BufferedStream.Marshal") ;
   end ;


   --------------------------------------------------------
   ---- unmarshalling objects from buffered streams    ----
   --------------------------------------------------------

   -- UnMarshal
   ------------
   function UnMarshal (Buff_Str : in Object)
                        return CORBA.Char is
   begin
      Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                     "BufferedStream.UnMarshal") ;
      return 'A' ;
   end ;

   -- UnMarshal
   ------------
   function UnMarshal (Buff_Str : in Object)
                        return CORBA.Unsigned_Long is
   begin
      Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                     "BufferedStream.UnMarshal") ;
      return Corba.Unsigned_Long(0) ;
   end ;

   -- UnMarshal
   ------------
   function UnMarshal (Buff_Str : in Object)
                        return CORBA.String is
   begin
      Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                     "BufferedStream.UnMarshal") ;
      return Corba.To_Corba_String("") ;
   end ;


   -------------------------------------------------------
   ----           others                              ----
   -------------------------------------------------------

   -- Put_Char_Array
   -----------------
   procedure Put_Char_Array (Self: in Object ;
                             B: in Corba.String ;
                             Size: in Corba.Unsigned_Long ;
                             Align: in Omni.Alignment_T := Omni.ALIGN_1 ;
                             StartMTU: in Corba.Boolean := False ;
                             At_Most_One: in Corba.Boolean := False ) is
   begin
      Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                     "BufferedStream.Put_Char_Array") ;
   end ;

   -- Get_Char_Array
   -----------------
   procedure Get_Char_Array (Self : in Object ;
                             B : in Corba.String ;
                             Size : in Corba.Unsigned_Long ;
                             Align : in Omni.Alignment_T := Omni.ALIGN_1 ;
                             StartMTU : in Corba.Boolean := False) is
   begin
      Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                     "BufferedStream.Get_Char_Array") ;
   end ;


end BufferedStream ;
