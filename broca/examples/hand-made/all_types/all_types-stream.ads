with Broca.Buffers;
with all_types;

package all_types.Stream is
   procedure Marshall
      (Stream : access Broca.Buffers.Buffer_Type;
       Val : Color);

   function Unmarshall
     (Stream : access Broca.Buffers.Buffer_Type)
       return Color;

   procedure Marshall
      (Stream : access Broca.Buffers.Buffer_Type;
       Val : my_exception_Members);

   function Unmarshall
      (Stream : access Broca.Buffers.Buffer_Type)
       return my_exception_Members;

--     procedure Unmarshall_And_Raise_my_exception
--        (Stream : access Broca.Buffers.Buffer_Type);
--
--     procedure Raise_my_exception
--       (Bod : my_exception_Members);

   procedure Marshall
      (Stream : access Broca.Buffers.Buffer_Type;
       Val : myUnion);

   function Unmarshall
      (Stream : access Broca.Buffers.Buffer_Type)
       return myUnion;

   procedure Marshall
      (Stream : access Broca.Buffers.Buffer_Type;
       Val : simple_array);

   function Unmarshall
      (Stream : access Broca.Buffers.Buffer_Type)
       return simple_array;

   procedure Marshall
      (Stream : access Broca.Buffers.Buffer_Type;
       Val : matrix);

   function Unmarshall
      (Stream : access Broca.Buffers.Buffer_Type)
       return matrix;

   procedure Marshall
      (Stream : access Broca.Buffers.Buffer_Type;
       Val : simple_struct);

   function Unmarshall
      (Stream : access Broca.Buffers.Buffer_Type)
       return simple_struct;

   procedure Marshall
      (Stream : access Broca.Buffers.Buffer_Type;
       Val : a_Array);

   function Unmarshall
      (Stream : access Broca.Buffers.Buffer_Type)
       return a_Array;

   procedure Marshall
      (Stream : access Broca.Buffers.Buffer_Type;
       Val : IDL_Sequence_Short.Sequence);

   function Unmarshall
      (Stream : access Broca.Buffers.Buffer_Type)
       return IDL_Sequence_Short.Sequence;

   procedure Marshall
      (Stream : access Broca.Buffers.Buffer_Type;
       Val : U_sequence);

   function Unmarshall
      (Stream : access Broca.Buffers.Buffer_Type)
       return U_sequence;

end all_types.Stream;
