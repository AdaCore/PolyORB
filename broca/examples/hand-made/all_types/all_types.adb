with Ada.Exceptions;
with CORBA.Object;
with Broca.Exceptions;
with Broca.Refs;
with Broca.Repository;
with Broca.GIOP;
with Broca.Object;
with Broca.CDR;
with CORBA;
with all_types.Stream;
with all_types;
use Ada.Exceptions;
use CORBA.Object;
use Broca.Exceptions;
use Broca.Refs;
use Broca.Repository;
use Broca.GIOP;
use Broca.Object;
use Broca.CDR;
use CORBA;
use all_types.Stream;
use all_types;
pragma Elaborate_All (Broca.Repository);

package body all_types is

   function Unchecked_To_Ref
     (Self : in CORBA.Object.Ref'Class)
      return Ref
   is
      Result : Ref;
   begin
      Broca.Refs.Set (Broca.Refs.Ref (Result),
                      Broca.Refs.Get (Broca.Refs.Ref (Self)));
      return Result;
   end Unchecked_To_Ref;

   function To_Ref
     (Self : in CORBA.Object.Ref'Class)
      return Ref
   is
      Res : Ref;
   begin
      Res := Unchecked_To_Ref (Self);
      if Is_A (Res, Repository_Id) then
         return Res;
      else
         Broca.Exceptions.Raise_Bad_Param;
      end if;
   end To_Ref;

   echoBoolean_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoBoolean");

   function echoBoolean
     (Self : in Ref;
      arg : in CORBA.Boolean)
      return CORBA.Boolean
   is
      Returns : CORBA.Boolean;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoBoolean_Operation);
         Marshall (Handler.Buffer'Access, arg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoBoolean;

   echoShort_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoShort");

   function echoShort
     (Self : in Ref;
      arg : in CORBA.Short)
      return CORBA.Short
   is
      Returns : CORBA.Short;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoShort_Operation);
         Marshall (Handler.Buffer'Access, arg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               --  Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoShort;

   echoLong_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoLong");

   function echoLong
     (Self : in Ref;
      arg : in CORBA.Long)
      return CORBA.Long
   is
      Returns : CORBA.Long;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoLong_Operation);
         Marshall (Handler.Buffer'Access, arg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               --  Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoLong;

   echoUShort_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoUShort");

   function echoUShort
     (Self : in Ref;
      arg : in CORBA.Unsigned_Short)
      return CORBA.Unsigned_Short
   is
      Returns : CORBA.Unsigned_Short;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoUShort_Operation);
         Marshall (Handler.Buffer'Access, arg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               --  Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoUShort;

   echoULong_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoULong");

   function echoULong
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
      Returns : CORBA.Unsigned_Long;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoULong_Operation);
         Marshall (Handler.Buffer'Access, arg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               --  Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoULong;

   echoFloat_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoFloat");

   function echoFloat
     (Self : in Ref;
      arg : in CORBA.Float)
      return CORBA.Float
   is
      Returns : CORBA.Float;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoFloat_Operation);
         Marshall (Handler.Buffer'Access, arg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               --  Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoFloat;

   echoDouble_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoDouble");

   function echoDouble
     (Self : in Ref;
      arg : in CORBA.Double)
      return CORBA.Double
   is
      Returns : CORBA.Double;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoDouble_Operation);
         Marshall (Handler.Buffer'Access, arg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               --  Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoDouble;

   echoChar_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoChar");

   function echoChar
     (Self : in Ref;
      arg : in CORBA.Char)
      return CORBA.Char
   is
      Returns : CORBA.Char;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoChar_Operation);
         Marshall (Handler.Buffer'Access, arg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               --  Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoChar;

   echoOctet_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoOctet");

   function echoOctet
     (Self : in Ref;
      arg : in CORBA.Octet)
      return CORBA.Octet
   is
      Returns : CORBA.Octet;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoOctet_Operation);
         Marshall (Handler.Buffer'Access, arg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               --  Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoOctet;

   echoString_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoString");

   function echoString
     (Self : in Ref;
      arg : in CORBA.String)
      return CORBA.String
   is
      Returns : CORBA.String;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoString_Operation);
         Marshall (Handler.Buffer'Access, arg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               --  Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoString;

--     echoRef_Operation : constant CORBA.Identifier :=
--       CORBA.To_CORBA_String ("echoRef");
--
--     function echoRef
--       (Self : in Ref;
--        arg : in Ref)
--        return Ref
--     is
--        Returns : Ref;
--        use Broca.CDR;
--        use Broca.Refs;
--        Handler : Broca.GIOP.Request_Handler;
--        Sr_Res : Broca.GIOP.Send_Request_Result_Type;
--     begin
--        loop
--           Broca.GIOP.Send_Request_Marshall
--             (Handler, Broca.Object.Object_Ptr (Get (Self)),
--              True, echoRef_Operation);
--           Marshall (Handler.Buffer'Access, arg);
--           Broca.GIOP.Send_Request_Send
--             (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
--           case Sr_Res is
--              when Broca.GIOP.Sr_Reply =>
--                 --  Inout and out parameters.
--                 Returns := Unmarshall (Handler.Buffer'Access);
--                 --  Returns := Unmarshall (Handler.Buffer'Access);
--                 return Returns;
--              when Broca.GIOP.Sr_No_Reply =>
--                 raise Program_Error;
--              when Broca.GIOP.Sr_User_Exception =>
--                 raise Program_Error;
--              when Broca.GIOP.Sr_Forward =>
--                 null;
--           end case;
--        end loop;
--     end echoRef;
--
   echoColor_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoColor");

   function echoColor
     (Self : in Ref;
      arg : in Color)
      return Color
   is
      Returns : Color;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoColor_Operation);
         Marshall (Handler.Buffer'Access, arg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               --  Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoColor;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out my_exception_Members) is
   begin
      Broca.Exceptions.User_Get_Members (From, To);
   end Get_Members;

--     testException_Operation : constant CORBA.Identifier :=
--       CORBA.To_CORBA_String ("testException");
--
--     procedure testException
--       (Self : in Ref;
--        arg : in CORBA.Long)
--     is
--        use Broca.CDR;
--        use Broca.Refs;
--        Handler : Broca.GIOP.Request_Handler;
--        Sr_Res : Broca.GIOP.Send_Request_Result_Type;
--     begin
--        loop
--           Broca.GIOP.Send_Request_Marshall
--             (Handler, Broca.Object.Object_Ptr (Get (Self)),
--              True, testException_Operation);
--           Marshall (Handler.Buffer'Access, arg);
--           Broca.GIOP.Send_Request_Send
--             (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
--           case Sr_Res is
--              when Broca.GIOP.Sr_Reply =>
--                 --  Inout and out parameters.
--                 return;
--              when Broca.GIOP.Sr_No_Reply =>
--                 raise Program_Error;
--              when Broca.GIOP.Sr_User_Exception =>
--                 declare
--                    Exception_Repository_Id : CORBA.String;
--                 begin
--                    Exception_Repository_Id
--                      := Unmarshall (Handler.Buffer'Access);
--                    if Exception_Repository_Id
--                      = "IDL:all_types/my_exception:1.0" then
--                       declare
--                          use all_types.Stream;
--                          Members : all_types.my_exception_Members;
--                       begin
--                          Members := Unmarshall (Handler.Buffer'Access);
--                          User_Raise_Exception
--                            (all_types.my_exception'Identity,
--                             new all_types.my_exception_Members'(Members));
--                       end;
--                    end if;
--                 end;
--                 raise Program_Error;
--              when Broca.GIOP.Sr_Forward =>
--                 null;
--           end case;
--        end loop;
--     end testException;

   echoUnion_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoUnion");

   function echoUnion
     (Self : in Ref;
      arg : in myUnion)
      return myUnion
   is
      Returns : myUnion;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoUnion_Operation);
         Marshall (Handler.Buffer'Access, arg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               --  Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoUnion;

   echoArray_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoArray");

   function echoArray
     (Self : in Ref;
      arg : in simple_array)
      return simple_array
   is
      Returns : simple_array;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoArray_Operation);
         Marshall (Handler.Buffer'Access, arg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               --  Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoArray;

   echoMatrix_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoMatrix");

   function echoMatrix
     (Self : in Ref;
      arg : in matrix)
      return matrix
   is
      Returns : matrix;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoMatrix_Operation);
         Marshall (Handler.Buffer'Access, arg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               --  Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoMatrix;

   echoStruct_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoStruct");

   function echoStruct
     (Self : in Ref;
      arg : in simple_struct)
      return simple_struct
   is
      Returns : simple_struct;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoStruct_Operation);
         Marshall (Handler.Buffer'Access, arg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               --  Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoStruct;

   echoUsequence_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoUsequence");

   function echoUsequence
     (Self : in Ref;
      arg : in U_sequence)
      return U_sequence
   is
      Returns : U_sequence;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoUsequence_Operation);
         Marshall (Handler.Buffer'Access, arg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               --  Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoUsequence;

   Counter_Get_Attribute : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("_get_Counter");

   function Get_Counter
     (Self : in Ref) return CORBA.Long
   is
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
      Result : CORBA.Long;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, Counter_Get_Attribute);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  OD: Operation dependant.
               --  Outcoming arguments.
               Result := Unmarshall (Handler.Buffer'Access);
               return Result;
            when Broca.GIOP.Sr_No_Reply |
                 Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end Get_Counter;

   myColor_Set_Attribute : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("_set_myColor");

   procedure Set_myColor
     (Self : in Ref; To : Color)
   is
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, myColor_Set_Attribute);
         Marshall (Handler.Buffer'Access, To);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               return;
            when Broca.GIOP.Sr_No_Reply |
                 Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end Set_myColor;

   myColor_Get_Attribute : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("_get_myColor");

   function Get_myColor
     (Self : in Ref) return Color
   is
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
      Result : Color;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, myColor_Get_Attribute);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)), True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  OD: Operation dependant.
               --  Outcoming arguments.
               Result := Unmarshall (Handler.Buffer'Access);
               return Result;
            when Broca.GIOP.Sr_No_Reply |
                 Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end Get_myColor;

   function Is_A
     (Self   : in Ref;
      Logical_Type_Id : in CORBA.String)
      return CORBA.Boolean
   is
      Type_Id : String := CORBA.To_Standard_String (Logical_Type_Id);
   begin
      if Type_Id = "IDL:all_types:1.0" or else
        Type_Id = "IDL:omg.org/CORBA/OBJECT:1.0" then
         return True;
      else
         return False;
      end if;
   end Is_A;

   type all_types_Factory_Type is new Broca.Repository.Factory_Type
      with null record;
   function Create (Factory : access all_types_Factory_Type)
                    return CORBA.Object.Ref'Class;

   function Create (Factory : access all_types_Factory_Type)
                    return CORBA.Object.Ref'Class is
      Res : Ref;
   begin
      Broca.Refs.Set (Broca.Refs.Ref (Res),
                      new Broca.Object.Object_Type);
      return Res;
   end Create;

   all_types_Factory : constant Broca.Repository.Factory_Ptr :=
      new all_types_Factory_Type'
        (Next => null, Type_Id => CORBA.RepositoryId (Repository_Id));
begin
   Broca.Repository.Register (all_types_Factory);
end all_types;
