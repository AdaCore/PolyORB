
class all_types_i : public POA_all_types,
		    public PortableServer::RefCountServantBase
{
public:
  inline all_types_i() {tmp = 0; tmpColor = all_types::Green;};
  virtual ~all_types_i() {};
  
  CORBA::Boolean echoBoolean(CORBA::Boolean arg) {return arg;};
  CORBA::Short echoShort(CORBA::Short arg) {return arg;};
  CORBA::Long echoLong(CORBA::Long arg) {return arg;};
  CORBA::UShort echoUShort(CORBA::UShort arg) {return arg;};
  CORBA::ULong echoULong(CORBA::ULong arg) {return arg;};
  CORBA::Float echoFloat(CORBA::Float arg) {return arg;};
  CORBA::Double echoDouble(CORBA::Double arg) {return arg;};
  CORBA::Char echoChar(CORBA::Char arg) {return arg;};
  CORBA::Octet echoOctet(CORBA::Octet arg) {return arg;};
  all_types::Color echoColor(all_types::Color arg) {return arg;};

  char* echoString(const char* arg) 
  {
    fprintf(stderr, "Thus spoke the client : %s", arg);
    return string_dup(arg);
  };

  all_types::U_sequence* echoUsequence(const all_types::U_sequence& arg) 
  {
    all_types::U_sequence *t = new 
      all_types::U_sequence(arg);			    
    return t;
  };
  
  all_types::B_sequence* echoBsequence(const all_types::B_sequence& arg)
  {
    all_types::B_sequence *t = new
      all_types::B_sequence(arg);
    return t;
  }
  all_types::simple_array_slice* echoArray(const all_types::simple_array arg)
  {
    return all_types::simple_array_dup(arg);
  };

  all_types::matrix_slice* echoMatrix(const all_types::matrix arg) 
  {
    return all_types::matrix_dup(arg);
  };

  all_types::bigmatrix_slice* echoBigMatrix(const all_types::bigmatrix arg) 
  {
    return all_types::bigmatrix_dup(arg);
  };


  all_types::simple_struct* echoStruct(const all_types::simple_struct & arg)
  {
    all_types::simple_struct* t = new 
      all_types::simple_struct(arg);

    return t;
  };

  all_types::array_struct echoArrayStruct(const all_types::array_struct & arg)
  {
    return (all_types::array_struct) arg;
  };

  all_types::nested_struct* echoNestedStruct(const all_types::nested_struct & arg)
  {
    all_types::nested_struct* t = new
      all_types::nested_struct(arg);

    return t;
  };

  all_types::myUnion echoUnion(const all_types::myUnion & arg)
  {
    return arg;
  }

  all_types::myUnionEnumSwitch* echoUnionEnumSwitch(const all_types::myUnionEnumSwitch & arg)
  {
    return (all_types::myUnionEnumSwitch*) &arg;
  }
  
  CORBA::Long Counter() {return ++tmp;};
  
  all_types::Color myColor() {return tmpColor;};
  void myColor(all_types::Color arg) {tmpColor = arg;};

  void testException(CORBA::Long arg) 
  {
    CORBA::UserException *u = new all_types::my_exception(arg);
    u->_raise();
  };


  void testUnknownException(CORBA::Long arg) 
  {
    throw 666;
  };

  _objref_all_types* echoRef( _objref_all_types* arg)
  {return arg;};
  

   CORBA::Object_ptr echoObject(CORBA::Object_ptr arg)
   {
     if (CORBA::is_nil (arg))
       return all_types::_nil();
     else
       {
	 CORBA::Object_ptr grouik;
	 grouik = arg->_duplicate(arg);
	 return grouik;
       }
   }

  all_types::otherAllTypes_ptr 
  echoOtherAllTypes(all_types::otherAllTypes_ptr arg)
  {
    return arg;
  }
  
  all_types::otherObject_ptr echoOtherObject(all_types::otherObject_ptr arg)
  {
    if (CORBA::is_nil (arg))
      return all_types::_nil();
    else
      {
	CORBA::Object_ptr grouik;
	 grouik = arg->_duplicate(arg);
	 return grouik;
      }
  }

  all_types::Money echoMoney(const all_types::Money& arg)
  {
    return arg;
  }

private:
  CORBA::Long tmp;
  all_types::Color tmpColor;
};

