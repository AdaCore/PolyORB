import idl.*;

public class Alltypes extends idl.all_typesPOA
{
    public boolean echoBoolean(boolean arg)
    {
	return arg;
    }

    public short echoShort(short arg)
    {
	return arg;
    }

    public int echoLong(int arg)
    {
	return arg; 
    }

    public short echoUShort(short arg)
    {
	return arg; 
    }

    public int echoULong(int arg)
    {
	return arg;
    }

    public float echoFloat(float arg)
    {
	return arg;
    }

    public double echoDouble(double arg)
    {
	return arg;
    }

    public char echoChar(char arg)
    {
	return arg;
    }

    public byte echoOctet(byte arg)
    {
	return arg;
    }

    public String echoString(String arg)
    {
	return arg;
    }

    public idl.all_types echoRef(idl.all_types arg)
    {
	return arg;
    }

    public org.omg.CORBA.Object echoObject(org.omg.CORBA.Object arg)
    {
	return arg;
    }

    public idl.all_typesPackage.Color echoColor(idl.all_typesPackage.Color arg)
    {
	return arg;
    }

    public void testException(int arg)
        throws idl.all_typesPackage.my_exception
    {
	throw new idl.all_typesPackage.my_exception(arg);
    }

    public void testUnknownException(int arg)
    {
	int i = 2;
	int j = 0;
	int k = i/j;
    }

    public idl.all_typesPackage.myUnion echoUnion(idl.all_typesPackage.myUnion arg)
    {
	return arg;
    }

    public idl.all_typesPackage.myUnionEnumSwitch echoUnionEnumSwitch(idl.all_typesPackage.myUnionEnumSwitch arg)
    {
	return arg;
    }

    public int[] echoArray(int[] arg)
    {
	return arg;
    }

    public int[][] echoMatrix(int[][] arg)
    {
	return arg;
    }

    public int[][] echoBigMatrix(int[][] arg)
    {
	return arg;
    }

    public idl.all_typesPackage.simple_struct echoStruct(idl.all_typesPackage.simple_struct arg)
    {
	return arg;
    }

    public idl.all_typesPackage.array_struct echoArrayStruct(idl.all_typesPackage.array_struct arg)
    {
	return arg;
    }

    public idl.all_typesPackage.nested_struct echoNestedStruct(idl.all_typesPackage.nested_struct arg)
    {
	return arg;
    }

    public short[] echoUsequence(short[] arg)
    {
	return arg;
    }

    public short[] echoBsequence(short[] arg)
    {
	return arg;
    }

    public java.math.BigDecimal echoMoney(java.math.BigDecimal arg)
    {
	return arg;
    }

    int global_counter = 0;

    public int Counter()
    {
	return global_counter ++;
    }

    idl.all_typesPackage.Color global_color;

    public idl.all_typesPackage.Color myColor()
    {
	return global_color;
    }

    public void myColor(idl.all_typesPackage.Color value)
    {
	global_color = value;
    }

}
