
public class Alltypes extends all_typesPOA
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

    public all_types echoRef(all_types arg)
    {
	return arg;
    }

    public org.omg.CORBA.Object echoObject(org.omg.CORBA.Object arg)
    {
	return arg;
    }

    public all_typesPackage.Color echoColor(all_typesPackage.Color arg)
    {
	return arg;
    }

    public void testException(int arg)
        throws all_typesPackage.my_exception
    {
	throw new all_typesPackage.my_exception(arg);
    }

    public void testUnknownException(int arg)
    {
	int i = 2;
	int j = 0;
	int k = i/j;
    }

    public all_typesPackage.myUnion echoUnion(all_typesPackage.myUnion arg)
    {
	return arg;
    }

    public all_typesPackage.myUnionEnumSwitch echoUnionEnumSwitch(all_typesPackage.myUnionEnumSwitch arg)
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

    public all_typesPackage.simple_struct echoStruct(all_typesPackage.simple_struct arg)
    {
	return arg;
    }

    public all_typesPackage.array_struct echoArrayStruct(all_typesPackage.array_struct arg)
    {
	return arg;
    }

    public all_typesPackage.nested_struct echoNestedStruct(all_typesPackage.nested_struct arg)
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

    all_typesPackage.Color global_color;

    public all_typesPackage.Color myColor()
    {
	return global_color;
    }

    public void myColor(all_typesPackage.Color value)
    {
	global_color = value;
    }

}
