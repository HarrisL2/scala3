package dotty.tools.backend.jvm.attributes;

import scala.tools.asm.Attribute;
import scala.tools.asm.ByteVector;
import scala.tools.asm.ClassReader;
import scala.tools.asm.ClassWriter;
import scala.tools.asm.Label;

/*
ClassTypeParameterCount_attribute {
    u2 attribute_name_index;
	u4 attribute_length;
	u2 count;
}
*/
public class ClassTypeParameterCount extends Attribute{
    private final int count;
    public ClassTypeParameterCount() {
        super("ClassTypeParameterCount");
        this.count = 0;
    }
    public ClassTypeParameterCount(int count) {
        super("ClassTypeParameterCount");
        this.count = count;
    }
    public int getCount() {
        return count;
    }
    @Override
    public boolean isUnknown() {
        return false;
    }
    @Override
    protected Attribute read(ClassReader cr, int off, int len, char[] buf, int codeOff, Label[] labels) {
        int localCount = cr.readUnsignedShort(off);
        return new MethodTypeParameterCount(localCount);
    }

    @Override
    protected ByteVector write(ClassWriter cw, byte[] code, int codeLength, int maxStack, int maxLocals) {
        ByteVector bv = new ByteVector();
        bv.putShort(count);
        return bv;
    }
}
