package dotty.tools.backend.jvm.attributes;

import java.util.ArrayList;
import java.util.List;

import scala.tools.asm.Attribute;
import scala.tools.asm.ByteVector;
import scala.tools.asm.ClassReader;
import scala.tools.asm.ClassWriter;
import scala.tools.asm.Label;

/*
MethodParameterType_attribute {
	u2 attribute_name_index;
	u4 attribute_length;
	u2 parameter_count;
	{	u1 K_M_indicator
        u2 index
    } typeBs[parameter_count];
}
*/
public class MethodParameterType extends Attribute{
    private final int count;
    private final List<TypeHints.TypeB> typeList;
    public MethodParameterType() {
        super("MethodParameterType");
        this.count = 0;
        this.typeList = new ArrayList<>();
    }

    public MethodParameterType(int count, List<TypeHints.TypeB> typeList) {
        super("MethodParameterType");
        this.count = count;
        this.typeList = typeList;
    }

    public int getCount() {
        return count;
    }

    public List<TypeHints.TypeB> getTypeList() {
        return typeList;
    }

    @Override
    public boolean isUnknown() {
        return false;
    }

    @Override
    public Attribute read(ClassReader cr, int off, int len, char[] buf, int codeOff, Label[] labels){
        int cur = off;
        int parameterCount = cr.readUnsignedShort(cur);
        cur += 2;
        List<TypeHints.TypeB> typeBs = new ArrayList<>();
        for (int i = 0; i < parameterCount; i++) {
            byte kind = (byte) cr.readByte(cur);
            cur += 1;
            int index = cr.readUnsignedShort(cur);
            cur += 2;
            typeBs.add(new TypeHints.TypeB(kind, index));
        }
        return new MethodParameterType(parameterCount, typeBs);
    }

    @Override
    public ByteVector write(ClassWriter cw, byte[] code, int codeLength, int maxStack, int maxLocals){
        ByteVector bv = new ByteVector();
        bv.putShort(count);
        for (TypeHints.TypeB typeB : typeList) {
            bv.putByte(typeB.getKind());
            bv.putShort(typeB.getIndex());
        }
        return bv;
    }

}
