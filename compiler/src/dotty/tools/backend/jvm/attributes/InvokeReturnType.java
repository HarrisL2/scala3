package dotty.tools.backend.jvm.attributes;

import java.util.ArrayList;
import java.util.List;

import scala.tools.asm.Attribute;
import scala.tools.asm.ByteVector;
import scala.tools.asm.ClassReader;
import scala.tools.asm.ClassWriter;
import scala.tools.asm.Label;

/*
InvokeReturnType_attribute {
	u2 attribute_name_index;
	u4 attribute_length;
	u2 typehint_length;
	{ 	u2 byecode_offset
		u1 K_M_indicator
        u2 outer_class_indicator
        u2 index
    } typeHints[typehint_length];
}
*/
public class InvokeReturnType extends Attribute{
    private final int count;
    private final List<TypeHints.TypeBHint> typeList;
    public InvokeReturnType() {
        super("InvokeReturnType");
        this.count = 0;
        this.typeList = new ArrayList<>();
    }
    public InvokeReturnType(int count, List<TypeHints.TypeBHint> typeList) {
        super("InvokeReturnType");
        assert count == typeList.size();
        this.count = count;
        this.typeList = typeList;
    }

    public int getCount() {
        return count;
    }

    public List<TypeHints.TypeBHint> getTypeList() {
        return typeList;
    }

    @Override
    public boolean isUnknown() {
        return false;
    }

    @Override
    public Attribute read(ClassReader cr, int off, int len, char[] buf, int codeOff, Label[] labels){
        int cur = off;
        int typeHintLength = cr.readUnsignedShort(cur);
        cur += 2;
        List<TypeHints.TypeBHint> typeHints = new ArrayList<>();
        for (int i = 0; i < typeHintLength; i++){
            int bytecodeOffset = cr.readUnsignedShort(cur);
            cur += 2;
            byte kind = (byte) cr.readByte(cur);
            cur += 1;
            int outerClassIndex = cr.readUnsignedShort(cur);
            cur += 2;
            int index = cr.readUnsignedShort(cur);
            cur += 2;
            TypeHints.TypeB typeB = new TypeHints.TypeB(kind, outerClassIndex, index);
            typeHints.add(new TypeHints.TypeBHint(bytecodeOffset, typeB));
        }
        return new InvokeReturnType(typeHintLength, typeHints);
    }

    @Override
    public ByteVector write(ClassWriter cw, byte[] code, int codeLength, int maxStack, int maxLocals) {
        ByteVector bv = new ByteVector();
        bv.putShort(count);
        for (TypeHints.TypeBHint typeHint : typeList) {
            bv.putShort(typeHint.getBytecodeOffset());
            TypeHints.TypeB typeB = typeHint.getTypeB();
            bv.putByte(typeB.getKind());
            bv.putShort(typeB.getOuterClassIndex());
            bv.putShort(typeB.getIndex());
        }
        return bv;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("InvokeReturnType{");
        sb.append("typeList=[");
        for (TypeHints.TypeBHint typeBHint : typeList) {
            sb.append(typeBHint.toString()).append(", ");
        }
        if (!typeList.isEmpty()) {
            sb.setLength(sb.length() - 2);
        }
        sb.append("]}");
        return sb.toString();
    }
}
