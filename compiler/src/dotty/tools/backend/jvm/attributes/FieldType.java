package dotty.tools.backend.jvm.attributes;

import scala.tools.asm.Attribute;
import scala.tools.asm.ByteVector;
import scala.tools.asm.ClassReader;
import scala.tools.asm.ClassWriter;
import scala.tools.asm.Label;

/*
FieldType_attribute{
    u2 attribute_name_index;
	u4 attribute_length;
    u1 K_M_indicator; (should be always K?)
	u2 index;
}
*/
public class FieldType extends Attribute{
    private final TypeHints.TypeB typeB;

    public FieldType() {
        super("FieldType");
        this.typeB = TypeHints.TypeB.NO_HINT;
    }

    public FieldType(TypeHints.TypeB typeB) {
        super("FieldType");
        this.typeB = typeB;
    }

    public TypeHints.TypeB getTypeB() {
        return typeB;
    }

    @Override
    public boolean isUnknown() {
        return false;
    }

    @Override
    @SuppressWarnings("UnusedAssignment")
    public Attribute read(ClassReader cr, int off, int len, char[] buf, int codeOff, Label[] labels) {
        int cur = off;
        byte kind = (byte) cr.readByte(cur);
        cur += 1;
        int index = cr.readUnsignedShort(cur);
        cur += 2;
        return new FieldType(new TypeHints.TypeB(kind, index));
    }

    @Override
    public ByteVector write(ClassWriter cw, byte[] code, int len, int maxStack, int maxLocals) {
        ByteVector bv = new ByteVector();
        bv.putByte(typeB.getKind());
        bv.putShort(typeB.getIndex());
        return bv;
    }
    
}
