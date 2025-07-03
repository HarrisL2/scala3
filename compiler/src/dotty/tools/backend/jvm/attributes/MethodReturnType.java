package dotty.tools.backend.jvm.attributes;

import scala.tools.asm.Attribute;
import scala.tools.asm.ByteVector;
import scala.tools.asm.ClassReader;
import scala.tools.asm.ClassWriter;
import scala.tools.asm.Label;

public class MethodReturnType extends Attribute{
    private final TypeHints.TypeB typeB;

    public MethodReturnType() {
        super("MethodReturnType");
        this.typeB = TypeHints.TypeB.NO_HINT;
    }

    public MethodReturnType(TypeHints.TypeB typeB) {
        super("MethodReturnType");
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
    public Attribute read(ClassReader cr, int off, int len, char[] buf, int codeOff, Label[] labels) {
        int cur = off;
        byte kind = (byte) cr.readByte(cur);
        cur += 1;
        int index = cr.readUnsignedShort(cur);
        //cur += 2;
        return new MethodReturnType(new TypeHints.TypeB(kind, index));
    }

    @Override
    public ByteVector write(ClassWriter cw, byte[] code, int codeLength, int maxStack, int maxLocals) {
        ByteVector bv = new ByteVector();
        bv.putByte(typeB.getKind());
        bv.putShort(typeB.getIndex());
        return bv;
    }
}
