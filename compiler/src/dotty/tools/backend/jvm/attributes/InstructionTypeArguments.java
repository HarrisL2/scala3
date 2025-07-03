package dotty.tools.backend.jvm.attributes;

import java.util.ArrayList;
import java.util.List;

import scala.tools.asm.Attribute;
import scala.tools.asm.ByteVector;
import scala.tools.asm.ClassReader;
import scala.tools.asm.ClassWriter;
import scala.tools.asm.Label;

public class InstructionTypeArguments extends Attribute {
    private final List<TypeHints.TypeAHint> typeArguments;
    public InstructionTypeArguments() {
        super("InstructionTypeArguments");
        this.typeArguments = new ArrayList<>();
    }
    public InstructionTypeArguments(List<TypeHints.TypeAHint> typeArguments) {
        super("InstructionTypeArguments");
        this.typeArguments = typeArguments;
    }
    @Override
    public boolean isUnknown() {
        return false;
    }
    @Override
    public Attribute read(ClassReader cr, int off, int len, char[] buf, int codeOff, Label[] labels) {
        int cur = off;
        int typeHintLength = cr.readUnsignedShort(cur);
        cur += 2;
        List<TypeHints.TypeAHint> typeHints = new ArrayList<>();
        for (int i = 0; i < typeHintLength; i++) {
            int bytecodeOffset = cr.readUnsignedShort(cur);
            cur += 2;
            int typeAnum = cr.readUnsignedShort(cur);
            cur += 2;
            List<TypeHints.TypeA> typeList = new ArrayList<>();
            for (int j = 0 ; j < typeAnum; j++) {
                byte kind = (byte) cr.readByte(cur);
                cur += 1;
                int index = cr.readUnsignedShort(cur);
                cur += 2;
                typeList.add(new TypeHints.TypeA(kind, index));
            }
            typeHints.add(new TypeHints.TypeAHint(bytecodeOffset, typeList));
        }
        return new InstructionTypeArguments(typeHints);
    }

    @Override
    public ByteVector write(ClassWriter cw, byte[] code, int codeLength, int maxStack, int maxLocals) {
        ByteVector bv = new ByteVector();
        bv.putShort(typeArguments.size());
        for (TypeHints.TypeAHint typeHint : typeArguments) {
            bv.putShort(typeHint.getBytecodeOffset());
            bv.putShort(typeHint.getTypeList().size());
            for (TypeHints.TypeA typeA : typeHint.getTypeList()) {
                bv.putByte(typeA.getKind());
                bv.putShort(typeA.getIndex());
            }
        }
        return bv;
    }

    public List<TypeHints.TypeAHint> getTypeArguments() {
        return typeArguments;
    }
}
