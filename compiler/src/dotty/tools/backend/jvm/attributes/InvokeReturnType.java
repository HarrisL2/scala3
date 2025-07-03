package dotty.tools.backend.jvm.attributes;

import java.util.ArrayList;
import java.util.List;

import scala.tools.asm.Attribute;
import scala.tools.asm.ByteVector;
import scala.tools.asm.ClassReader;
import scala.tools.asm.ClassWriter;
import scala.tools.asm.Label;

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
            int typeBnum = cr.readUnsignedShort(cur);
            assert typeBnum == 1 : " can only have one typeB";
            cur += 2;
            byte kind = (byte) cr.readByte(cur);
            cur += 1;
            int index = cr.readUnsignedShort(cur);
            cur += 2;
            TypeHints.TypeB typeB = new TypeHints.TypeB(kind, index);
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
            bv.putShort(1);
            TypeHints.TypeB typeB = typeHint.getTypeB();
            bv.putByte(typeB.getKind());
            bv.putShort(typeB.getIndex());
        }
        return bv;
    }
    
}
