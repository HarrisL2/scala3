package dotty.tools.backend.jvm.attributes;

import scala.tools.asm.Attribute;
import scala.tools.asm.ByteVector;
import scala.tools.asm.ClassReader;
import scala.tools.asm.ClassWriter;
import scala.tools.asm.Label;

/*
ExtraBoxUnbox_attribute {
  u2 attribute_name_index;
  u4 attribute_length;
  u2 count;
  u2 offsets[count];
}
*/
public class ExtraBoxUnbox extends Attribute {
    public final int[] offsets;

    public ExtraBoxUnbox(int[] offsets) {
        super("ExtraBoxUnbox");
        this.offsets = offsets;
    }

    @Override
    public boolean isUnknown() {
        return false;
    }

    @Override
    public Attribute read(ClassReader cr, int off, int len, char[] buf, int codeOff, Label[] labels) {
        int cur = off;
        int count = cr.readUnsignedShort(cur);
        cur += 2;
        int[] offs = new int[count];
        for (int i = 0; i < count; i++) {
            offs[i] = cr.readUnsignedShort(cur);
            cur += 2;
        }
        return new ExtraBoxUnbox(offs);
    }

    @Override
    public ByteVector write(ClassWriter cw, byte[] code, int len, int maxStack, int maxLocals) {
        ByteVector bv = new ByteVector();
        bv.putShort(offsets.length);
        for (int offset : offsets) {
            bv.putShort(offset);
        }
        return bv;
    }
}