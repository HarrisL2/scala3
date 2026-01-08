package dotty.tools.backend.jvm.attributes;

import java.util.List;
import scala.tools.asm.Attribute;
import scala.tools.asm.ByteVector;
import scala.tools.asm.ClassReader;
import scala.tools.asm.ClassWriter;
import scala.tools.asm.Label;

public class BCNewTypeArgs extends Attribute {
    public static class Entry {
        public final int offset;
        public final int[] localIndices;

        public Entry(int offset, int[] localIndices) {
            this.offset = offset;
            this.localIndices = localIndices;
        }
    }

    private final List<Entry> entries;

    public BCNewTypeArgs(List<Entry> entries) {
        super("BCNewTypeArgs");
        this.entries = entries;
    }

    @Override
    public boolean isUnknown() {
        return false;
    }
        
    @Override
    protected ByteVector write(ClassWriter cw, byte[] code, int len, int maxStack, int maxLocals) {
        ByteVector bv = new ByteVector();
        bv.putShort(entries.size());
        for (Entry entry : entries) {
            bv.putShort(entry.offset);
            bv.putShort(entry.localIndices.length);
            for (int localIndex : entry.localIndices) {
                bv.putShort(localIndex);
            }
        }
        return bv;
    }
}