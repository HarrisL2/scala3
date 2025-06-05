package dotty.tools.backend.jvm.attributes;

public class TypeHints{
    public static class TypeB {
        public static final TypeB NO_HINT = 
            new TypeB((byte)0, -1);

        public static final byte K_KIND = 'K';
        public static final byte M_KIND = 'M';
        public static final byte ARR_K_KIND = 'k';
        public static final byte ARR_M_KIND = 'm';

        private final byte kind;
        private final int index;

        public TypeB(byte kind, int index) {
            this.kind = kind;
            this.index = index;
        }

        public byte getKind() {
            return kind;
        }

        public int getIndex() {
            return index;
        }

        public boolean isNoHint() {
            return this == NO_HINT || (this.kind == 0 && this.index == -1);
        }
    }
}