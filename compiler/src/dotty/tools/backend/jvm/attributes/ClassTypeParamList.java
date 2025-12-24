package dotty.tools.backend.jvm.attributes;

import scala.tools.asm.Attribute;
import scala.tools.asm.ByteVector;
import scala.tools.asm.ClassReader;
import scala.tools.asm.ClassWriter;
import scala.tools.asm.Label;

/*
{
   u2 attribute_name_index;
   u4 attribute_length;
   u2 class_type_param_num;
   u2 type_param_field_ref_index[class_type_param_num]; // points to a FieldRef entry in constant pool. CONSTANT_Fieldref_info.class_index must points to the current class
}
*/
public class ClassTypeParamList extends Attribute{
    private final String owner;
    private final String[] fieldNames;

    public ClassTypeParamList(String owner, String[] fieldNames) {
        super("ClassTypeParamList");
        this.owner = owner;
        this.fieldNames = fieldNames;
    }

    @Override
    public boolean isUnknown() {
        return false;
    }

    @Override
    protected Attribute read(ClassReader cr, int off, int len, char[] buf, int codeOff, Label[] labels) {
        int cur = off;
        int count = cr.readUnsignedShort(off);
        cur += 2;
        String[] fieldNames = new String[count];
        for (int i = 0; i < count; i++){
            int fieldRefIndex = cr.readUnsignedShort(cur);
            cur += 2;
            fieldNames[i] = cr.readUTF8(cr.getItem(fieldRefIndex), buf); // ?
        }
        return new ClassTypeParamList("", fieldNames); // ?
    }

    @Override
    protected ByteVector write(ClassWriter cw, byte[] code, int len, int maxStack, int maxLocals) {
        ByteVector bv = new ByteVector();
        bv.putShort(fieldNames.length);
        for (String fieldName : fieldNames) {
            int fieldRefIndex = cw.newField(owner, fieldName, "B"); //should be already in the constant pool
            bv.putShort(fieldRefIndex);
        }
        return bv;
    }
}