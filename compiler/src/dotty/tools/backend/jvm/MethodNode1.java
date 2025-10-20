/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package dotty.tools.backend.jvm;

import dotty.tools.backend.jvm.attributes.InstructionTypeArguments;
import dotty.tools.backend.jvm.attributes.InvokeReturnType;
import java.util.Map;

import dotty.tools.backend.jvm.attributes.TypeHints;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import scala.tools.asm.Label;
import scala.tools.asm.Opcodes;
import scala.tools.asm.tree.VarInsnNode;
import scala.tools.asm.tree.LdcInsnNode;
import scala.tools.asm.tree.TableSwitchInsnNode;
import scala.tools.asm.tree.LookupSwitchInsnNode;
import scala.tools.asm.tree.LabelNode;
import scala.tools.asm.tree.LineNumberNode;
import scala.tools.asm.tree.MethodNode;
import scala.tools.asm.tree.AbstractInsnNode;
import scala.tools.asm.tree.TypeInsnNode;
/**
 * A subclass of {@link MethodNode} to customize the representation of
 * label nodes with {@link LabelNode1}.
 */
public class MethodNode1 extends MethodNode {
    private final boolean DEBUG = false;

    public Map<AbstractInsnNode, TypeHints.TypeB> invokeReturnTypeBs = new LinkedHashMap<>();

    public Map<AbstractInsnNode, List<TypeHints.TypeA>> instructionTypeArgTypeAs = new LinkedHashMap<>();

    private Map<AbstractInsnNode, Integer> offsetMap = new LinkedHashMap<>();

    private InvokeReturnType invokeReturnTypeAttribute;

    private InstructionTypeArguments instructionTypeArgumentsAttribute;

    public MethodNode1(int api, int access, String name, String descriptor, String signature, String[] exceptions) {
        super(api, access, name, descriptor, signature, exceptions);
    }

    public MethodNode1(int access, String name, String descriptor, String signature, String[] exceptions) {
        this(Opcodes.ASM6, access, name, descriptor, signature, exceptions);
    }

    public MethodNode1(int api) {
        super(api);
    }

    public MethodNode1() {
        this(Opcodes.ASM6);
    }

    public void genOffsetMap(){
        if (!offsetMap.isEmpty() && DEBUG) {
            System.out.println("already non empty");
            return;
        }
        int currentOffset = 0;
        for (AbstractInsnNode insn : instructions){
            if (insn instanceof LabelNode || 
                insn instanceof LineNumberNode){
                continue;
            }
            offsetMap.put(insn, currentOffset);
            int opcode = insn.getOpcode();
            switch (opcode) {
                case Opcodes.NOP:
                case Opcodes.ACONST_NULL:
                case Opcodes.ICONST_M1:
                case Opcodes.ICONST_0:
                case Opcodes.ICONST_1:
                case Opcodes.ICONST_2:
                case Opcodes.ICONST_3:
                case Opcodes.ICONST_4:
                case Opcodes.ICONST_5:
                case Opcodes.LCONST_0:
                case Opcodes.LCONST_1:
                case Opcodes.FCONST_0:
                case Opcodes.FCONST_1:
                case Opcodes.FCONST_2:
                case Opcodes.DCONST_0:
                case Opcodes.DCONST_1:
                case Opcodes.IALOAD:
                case Opcodes.LALOAD:
                case Opcodes.FALOAD:
                case Opcodes.DALOAD:
                case Opcodes.AALOAD:
                case Opcodes.BALOAD:
                case Opcodes.CALOAD:
                case Opcodes.SALOAD:
                case Opcodes.IASTORE:
                case Opcodes.LASTORE:
                case Opcodes.FASTORE:
                case Opcodes.DASTORE:
                case Opcodes.AASTORE:
                case Opcodes.BASTORE:
                case Opcodes.CASTORE:
                case Opcodes.SASTORE:
                case Opcodes.POP:
                case Opcodes.POP2:
                case Opcodes.DUP:
                case Opcodes.DUP_X1:
                case Opcodes.DUP_X2:
                case Opcodes.DUP2:
                case Opcodes.DUP2_X1:
                case Opcodes.DUP2_X2:
                case Opcodes.SWAP:
                case Opcodes.IADD:
                case Opcodes.LADD:
                case Opcodes.FADD:
                case Opcodes.DADD:
                case Opcodes.ISUB:
                case Opcodes.LSUB:
                case Opcodes.FSUB:
                case Opcodes.DSUB:
                case Opcodes.IMUL:
                case Opcodes.LMUL:
                case Opcodes.FMUL:
                case Opcodes.DMUL:
                case Opcodes.IDIV:
                case Opcodes.LDIV:
                case Opcodes.FDIV:
                case Opcodes.DDIV:
                case Opcodes.IREM:
                case Opcodes.LREM:
                case Opcodes.FREM:
                case Opcodes.DREM:
                case Opcodes.INEG:
                case Opcodes.LNEG:
                case Opcodes.FNEG:
                case Opcodes.DNEG:
                case Opcodes.ISHL:
                case Opcodes.LSHL:
                case Opcodes.ISHR:
                case Opcodes.LSHR:
                case Opcodes.IUSHR:
                case Opcodes.LUSHR:
                case Opcodes.IAND:
                case Opcodes.LAND:
                case Opcodes.IOR:
                case Opcodes.LOR:
                case Opcodes.IXOR:
                case Opcodes.LXOR:
                case Opcodes.I2L:
                case Opcodes.I2F:
                case Opcodes.I2D:
                case Opcodes.L2I:
                case Opcodes.L2F:
                case Opcodes.L2D:
                case Opcodes.F2I:
                case Opcodes.F2L:
                case Opcodes.F2D:
                case Opcodes.D2I:
                case Opcodes.D2L:
                case Opcodes.D2F:
                case Opcodes.I2B:
                case Opcodes.I2C:
                case Opcodes.I2S:
                case Opcodes.LCMP:
                case Opcodes.FCMPL:
                case Opcodes.FCMPG:
                case Opcodes.DCMPL:
                case Opcodes.DCMPG:
                case Opcodes.IRETURN:
                case Opcodes.LRETURN:
                case Opcodes.FRETURN:
                case Opcodes.DRETURN:
                case Opcodes.ARETURN:
                case Opcodes.RETURN:
                case Opcodes.ARRAYLENGTH:
                case Opcodes.ATHROW:
                case Opcodes.MONITORENTER:
                case Opcodes.MONITOREXIT:
                case Constants1.ILOAD_0:
                case Constants1.ILOAD_1:
                case Constants1.ILOAD_2:
                case Constants1.ILOAD_3:
                case Constants1.LLOAD_0:
                case Constants1.LLOAD_1:
                case Constants1.LLOAD_2:
                case Constants1.LLOAD_3:
                case Constants1.FLOAD_0:
                case Constants1.FLOAD_1:
                case Constants1.FLOAD_2:
                case Constants1.FLOAD_3:
                case Constants1.DLOAD_0:
                case Constants1.DLOAD_1:
                case Constants1.DLOAD_2:
                case Constants1.DLOAD_3:
                case Constants1.ALOAD_0:
                case Constants1.ALOAD_1:
                case Constants1.ALOAD_2:
                case Constants1.ALOAD_3:
                case Constants1.ISTORE_0:
                case Constants1.ISTORE_1:
                case Constants1.ISTORE_2:
                case Constants1.ISTORE_3:
                case Constants1.LSTORE_0:
                case Constants1.LSTORE_1:
                case Constants1.LSTORE_2:
                case Constants1.LSTORE_3:
                case Constants1.FSTORE_0:
                case Constants1.FSTORE_1:
                case Constants1.FSTORE_2:
                case Constants1.FSTORE_3:
                case Constants1.DSTORE_0:
                case Constants1.DSTORE_1:
                case Constants1.DSTORE_2:
                case Constants1.DSTORE_3:
                case Constants1.ASTORE_0:
                case Constants1.ASTORE_1:
                case Constants1.ASTORE_2:
                case Constants1.ASTORE_3:
                    currentOffset += 1;
                    break;
                case Opcodes.IFEQ:
                case Opcodes.IFNE:
                case Opcodes.IFLT:
                case Opcodes.IFGE:
                case Opcodes.IFGT:
                case Opcodes.IFLE:
                case Opcodes.IF_ICMPEQ:
                case Opcodes.IF_ICMPNE:
                case Opcodes.IF_ICMPLT:
                case Opcodes.IF_ICMPGE:
                case Opcodes.IF_ICMPGT:
                case Opcodes.IF_ICMPLE:
                case Opcodes.IF_ACMPEQ:
                case Opcodes.IF_ACMPNE:
                case Opcodes.GOTO:
                case Opcodes.JSR:
                case Opcodes.IFNULL:
                case Opcodes.IFNONNULL:
                    currentOffset += 3;
                    break;
                case Constants1.ASM_IFEQ:
                case Constants1.ASM_IFNE:
                case Constants1.ASM_IFLT:
                case Constants1.ASM_IFGE:
                case Constants1.ASM_IFGT:
                case Constants1.ASM_IFLE:
                case Constants1.ASM_IF_ICMPEQ:
                case Constants1.ASM_IF_ICMPNE:
                case Constants1.ASM_IF_ICMPLT:
                case Constants1.ASM_IF_ICMPGE:
                case Constants1.ASM_IF_ICMPGT:
                case Constants1.ASM_IF_ICMPLE:
                case Constants1.ASM_IF_ACMPEQ:
                case Constants1.ASM_IF_ACMPNE:
                case Constants1.ASM_GOTO:
                case Constants1.ASM_JSR:
                case Constants1.ASM_IFNULL:
                case Constants1.ASM_IFNONNULL:
                    currentOffset += 3;
                    break;
                case Constants1.GOTO_W:
                case Constants1.JSR_W:
                case Constants1.ASM_GOTO_W:
                    currentOffset += 5;
                    break;
                case Constants1.WIDE:
                    //TODO: unimplemented
                    throw new IllegalArgumentException();
                case Opcodes.TABLESWITCH:
                    // Skip 0 to 3 padding bytes.
                    currentOffset += 4 - (currentOffset & 3);
                    TableSwitchInsnNode tbl = (TableSwitchInsnNode) insn;
                    int numTableEntries = tbl.max - tbl.min + 1;
                    currentOffset += 12;
                    while (numTableEntries-- > 0) currentOffset += 4;
                    break;
                case Opcodes.LOOKUPSWITCH:
                    // Skip 0 to 3 padding bytes.
                    currentOffset += 4 - (currentOffset & 3);
                    LookupSwitchInsnNode lkup = (LookupSwitchInsnNode) insn;
                    int numSwitchCases = lkup.keys.size();
                    currentOffset += 8;
                    while (numSwitchCases-- > 0) currentOffset += 8;
                    break;
                case Opcodes.ILOAD:
                case Opcodes.LLOAD:
                case Opcodes.FLOAD:
                case Opcodes.DLOAD:
                case Opcodes.ALOAD:
                case Opcodes.ISTORE:
                case Opcodes.LSTORE:
                case Opcodes.FSTORE:
                case Opcodes.DSTORE:
                case Opcodes.ASTORE:
                    VarInsnNode varInsn = (VarInsnNode) insn;
                    currentOffset += (varInsn.var > 3) ? 2 : 1;
                    break;
                case Opcodes.RET:
                case Opcodes.BIPUSH:
                case Opcodes.NEWARRAY:
                    currentOffset += 2;
                    break;
                case Opcodes.LDC:
                    LdcInsnNode ldcInsn = (LdcInsnNode) insn;
                    Object cst = ldcInsn.cst;
                    currentOffset += (cst instanceof Long || cst instanceof Double) ? 3 : 2;
                    break;
                case Opcodes.SIPUSH:
                case Constants1.LDC_W:
                case Constants1.LDC2_W:
                case Opcodes.GETSTATIC:
                case Opcodes.PUTSTATIC:
                case Opcodes.GETFIELD:
                case Opcodes.PUTFIELD:
                case Opcodes.INVOKEVIRTUAL:
                case Opcodes.INVOKESPECIAL:
                case Opcodes.INVOKESTATIC:
                case Opcodes.NEW:
                case Opcodes.ANEWARRAY:
                case Opcodes.CHECKCAST:
                case Opcodes.INSTANCEOF:
                case Opcodes.IINC:
                    currentOffset += 3;
                    break;
                case Opcodes.INVOKEINTERFACE:
                case Opcodes.INVOKEDYNAMIC:
                    currentOffset += 5;
                    break;
                case Opcodes.MULTIANEWARRAY:
                    currentOffset += 4;
                    break;
                case -1: //for labels
                    continue;
                default:
                    System.out.println("unhandled opcode: " + opcode + " for insn: " + insn);
                    throw new IllegalArgumentException();
            }
        }
    }

    public void printOffsetMap() {
        if (invokeReturnTypeBs.isEmpty() || instructionTypeArgTypeAs.isEmpty() || !DEBUG) {
            return;
        }
        System.out.println("for method " + name + " offsetMap:");
        for (Map.Entry<AbstractInsnNode, Integer> entry : offsetMap.entrySet()) {
            AbstractInsnNode insn = entry.getKey();
            Integer offset = entry.getValue();
            System.out.print(String.format("%3d : ", offset));
            InsnPrinter.printInsn(insn);
        }
    }

    public void printInvokeReturnTypeBs(){
        if (invokeReturnTypeBs.isEmpty() &&!DEBUG) {
            return;
        }
        System.out.println("for method " + name + " invokeReturnTypeBs:");
        for (Map.Entry<AbstractInsnNode, TypeHints.TypeB> entry : invokeReturnTypeBs.entrySet()) {
            AbstractInsnNode insn = entry.getKey();
            TypeHints.TypeB typeB = entry.getValue();
            System.out.println(insn + " -> " + typeB);
        }
    }

    public void printInstructionTypeArgTypeAs(){
        if (instructionTypeArgTypeAs.isEmpty() || !DEBUG) {
            return;
        }
        System.out.println("for method " + name + " instructionTypeArgTypeAs:");
        for (Map.Entry<AbstractInsnNode, List<TypeHints.TypeA>> entry : instructionTypeArgTypeAs.entrySet()) {
            AbstractInsnNode insn = entry.getKey();
            List<TypeHints.TypeA> typeAs = entry.getValue();
            System.out.println(insn + " -> " + typeAs);
        }
    }

    public boolean hasTypeHints(){
        return (!invokeReturnTypeBs.isEmpty() || !instructionTypeArgTypeAs.isEmpty());
    }

    public void setAttribtues(){
        if (!hasTypeHints()) return;
        genOffsetMap();
        if (DEBUG) printOffsetMap();
        List<TypeHints.TypeBHint> typeBHintList = new ArrayList<>();
        for (Map.Entry<AbstractInsnNode, TypeHints.TypeB> entry : invokeReturnTypeBs.entrySet()){
            AbstractInsnNode insn = entry.getKey();
            TypeHints.TypeB typeB = entry.getValue();
            if (typeB.isNoHint()) continue;
            Integer bcOffset = offsetMap.getOrDefault(insn, -1);
            if (bcOffset != -1){
                TypeHints.TypeBHint retTypeBHint = new TypeHints.TypeBHint(bcOffset, typeB);
                typeBHintList.add(retTypeBHint);
            } else {
                throw new IllegalArgumentException();
            }
        }
        if (typeBHintList.size() != 0){
            this.invokeReturnTypeAttribute = new InvokeReturnType(typeBHintList.size(), typeBHintList);
            if (DEBUG) System.out.println("invokeReturnTypeAttribute: " + invokeReturnTypeAttribute);
            visitAttribute(invokeReturnTypeAttribute);
        }
        List<TypeHints.TypeAHint> typeAHintList = new ArrayList<>();
        for (Map.Entry<AbstractInsnNode, List<TypeHints.TypeA>> entry : instructionTypeArgTypeAs.entrySet()){
            AbstractInsnNode insn = entry.getKey();
            List<TypeHints.TypeA> typeAlst = entry.getValue();
            Integer bcOffset = offsetMap.getOrDefault(insn, -1);
            if (bcOffset != -1){
                TypeHints.TypeAHint argsTypeAHint = new TypeHints.TypeAHint(bcOffset, typeAlst);
                typeAHintList.add(argsTypeAHint);
            } else {
                throw new IllegalArgumentException();
            }
        }
        if (typeAHintList.size() != 0){
            this.instructionTypeArgumentsAttribute = new InstructionTypeArguments(typeAHintList);
            if (DEBUG) System.out.println("instructionTypeArgumentsAttribute: " + instructionTypeArgumentsAttribute);
            visitAttribute(instructionTypeArgumentsAttribute);
        }

    }

    @Override
    protected LabelNode getLabelNode(Label label) {
        if (!(label.info instanceof LabelNode)) {
            label.info = new LabelNode1(label);
        }
        return (LabelNode) label.info;
    }

    public void visitTypeInsn(int opcode, String type, List<TypeHints.TypeA> instrTypeA){
        if (opcode == Opcodes.NEW && instrTypeA.size() > 0){
            if (DEBUG) System.out.println("visitTypeInsn NEW with instrTypeA = " + instrTypeA);
            AbstractInsnNode node = new TypeInsnNode(opcode, type);
            instructions.add(node);
            if (instrTypeA != null){
                instructionTypeArgTypeAs.put(node, instrTypeA);
            }
        } else {
            visitTypeInsn(opcode, type);
        }
    }
}
