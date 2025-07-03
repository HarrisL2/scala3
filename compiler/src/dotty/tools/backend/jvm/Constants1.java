package dotty.tools.backend.jvm;

import scala.tools.asm.Opcodes;

final class Constants1 {
    static final int F_INSERT = 256;

  // The JVM opcode values which are not part of the ASM public API.
  // See https://docs.oracle.com/javase/specs/jvms/se9/html/jvms-6.html.

  static final int LDC_W = 19;
  static final int LDC2_W = 20;
  static final int ILOAD_0 = 26;
  static final int ILOAD_1 = 27;
  static final int ILOAD_2 = 28;
  static final int ILOAD_3 = 29;
  static final int LLOAD_0 = 30;
  static final int LLOAD_1 = 31;
  static final int LLOAD_2 = 32;
  static final int LLOAD_3 = 33;
  static final int FLOAD_0 = 34;
  static final int FLOAD_1 = 35;
  static final int FLOAD_2 = 36;
  static final int FLOAD_3 = 37;
  static final int DLOAD_0 = 38;
  static final int DLOAD_1 = 39;
  static final int DLOAD_2 = 40;
  static final int DLOAD_3 = 41;
  static final int ALOAD_0 = 42;
  static final int ALOAD_1 = 43;
  static final int ALOAD_2 = 44;
  static final int ALOAD_3 = 45;
  static final int ISTORE_0 = 59;
  static final int ISTORE_1 = 60;
  static final int ISTORE_2 = 61;
  static final int ISTORE_3 = 62;
  static final int LSTORE_0 = 63;
  static final int LSTORE_1 = 64;
  static final int LSTORE_2 = 65;
  static final int LSTORE_3 = 66;
  static final int FSTORE_0 = 67;
  static final int FSTORE_1 = 68;
  static final int FSTORE_2 = 69;
  static final int FSTORE_3 = 70;
  static final int DSTORE_0 = 71;
  static final int DSTORE_1 = 72;
  static final int DSTORE_2 = 73;
  static final int DSTORE_3 = 74;
  static final int ASTORE_0 = 75;
  static final int ASTORE_1 = 76;
  static final int ASTORE_2 = 77;
  static final int ASTORE_3 = 78;
  static final int WIDE = 196;
  static final int GOTO_W = 200;
  static final int JSR_W = 201;

  // Constants to convert between normal and wide jump instructions.

  // The delta between the GOTO_W and JSR_W opcodes and GOTO and JUMP.
  static final int WIDE_JUMP_OPCODE_DELTA = GOTO_W - Opcodes.GOTO;

  // Constants to convert JVM opcodes to the equivalent ASM specific opcodes, and vice versa.

  // The delta between the ASM_IFEQ, ..., ASM_IF_ACMPNE, ASM_GOTO and ASM_JSR opcodes
  // and IFEQ, ..., IF_ACMPNE, GOTO and JSR.
  static final int ASM_OPCODE_DELTA = 49;

  // The delta between the ASM_IFNULL and ASM_IFNONNULL opcodes and IFNULL and IFNONNULL.
  static final int ASM_IFNULL_OPCODE_DELTA = 20;

  // ASM specific opcodes, used for long forward jump instructions.

  static final int ASM_IFEQ = Opcodes.IFEQ + ASM_OPCODE_DELTA;
  static final int ASM_IFNE = Opcodes.IFNE + ASM_OPCODE_DELTA;
  static final int ASM_IFLT = Opcodes.IFLT + ASM_OPCODE_DELTA;
  static final int ASM_IFGE = Opcodes.IFGE + ASM_OPCODE_DELTA;
  static final int ASM_IFGT = Opcodes.IFGT + ASM_OPCODE_DELTA;
  static final int ASM_IFLE = Opcodes.IFLE + ASM_OPCODE_DELTA;
  static final int ASM_IF_ICMPEQ = Opcodes.IF_ICMPEQ + ASM_OPCODE_DELTA;
  static final int ASM_IF_ICMPNE = Opcodes.IF_ICMPNE + ASM_OPCODE_DELTA;
  static final int ASM_IF_ICMPLT = Opcodes.IF_ICMPLT + ASM_OPCODE_DELTA;
  static final int ASM_IF_ICMPGE = Opcodes.IF_ICMPGE + ASM_OPCODE_DELTA;
  static final int ASM_IF_ICMPGT = Opcodes.IF_ICMPGT + ASM_OPCODE_DELTA;
  static final int ASM_IF_ICMPLE = Opcodes.IF_ICMPLE + ASM_OPCODE_DELTA;
  static final int ASM_IF_ACMPEQ = Opcodes.IF_ACMPEQ + ASM_OPCODE_DELTA;
  static final int ASM_IF_ACMPNE = Opcodes.IF_ACMPNE + ASM_OPCODE_DELTA;
  static final int ASM_GOTO = Opcodes.GOTO + ASM_OPCODE_DELTA;
  static final int ASM_JSR = Opcodes.JSR + ASM_OPCODE_DELTA;
  static final int ASM_IFNULL = Opcodes.IFNULL + ASM_IFNULL_OPCODE_DELTA;
  static final int ASM_IFNONNULL = Opcodes.IFNONNULL + ASM_IFNULL_OPCODE_DELTA;
  static final int ASM_GOTO_W = 220;
}