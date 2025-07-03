package dotty.tools.backend.jvm;

import scala.tools.asm.tree.AbstractInsnNode;
import scala.tools.asm.util.Textifier;
import scala.tools.asm.MethodVisitor;
import scala.tools.asm.util.TraceMethodVisitor;

import java.io.PrintWriter;

public class InsnPrinter {
    public static void printInsn(AbstractInsnNode insn){
        Textifier textifier = new Textifier();
        MethodVisitor printer = new TraceMethodVisitor(textifier);
        insn.accept(printer);
        PrintWriter pw = new PrintWriter(System.out);
        textifier.print(pw);
        pw.flush();
    }
}