/**
 *  Copyright (C) 2010 dennis zhuang (killme2008@gmail.com)
 *
 *  This library is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published
 *  by the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/
package com.googlecode.aviator.code.asm;

import static com.googlecode.aviator.asm.Opcodes.*;

import java.io.PrintWriter;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicLong;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.ClassExpression;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.asm.ClassVisitor;
import com.googlecode.aviator.asm.ClassWriter;
import com.googlecode.aviator.asm.Label;
import com.googlecode.aviator.asm.MethodVisitor;
import com.googlecode.aviator.asm.util.CheckClassAdapter;
import com.googlecode.aviator.asm.util.TraceClassVisitor;
import com.googlecode.aviator.code.CodeGenerator;
import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.lexer.token.NumberToken;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.lexer.token.Variable;
import com.googlecode.aviator.parser.AviatorClassLoader;


/**
 * Code generator using asm
 * 
 * @author dennis
 * 
 */
public class ASMCodeGenerator implements CodeGenerator {
    // Class Writer to generate class
    private final ClassWriter classWriter;
    // Trace visitor
    private ClassVisitor traceClassVisitor;
    // Check visitor
    private ClassVisitor checkClassAdapter;
    // Method visitor
    private MethodVisitor mv;
    // Class name
    private final String className;
    // Class loader to define generated class
    private final AviatorClassLoader classLoader;

    private static final AtomicLong CLASS_COUNTER = new AtomicLong();

    /**
     * Operands count to check stack frames
     */
    private int operandsCount = 0;

    private int maxStacks = 0;
    private int maxLocals = 1;


    private void setMaxStacks(int newMaxStacks) {
        if (newMaxStacks > this.maxStacks) {
            this.maxStacks = newMaxStacks;
        }
    }


    public ASMCodeGenerator(AviatorClassLoader classLoader, boolean trace) {
        this.classLoader = classLoader;
        // Generate inner class name
        className = "Script_" + System.currentTimeMillis() + "_" + CLASS_COUNTER.getAndIncrement();
        // Auto compute frames
        classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
        if (trace) {
            traceClassVisitor = new TraceClassVisitor(classWriter, new PrintWriter(System.out));
            checkClassAdapter = new CheckClassAdapter(traceClassVisitor);
        }
        else {
            checkClassAdapter = new CheckClassAdapter(classWriter);
        }
        makeConstructor();
        startVisitMethodCode();
    }


    private void startVisitMethodCode() {
        mv =
                checkClassAdapter.visitMethod(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, "run",
                    "(Ljava/util/Map;)Ljava/lang/Object;",
                    "(Ljava/util/Map<Ljava/lang/String;Ljava/lang/Object;>;)Ljava/lang/Object;", null);
        mv.visitCode();
    }


    private void endVisitCode() {

        if (this.operandsCount > 0) {
            loadEnv();
            mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/runtime/type/AviatorObject", "getValue",
                "(Ljava/util/Map;)Ljava/lang/Object;");
            mv.visitInsn(ARETURN);
            popOperand();
            popOperand();
        }
        else {
            mv.visitInsn(ACONST_NULL);
            mv.visitInsn(ARETURN);
            pushOperand(0);
            popOperand();
        }
        if (this.operandsCount > 0) {
            throw new CompileExpressionErrorException("operand stack is not empty,count=" + operandsCount);
        }
        mv.visitMaxs(maxStacks, maxLocals);
        mv.visitEnd();

        checkClassAdapter.visitEnd();
    }


    /**
     * Make a default constructor
     */
    private void makeConstructor() {
        checkClassAdapter.visit(AviatorEvaluator.BYTECODE_VER, ACC_PUBLIC + ACC_SUPER, className, null, "java/lang/Object",
            null);

        {
            mv = checkClassAdapter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
            mv.visitInsn(RETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }
    }


    /**
     * Make a label
     * 
     * @return
     */
    private Label makeLabel() {
        return new Label();
    }


    /*
     * (non-Javadoc)
     * 
     * @see
     * com.googlecode.aviator.code.CodeGenerator#onAdd(com.googlecode.aviator
     * .lexer.token.Token)
     */
    public void onAdd(Token<?> lookhead) {
        doArthOperation("add");
    }


    /**
     * Do arithmetic operation
     * 
     * @param methodName
     */
    private void doArthOperation(String methodName) {
        loadEnv();
        mv
            .visitMethodInsn(
                INVOKEVIRTUAL,
                "com/googlecode/aviator/runtime/type/AviatorObject",
                methodName,
                "(Lcom/googlecode/aviator/runtime/type/AviatorObject;Ljava/util/Map;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");
        popOperand();
        popOperand();
    }


    /**
     * Pop a operand from stack
     */
    private void popOperand() {
        this.operandsCount--;
    }


    /*
     * (non-Javadoc)
     * 
     * @see
     * com.googlecode.aviator.code.CodeGenerator#onSub(com.googlecode.aviator
     * .lexer.token.Token)
     */
    public void onSub(Token<?> lookhead) {
        doArthOperation("sub");
    }


    /*
     * (non-Javadoc)
     * 
     * @see
     * com.googlecode.aviator.code.CodeGenerator#onMult(com.googlecode.aviator
     * .lexer.token.Token)
     */
    public void onMult(Token<?> lookhead) {
        doArthOperation("mult");
    }


    /*
     * (non-Javadoc)
     * 
     * @see
     * com.googlecode.aviator.code.CodeGenerator#onDiv(com.googlecode.aviator
     * .lexer.token.Token)
     */
    public void onDiv(Token<?> lookhead) {
        doArthOperation("div");
    }


    /*
     * (non-Javadoc)
     * 
     * @see
     * com.googlecode.aviator.code.CodeGenerator#onMod(com.googlecode.aviator
     * .lexer.token.Token)
     */
    public void onMod(Token<?> lookhead) {
        doArthOperation("mod");
    }


    /**
     * Do logic operation "&&" left operand
     */
    public void onAndLeft(Token<?> lookhead) {
        loadEnv();
        mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/runtime/type/AviatorObject", "booleanValue",
            "(Ljava/util/Map;)Z");
        Label l0 = new Label();
        l0stack.push(l0);
        mv.visitJumpInsn(IFEQ, l0);

        popOperand(); // boolean object
        popOperand(); // environment

    }


    /**
     * Do logic operation "&&" right operand
     */
    public void onAndRight(Token<?> lookhead) {
        loadEnv();
        mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/runtime/type/AviatorObject", "booleanValue",
            "(Ljava/util/Map;)Z");
        mv.visitJumpInsn(IFEQ, l0stack.peek());
        // Result is true
        mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/runtime/type/AviatorBoolean", "TRUE",
            "Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
        Label l1 = new Label();
        mv.visitJumpInsn(GOTO, l1);
        mv.visitLabel(l0stack.pop());
        // Result is false
        mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/runtime/type/AviatorBoolean", "FALSE",
            "Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
        mv.visitLabel(l1);

        popOperand(); // boolean object
        popOperand(); // environment
        pushOperand(0);
    }

    /**
     * Label stack for ternary operator
     */
    private final Stack<Label> l0stack = new Stack<Label>();
    private final Stack<Label> l1stack = new Stack<Label>();


    public void onTernaryBoolean(Token<?> lookhead) {
        loadEnv();
        mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/runtime/type/AviatorObject", "booleanValue",
            "(Ljava/util/Map;)Z");
        Label l0 = new Label();
        Label l1 = new Label();
        l0stack.push(l0);
        l1stack.push(l1);
        mv.visitJumpInsn(IFEQ, l0);
        popOperand();
        popOperand();
        pushOperand(1); // add two booleans

        popOperand(); // pop the last result
    }


    public void onTernaryLeft(Token<?> lookhead) {
        mv.visitJumpInsn(GOTO, l1stack.peek());
        mv.visitLabel(l0stack.pop());
        popOperand(); // pop one boolean
    }


    public void onTernaryRight(Token<?> lookhead) {
        mv.visitLabel(l1stack.pop());
        popOperand(); // pop one boolean
    }


    /**
     * Do logic operation "||" right operand
     */
    public void onJoinRight(Token<?> lookhead) {
        loadEnv();
        mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/runtime/type/AviatorObject", "booleanValue",
            "(Ljava/util/Map;)Z");
        Label l1 = new Label();
        mv.visitJumpInsn(IFNE, l0stack.peek());
        // Result is False
        mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/runtime/type/AviatorBoolean", "FALSE",
            "Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
        mv.visitJumpInsn(GOTO, l1);
        mv.visitLabel(l0stack.pop());
        // Result is True
        mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/runtime/type/AviatorBoolean", "TRUE",
            "Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
        mv.visitLabel(l1);
        popOperand();
        popOperand();
        pushOperand(0);

    }


    /**
     * Do logic operation "||" left operand
     */
    public void onJoinLeft(Token<?> lookhead) {
        loadEnv();
        mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/runtime/type/AviatorObject", "booleanValue",
            "(Ljava/util/Map;)Z");
        Label l0 = new Label();
        l0stack.push(l0);
        mv.visitJumpInsn(IFNE, l0);

        popOperand();
        popOperand();

    }


    public void onEq(Token<?> lookhead) {
        doCompareAndJump(IFNE);
    }


    public void onMatch(Token<?> lookhead) {
        this.mv.visitInsn(SWAP);
        loadEnv();
        mv
            .visitMethodInsn(
                INVOKEVIRTUAL,
                "com/googlecode/aviator/runtime/type/AviatorObject",
                "match",
                "(Lcom/googlecode/aviator/runtime/type/AviatorObject;Ljava/util/Map;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");

        popOperand();
        popOperand();
        popOperand();
        pushOperand(0);
    }


    public void onNeq(Token<?> lookhead) {
        doCompareAndJump(IFEQ);
    }


    private void doCompareAndJump(int ints) {
        loadEnv();
        mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/runtime/type/AviatorObject", "compare",
            "(Lcom/googlecode/aviator/runtime/type/AviatorObject;Ljava/util/Map;)I");
        Label l0 = makeLabel();
        Label l1 = makeLabel();
        mv.visitJumpInsn(ints, l0);
        mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/runtime/type/AviatorBoolean", "TRUE",
            "Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
        mv.visitJumpInsn(GOTO, l1);
        mv.visitLabel(l0);
        mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/runtime/type/AviatorBoolean", "FALSE",
            "Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
        mv.visitLabel(l1);
        popOperand();
        popOperand();
        popOperand();
        pushOperand(0);
    }


    public void onGe(Token<?> lookhead) {
        doCompareAndJump(IFLT);
    }


    public void onGt(Token<?> lookhead) {
        doCompareAndJump(IFLE);
    }


    public void onLe(Token<?> lookhead) {
        doCompareAndJump(IFGT);

    }


    public void onLt(Token<?> lookhead) {
        doCompareAndJump(IFGE);
    }


    /**
     * 
     * @param extras
     *            额外的栈空间大小
     */
    public void pushOperand(int extras) {
        this.operandsCount++;
        this.operandsCount += extras;
        setMaxStacks(this.operandsCount);
    }


    /**
     * Logic operation '!'
     */
    public void onNot(Token<?> lookhead) {
        pushOperand(0);

        mv.visitTypeInsn(CHECKCAST, "com/googlecode/aviator/runtime/type/AviatorObject");
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/runtime/type/AviatorObject", "not",
            "(Ljava/util/Map;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");

        popOperand();
        popOperand();
        pushOperand(0);
    }


    /*
     * (non-Javadoc)
     * 
     * @see
     * com.googlecode.aviator.code.CodeGenerator#onNeg(com.googlecode.aviator
     * .lexer.token.Token, int)
     */
    public void onNeg(Token<?> lookhead) {
        pushOperand(0);

        mv.visitTypeInsn(CHECKCAST, "com/googlecode/aviator/runtime/type/AviatorObject");
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/runtime/type/AviatorObject", "neg",
            "(Ljava/util/Map;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");
        popOperand();
        popOperand();
        pushOperand(0);
    }


    /*
     * (non-Javadoc)
     * 
     * @see com.googlecode.aviator.code.CodeGenerator#getResult()
     */
    public Expression getResult() {
        endVisitCode();
        byte[] bytes = this.classWriter.toByteArray();
        try {
            return new ClassExpression(classLoader.defineClass(className, bytes));
        }
        catch (Exception e) {
            throw new CompileExpressionErrorException("define class error", e);
        }
    }


    /*
     * (non-Javadoc)
     * 
     * @see
     * com.googlecode.aviator.code.CodeGenerator#onConstant(com.googlecode.aviator
     * .lexer.token.Token)
     */
    public void onConstant(Token<?> lookhead) {
        if (lookhead == null) {
            return;
        }
        // load token to stack
        switch (lookhead.getType()) {
        case Number:
            // load numbers
            NumberToken numberToken = (NumberToken) lookhead;
            if (numberToken.getNumber() instanceof Double) {
                mv.visitTypeInsn(NEW, "com/googlecode/aviator/runtime/type/AviatorDouble");
                mv.visitInsn(DUP);
                mv.visitLdcInsn(numberToken.getNumber());
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/Double", "valueOf", "(D)Ljava/lang/Double;");
                mv.visitMethodInsn(INVOKESPECIAL, "com/googlecode/aviator/runtime/type/AviatorDouble", "<init>",
                    "(Ljava/lang/Number;)V");
            }
            else {
                mv.visitTypeInsn(NEW, "com/googlecode/aviator/runtime/type/AviatorLong");
                mv.visitInsn(DUP);
                mv.visitLdcInsn(numberToken.getNumber());
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/Long", "valueOf", "(J)Ljava/lang/Long;");
                mv.visitMethodInsn(INVOKESPECIAL, "com/googlecode/aviator/runtime/type/AviatorLong", "<init>",
                    "(Ljava/lang/Number;)V");
            }
            pushOperand(2);
            popOperand();
            popOperand();
            break;
        case String:
            // load string
            mv.visitTypeInsn(NEW, "com/googlecode/aviator/runtime/type/AviatorString");
            mv.visitInsn(DUP);
            mv.visitLdcInsn(lookhead.getValue(null));
            mv.visitMethodInsn(INVOKESPECIAL, "com/googlecode/aviator/runtime/type/AviatorString", "<init>",
                "(Ljava/lang/String;)V");
            pushOperand(2);
            popOperand();
            popOperand();
            break;
        case Pattern:
            // load pattern
            mv.visitTypeInsn(NEW, "com/googlecode/aviator/runtime/type/AviatorPattern");
            mv.visitInsn(DUP);
            mv.visitLdcInsn(lookhead.getValue(null));
            mv.visitMethodInsn(INVOKESPECIAL, "com/googlecode/aviator/runtime/type/AviatorPattern", "<init>",
                "(Ljava/lang/String;)V");
            pushOperand(2);
            popOperand();
            popOperand();
            break;
        case Variable:
            // load variable
            Variable variable = (Variable) lookhead;
            if (variable.equals(Variable.TRUE)) {
                mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/runtime/type/AviatorBoolean", "TRUE",
                    "Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
                pushOperand(0);
            }
            else if (variable.equals(Variable.FALSE)) {
                mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/runtime/type/AviatorBoolean", "FALSE",
                    "Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
                pushOperand(0);
            }
            else if (variable.equals(Variable.NIL)) {
                mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/runtime/type/AviatorNil", "NIL",
                    "Lcom/googlecode/aviator/runtime/type/AviatorNil;");
                pushOperand(0);
            }
            else {
                // check if it is a function name
                // if
                // (AviatorEvaluator.FUNC_MAP.keySet().contains(variable.getLexeme()))
                // {
                // throw new CompileExpressionErrorException("index=" +
                // variable.getStartIndex() + ","
                // + variable.getLexeme() +
                // " is a function name,please don't use it as variable");
                // }

                mv.visitTypeInsn(NEW, "com/googlecode/aviator/runtime/type/AviatorJavaType");
                mv.visitInsn(DUP);
                mv.visitLdcInsn(variable.getLexeme());
                mv.visitMethodInsn(INVOKESPECIAL, "com/googlecode/aviator/runtime/type/AviatorJavaType", "<init>",
                    "(Ljava/lang/String;)V");
                pushOperand(2);
                popOperand();
                popOperand();

            }
            break;
        }

    }


    public void onMethodInvoke(Token<?> lookhead) {
        final MethodMetaData methodMetaData = this.methodMetaDataStack.pop();
        mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/runtime/method/AviatorMethod", "invoke",
            "(Ljava/util/Map;Ljava/util/List;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");
        popOperand(); // method object
        popOperand(); // env map
        popOperand(); // argument list
        // pop operands
        for (int i = 0; i < methodMetaData.parameterCount; i++) {
            popOperand();
        }
        pushOperand(0);
    }


    public void onMethodParameter(Token<?> lookhead) {
        this.methodMetaDataStack.peek().parameterCount++;
        // add parameter to list
        mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z");
        // pop boolean
        mv.visitInsn(POP);
        mv.visitVarInsn(ALOAD, this.methodMetaDataStack.peek().parameterListIndex);
    }

    private static class MethodMetaData {
        String methodName;

        int parameterCount;
        int parameterListIndex;


        public MethodMetaData(String methodName, int parameterListIndex) {
            super();
            this.methodName = methodName;
            this.parameterCount = 0;
            this.parameterListIndex = parameterListIndex;
        }

    }

    private final Stack<MethodMetaData> methodMetaDataStack = new Stack<MethodMetaData>();


    public void onElementStart(Token<?> lookhead) {
        onConstant(lookhead);
        loadEnv();
    }


    public void onElementEnd(Token<?> lookhead) {
        mv
            .visitMethodInsn(
                INVOKEVIRTUAL,
                "com/googlecode/aviator/runtime/type/AviatorJavaType",
                "getElement",
                "(Ljava/util/Map;Lcom/googlecode/aviator/runtime/type/AviatorObject;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");

        popOperand();
        popOperand();
        popOperand();
        pushOperand(0);
    }


    public int getLocalIndex() {
        return maxLocals++;
    }


    public void onMethodName(Token<?> lookhead) {
        String methodName = lookhead.getLexeme();
        createAviatorMethodObject(methodName);
        loadEnv();
        final int parameterLocalIndex = createArugmentList();
        methodMetaDataStack.push(new MethodMetaData(methodName, parameterLocalIndex));

        // pushOperand(0);

    }


    private int createArugmentList() {
        // create argument list
        pushOperand(0);
        pushOperand(0);
        mv.visitTypeInsn(NEW, "java/util/ArrayList");
        mv.visitInsn(DUP);
        popOperand();
        mv.visitMethodInsn(INVOKESPECIAL, "java/util/ArrayList", "<init>", "()V");
        // store to local variable
        final int parameterLocalIndex = getLocalIndex();
        mv.visitVarInsn(ASTORE, parameterLocalIndex);
        mv.visitVarInsn(ALOAD, parameterLocalIndex);
        return parameterLocalIndex;
    }


    private void loadEnv() {
        // load env
        pushOperand(0);
        mv.visitVarInsn(ALOAD, 0);
    }


    private void createAviatorMethodObject(String methodName) {
        pushOperand(0);
        pushOperand(0);
        pushOperand(0);
        mv.visitTypeInsn(NEW, "com/googlecode/aviator/runtime/method/AviatorMethod");
        mv.visitInsn(DUP);
        mv.visitLdcInsn(methodName);
        popOperand();
        popOperand();
        mv.visitMethodInsn(INVOKESPECIAL, "com/googlecode/aviator/runtime/method/AviatorMethod", "<init>",
            "(Ljava/lang/String;)V");
    }
}
