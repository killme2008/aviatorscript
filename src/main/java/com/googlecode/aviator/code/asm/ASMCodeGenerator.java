/**
 * Copyright (C) 2010 dennis zhuang (killme2008@gmail.com)
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this program;
 * if not, write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/
package com.googlecode.aviator.code.asm;

import static com.googlecode.aviator.asm.Opcodes.ACC_FINAL;
import static com.googlecode.aviator.asm.Opcodes.ACC_PRIVATE;
import static com.googlecode.aviator.asm.Opcodes.ACC_PUBLIC;
import static com.googlecode.aviator.asm.Opcodes.ACC_SUPER;
import static com.googlecode.aviator.asm.Opcodes.ACONST_NULL;
import static com.googlecode.aviator.asm.Opcodes.ALOAD;
import static com.googlecode.aviator.asm.Opcodes.ARETURN;
import static com.googlecode.aviator.asm.Opcodes.ASTORE;
import static com.googlecode.aviator.asm.Opcodes.CHECKCAST;
import static com.googlecode.aviator.asm.Opcodes.DUP;
import static com.googlecode.aviator.asm.Opcodes.GETFIELD;
import static com.googlecode.aviator.asm.Opcodes.GETSTATIC;
import static com.googlecode.aviator.asm.Opcodes.GOTO;
import static com.googlecode.aviator.asm.Opcodes.IFEQ;
import static com.googlecode.aviator.asm.Opcodes.IFGE;
import static com.googlecode.aviator.asm.Opcodes.IFGT;
import static com.googlecode.aviator.asm.Opcodes.IFLE;
import static com.googlecode.aviator.asm.Opcodes.IFLT;
import static com.googlecode.aviator.asm.Opcodes.IFNE;
import static com.googlecode.aviator.asm.Opcodes.INVOKEINTERFACE;
import static com.googlecode.aviator.asm.Opcodes.INVOKESPECIAL;
import static com.googlecode.aviator.asm.Opcodes.INVOKESTATIC;
import static com.googlecode.aviator.asm.Opcodes.INVOKEVIRTUAL;
import static com.googlecode.aviator.asm.Opcodes.NEW;
import static com.googlecode.aviator.asm.Opcodes.PUTFIELD;
import static com.googlecode.aviator.asm.Opcodes.RETURN;
import static com.googlecode.aviator.asm.Opcodes.SWAP;
import java.io.OutputStream;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicLong;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.asm.ClassWriter;
import com.googlecode.aviator.asm.Label;
import com.googlecode.aviator.asm.MethodVisitor;
import com.googlecode.aviator.asm.Opcodes;
import com.googlecode.aviator.code.CodeGenerator;
import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.lexer.token.NumberToken;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.lexer.token.Variable;
import com.googlecode.aviator.parser.AviatorClassLoader;
import com.googlecode.aviator.runtime.op.OperationRuntime;
import com.googlecode.aviator.utils.TypeUtils;


/**
 * Code generator using asm
 *
 * @author dennis
 *
 */
public class ASMCodeGenerator implements CodeGenerator {
  private static final String FIELD_PREFIX = "var_";
  // Class Writer to generate class
  // private final ClassWriter clazzWriter;
  // Trace visitor
  // private ClassVisitor traceClassVisitor;
  // Check visitor
  private ClassWriter classWriter;
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
  private int maxLocals = 2;

  private int fieldCounter = 0;

  private final Map<String/* variable name */, String/* inner var name */> innerVarMap =
      new HashMap<String, String>();

  private final Map<String/* method name */, String/* inner method name */> innerMethodMap =
      new HashMap<String, String>();

  private Map<String, Integer/* counter */> varTokens = new LinkedHashMap<String, Integer>();
  private Map<String, Integer/* counter */> methodTokens = new HashMap<String, Integer>();

  private final Map<Label, Map<String/* inner name */, Integer/* local index */>> labelNameIndexMap =
      new HashMap<Label, Map<String, Integer>>();

  private static final Label START_LABEL = new Label();

  private Label currentLabel = START_LABEL;


  private void setMaxStacks(int newMaxStacks) {
    if (newMaxStacks > this.maxStacks) {
      this.maxStacks = newMaxStacks;
    }
  }


  public ASMCodeGenerator(AviatorClassLoader classLoader, OutputStream traceOut, boolean trace) {
    this.classLoader = classLoader;
    // Generate inner class name
    this.className = "Script_" + System.currentTimeMillis() + "_" + CLASS_COUNTER.getAndIncrement();
    // Auto compute frames
    this.classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
    // if (trace) {
    // this.traceClassVisitor = new TraceClassVisitor(this.clazzWriter, new PrintWriter(traceOut));
    // this.classWriter = new CheckClassAdapter(this.traceClassVisitor);
    // } else {
    // this.classWriter = new CheckClassAdapter(this.clazzWriter);
    // }
    this.visitClass();
  }


  public void start() {
    this.makeConstructor();
    this.startVisitMethodCode();
  }


  private void startVisitMethodCode() {
    this.mv = this.classWriter.visitMethod(ACC_PUBLIC + +ACC_FINAL, "execute0",
        "(Ljava/util/Map;)Ljava/lang/Object;",
        "(Ljava/util/Map<Ljava/lang/String;Ljava/lang/Object;>;)Ljava/lang/Object;", null);
    this.mv.visitCode();
  }


  private void endVisitMethodCode() {
    if (this.operandsCount > 0) {
      this.loadEnv();
      this.mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/runtime/type/AviatorObject",
          "getValue", "(Ljava/util/Map;)Ljava/lang/Object;");
      this.mv.visitInsn(ARETURN);
      this.popOperand();
      this.popOperand();
    } else {
      this.mv.visitInsn(ACONST_NULL);
      this.mv.visitInsn(ARETURN);
      this.pushOperand();
      this.popOperand();
    }
    if (this.operandsCount > 0) {
      throw new CompileExpressionErrorException(
          "operand stack is not empty,count=" + this.operandsCount);
    }
    this.mv.visitMaxs(this.maxStacks, this.maxLocals);
    this.mv.visitEnd();

  }


  private void endVisitClass() {
    this.classWriter.visitEnd();
  }


  /**
   * Make a default constructor
   */
  private void makeConstructor() {
    {
      this.mv =
          this.classWriter.visitMethod(ACC_PUBLIC, "<init>", "(Ljava/util/List;)V", null, null);
      this.mv.visitCode();
      this.mv.visitVarInsn(ALOAD, 0);
      this.mv.visitVarInsn(ALOAD, 1);
      this.mv.visitMethodInsn(INVOKESPECIAL, "com/googlecode/aviator/ClassExpression", "<init>",
          "(Ljava/util/List;)V");
      if (!this.innerVarMap.isEmpty()) {
        for (Map.Entry<String, String> entry : this.innerVarMap.entrySet()) {
          String outterName = entry.getKey();
          String innerName = entry.getValue();
          this.mv.visitVarInsn(ALOAD, 0);
          this.mv.visitTypeInsn(NEW, "com/googlecode/aviator/runtime/type/AviatorJavaType");
          this.mv.visitInsn(DUP);
          this.mv.visitLdcInsn(outterName);
          this.mv.visitMethodInsn(INVOKESPECIAL,
              "com/googlecode/aviator/runtime/type/AviatorJavaType", "<init>",
              "(Ljava/lang/String;)V");
          this.mv.visitFieldInsn(PUTFIELD, this.className, innerName,
              "Lcom/googlecode/aviator/runtime/type/AviatorJavaType;");
        }
      }
      if (!this.innerMethodMap.isEmpty()) {
        for (Map.Entry<String, String> entry : this.innerMethodMap.entrySet()) {
          String outterName = entry.getKey();
          String innerName = entry.getValue();
          this.mv.visitVarInsn(ALOAD, 0);
          this.mv.visitLdcInsn(outterName);
          this.mv.visitMethodInsn(INVOKESTATIC, "com/googlecode/aviator/AviatorEvaluator",
              "getFunction",
              "(Ljava/lang/String;)Lcom/googlecode/aviator/runtime/type/AviatorFunction;");
          this.mv.visitFieldInsn(PUTFIELD, this.className, innerName,
              "Lcom/googlecode/aviator/runtime/type/AviatorFunction;");
        }
      }

      this.mv.visitInsn(RETURN);
      this.mv.visitMaxs(4, 1);
      this.mv.visitEnd();
    }
  }


  private void visitClass() {
    this.classWriter.visit(AviatorEvaluator.BYTECODE_VER, ACC_PUBLIC + ACC_SUPER, this.className,
        null, "com/googlecode/aviator/ClassExpression", null);
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
   * @see com.googlecode.aviator.code.CodeGenerator#onAdd(com.googlecode.aviator .lexer.token.Token)
   */
  @Override
  public void onAdd(Token<?> lookhead) {
    this.visitBinOperator(OperatorType.ADD, "add");
  }

  private void loadOpType(OperatorType opType) {
    this.pushOperand();
    this.mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/lexer/token/OperatorType",
        opType.name(), "Lcom/googlecode/aviator/lexer/token/OperatorType;");
  }


  /**
   * Pop a operand from stack
   */
  private void popOperand() {
    this.operandsCount--;
  }


  /**
   * Pop a operand from stack
   */
  private void popOperand(int n) {
    this.operandsCount -= n;
  }


  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.code.CodeGenerator#onSub(com.googlecode.aviator .lexer.token.Token)
   */
  @Override
  public void onSub(Token<?> lookhead) {
    this.visitBinOperator(OperatorType.SUB, "sub");
  }


  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.code.CodeGenerator#onMult(com.googlecode.aviator
   * .lexer.token.Token)
   */
  @Override
  public void onMult(Token<?> lookhead) {
    this.visitBinOperator(OperatorType.MULT, "mult");
  }


  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.code.CodeGenerator#onDiv(com.googlecode.aviator .lexer.token.Token)
   */
  @Override
  public void onDiv(Token<?> lookhead) {
    this.visitBinOperator(OperatorType.DIV, "div");
  }


  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.code.CodeGenerator#onMod(com.googlecode.aviator .lexer.token.Token)
   */
  @Override
  public void onMod(Token<?> lookhead) {
    this.visitBinOperator(OperatorType.MOD, "mod");
  }


  /**
   * Do logic operation "&&" left operand
   */
  @Override
  public void onAndLeft(Token<?> lookhead) {
    this.loadEnv();
    this.visitLeftBranch(IFEQ, OperatorType.AND);
  }


  private void visitBoolean() {
    this.mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/runtime/type/AviatorObject",
        "booleanValue", "(Ljava/util/Map;)Z");
  }


  private void pushLabel0(Label l0) {
    this.l0stack.push(l0);
  }


  /**
   * Do logic operation "&&" right operand
   */
  @Override
  public void onAndRight(Token<?> lookhead) {
    this.visitRightBranch(IFEQ, OperatorType.AND);
    this.popOperand(); // boolean object
    this.popOperand(); // environment
    this.pushOperand();
  }


  private void visitRightBranch(int ints, OperatorType opType) {
    if (!OperationRuntime.hasRuntimeContext(opType)) {
      this.loadEnv();
      String first = "TRUE";
      String second = "FALSE";
      if (opType == OperatorType.OR) {
        first = "FALSE";
        second = "TRUE";
      }

      this.visitBoolean();
      this.mv.visitJumpInsn(ints, this.peekLabel0());
      // Result is true
      this.mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/runtime/type/AviatorBoolean", first,
          "Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
      Label l1 = this.makeLabel();
      this.mv.visitJumpInsn(GOTO, l1);
      this.visitLabel(this.popLabel0());
      // Result is false
      this.mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/runtime/type/AviatorBoolean",
          second, "Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
      this.visitLabel(l1);
    } else {
      this.loadOpType(opType);
      this.mv.visitMethodInsn(INVOKESTATIC, "com/googlecode/aviator/runtime/op/OperationRuntime",
          "eval",
          "(Lcom/googlecode/aviator/runtime/type/AviatorObject;Ljava/util/Map;Lcom/googlecode/aviator/runtime/type/AviatorObject;Lcom/googlecode/aviator/lexer/token/OperatorType;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");
      this.popOperand();
    }
  }

  /**
   * Label stack for ternary operator
   */
  private final Stack<Label> l0stack = new Stack<Label>();
  private final Stack<Label> l1stack = new Stack<Label>();


  @Override
  public void onTernaryBoolean(Token<?> lookhead) {
    this.loadEnv();
    this.visitBoolean();
    Label l0 = this.makeLabel();
    Label l1 = this.makeLabel();
    this.pushLabel0(l0);
    this.pushLabel1(l1);
    this.mv.visitJumpInsn(IFEQ, l0);
    this.popOperand();
    this.popOperand();
    this.pushOperand(1); // add two booleans

    this.popOperand(); // pop the last result
  }


  private void pushLabel1(Label l1) {
    this.l1stack.push(l1);
  }


  @Override
  public void onTernaryLeft(Token<?> lookhead) {
    this.mv.visitJumpInsn(GOTO, this.peekLabel1());
    this.visitLabel(this.popLabel0());
    this.popOperand(); // pop one boolean
  }


  private Label peekLabel1() {
    return this.l1stack.peek();
  }


  @Override
  public void onTernaryRight(Token<?> lookhead) {
    this.visitLabel(this.popLabel1());
    this.popOperand(); // pop one boolean
  }


  private Label popLabel1() {
    return this.l1stack.pop();
  }


  /**
   * Do logic operation "||" right operand
   */
  @Override
  public void onJoinRight(Token<?> lookhead) {
    this.visitRightBranch(IFNE, OperatorType.OR);
    this.popOperand();
    this.popOperand();
    this.pushOperand();

  }


  private void visitLabel(Label label) {
    this.mv.visitLabel(label);
    this.currentLabel = label;
  }


  private Label peekLabel0() {
    return this.l0stack.peek();
  }


  private Label popLabel0() {
    return this.l0stack.pop();
  }


  /**
   * Do logic operation "||" left operand
   */
  @Override
  public void onJoinLeft(Token<?> lookhead) {
    this.loadEnv();
    this.visitLeftBranch(IFNE, OperatorType.OR);
  }


  private void visitLeftBranch(int ints, OperatorType opType) {
    if (!OperationRuntime.hasRuntimeContext(opType)) {
      this.visitBoolean();
      Label l0 = this.makeLabel();
      this.pushLabel0(l0);
      this.mv.visitJumpInsn(ints, l0);
      this.popOperand();
    }
    this.popOperand();
  }


  @Override
  public void onEq(Token<?> lookhead) {
    this.doCompareAndJump(IFNE, OperatorType.EQ);
  }


  @Override
  public void onMatch(Token<?> lookhead) {
    this.visitBinOperator(OperatorType.MATCH, "match");
    this.popOperand();
    this.pushOperand();
  }


  @Override
  public void onNeq(Token<?> lookhead) {
    this.doCompareAndJump(IFEQ, OperatorType.NEQ);
  }


  private void doCompareAndJump(int ints, OperatorType opType) {
    this.loadEnv();
    this.visitCompare(ints, opType);
    this.popOperand();
    this.popOperand();
  }


  private void visitCompare(int ints, OperatorType opType) {
    if (!OperationRuntime.hasRuntimeContext(opType)) {
      this.mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/runtime/type/AviatorObject",
          "compare", "(Lcom/googlecode/aviator/runtime/type/AviatorObject;Ljava/util/Map;)I");
      Label l0 = this.makeLabel();
      Label l1 = this.makeLabel();
      this.mv.visitJumpInsn(ints, l0);
      this.mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/runtime/type/AviatorBoolean",
          "TRUE", "Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
      this.mv.visitJumpInsn(GOTO, l1);
      this.visitLabel(l0);
      this.mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/runtime/type/AviatorBoolean",
          "FALSE", "Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
      this.visitLabel(l1);
    } else {
      this.loadOpType(opType);
      this.mv.visitMethodInsn(INVOKESTATIC, "com/googlecode/aviator/runtime/op/OperationRuntime",
          "eval",
          "(Lcom/googlecode/aviator/runtime/type/AviatorObject;Lcom/googlecode/aviator/runtime/type/AviatorObject;Ljava/util/Map;Lcom/googlecode/aviator/lexer/token/OperatorType;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");
      this.popOperand();
    }

  }


  @Override
  public void onGe(Token<?> lookhead) {
    this.doCompareAndJump(IFLT, OperatorType.GE);
  }


  @Override
  public void onGt(Token<?> lookhead) {
    this.doCompareAndJump(IFLE, OperatorType.GT);
  }


  @Override
  public void onLe(Token<?> lookhead) {
    this.doCompareAndJump(IFGT, OperatorType.LE);

  }


  @Override
  public void onLt(Token<?> lookhead) {
    this.doCompareAndJump(IFGE, OperatorType.LT);
  }


  /**
   *
   * @param extras 额外的栈空间大小
   */
  public void pushOperand(int extras) {
    this.operandsCount++;
    this.operandsCount += extras;
    this.setMaxStacks(this.operandsCount);
  }


  /**
   * Logic operation '!'
   */
  @Override
  public void onNot(Token<?> lookhead) {
    this.visitUnaryOperator(OperatorType.NOT, "not");
  }

  private void visitBinOperator(OperatorType opType, String methodName) {
    if (!OperationRuntime.hasRuntimeContext(opType)) {
      // swap arguments for regular-expression match operator.
      if (opType == OperatorType.MATCH) {
        this.mv.visitInsn(SWAP);
      }
      this.loadEnv();
      this.mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/runtime/type/AviatorObject",
          methodName,
          "(Lcom/googlecode/aviator/runtime/type/AviatorObject;Ljava/util/Map;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");
    } else {
      this.loadEnv();
      this.loadOpType(opType);
      this.mv.visitMethodInsn(INVOKESTATIC, "com/googlecode/aviator/runtime/op/OperationRuntime",
          "eval",
          "(Lcom/googlecode/aviator/runtime/type/AviatorObject;Lcom/googlecode/aviator/runtime/type/AviatorObject;Ljava/util/Map;Lcom/googlecode/aviator/lexer/token/OperatorType;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");
      this.popOperand();
    }
    this.popOperand();
    this.popOperand();
  }

  private void visitUnaryOperator(OperatorType opType, String methodName) {
    this.mv.visitTypeInsn(CHECKCAST, "com/googlecode/aviator/runtime/type/AviatorObject");
    this.loadEnv();

    if (!OperationRuntime.hasRuntimeContext(opType)) {
      this.mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/runtime/type/AviatorObject",
          methodName, "(Ljava/util/Map;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");
    } else {
      this.loadOpType(opType);
      this.mv.visitMethodInsn(INVOKESTATIC, "com/googlecode/aviator/runtime/op/OperationRuntime",
          "eval",
          "(Lcom/googlecode/aviator/runtime/type/AviatorObject;Ljava/util/Map;Lcom/googlecode/aviator/lexer/token/OperatorType;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");
      this.popOperand();
    }


    this.popOperand();
  }


  /**
   * Bit operation '~'
   */
  @Override
  public void onBitNot(Token<?> lookhead) {
    this.visitUnaryOperator(OperatorType.BIT_NOT, "bitNot");
  }


  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.code.CodeGenerator#onNeg(com.googlecode.aviator .lexer.token.Token,
   * int)
   */
  @Override
  public void onNeg(Token<?> lookhead) {
    this.visitUnaryOperator(OperatorType.NEG, "neg");
  }


  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.code.CodeGenerator#getResult()
   */
  @Override
  public Expression getResult() {
    this.end();

    byte[] bytes = this.classWriter.toByteArray();
    try {
      Class<?> defineClass = ClassDefiner.defineClass(this.className, bytes, this.classLoader);
      Constructor<?> constructor = defineClass.getConstructor(List.class);
      return (Expression) constructor.newInstance(new ArrayList<String>(this.varTokens.keySet()));
    } catch (Exception e) {
      throw new CompileExpressionErrorException("define class error", e);
    }
  }

  private void end() {
    this.endVisitMethodCode();
    this.endVisitClass();
  }


  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.code.CodeGenerator#onConstant(com.googlecode.aviator
   * .lexer.token.Token)
   */
  @Override
  public void onConstant(Token<?> lookhead) {
    if (lookhead == null) {
      return;
    }
    // load token to stack
    switch (lookhead.getType()) {
      case Number:
        // load numbers
        NumberToken numberToken = (NumberToken) lookhead;
        Number number = numberToken.getNumber();

        if (TypeUtils.isBigInt(number)) {
          this.mv.visitLdcInsn(numberToken.getLexeme());
          this.mv.visitMethodInsn(INVOKESTATIC, "com/googlecode/aviator/runtime/type/AviatorBigInt",
              "valueOf", "(Ljava/lang/String;)Lcom/googlecode/aviator/runtime/type/AviatorBigInt;");
        } else if (TypeUtils.isDecimal(number)) {
          this.mv.visitLdcInsn(numberToken.getLexeme());
          this.mv.visitMethodInsn(INVOKESTATIC,
              "com/googlecode/aviator/runtime/type/AviatorDecimal", "valueOf",
              "(Ljava/lang/String;)Lcom/googlecode/aviator/runtime/type/AviatorDecimal;");
        } else if (TypeUtils.isDouble(number)) {
          this.mv.visitLdcInsn(number);
          this.mv.visitMethodInsn(INVOKESTATIC, "com/googlecode/aviator/runtime/type/AviatorDouble",
              "valueOf", "(D)Lcom/googlecode/aviator/runtime/type/AviatorDouble;");
        } else {
          this.mv.visitLdcInsn(number);
          this.mv.visitMethodInsn(INVOKESTATIC, "com/googlecode/aviator/runtime/type/AviatorLong",
              "valueOf", "(J)Lcom/googlecode/aviator/runtime/type/AviatorLong;");
        }
        this.pushOperand();
        // this.popOperand();
        // this.popOperand();
        break;
      case String:
        // load string
        this.mv.visitTypeInsn(NEW, "com/googlecode/aviator/runtime/type/AviatorString");
        this.mv.visitInsn(DUP);
        this.mv.visitLdcInsn(lookhead.getValue(null));
        this.mv.visitMethodInsn(INVOKESPECIAL, "com/googlecode/aviator/runtime/type/AviatorString",
            "<init>", "(Ljava/lang/String;)V");
        this.pushOperand(2);
        this.popOperand();
        this.popOperand();
        break;
      case Pattern:
        // load pattern
        this.mv.visitTypeInsn(NEW, "com/googlecode/aviator/runtime/type/AviatorPattern");
        this.mv.visitInsn(DUP);
        this.mv.visitLdcInsn(lookhead.getValue(null));
        this.mv.visitMethodInsn(INVOKESPECIAL, "com/googlecode/aviator/runtime/type/AviatorPattern",
            "<init>", "(Ljava/lang/String;)V");
        this.pushOperand(2);
        this.popOperand();
        this.popOperand();
        break;
      case Variable:
        // load variable
        Variable variable = (Variable) lookhead;

        if (variable.equals(Variable.TRUE)) {
          this.mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/runtime/type/AviatorBoolean",
              "TRUE", "Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
          this.pushOperand();
        } else if (variable.equals(Variable.FALSE)) {
          this.mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/runtime/type/AviatorBoolean",
              "FALSE", "Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
          this.pushOperand();
        } else if (variable.equals(Variable.NIL)) {
          this.mv.visitFieldInsn(GETSTATIC, "com/googlecode/aviator/runtime/type/AviatorNil", "NIL",
              "Lcom/googlecode/aviator/runtime/type/AviatorNil;");
          this.pushOperand();
        } else {
          String outterVarName = variable.getLexeme();
          String innerVarName = this.innerVarMap.get(outterVarName);
          if (innerVarName != null) {
            // Is it stored in local?
            Map<String, Integer> name2Index = this.labelNameIndexMap.get(this.currentLabel);
            if (name2Index != null && name2Index.get(innerVarName) != null) {
              int localIndex = name2Index.get(innerVarName);
              this.mv.visitVarInsn(ALOAD, localIndex);
              this.pushOperand();
            } else {
              // Get field at first time
              this.mv.visitVarInsn(ALOAD, 0);
              this.mv.visitFieldInsn(GETFIELD, this.className, innerVarName,
                  "Lcom/googlecode/aviator/runtime/type/AviatorJavaType;");
              // Variable is used more than once,store it to local
              if (this.varTokens.get(outterVarName) > 1) {
                this.mv.visitInsn(DUP);
                int localIndex = this.getLocalIndex();
                this.mv.visitVarInsn(ASTORE, localIndex);
                if (name2Index == null) {
                  name2Index = new HashMap<String, Integer>();
                  this.labelNameIndexMap.put(this.currentLabel, name2Index);
                }
                name2Index.put(innerVarName, localIndex);
                this.pushOperand(2);
                this.popOperand();
                this.popOperand();
              } else {
                this.pushOperand(1);
                this.popOperand();
              }
            }

          } else {
            this.mv.visitTypeInsn(NEW, "com/googlecode/aviator/runtime/type/AviatorJavaType");
            this.mv.visitInsn(DUP);
            this.mv.visitLdcInsn(outterVarName);
            this.mv.visitMethodInsn(INVOKESPECIAL,
                "com/googlecode/aviator/runtime/type/AviatorJavaType", "<init>",
                "(Ljava/lang/String;)V");
            this.pushOperand(2);
            this.popOperand();
            this.popOperand();
          }

        }
        break;
    }

  }


  public void initVariables(Map<String, Integer/* counter */> varTokens) {
    this.varTokens = varTokens;
    for (String outterVarName : varTokens.keySet()) {
      // Use inner variable name instead of outter variable name
      String innerVarName = this.getInnerName(outterVarName);
      this.innerVarMap.put(outterVarName, innerVarName);
      this.classWriter.visitField(ACC_PRIVATE + ACC_FINAL, innerVarName,
          "Lcom/googlecode/aviator/runtime/type/AviatorJavaType;", null, null).visitEnd();

    }
  }


  public void initMethods(Map<String, Integer/* counter */> methods) {
    this.methodTokens = methods;
    for (String outterMethodName : methods.keySet()) {
      // Use inner method name instead of outter method name
      String innerMethodName = this.getInnerName(outterMethodName);
      this.innerMethodMap.put(outterMethodName, innerMethodName);
      this.classWriter.visitField(ACC_PRIVATE + ACC_FINAL, innerMethodName,
          "Lcom/googlecode/aviator/runtime/type/AviatorFunction;", null, null).visitEnd();

    }
  }


  private String getInnerName(String varName) {
    return FIELD_PREFIX + this.fieldCounter++;
  }


  private String getInvokeMethodDesc(int paramCount) {
    StringBuilder sb = new StringBuilder("(Ljava/util/Map;");
    if (paramCount <= 20) {
      for (int i = 0; i < paramCount; i++) {
        sb.append("Lcom/googlecode/aviator/runtime/type/AviatorObject;");
      }
    } else {
      for (int i = 0; i < 20; i++) {
        sb.append("Lcom/googlecode/aviator/runtime/type/AviatorObject;");
      }
      // variadic params as an array
      sb.append("[Lcom/googlecode/aviator/runtime/type/AviatorObject;");
    }
    sb.append(")Lcom/googlecode/aviator/runtime/type/AviatorObject;");
    return sb.toString();
  }


  @Override
  public void onMethodInvoke(Token<?> lookhead) {
    final MethodMetaData methodMetaData = this.methodMetaDataStack.pop();
    final int parameterCount = methodMetaData.parameterCount;
    if (parameterCount >= 20) {
      if (parameterCount == 20) {
        // pop the list
        this.mv.visitInsn(Opcodes.POP);
        this.popOperand();
      } else {
        // to array
        this.mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "size", "()I");
        this.mv.visitTypeInsn(Opcodes.ANEWARRAY,
            "com/googlecode/aviator/runtime/type/AviatorObject");
        int arrayIndex = this.getLocalIndex();
        this.mv.visitVarInsn(ASTORE, arrayIndex);
        this.mv.visitVarInsn(ALOAD, methodMetaData.variadicListIndex);
        this.mv.visitVarInsn(ALOAD, arrayIndex);
        this.mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "toArray",
            "([Ljava/lang/Object;)[Ljava/lang/Object;");

        this.mv.visitTypeInsn(CHECKCAST, "[Lcom/googlecode/aviator/runtime/type/AviatorObject;");

        this.popOperand(); // pop list to get size
        this.pushOperand(); // new array, store and load it
        this.pushOperand(); // load list
        this.popOperand(); // list.toArray
      }
    }
    this.mv.visitMethodInsn(INVOKEINTERFACE, "com/googlecode/aviator/runtime/type/AviatorFunction",
        "call", this.getInvokeMethodDesc(parameterCount));

    this.popOperand(); // method object
    this.popOperand(); // env map
    // pop operands
    if (parameterCount <= 20) {
      this.popOperand(parameterCount);
    } else {
      // 20 params + one array
      this.popOperand(21);
    }
    // push result
    this.pushOperand();
  }


  @Override
  public void onMethodParameter(Token<?> lookhead) {
    MethodMetaData currentMethodMetaData = this.methodMetaDataStack.peek();
    if (currentMethodMetaData.parameterCount >= 20) {
      // Add last param to variadic param list
      assert currentMethodMetaData.variadicListIndex >= 0;
      this.mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z");
      this.mv.visitInsn(Opcodes.POP);
      this.mv.visitVarInsn(ALOAD, currentMethodMetaData.variadicListIndex);
      this.popOperand(); // pop list
      this.popOperand(); // pop param
      this.pushOperand(); // list.add result
      this.popOperand(); // pop last result
      this.pushOperand(); // load list
    }

    currentMethodMetaData.parameterCount++;
    if (currentMethodMetaData.parameterCount == 20) {
      // create variadic params list for further params
      this.mv.visitTypeInsn(NEW, "java/util/ArrayList");
      this.mv.visitInsn(DUP);
      this.mv.visitMethodInsn(INVOKESPECIAL, "java/util/ArrayList", "<init>", "()V");
      int listIndex = this.getLocalIndex();
      this.mv.visitVarInsn(ASTORE, listIndex);
      this.mv.visitVarInsn(ALOAD, listIndex);
      currentMethodMetaData.variadicListIndex = listIndex;
      this.pushOperand(); // new list
    }

    // // add parameter to list
    // this.mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "add",
    // "(Ljava/lang/Object;)Z");
    // // pop boolean
    // this.mv.visitInsn(POP);
    // this.mv.visitVarInsn(ALOAD,
    // this.methodMetaDataStack.peek().parameterListIndex);
  }


  private void pushOperand() {
    this.pushOperand(0);
  }

  private static class MethodMetaData {
    int parameterCount = 0;
    int variadicListIndex = -1;


    public MethodMetaData(String methodName) {
      super();
    }
  }

  private final Stack<MethodMetaData> methodMetaDataStack = new Stack<MethodMetaData>();


  @Override
  public void onArray(Token<?> lookhead) {
    this.onConstant(lookhead);
  }


  @Override
  public void onArrayIndexStart(Token<?> token) {
    this.loadEnv();
  }


  @Override
  public void onArrayIndexEnd(Token<?> lookhead) {
    if (!OperationRuntime.hasRuntimeContext(OperatorType.INDEX)) {
      this.mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/runtime/type/AviatorObject",
          "getElement",
          "(Ljava/util/Map;Lcom/googlecode/aviator/runtime/type/AviatorObject;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");
    } else {
      this.loadOpType(OperatorType.INDEX);
      this.mv.visitMethodInsn(INVOKESTATIC, "com/googlecode/aviator/runtime/op/OperationRuntime",
          "eval",
          "(Lcom/googlecode/aviator/runtime/type/AviatorObject;Ljava/util/Map;Lcom/googlecode/aviator/runtime/type/AviatorObject;Lcom/googlecode/aviator/lexer/token/OperatorType;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");
      this.popOperand();
    }

    this.popOperand();
    this.popOperand();
    this.popOperand();
    this.pushOperand();
  }


  public int getLocalIndex() {
    return this.maxLocals++;
  }


  @Override
  public void onMethodName(Token<?> lookhead) {
    String outtterMethodName = lookhead.getLexeme();
    String innerMethodName = this.innerMethodMap.get(outtterMethodName);
    if (innerMethodName != null) {
      this.loadAviatorFunction(outtterMethodName, innerMethodName);
    } else {
      this.createAviatorFunctionObject(outtterMethodName);
    }
    this.loadEnv();
    this.methodMetaDataStack.push(new MethodMetaData(outtterMethodName));
  }


  private void loadAviatorFunction(String outterMethodName, String innerMethodName) {
    Map<String, Integer> name2Index = this.labelNameIndexMap.get(this.currentLabel);
    // Is it stored in local?
    if (name2Index != null && name2Index.containsKey(innerMethodName)) {
      int localIndex = name2Index.get(innerMethodName);
      this.mv.visitVarInsn(ALOAD, localIndex);
      this.pushOperand();
    } else {
      this.mv.visitVarInsn(ALOAD, 0);
      this.mv.visitFieldInsn(GETFIELD, this.className, innerMethodName,
          "Lcom/googlecode/aviator/runtime/type/AviatorFunction;");
      // Method is used more than once,store it to local for reusing
      if (this.methodTokens.get(outterMethodName) > 1) {
        this.mv.visitInsn(DUP);
        int localIndex = this.getLocalIndex();
        this.mv.visitVarInsn(ASTORE, localIndex);
        if (name2Index == null) {
          name2Index = new HashMap<String, Integer>();
          this.labelNameIndexMap.put(this.currentLabel, name2Index);
        }
        name2Index.put(innerMethodName, localIndex);
        this.pushOperand(1);
        this.popOperand();
      } else {
        this.pushOperand();
      }
    }
  }


  // private int createArugmentList() {
  // // create argument list
  // this.pushOperand(0);
  // this.pushOperand(0);
  // this.mv.visitTypeInsn(NEW, "java/util/ArrayList");
  // this.mv.visitInsn(DUP);
  // this.popOperand();
  // this.mv.visitMethodInsn(INVOKESPECIAL, "java/util/ArrayList", "<init>",
  // "()V");
  // // store to local variable
  // final int parameterLocalIndex = this.getLocalIndex();
  // this.mv.visitVarInsn(ASTORE, parameterLocalIndex);
  // this.mv.visitVarInsn(ALOAD, parameterLocalIndex);
  // return parameterLocalIndex;
  // }

  private void loadEnv() {
    // load env
    this.pushOperand();
    this.mv.visitVarInsn(ALOAD, 1);
  }


  private void createAviatorFunctionObject(String methodName) {
    this.pushOperand();
    this.mv.visitLdcInsn(methodName);
    this.mv.visitMethodInsn(INVOKESTATIC, "com/googlecode/aviator/AviatorEvaluator", "getFunction",
        "(Ljava/lang/String;)Lcom/googlecode/aviator/runtime/type/AviatorFunction;");
    this.popOperand();
    this.pushOperand();
  }


  @Override
  public void onBitAnd(Token<?> lookhead) {
    this.visitBinOperator(OperatorType.BIT_AND, "bitAnd");
  }


  @Override
  public void onBitOr(Token<?> lookhead) {
    this.visitBinOperator(OperatorType.BIT_OR, "bitOr");
  }


  @Override
  public void onBitXor(Token<?> lookhead) {
    this.visitBinOperator(OperatorType.BIT_XOR, "bitXor");
  }


  @Override
  public void onShiftLeft(Token<?> lookhead) {
    this.visitBinOperator(OperatorType.SHIFT_LEFT, "shiftLeft");

  }


  @Override
  public void onShiftRight(Token<?> lookhead) {
    this.visitBinOperator(OperatorType.SHIFT_RIGHT, "shiftRight");

  }


  @Override
  public void onUnsignedShiftRight(Token<?> lookhead) {
    this.visitBinOperator(OperatorType.U_SHIFT_RIGHT, "unsignedShiftRight");

  }

}
