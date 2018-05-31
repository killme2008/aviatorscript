package com.googlecode.aviator.code;
/**
 * lamdba function generator
 *
 * @author dennis
 *
 */

import static com.googlecode.aviator.asm.Opcodes.AASTORE;
import static com.googlecode.aviator.asm.Opcodes.ACC_FINAL;
import static com.googlecode.aviator.asm.Opcodes.ACC_PUBLIC;
import static com.googlecode.aviator.asm.Opcodes.ACC_SUPER;
import static com.googlecode.aviator.asm.Opcodes.ALOAD;
import static com.googlecode.aviator.asm.Opcodes.ARETURN;
import static com.googlecode.aviator.asm.Opcodes.ASTORE;
import static com.googlecode.aviator.asm.Opcodes.GETFIELD;
import static com.googlecode.aviator.asm.Opcodes.INVOKEINTERFACE;
import static com.googlecode.aviator.asm.Opcodes.INVOKESPECIAL;
import static com.googlecode.aviator.asm.Opcodes.INVOKESTATIC;
import static com.googlecode.aviator.asm.Opcodes.INVOKEVIRTUAL;
import static com.googlecode.aviator.asm.Opcodes.RETURN;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.asm.ClassWriter;
import com.googlecode.aviator.asm.MethodVisitor;
import com.googlecode.aviator.asm.Opcodes;
import com.googlecode.aviator.code.asm.ClassDefiner;
import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.parser.AviatorClassLoader;
import com.googlecode.aviator.parser.Parser;
import com.googlecode.aviator.parser.ScopeInfo;
import com.googlecode.aviator.runtime.LambdaFunctionBootstrap;
import com.googlecode.aviator.runtime.function.LambdaFunction;
import com.googlecode.aviator.utils.Env;

/**
 * Lambda function generator
 *
 * @author dennis
 *
 */
public class LambdaGenerator implements CodeGenerator {
  private ClassWriter classWriter;
  private List<String> arguments;
  private CodeGenerator codeGenerator;
  private CodeGenerator parentCodeGenerator;
  private AviatorClassLoader classLoader;
  private AviatorEvaluatorInstance instance;
  private String className;
  private static final AtomicLong LAMBDA_COUNTER = new AtomicLong();
  private MethodVisitor mv;
  private ScopeInfo scopeInfo;

  public LambdaGenerator(AviatorEvaluatorInstance instance, CodeGenerator parentCodeGenerator,
      Parser parser, AviatorClassLoader classLoader) {
    this.arguments = new ArrayList<String>();
    this.instance = instance;
    this.parentCodeGenerator = parentCodeGenerator;
    this.codeGenerator = instance.newCodeGenerator(classLoader);
    this.codeGenerator.setParser(parser);
    this.classLoader = classLoader;
    // Generate lambda class name
    this.className =
        "Lambda_" + System.currentTimeMillis() + "_" + LAMBDA_COUNTER.getAndIncrement();
    // Auto compute frames
    this.classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
    this.visitClass();
    this.makeConstructor();
    this.makeGetName();
  }


  public ScopeInfo getScopeInfo() {
    return scopeInfo;
  }


  public void setScopeInfo(ScopeInfo scopeInfo) {
    this.scopeInfo = scopeInfo;
  }


  @Override
  public void setParser(Parser parser) {
    this.codeGenerator.setParser(parser);
  }


  /**
   * Make a default constructor
   */
  private void makeConstructor() {
    {
      this.mv = this.classWriter.visitMethod(ACC_PUBLIC, "<init>",
          "(Ljava/util/List;Lcom/googlecode/aviator/Expression;Lcom/googlecode/aviator/utils/Env;)V",
          null, null);
      this.mv.visitCode();
      this.mv.visitVarInsn(ALOAD, 0);
      this.mv.visitVarInsn(ALOAD, 1);
      this.mv.visitVarInsn(ALOAD, 2);
      this.mv.visitVarInsn(ALOAD, 3);
      this.mv.visitMethodInsn(INVOKESPECIAL,
          "com/googlecode/aviator/runtime/function/LambdaFunction", "<init>",
          "(Ljava/util/List;Lcom/googlecode/aviator/Expression;Lcom/googlecode/aviator/utils/Env;)V");

      this.mv.visitInsn(RETURN);
      this.mv.visitMaxs(4, 1);
      this.mv.visitEnd();
    }
  }


  /**
   * Make a getName method
   */
  private void makeGetName() {
    {
      this.mv = this.classWriter.visitMethod(ACC_PUBLIC + +ACC_FINAL, "getName",
          "()Ljava/lang/String;", "()Ljava/lang/String;", null);
      this.mv.visitCode();
      this.mv.visitLdcInsn(this.className);
      this.mv.visitInsn(ARETURN);
      this.mv.visitMaxs(1, 1);
      this.mv.visitEnd();
    }
  }

  /**
   * Compile a call method to invoke lambda compiled body expression.
   */
  public void compileCallMethod() {
    int argsNumber = this.arguments.size();
    int arrayIndex = 2 + argsNumber;
    if (argsNumber < 20) {
      StringBuilder argsDescSb = new StringBuilder();
      for (int i = 0; i < argsNumber; i++) {
        argsDescSb.append("Lcom/googlecode/aviator/runtime/type/AviatorObject;");
      }
      String argsDec = argsDescSb.toString();

      this.mv = this.classWriter.visitMethod(ACC_PUBLIC + +ACC_FINAL, "call",
          "(Ljava/util/Map;" + argsDec + ")Lcom/googlecode/aviator/runtime/type/AviatorObject;",
          "(Ljava/util/Map<Ljava/lang/String;Ljava/lang/Object;>;" + argsDec
              + ")Lcom/googlecode/aviator/runtime/type/AviatorObject;",
          null);
      this.mv.visitCode();

      // load expression field
      this.mv.visitIntInsn(ALOAD, 0);
      this.mv.visitFieldInsn(GETFIELD, this.className, "expression",
          "Lcom/googlecode/aviator/Expression;");
      // this pointer
      this.mv.visitIntInsn(ALOAD, 0);
      // load env
      this.mv.visitIntInsn(ALOAD, 1);
      // new array
      this.mv.visitLdcInsn(argsNumber);
      this.mv.visitTypeInsn(Opcodes.ANEWARRAY, "com/googlecode/aviator/runtime/type/AviatorObject");
      this.mv.visitVarInsn(ASTORE, arrayIndex);
      // load other arguments
      for (int i = 0; i < argsNumber; i++) {
        this.mv.visitVarInsn(ALOAD, arrayIndex);
        this.mv.visitLdcInsn(i);
        this.mv.visitVarInsn(ALOAD, i + 2);
        this.mv.visitInsn(AASTORE);
      }
      this.mv.visitVarInsn(ALOAD, arrayIndex);
      this.mv.visitMethodInsn(INVOKEVIRTUAL,
          "com/googlecode/aviator/runtime/function/LambdaFunction", "newEnv",
          "(Ljava/util/Map;[Lcom/googlecode/aviator/runtime/type/AviatorObject;)Ljava/util/Map;");
      // execute body expression
      this.mv.visitMethodInsn(INVOKEINTERFACE, "com/googlecode/aviator/Expression", "execute",
          "(Ljava/util/Map;)Ljava/lang/Object;");
      // get the result
      this.mv.visitMethodInsn(INVOKESTATIC,
          "com/googlecode/aviator/runtime/type/AviatorRuntimeJavaType", "valueOf",
          "(Ljava/lang/Object;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");
      this.mv.visitInsn(ARETURN);
      this.mv.visitMaxs(5, 1);
      this.mv.visitEnd();
    } else {
      throw new CompileExpressionErrorException("Lambda function arguments number at most 20.");
    }
  }

  private void visitClass() {
    this.classWriter.visit(instance.getBytecodeVersion(), ACC_PUBLIC + ACC_SUPER, this.className,
        null, "com/googlecode/aviator/runtime/function/LambdaFunction", null);
  }



  private void endVisitClass() {
    this.classWriter.visitEnd();
  }

  public LambdaFunctionBootstrap getLmabdaBootstrap() {
    Expression expression = this.getResult();
    this.endVisitClass();
    byte[] bytes = this.classWriter.toByteArray();
    try {
      Class<?> defineClass =
          ClassDefiner.defineClass(this.className, LambdaFunction.class, bytes, this.classLoader);
      Constructor<?> constructor =
          defineClass.getConstructor(List.class, Expression.class, Env.class);
      // MethodHandle methodHandle = MethodHandles.lookup().findConstructor(defineClass,
      // MethodType.methodType(Void.class, List.class, Expression.class, Env.class));
      MethodHandle methodHandle = MethodHandles.lookup().unreflectConstructor(constructor);
      return new LambdaFunctionBootstrap(this.className, expression, methodHandle, arguments);
    } catch (Exception e) {
      throw new CompileExpressionErrorException("define lambda class error", e);
    }
  }


  public void addArgument(String name) {
    this.arguments.add(name);
  }



  @Override
  public void onShiftRight(Token<?> lookhead) {
    codeGenerator.onShiftRight(lookhead);
  }



  @Override
  public void onShiftLeft(Token<?> lookhead) {
    codeGenerator.onShiftLeft(lookhead);
  }



  @Override
  public void onUnsignedShiftRight(Token<?> lookhead) {
    codeGenerator.onUnsignedShiftRight(lookhead);
  }



  @Override
  public void onBitOr(Token<?> lookhead) {
    codeGenerator.onBitOr(lookhead);
  }



  @Override
  public void onBitAnd(Token<?> lookhead) {
    codeGenerator.onBitAnd(lookhead);
  }



  @Override
  public void onBitXor(Token<?> lookhead) {
    codeGenerator.onBitXor(lookhead);
  }



  @Override
  public void onBitNot(Token<?> lookhead) {
    codeGenerator.onBitNot(lookhead);
  }



  @Override
  public void onAdd(Token<?> lookhead) {
    codeGenerator.onAdd(lookhead);
  }



  @Override
  public void onSub(Token<?> lookhead) {
    codeGenerator.onSub(lookhead);
  }



  @Override
  public void onMult(Token<?> lookhead) {
    codeGenerator.onMult(lookhead);
  }



  @Override
  public void onDiv(Token<?> lookhead) {
    codeGenerator.onDiv(lookhead);
  }



  @Override
  public void onAndLeft(Token<?> lookhead) {
    codeGenerator.onAndLeft(lookhead);
  }



  @Override
  public void onAndRight(Token<?> lookhead) {
    codeGenerator.onAndRight(lookhead);
  }



  @Override
  public void onTernaryBoolean(Token<?> lookhead) {
    codeGenerator.onTernaryBoolean(lookhead);
  }



  @Override
  public void onTernaryLeft(Token<?> lookhead) {
    codeGenerator.onTernaryLeft(lookhead);
  }



  @Override
  public void onTernaryRight(Token<?> lookhead) {
    codeGenerator.onTernaryRight(lookhead);
  }



  @Override
  public void onTernaryEnd(Token<?> lookhead) {
    this.codeGenerator.onTernaryEnd(lookhead);
  }


  @Override
  public void onJoinLeft(Token<?> lookhead) {
    codeGenerator.onJoinLeft(lookhead);
  }



  @Override
  public void onJoinRight(Token<?> lookhead) {
    codeGenerator.onJoinRight(lookhead);
  }



  @Override
  public void onEq(Token<?> lookhead) {
    codeGenerator.onEq(lookhead);
  }



  @Override
  public void onMatch(Token<?> lookhead) {
    codeGenerator.onMatch(lookhead);
  }



  @Override
  public void onNeq(Token<?> lookhead) {
    codeGenerator.onNeq(lookhead);
  }



  @Override
  public void onLt(Token<?> lookhead) {
    codeGenerator.onLt(lookhead);
  }



  @Override
  public void onLe(Token<?> lookhead) {
    codeGenerator.onLe(lookhead);
  }



  @Override
  public void onGt(Token<?> lookhead) {
    codeGenerator.onGt(lookhead);
  }



  @Override
  public void onGe(Token<?> lookhead) {
    codeGenerator.onGe(lookhead);
  }



  @Override
  public void onMod(Token<?> lookhead) {
    codeGenerator.onMod(lookhead);
  }



  @Override
  public void onNot(Token<?> lookhead) {
    codeGenerator.onNot(lookhead);
  }



  @Override
  public void onNeg(Token<?> lookhead) {
    codeGenerator.onNeg(lookhead);
  }


  @Override
  public Expression getResult() {
    return codeGenerator.getResult();
  }


  @Override
  public void onConstant(Token<?> lookhead) {
    codeGenerator.onConstant(lookhead);
  }

  @Override
  public void onMethodName(Token<?> lookhead) {
    codeGenerator.onMethodName(lookhead);
  }



  @Override
  public void onMethodParameter(Token<?> lookhead) {
    codeGenerator.onMethodParameter(lookhead);
  }

  @Override
  public void onMethodInvoke(Token<?> lookhead) {
    codeGenerator.onMethodInvoke(lookhead);
  }



  @Override
  public void onLambdaDefineStart(Token<?> lookhead) {
    codeGenerator.onLambdaDefineStart(lookhead);
  }



  @Override
  public void onLambdaArgument(Token<?> lookhead) {
    codeGenerator.onLambdaArgument(lookhead);
  }



  @Override
  public void onLambdaBodyStart(Token<?> lookhead) {
    codeGenerator.onLambdaBodyStart(lookhead);
  }



  @Override
  public void onLambdaBodyEnd(Token<?> lookhead) {
    // should call parent generator
    parentCodeGenerator.onLambdaBodyEnd(lookhead);
  }

  @Override
  public void onArray(Token<?> lookhead) {
    codeGenerator.onArray(lookhead);
  }

  @Override
  public void onArrayIndexStart(Token<?> token) {
    codeGenerator.onArrayIndexStart(token);
  }

  @Override
  public void onArrayIndexEnd(Token<?> lookhead) {
    codeGenerator.onArrayIndexEnd(lookhead);
  }

}
