package com.googlecode.aviator.code;
/**
 * lamdba function generator
 *
 * @author dennis
 *
 */

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.parser.AviatorClassLoader;
import com.googlecode.aviator.parser.Parser;
import com.googlecode.aviator.parser.ScopeInfo;
import com.googlecode.aviator.runtime.FunctionParam;
import com.googlecode.aviator.runtime.LambdaFunctionBootstrap;

/**
 * Lambda function generator
 *
 * @author dennis
 *
 */
public class LambdaGenerator implements CodeGenerator {
  // private final ClassWriter classWriter;
  private final List<FunctionParam> params;
  private final CodeGenerator codeGenerator;
  private final CodeGenerator parentCodeGenerator;
  // private final AviatorClassLoader classLoader;
  // private final AviatorEvaluatorInstance instance;
  private final String className;
  private static final AtomicLong LAMBDA_COUNTER = new AtomicLong();
  // private MethodVisitor mv;
  private ScopeInfo scopeInfo;
  private final boolean newLexicalScope;
  private final boolean inheritEnv;

  public LambdaGenerator(final AviatorEvaluatorInstance instance,
      final CodeGenerator parentCodeGenerator, final Parser parser,
      final AviatorClassLoader classLoader, final String sourceFile, final boolean newLexicalScope,
      final boolean inheritEnv) {
    this.params = new ArrayList<>();
    // this.instance = instance;
    this.parentCodeGenerator = parentCodeGenerator;
    this.codeGenerator = instance.newCodeGenerator(classLoader, sourceFile);
    this.codeGenerator.setParser(parser);
    // this.classLoader = classLoader;
    this.newLexicalScope = newLexicalScope;
    this.inheritEnv = inheritEnv;
    // Generate lambda class name
    this.className =
        "Lambda_" + System.currentTimeMillis() + "_" + LAMBDA_COUNTER.getAndIncrement();
    // Auto compute frames
    // this.classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
    // visitClass();
    // makeConstructor();
    // makeGetName();
  }


  public ScopeInfo getScopeInfo() {
    return this.scopeInfo;
  }


  public void setScopeInfo(final ScopeInfo scopeInfo) {
    this.scopeInfo = scopeInfo;
  }


  @Override
  public void setParser(final Parser parser) {
    this.codeGenerator.setParser(parser);
  }


  // /**
  // * Make a default constructor
  // */
  // private void makeConstructor() {
  // {
  // this.mv = this.classWriter.visitMethod(ACC_PUBLIC, "<init>",
  // "(Ljava/util/List;Lcom/googlecode/aviator/Expression;Lcom/googlecode/aviator/utils/Env;)V",
  // null, null);
  // this.mv.visitCode();
  // this.mv.visitVarInsn(ALOAD, 0);
  // this.mv.visitVarInsn(ALOAD, 1);
  // this.mv.visitVarInsn(ALOAD, 2);
  // this.mv.visitVarInsn(ALOAD, 3);
  // this.mv.visitMethodInsn(INVOKESPECIAL,
  // "com/googlecode/aviator/runtime/function/LambdaFunction", "<init>",
  // "(Ljava/util/List;Lcom/googlecode/aviator/Expression;Lcom/googlecode/aviator/utils/Env;)V");
  //
  // this.mv.visitInsn(RETURN);
  // this.mv.visitMaxs(4, 1);
  // this.mv.visitEnd();
  // }
  // }
  //

  /**
   * Make a getName method
   */
  // private void makeGetName() {
  // {
  // this.mv = this.classWriter.visitMethod(ACC_PUBLIC + +ACC_FINAL, "getName",
  // "()Ljava/lang/String;", "()Ljava/lang/String;", null);
  // this.mv.visitCode();
  // this.mv.visitLdcInsn(this.className);
  // this.mv.visitInsn(ARETURN);
  // this.mv.visitMaxs(1, 1);
  // this.mv.visitEnd();
  // }
  // }

  /**
   * Compile a call method to invoke lambda compiled body expression.
   */
  // public void compileCallMethod() {
  // int argsNumber = this.params.size();
  // int arrayIndex = 2 + argsNumber;
  // if (argsNumber < 20) {
  // StringBuilder argsDescSb = new StringBuilder();
  // for (int i = 0; i < argsNumber; i++) {
  // argsDescSb.append("Lcom/googlecode/aviator/runtime/type/AviatorObject;");
  // }
  // String argsDec = argsDescSb.toString();
  //
  // this.mv = this.classWriter.visitMethod(ACC_PUBLIC + +ACC_FINAL, "call",
  // "(Ljava/util/Map;" + argsDec + ")Lcom/googlecode/aviator/runtime/type/AviatorObject;",
  // "(Ljava/util/Map<Ljava/lang/String;Ljava/lang/Object;>;" + argsDec
  // + ")Lcom/googlecode/aviator/runtime/type/AviatorObject;",
  // null);
  // this.mv.visitCode();
  //
  // // load expression field
  // this.mv.visitIntInsn(ALOAD, 0);
  // this.mv.visitFieldInsn(GETFIELD, this.className, "expression",
  // "Lcom/googlecode/aviator/BaseExpression;");
  // // this pointer
  // this.mv.visitIntInsn(ALOAD, 0);
  // // load env
  // this.mv.visitIntInsn(ALOAD, 1);
  // // new array
  // this.mv.visitLdcInsn(argsNumber);
  // this.mv.visitTypeInsn(Opcodes.ANEWARRAY, "com/googlecode/aviator/runtime/type/AviatorObject");
  // this.mv.visitVarInsn(ASTORE, arrayIndex);
  // // load other arguments
  // for (int i = 0; i < argsNumber; i++) {
  // this.mv.visitVarInsn(ALOAD, arrayIndex);
  // this.mv.visitLdcInsn(i);
  // this.mv.visitVarInsn(ALOAD, i + 2);
  // this.mv.visitInsn(AASTORE);
  // }
  // this.mv.visitVarInsn(ALOAD, arrayIndex);
  // this.mv.visitMethodInsn(INVOKEVIRTUAL,
  // "com/googlecode/aviator/runtime/function/LambdaFunction", "newEnv",
  // "(Ljava/util/Map;[Lcom/googlecode/aviator/runtime/type/AviatorObject;)Ljava/util/Map;");
  // // execute body expression
  // this.mv.visitMethodInsn(INVOKEVIRTUAL, "com/googlecode/aviator/BaseExpression",
  // "executeDirectly", "(Ljava/util/Map;)Ljava/lang/Object;");
  // // get the result
  // this.mv.visitMethodInsn(INVOKESTATIC,
  // "com/googlecode/aviator/runtime/type/AviatorRuntimeJavaType", "valueOf",
  // "(Ljava/lang/Object;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");
  // this.mv.visitInsn(ARETURN);
  // this.mv.visitMaxs(5, 1);
  // this.mv.visitEnd();
  // } else {
  // throw new CompileExpressionErrorException("Lambda function arguments number at most 20.");
  // }
  // }
  //
  // private void visitClass() {
  // this.classWriter.visit(this.instance.getBytecodeVersion(), ACC_PUBLIC + ACC_SUPER,
  // this.className, null, "com/googlecode/aviator/runtime/function/LambdaFunction", null);
  // }
  //
  //
  //
  // private void endVisitClass() {
  // this.classWriter.visitEnd();
  // }

  public LambdaFunctionBootstrap getLmabdaBootstrap() {
    Expression expression = getResult(!this.newLexicalScope);
    // endVisitClass();
    // byte[] bytes = this.classWriter.toByteArray();
    // try {

    // Class<?> defineClass =
    // ClassDefiner.defineClass(this.className, LambdaFunction.class, bytes, this.classLoader);
    //
    // Constructor<?> constructor =
    // defineClass.getConstructor(List.class, Expression.class, Env.class);
    // MethodHandle methodHandle = MethodHandles.lookup().unreflectConstructor(constructor);
    return new LambdaFunctionBootstrap(this.className, expression, this.params, this.inheritEnv);
    // } catch (Exception e) {
    // throw new CompileExpressionErrorException("define lambda class error", e);
    // }
  }


  public void addParam(final FunctionParam name) {
    this.params.add(name);
  }


  @Override
  public void onShiftRight(final Token<?> lookhead) {
    this.codeGenerator.onShiftRight(lookhead);
  }



  @Override
  public void onShiftLeft(final Token<?> lookhead) {
    this.codeGenerator.onShiftLeft(lookhead);
  }



  @Override
  public void onUnsignedShiftRight(final Token<?> lookhead) {
    this.codeGenerator.onUnsignedShiftRight(lookhead);
  }



  @Override
  public void onAssignment(final Token<?> lookhead) {
    this.codeGenerator.onAssignment(lookhead);
  }


  @Override
  public void onBitOr(final Token<?> lookhead) {
    this.codeGenerator.onBitOr(lookhead);
  }


  @Override
  public void onBitAnd(final Token<?> lookhead) {
    this.codeGenerator.onBitAnd(lookhead);
  }



  @Override
  public void onBitXor(final Token<?> lookhead) {
    this.codeGenerator.onBitXor(lookhead);
  }



  @Override
  public void onBitNot(final Token<?> lookhead) {
    this.codeGenerator.onBitNot(lookhead);
  }



  @Override
  public void onAdd(final Token<?> lookhead) {
    this.codeGenerator.onAdd(lookhead);
  }

  @Override
  public void onSub(final Token<?> lookhead) {
    this.codeGenerator.onSub(lookhead);
  }



  @Override
  public void onMult(final Token<?> lookhead) {
    this.codeGenerator.onMult(lookhead);
  }



  @Override
  public void onExponent(final Token<?> lookhead) {
    this.codeGenerator.onExponent(lookhead);
  }


  @Override
  public void onDiv(final Token<?> lookhead) {
    this.codeGenerator.onDiv(lookhead);
  }



  @Override
  public void onAndLeft(final Token<?> lookhead) {
    this.codeGenerator.onAndLeft(lookhead);
  }



  @Override
  public void onAndRight(final Token<?> lookhead) {
    this.codeGenerator.onAndRight(lookhead);
  }



  @Override
  public void onTernaryBoolean(final Token<?> lookhead) {
    this.codeGenerator.onTernaryBoolean(lookhead);
  }



  @Override
  public void onTernaryLeft(final Token<?> lookhead) {
    this.codeGenerator.onTernaryLeft(lookhead);
  }



  @Override
  public void onTernaryRight(final Token<?> lookhead) {
    this.codeGenerator.onTernaryRight(lookhead);
  }



  @Override
  public void onTernaryEnd(final Token<?> lookhead) {
    this.codeGenerator.onTernaryEnd(lookhead);
  }


  @Override
  public void onJoinLeft(final Token<?> lookhead) {
    this.codeGenerator.onJoinLeft(lookhead);
  }



  @Override
  public void onJoinRight(final Token<?> lookhead) {
    this.codeGenerator.onJoinRight(lookhead);
  }



  @Override
  public void onEq(final Token<?> lookhead) {
    this.codeGenerator.onEq(lookhead);
  }



  @Override
  public void onMatch(final Token<?> lookhead) {
    this.codeGenerator.onMatch(lookhead);
  }



  @Override
  public void onNeq(final Token<?> lookhead) {
    this.codeGenerator.onNeq(lookhead);
  }



  @Override
  public void onLt(final Token<?> lookhead) {
    this.codeGenerator.onLt(lookhead);
  }



  @Override
  public void onLe(final Token<?> lookhead) {
    this.codeGenerator.onLe(lookhead);
  }



  @Override
  public void onGt(final Token<?> lookhead) {
    this.codeGenerator.onGt(lookhead);
  }



  @Override
  public void onGe(final Token<?> lookhead) {
    this.codeGenerator.onGe(lookhead);
  }



  @Override
  public void onMod(final Token<?> lookhead) {
    this.codeGenerator.onMod(lookhead);
  }



  @Override
  public void onNot(final Token<?> lookhead) {
    this.codeGenerator.onNot(lookhead);
  }



  @Override
  public void onNeg(final Token<?> lookhead) {
    this.codeGenerator.onNeg(lookhead);
  }


  @Override
  public Expression getResult(final boolean unboxObject) {
    return this.codeGenerator.getResult(unboxObject);
  }


  @Override
  public void onConstant(final Token<?> lookhead) {
    this.codeGenerator.onConstant(lookhead);
  }

  @Override
  public void onMethodName(final Token<?> lookhead) {
    this.codeGenerator.onMethodName(lookhead);
  }



  @Override
  public void onMethodParameter(final Token<?> lookhead) {
    this.codeGenerator.onMethodParameter(lookhead);
  }

  @Override
  public void onMethodInvoke(final Token<?> lookhead) {
    this.codeGenerator.onMethodInvoke(lookhead);
  }



  @Override
  public void onLambdaDefineStart(final Token<?> lookhead) {
    this.codeGenerator.onLambdaDefineStart(lookhead);
  }



  @Override
  public void onLambdaArgument(final Token<?> lookhead, final FunctionParam param) {
    this.codeGenerator.onLambdaArgument(lookhead, param);
  }

  @Override
  public void onLambdaBodyStart(final Token<?> lookhead) {
    this.codeGenerator.onLambdaBodyStart(lookhead);
  }

  @Override
  public void onLambdaBodyEnd(final Token<?> lookhead) {
    // should call parent generator
    this.parentCodeGenerator.onLambdaBodyEnd(lookhead);
  }

  @Override
  public void onArray(final Token<?> lookhead) {
    this.codeGenerator.onArray(lookhead);
  }

  @Override
  public void onArrayIndexStart(final Token<?> token) {
    this.codeGenerator.onArrayIndexStart(token);
  }

  @Override
  public void onArrayIndexEnd(final Token<?> lookhead) {
    this.codeGenerator.onArrayIndexEnd(lookhead);
  }

}
