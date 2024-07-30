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
        "AviatorScript_" + System.currentTimeMillis() + "_" + LAMBDA_COUNTER.getAndIncrement();
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
  public void onShiftRight(final Token<?> lookahead) {
    this.codeGenerator.onShiftRight(lookahead);
  }



  @Override
  public void onShiftLeft(final Token<?> lookahead) {
    this.codeGenerator.onShiftLeft(lookahead);
  }



  @Override
  public void onUnsignedShiftRight(final Token<?> lookahead) {
    this.codeGenerator.onUnsignedShiftRight(lookahead);
  }



  @Override
  public void onAssignment(final Token<?> lookahead) {
    this.codeGenerator.onAssignment(lookahead);
  }


  @Override
  public void onBitOr(final Token<?> lookahead) {
    this.codeGenerator.onBitOr(lookahead);
  }


  @Override
  public void onBitAnd(final Token<?> lookahead) {
    this.codeGenerator.onBitAnd(lookahead);
  }



  @Override
  public void onBitXor(final Token<?> lookahead) {
    this.codeGenerator.onBitXor(lookahead);
  }



  @Override
  public void onBitNot(final Token<?> lookahead) {
    this.codeGenerator.onBitNot(lookahead);
  }



  @Override
  public void onAdd(final Token<?> lookahead) {
    this.codeGenerator.onAdd(lookahead);
  }

  @Override
  public void onSub(final Token<?> lookahead) {
    this.codeGenerator.onSub(lookahead);
  }



  @Override
  public void onMult(final Token<?> lookahead) {
    this.codeGenerator.onMult(lookahead);
  }



  @Override
  public void onExponent(final Token<?> lookahead) {
    this.codeGenerator.onExponent(lookahead);
  }


  @Override
  public void onDiv(final Token<?> lookahead) {
    this.codeGenerator.onDiv(lookahead);
  }



  @Override
  public void onAndLeft(final Token<?> lookahead) {
    this.codeGenerator.onAndLeft(lookahead);
  }



  @Override
  public void onAndRight(final Token<?> alookahead) {
    this.codeGenerator.onAndRight(alookahead);
  }



  @Override
  public void onTernaryBoolean(final Token<?> lookahead) {
    this.codeGenerator.onTernaryBoolean(lookahead);
  }



  @Override
  public void onTernaryLeft(final Token<?> lookahead) {
    this.codeGenerator.onTernaryLeft(lookahead);
  }



  @Override
  public void onTernaryRight(final Token<?> lookahead) {
    this.codeGenerator.onTernaryRight(lookahead);
  }



  @Override
  public void onTernaryEnd(final Token<?> lookahead) {
    this.codeGenerator.onTernaryEnd(lookahead);
  }


  @Override
  public void onJoinLeft(final Token<?> lookahead) {
    this.codeGenerator.onJoinLeft(lookahead);
  }



  @Override
  public void onJoinRight(final Token<?> lookahead) {
    this.codeGenerator.onJoinRight(lookahead);
  }



  @Override
  public void onEq(final Token<?> lookahead) {
    this.codeGenerator.onEq(lookahead);
  }



  @Override
  public void onMatch(final Token<?> lookahead) {
    this.codeGenerator.onMatch(lookahead);
  }



  @Override
  public void onNeq(final Token<?> lookahead) {
    this.codeGenerator.onNeq(lookahead);
  }



  @Override
  public void onLt(final Token<?> lookahead) {
    this.codeGenerator.onLt(lookahead);
  }



  @Override
  public void onLe(final Token<?> lookahead) {
    this.codeGenerator.onLe(lookahead);
  }



  @Override
  public void onGt(final Token<?> lookahead) {
    this.codeGenerator.onGt(lookahead);
  }



  @Override
  public void onGe(final Token<?> lookahead) {
    this.codeGenerator.onGe(lookahead);
  }



  @Override
  public void onMod(final Token<?> lookahead) {
    this.codeGenerator.onMod(lookahead);
  }



  @Override
  public void onNot(final Token<?> lookahead) {
    this.codeGenerator.onNot(lookahead);
  }



  @Override
  public void onNeg(final Token<?> lookahead) {
    this.codeGenerator.onNeg(lookahead);
  }


  @Override
  public Expression getResult(final boolean unboxObject) {
    return this.codeGenerator.getResult(unboxObject);
  }


  @Override
  public void onConstant(final Token<?> lookahead) {
    this.codeGenerator.onConstant(lookahead);
  }

  @Override
  public void onMethodName(final Token<?> lookahead) {
    this.codeGenerator.onMethodName(lookahead);
  }



  @Override
  public void onMethodParameter(final Token<?> lookahead) {
    this.codeGenerator.onMethodParameter(lookahead);
  }

  @Override
  public void onMethodInvoke(final Token<?> lookahead) {
    this.codeGenerator.onMethodInvoke(lookahead);
  }



  @Override
  public void onLambdaDefineStart(final Token<?> lookahead) {
    this.codeGenerator.onLambdaDefineStart(lookahead);
  }



  @Override
  public void onLambdaArgument(final Token<?> lookahead, final FunctionParam param) {
    this.codeGenerator.onLambdaArgument(lookahead, param);
  }

  @Override
  public void onLambdaBodyStart(final Token<?> lookahead) {
    this.codeGenerator.onLambdaBodyStart(lookahead);
  }

  @Override
  public void onLambdaBodyEnd(final Token<?> lookahead) {
    // should call parent generator
    this.parentCodeGenerator.onLambdaBodyEnd(lookahead);
  }

  @Override
  public void onArray(final Token<?> lookahead) {
    this.codeGenerator.onArray(lookahead);
  }

  @Override
  public void onArrayIndexStart(final Token<?> token) {
    this.codeGenerator.onArrayIndexStart(token);
  }

  @Override
  public void onArrayIndexEnd(final Token<?> lookahead) {
    this.codeGenerator.onArrayIndexEnd(lookahead);
  }

}
