package com.googlecode.aviator.code.interpreter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.ExpressionAccessor;
import com.googlecode.aviator.InterpretExpression;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.code.BaseEvalCodeGenerator;
import com.googlecode.aviator.code.LambdaGenerator;
import com.googlecode.aviator.code.asm.ASMCodeGenerator.MethodMetaData;
import com.googlecode.aviator.code.interpreter.ir.AssertTypeIR;
import com.googlecode.aviator.code.interpreter.ir.AssertTypeIR.AssertTypes;
import com.googlecode.aviator.code.interpreter.ir.BranchIfIR;
import com.googlecode.aviator.code.interpreter.ir.BranchUnlessIR;
import com.googlecode.aviator.code.interpreter.ir.ClearIR;
import com.googlecode.aviator.code.interpreter.ir.GotoIR;
import com.googlecode.aviator.code.interpreter.ir.JumpIR;
import com.googlecode.aviator.code.interpreter.ir.Label;
import com.googlecode.aviator.code.interpreter.ir.LoadIR;
import com.googlecode.aviator.code.interpreter.ir.NewLambdaIR;
import com.googlecode.aviator.code.interpreter.ir.OperatorIR;
import com.googlecode.aviator.code.interpreter.ir.PopIR;
import com.googlecode.aviator.code.interpreter.ir.SendIR;
import com.googlecode.aviator.code.interpreter.ir.SourceInfo;
import com.googlecode.aviator.code.interpreter.ir.VisitLabelIR;
import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.lexer.token.Token.TokenType;
import com.googlecode.aviator.parser.AviatorClassLoader;
import com.googlecode.aviator.parser.VariableMeta;
import com.googlecode.aviator.runtime.FunctionArgument;
import com.googlecode.aviator.runtime.FunctionParam;
import com.googlecode.aviator.runtime.LambdaFunctionBootstrap;
import com.googlecode.aviator.runtime.op.OperationRuntime;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.utils.Constants;
import com.googlecode.aviator.utils.IdentityHashSet;

/**
 * Generate expression based on IR for interpreting.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class InterpretCodeGenerator extends BaseEvalCodeGenerator {
  private final List<IR> instruments = new ArrayList<>();
  private Set<Token<?>> constantPool = Collections.emptySet();

  private int labelNum;

  private final Stack<Label> labels0 = new Stack<>();

  private final Stack<Label> labels1 = new Stack<>();

  private Label currLabel;

  private void visitLabel(final Label label) {
    this.currLabel = label;
    emit(new VisitLabelIR(label));
  }

  private void pushLabel0(final Label label) {
    this.labels0.push(label);
  }

  private Label popLabel0() {
    return this.labels0.pop();
  }

  private Label peekLabel0() {
    return this.labels0.peek();
  }

  private void pushLabel1(final Label label) {
    this.labels1.push(label);
  }

  private Label popLabel1() {
    return this.labels1.pop();
  }

  private Label peekLabel1() {
    return this.labels1.peek();
  }


  private Label makeLabel() {
    return new Label(this.labelNum++);
  }



  @Override
  public void start() {
    // not implemented yet
  }

  @Override
  public void initVariables(final Map<String, VariableMeta> vars) {
    this.variables = vars;
  }

  @Override
  public void initConstants(final Set<Token<?>> constants) {
    if (constants.isEmpty()) {
      return;
    }
    this.constantPool = constants;
  }

  @Override
  public void genNewLambdaCode(final LambdaFunctionBootstrap bootstrap) {
    emit(new NewLambdaIR(bootstrap.getName()));
  }

  public InterpretCodeGenerator(final AviatorEvaluatorInstance instance, final String sourceFile,
      final AviatorClassLoader classLoader) {
    super(instance, sourceFile, classLoader);
  }

  @Override
  public void onAssignment(final Token<?> lookahead) {
    if (lookahead.getMeta(Constants.DEFINE_META, false)) {
      emit(OperatorIR.DEF);
    } else {
      emit(OperatorIR.ASSIGN);
    }
  }

  @Override
  public void onShiftRight(final Token<?> lookahead) {
    emit(OperatorIR.SHIFT_RIGHT);
  }

  @Override
  public void onShiftLeft(final Token<?> lookahead) {
    emit(OperatorIR.SHIFT_LEFT);
  }

  @Override
  public void onUnsignedShiftRight(final Token<?> lookahead) {
    emit(OperatorIR.UNSIGNED_SHIFT_RIGHT);
  }

  @Override
  public void onBitOr(final Token<?> lookahead) {
    emit(OperatorIR.BIT_OR);
  }

  @Override
  public void onBitAnd(final Token<?> lookahead) {
    emit(OperatorIR.BIT_AND);
  }

  @Override
  public void onBitXor(final Token<?> lookahead) {
    emit(OperatorIR.BIT_XOR);
  }

  @Override
  public void onBitNot(final Token<?> lookahead) {
    emit(OperatorIR.BIT_NOT);
  }

  @Override
  public void onAdd(final Token<?> lookahead) {
    emit(OperatorIR.ADD);
  }

  @Override
  public void onSub(final Token<?> lookahead) {
    emit(OperatorIR.SUB);

  }

  @Override
  public void onMult(final Token<?> lookahead) {
    emit(OperatorIR.MULT);
  }

  @Override
  public void onExponent(final Token<?> loohead) {
    emit(OperatorIR.EXP);
  }

  @Override
  public void onDiv(final Token<?> lookahead) {
    emit(OperatorIR.DIV);

  }

  @Override
  public void onAndLeft(final Token<?> lookahead) {
    if (!OperationRuntime.containsOpFunction(this.compileEnv, OperatorType.AND)) {
      emit(new AssertTypeIR(AssertTypes.Bool));
      Label label = makeLabel();
      pushLabel0(label);
      this.instruments
          .add(new BranchUnlessIR(label, new SourceInfo(this.sourceFile, lookahead.getLineNo())));
      emit(PopIR.INSTANCE);
    }
  }

  private void emit(IR ir) {
    if (ir instanceof OperatorIR) {
      // check if operator is override.
      final OperatorType op = ((OperatorIR) ir).getOp();
      AviatorFunction fn = this.instance.getOpFunction(op);
      if (fn != null) {
        // replace it with new IR
        ir = new OperatorIR(op, fn);
      }
    }

    this.instruments.add(ir);
  }

  @Override
  public void onAndRight(final Token<?> alookahead) {
    if (!OperationRuntime.containsOpFunction(this.compileEnv, OperatorType.AND)) {
      emit(new AssertTypeIR(AssertTypes.Bool));
      Label label = popLabel0();
      visitLabel(label);
    } else {
      emit(OperatorIR.AND);
    }
  }

  @Override
  public void onTernaryBoolean(final Token<?> lookahead) {
    Label label0 = makeLabel();
    pushLabel0(label0);
    Label label1 = makeLabel();
    pushLabel1(label1);
    this.instruments
        .add(new BranchUnlessIR(label0, new SourceInfo(this.sourceFile, lookahead.getLineNo())));
    emit(PopIR.INSTANCE);
  }

  @Override
  public void onTernaryLeft(final Token<?> lookahead) {
    this.instruments
        .add(new GotoIR(peekLabel1(), new SourceInfo(this.sourceFile, lookahead.getLineNo())));

    // emit(PopIR.INSTANCE);
    Label label0 = popLabel0();
    visitLabel(label0);
    emit(PopIR.INSTANCE);
  }

  @Override
  public void onTernaryRight(final Token<?> lookahead) {
    Label label1 = popLabel1();
    visitLabel(label1);
  }

  @Override
  public void onTernaryEnd(final Token<?> lookahead) {
    emit(ClearIR.INSTANCE);
  }

  @Override
  public void onJoinLeft(final Token<?> lookahead) {
    if (!OperationRuntime.containsOpFunction(this.compileEnv, OperatorType.AND)) {
      emit(new AssertTypeIR(AssertTypes.Bool));
      Label label = makeLabel();
      pushLabel0(label);
      this.instruments
          .add(new BranchIfIR(label, new SourceInfo(this.sourceFile, lookahead.getLineNo())));
      emit(PopIR.INSTANCE);
    }
  }

  @Override
  public void onJoinRight(final Token<?> lookahead) {
    if (!OperationRuntime.containsOpFunction(this.compileEnv, OperatorType.AND)) {
      emit(new AssertTypeIR(AssertTypes.Bool));
      Label label = popLabel0();
      visitLabel(label);
    } else {
      emit(OperatorIR.OR);
    }
  }

  @Override
  public void onEq(final Token<?> lookahead) {
    emit(OperatorIR.EQ);
  }

  @Override
  public void onMatch(final Token<?> lookahead) {
    emit(OperatorIR.MATCH);
  }

  @Override
  public void onNeq(final Token<?> lookahead) {
    emit(OperatorIR.NE);
  }

  @Override
  public void onLt(final Token<?> lookahead) {
    emit(OperatorIR.LT);
  }

  @Override
  public void onLe(final Token<?> lookahead) {
    emit(OperatorIR.LE);
  }

  @Override
  public void onGt(final Token<?> lookahead) {
    emit(OperatorIR.GT);
  }

  @Override
  public void onGe(final Token<?> lookahead) {
    emit(OperatorIR.GE);
  }

  @Override
  public void onMod(final Token<?> lookahead) {
    emit(OperatorIR.MOD);
  }

  @Override
  public void onNot(final Token<?> lookahead) {
    emit(OperatorIR.NOT);
  }

  @Override
  public void onNeg(final Token<?> lookahead) {
    emit(OperatorIR.NEG);
  }

  @Override
  public Expression getResult(final boolean unboxObject) {
    final List<IR> instruments = this.instruments;

    // for (IR ir : instruments) {
    // System.out.println(ir);
    // }

    optimize(instruments);
    resolveLabels(instruments);

    final InterpretExpression exp =
        new InterpretExpression(this.instance, new ArrayList<VariableMeta>(this.variables.values()),
            this.constantPool, this.symbolTable, instruments, unboxObject);
    ExpressionAccessor.setLambdaBootstraps(exp, this.lambdaBootstraps);
    ExpressionAccessor.setSourceFile(exp, this.sourceFile);
    ExpressionAccessor.setFuncsArgs(exp, this.funcsArgs);
    ExpressionAccessor.setFunctionNames(exp, new ArrayList<String>(this.methodTokens.keySet()));

    return exp;
  }

  private void optimize(final List<IR> instruments) {
    // TODO
  }

  private void resolveLabels(final List<IR> instruments) {
    Map<Label, Integer/* pc */> label2pc = new IdentityHashMap<Label, Integer>();
    ListIterator<IR> it = instruments.listIterator();

    int i = 0;
    while (it.hasNext()) {
      IR ir = it.next();
      // Find all visit_label IR, replace them with pc.
      if (ir instanceof VisitLabelIR) {
        it.remove();
        label2pc.put(((VisitLabelIR) ir).getLabel(), i);
      } else {
        i = i + 1;
      }
    }

    // resolve label to pc
    for (IR ir : instruments) {
      if (ir instanceof JumpIR) {
        ((JumpIR) ir).setPc(label2pc.get(((JumpIR) ir).getLabel()));
      }
    }
  }

  private static Set<TokenType> LOAD_CONSTANTS_TYPE = new IdentityHashSet<>();
  {
    LOAD_CONSTANTS_TYPE.add(TokenType.Number);
    LOAD_CONSTANTS_TYPE.add(TokenType.String);
    LOAD_CONSTANTS_TYPE.add(TokenType.Pattern);
    LOAD_CONSTANTS_TYPE.add(TokenType.Variable);
  }

  @Override
  public void onConstant(final Token<?> lookahead) {
    if (LOAD_CONSTANTS_TYPE.contains(lookahead.getType())) {
      VariableMeta meta = null;

      if (lookahead.getType() == TokenType.Variable) {
        meta = this.variables.get(lookahead.getLexeme());
      }

      emit(new LoadIR(this.sourceFile, lookahead, meta, this.constantPool.contains(lookahead)));
    }
  }

  @Override
  public void onMethodName(final Token<?> lookahead) {
    final MethodMetaData metadata = new MethodMetaData(lookahead,
        lookahead.getType() == TokenType.Delegate ? null : lookahead.getLexeme());
    this.methodMetaDataStack.push(metadata);
  }

  @Override
  public void onMethodParameter(final Token<?> lookahead) {
    MethodMetaData currentMethodMetaData = this.methodMetaDataStack.peek();
    currentMethodMetaData.parameterCount++;
  }

  @Override
  public void onMethodInvoke(final Token<?> lookahead) {

    final MethodMetaData methodMetaData = this.methodMetaDataStack.pop();
    @SuppressWarnings("unchecked")
    final List<FunctionArgument> params = lookahead != null
        ? (List<FunctionArgument>) lookahead.getMeta(Constants.PARAMS_META, Collections.EMPTY_LIST)
        : Collections.<FunctionArgument>emptyList();

    if (this.instance.getOptionValue(Options.CAPTURE_FUNCTION_ARGS).bool) {
      int funcId = getNextFuncInvocationId();
      getFuncsArgs().put(funcId, Collections.unmodifiableList(params));
      methodMetaData.funcId = funcId;
    }

    emit(new SendIR(methodMetaData.methodName, methodMetaData.parameterCount,
        methodMetaData.token.getMeta(Constants.UNPACK_ARGS, false), methodMetaData.funcId,
        new SourceInfo(this.sourceFile, methodMetaData.token.getLineNo())));
  }

  @Override
  public void onLambdaDefineStart(final Token<?> lookahead) {
    if (this.lambdaGenerator == null) {
      Boolean newLexicalScope = lookahead.getMeta(Constants.SCOPE_META, false);
      Boolean inheritEnv = lookahead.getMeta(Constants.INHERIT_ENV_META, false);
      // TODO cache?
      this.lambdaGenerator = new LambdaGenerator(this.instance, this, this.parser, this.classLoader,
          this.sourceFile, newLexicalScope, inheritEnv);
      this.lambdaGenerator.setScopeInfo(this.parser.enterScope(newLexicalScope));
    } else {
      throw new CompileExpressionErrorException("Compile lambda error");
    }

  }

  @Override
  public void onLambdaArgument(final Token<?> lookahead, final FunctionParam param) {
    this.lambdaGenerator.addParam(param);
  }

  @Override
  public void onLambdaBodyStart(final Token<?> lookahead) {
    this.parentCodeGenerator = this.parser.getCodeGenerator();
    this.parser.setCodeGenerator(this.lambdaGenerator);
  }

  @Override
  public void onLambdaBodyEnd(final Token<?> lookahead) {
    // this.lambdaGenerator.compileCallMethod();
    LambdaFunctionBootstrap bootstrap = this.lambdaGenerator.getLmabdaBootstrap();
    if (this.lambdaBootstraps == null) {
      // keep in order
      this.lambdaBootstraps = new LinkedHashMap<String, LambdaFunctionBootstrap>();
    }
    this.lambdaBootstraps.put(bootstrap.getName(), bootstrap);
    genNewLambdaCode(bootstrap);
    this.parser.restoreScope(this.lambdaGenerator.getScopeInfo());
    this.lambdaGenerator = null;
    this.parser.setCodeGenerator(this.parentCodeGenerator);
  }

  @Override
  public void onArray(final Token<?> lookahead) {
    onConstant(lookahead);
  }

  @Override
  public void onArrayIndexStart(final Token<?> token) {}

  @Override
  public void onArrayIndexEnd(final Token<?> lookahead) {
    emit(OperatorIR.INDEX);
  }

}
