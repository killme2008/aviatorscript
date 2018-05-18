package com.googlecode.aviator.lexer.token;

import java.util.Map;


/**
 * Delegate token,wrap a token with special syntax structure
 *
 * @author dennis
 *
 */
public class DelegateToken extends AbstractToken<Token<?>> {

  private final Token<?> token;
  private final DelegateTokenType delegateTokenType;

  public static enum DelegateTokenType {
    And_Left, Join_Left, Ternary_Boolean, Ternary_Left, Array, Index_Start, //
    Method_Name, Method_Param, Lambda_Define, Lambda_Argument, Lambda_Body_Start, Lambda_Body_End, //
    Ternay_End
  }


  public Token<?> getToken() {
    return token;
  }


  public DelegateTokenType getDelegateTokenType() {
    return delegateTokenType;
  }


  public DelegateToken(int startIndex, Token<?> token, DelegateTokenType type) {
    super(startIndex, token != null ? token.getLexeme() : "");
    this.token = token;
    delegateTokenType = type;
  }


  @Override
  public com.googlecode.aviator.lexer.token.Token.TokenType getType() {
    return TokenType.Delegate;
  }


  @Override
  public Token<?> getValue(Map<String, Object> env) {
    return this.token;
  }

}
