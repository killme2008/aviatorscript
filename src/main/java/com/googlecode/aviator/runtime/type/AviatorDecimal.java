package com.googlecode.aviator.runtime.type;

import java.math.BigDecimal;


/**
 * Aviator Big Decimal
 * 
 * @since 2.3.0
 * @author dennis<killme2008@gmail.com>
 * 
 */
public class AviatorDecimal extends AviatorNumber {

    public AviatorDecimal(Number number) {
        super(number);
    }


    public static final AviatorDecimal valueOf(BigDecimal d) {
        return new AviatorDecimal(d);
    }


    @Override
    public AviatorObject innerSub(AviatorNumber other) {
        return AviatorDecimal.valueOf(this.toDecimal().subtract(other.toDecimal()));
    }


    @Override
    public AviatorObject innerMult(AviatorNumber other) {
        return AviatorDecimal.valueOf(this.toDecimal().multiply(other.toDecimal()));
    }


    @Override
    public AviatorObject innerMod(AviatorNumber other) {
        return AviatorDecimal.valueOf(this.toDecimal().remainder(other.toDecimal()));
    }


    @Override
    public AviatorObject innerDiv(AviatorNumber other) {
        return AviatorDecimal.valueOf(this.toDecimal().divide(other.toDecimal()));
    }


    @Override
    public AviatorNumber innerAdd(AviatorNumber other) {
        return AviatorDecimal.valueOf(this.toDecimal().add(other.toDecimal()));
    }


    @Override
    public int innerCompare(AviatorNumber other) {
        return this.toDecimal().compareTo(other.toDecimal());
    }


    @Override
    public AviatorType getAviatorType() {
        return AviatorType.Decimal;
    }

}
