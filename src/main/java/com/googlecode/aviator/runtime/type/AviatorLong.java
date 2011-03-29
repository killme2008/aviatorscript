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
package com.googlecode.aviator.runtime.type;

import java.util.Map;

import com.googlecode.aviator.exception.ExpressionRuntimeException;


/**
 * Aviator long type
 * 
 * @author dennis
 * 
 */
public class AviatorLong extends AviatorNumber {

    public AviatorLong(Number number) {
        super(number);

    }


    @Override
    public AviatorObject neg(Map<String, Object> env) {
        return new AviatorLong(-this.number.longValue());
    }


    @Override
    public int innerCompare(AviatorObject other) {
        ensureNumber(other);
        AviatorNumber otherNum = (AviatorNumber) other;
        if (other instanceof AviatorLong) {
            if (this.number.longValue() > otherNum.longValue()) {
                return 1;
            }
            else if (this.number.longValue() < otherNum.longValue()) {
                return -1;
            }
            else {
                return 0;
            }
        }
        else if (other instanceof AviatorDouble) {
            if (this.number.doubleValue() > otherNum.doubleValue()) {
                return 1;
            }
            else if (this.number.doubleValue() < otherNum.doubleValue()) {
                return -1;
            }
            else {
                return 0;
            }
        }
        else {
            throw new ExpressionRuntimeException("Could not compare " + this + " with " + other);
        }
    }


    @Override
    public AviatorObject innerDiv(AviatorObject other) {
        ensureNumber(other);
        AviatorNumber otherNum = (AviatorNumber) other;
        if (other instanceof AviatorLong) {
            return new AviatorLong(this.number.longValue() / otherNum.longValue());
        }
        else {
            return new AviatorDouble(this.number.doubleValue() / otherNum.doubleValue());
        }
    }


    @Override
    public AviatorNumber innerAdd(AviatorNumber other) {
        ensureNumber(other);
        AviatorNumber otherNum = other;
        if (other instanceof AviatorLong) {
            return new AviatorLong(this.number.longValue() + otherNum.longValue());
        }
        else {
            return new AviatorDouble(this.number.doubleValue() + otherNum.doubleValue());
        }
    }


    @Override
    public AviatorObject innerMod(AviatorObject other) {
        ensureNumber(other);
        AviatorNumber otherNum = (AviatorNumber) other;
        if (other instanceof AviatorLong) {
            return new AviatorLong(this.number.longValue() % otherNum.longValue());
        }
        else {
            return new AviatorDouble(this.number.doubleValue() % otherNum.doubleValue());
        }
    }


    @Override
    public AviatorObject innerMult(AviatorObject other) {
        ensureNumber(other);
        AviatorNumber otherNum = (AviatorNumber) other;
        if (other instanceof AviatorLong) {
            return new AviatorLong(this.number.longValue() * otherNum.longValue());
        }
        else {
            return new AviatorDouble(this.number.doubleValue() * otherNum.doubleValue());
        }
    }


    @Override
    public AviatorObject innerSub(AviatorObject other) {
        ensureNumber(other);
        AviatorNumber otherNum = (AviatorNumber) other;
        if (other instanceof AviatorLong) {
            return new AviatorLong(this.number.longValue() - otherNum.longValue());
        }
        else {
            return new AviatorDouble(this.number.doubleValue() - otherNum.doubleValue());
        }
    }

}
