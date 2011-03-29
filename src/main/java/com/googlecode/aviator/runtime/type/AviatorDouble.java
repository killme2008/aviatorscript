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


/**
 * Aviator double type
 * 
 * @author dennis
 * 
 */
public class AviatorDouble extends AviatorNumber {

    public AviatorDouble(Number number) {
        super(number);
    }


    @Override
    public int innerCompare(AviatorObject other) {
        ensureNumber(other);
        AviatorNumber otherNum = (AviatorNumber) other;
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


    @Override
    public AviatorObject neg(Map<String, Object> env) {
        return new AviatorDouble(-this.number.doubleValue());
    }


    @Override
    public AviatorObject innerDiv(AviatorObject other) {
        ensureNumber(other);
        AviatorNumber otherNum = (AviatorNumber) other;
        return new AviatorDouble(this.number.doubleValue() / otherNum.doubleValue());
    }


    @Override
    public AviatorNumber innerAdd(AviatorNumber other) {
        ensureNumber(other);
        AviatorNumber otherNum = other;
        return new AviatorDouble(this.number.doubleValue() + otherNum.doubleValue());
    }


    @Override
    public AviatorObject innerMod(AviatorObject other) {
        ensureNumber(other);
        AviatorNumber otherNum = (AviatorNumber) other;
        return new AviatorDouble(this.number.doubleValue() % otherNum.doubleValue());
    }


    @Override
    public AviatorObject innerMult(AviatorObject other) {
        ensureNumber(other);
        AviatorNumber otherNum = (AviatorNumber) other;
        return new AviatorDouble(this.number.doubleValue() * otherNum.doubleValue());
    }


    @Override
    public AviatorObject innerSub(AviatorObject other) {
        ensureNumber(other);
        AviatorNumber otherNum = (AviatorNumber) other;
        return new AviatorDouble(this.number.doubleValue() - otherNum.doubleValue());
    }
}
