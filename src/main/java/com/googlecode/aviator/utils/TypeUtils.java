/**
 *  Copyright (C) [2010-2012] dennis zhuang (killme2008@gmail.com)
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
package com.googlecode.aviator.utils;

import java.math.BigDecimal;
import java.math.BigInteger;


/**
 * Java type to aviator type utilities
 * 
 * @author boyan
 * 
 */
public class TypeUtils {

    public static final boolean isBigInt(Object value) {
        return value instanceof BigInteger;
    }


    public static final boolean isDecimal(Object value) {
        return value instanceof BigDecimal;
    }

    public static final boolean isLong(Object value) {
        return value instanceof Integer || value instanceof Long || value instanceof Byte || value instanceof Short;
    }


    public static final boolean isDouble(Object value) {
        return value instanceof Float || value instanceof Double;

    }


    public static final boolean isString(Object value) {
        return value instanceof String || value instanceof Character;
    }

}
