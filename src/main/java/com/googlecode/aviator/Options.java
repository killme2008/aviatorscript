package com.googlecode.aviator;

/**
 * Aviator Evaluator Configuration options.
 * 
 * @author dennis
 *
 */
public enum Options {
	/**
	 * Always use double as BigDecimal.
	 * 
	 * @since 2.3.4
	 */
	ALWAYS_USE_DOUBLE_AS_DECIMAL;

	/**
	 * Returns the default value of option.
	 * 
	 * @return
	 */
	public Object getDefaultValue() {
		switch (this) {
		case ALWAYS_USE_DOUBLE_AS_DECIMAL:
			return false;
		}
		return null;
	}
}
