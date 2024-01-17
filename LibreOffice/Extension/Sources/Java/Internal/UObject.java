/* (C) Stefan John / Stenway / Stenway.com / 2024 */

package com.stenway.libreoffice.rsvextension;

import com.sun.star.uno.UnoRuntime;

/*
	||	Class UnoRuntime:
	||	->	https://www.openoffice.org/api/docs/java/ref/com/sun/star/uno/UnoRuntime.html
*/

abstract class UObject {
	public static <T> T asInterface(Class<T> cls, Object obj) {
		if (obj == null) { throw new RuntimeException("Could not get interface for '"+cls+"' because object is null");}
		T result = UnoRuntime.queryInterface(cls, obj);
		if (result == null) { throw new RuntimeException("Could not get interface for '"+cls+"'"); }
		return result;
	}
}