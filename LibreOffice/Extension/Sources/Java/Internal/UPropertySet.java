/* (C) Stefan John / Stenway / Stenway.com / 2024 */

package com.stenway.libreoffice.rsvextension;

import com.sun.star.beans.PropertyValue;
import com.sun.star.beans.XPropertySet;

/*
	||	Interface XPropertySet:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/beans/XPropertySet.html
*/

abstract class UPropertySet {
	public static void setProperty(Object obj, String propertyName, Object value) {
		XPropertySet propertySet = UObject.asInterface(XPropertySet.class, obj);
		setProperty(propertySet, propertyName, value);
	}
	
	public static void setProperty(XPropertySet propertySet, String propertyName, Object value) {
		try {
			propertySet.setPropertyValue(propertyName, value);
		} catch(Exception e) {
			throw new RuntimeException("Could not set property '"+propertyName+"'", e);
		}
	}
}