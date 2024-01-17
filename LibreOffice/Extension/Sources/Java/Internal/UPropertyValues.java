/* (C) Stefan John / Stenway / Stenway.com / 2024 */

package com.stenway.libreoffice.rsvextension;

import com.sun.star.beans.PropertyValue;
import com.sun.star.uno.AnyConverter;
import com.sun.star.uno.Type;

/*
	||	Struct PropertyValue:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/beans/PropertyValue.html
*/

abstract class UPropertyValues {
	public static PropertyValue getPropertyValueOrNull(PropertyValue[] propertyValues, String name) {
		for (PropertyValue propertyValue : propertyValues) {
			if (propertyValue.Name.equals(name)) {
				return propertyValue;
			}
		}
		return null;
	}
	
	public static String getStringOrNull(PropertyValue[] propertyValues, String name) {
		PropertyValue propertyValue = getPropertyValueOrNull(propertyValues, name);
		if (propertyValue == null) { return null; }
		return AnyConverter.toString(propertyValue.Value);
	}
	
	@SuppressWarnings("unchecked")
	public static <T> T getObjectOrNull(PropertyValue[] propertyValues, String name, Class<T> cls) {
		PropertyValue propertyValue = getPropertyValueOrNull(propertyValues, name);
		if (propertyValue == null) { return null; }
		T result = (T)AnyConverter.toObject(cls, propertyValue.Value);
		return result;
	}
}