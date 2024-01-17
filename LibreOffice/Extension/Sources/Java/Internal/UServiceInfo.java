/* (C) Stefan John / Stenway / Stenway.com / 2024 */

package com.stenway.libreoffice.rsvextension;

import com.sun.star.lang.XComponent;
import com.sun.star.lang.XServiceInfo;
import com.sun.star.uno.UnoRuntime;

/*
	||	Interface XServiceInfo:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/lang/XServiceInfo.html
*/

abstract class UServiceInfo {
	public static boolean supportsService(XComponent component, String serviceName) {
		if (component == null) { return false; }
		XServiceInfo serviceInfo = UnoRuntime.queryInterface(XServiceInfo.class, component);
		return (serviceInfo != null && serviceInfo.supportsService(serviceName));
	}
}