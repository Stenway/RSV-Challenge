/* (C) Stefan John / Stenway / Stenway.com / 2024 */

package com.stenway.libreoffice.rsvextension;

import com.sun.star.uno.XComponentContext;
import com.sun.star.lang.XMultiComponentFactory;

import com.sun.star.awt.XToolkit;
import com.sun.star.ucb.XSimpleFileAccess;

/*
	||	Interface XComponentContext:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/uno/XComponentContext.html
	||
	||	Interface XMultiComponentFactory:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/lang/XMultiComponentFactory.html
*/

class UServiceManager {
	private XComponentContext componentContext;
	private XMultiComponentFactory serviceManager;
	
	public UServiceManager(XComponentContext componentContext) {
		this.componentContext = componentContext;
		this.serviceManager = componentContext.getServiceManager();
	}
	
	public <T> T createInstance(Class<T> cls, String name) {
		Object obj = null;
		try {
			obj = this.serviceManager.createInstanceWithContext(name, this.componentContext); 
		} catch (Exception e) {
			throw new RuntimeException("Could not create instance of '"+name+"'", e);
		}
		return UObject.asInterface(cls, obj);
	}
	
	public XToolkit createToolkitInstance() { return createInstance(XToolkit.class, "com.sun.star.awt.Toolkit"); }
	public XSimpleFileAccess createSimpleFileAccessInstance() { return createInstance(XSimpleFileAccess.class, "com.sun.star.ucb.SimpleFileAccess"); }
}