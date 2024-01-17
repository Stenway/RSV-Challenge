/* (C) Stefan John / Stenway / Stenway.com / 2024 */

package com.stenway.libreoffice.rsvextension;

import com.sun.star.awt.MessageBoxButtons;
import com.sun.star.awt.MessageBoxType;
import com.sun.star.awt.XMessageBox;
import com.sun.star.awt.XMessageBoxFactory;
import com.sun.star.awt.XToolkit;

import java.io.PrintWriter;
import java.io.StringWriter;

/*
	||	Interface XToolkit:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/awt/XToolkit.html
	||
	||	Interface XMessageBoxFactory:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/awt/XMessageBoxFactory.html
	||
	||	Interface XMessageBox:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/awt/XMessageBox.html
	||
	||	Enum MessageBoxType:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/awt/MessageBoxType.html
	||
	||	Constants Group MessageBoxButtons:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/awt/MessageBoxButtons.html
*/

abstract class UMessageBox {
	public static short show(UServiceManager serviceManager, MessageBoxType type, int buttons, String title, String message) {
		XToolkit toolkit = serviceManager.createToolkitInstance();
		XMessageBoxFactory messageBoxFactory = UObject.asInterface(XMessageBoxFactory.class, toolkit);
		XMessageBox messageBox = messageBoxFactory.createMessageBox(null, type, buttons, title, message);
		return messageBox.execute();
	}
	
	public static void showError(UServiceManager serviceManager, String title, String message) {
		show(serviceManager, MessageBoxType.ERRORBOX, MessageBoxButtons.BUTTONS_OK, title, message);
	}
	
	public static void showWarning(UServiceManager serviceManager, String title, String message) {
		show(serviceManager, MessageBoxType.WARNINGBOX, MessageBoxButtons.BUTTONS_OK, title, message);
	}
	
	public static String getExceptionText(Exception e) {
		StringWriter sw = new StringWriter();
		e.printStackTrace(new PrintWriter(sw));
		return sw.toString();
	}
}