/* (C) Stefan John / Stenway / Stenway.com / 2024 */

package com.stenway.libreoffice.rsvextension;

import com.sun.star.beans.PropertyValue;
import com.sun.star.io.XOutputStream;
import com.sun.star.ucb.XSimpleFileAccess;
import java.io.ByteArrayOutputStream;

/*
	||	Interface XOutputStream:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/io/XOutputStream.html
	||
	||	Interface XSimpleFileAccess:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/ucb/XSimpleFileAccess.html
*/

class UOutputStream {
	public XOutputStream outputStream;
	public boolean mustBeClosed;
	
	public UOutputStream(UServiceManager serviceManager, PropertyValue[] propertyValues) {
		String fileName = UPropertyValues.getStringOrNull(propertyValues, "FileName");
		outputStream = UPropertyValues.getObjectOrNull(propertyValues, "OutputStream", XOutputStream.class);
		if (outputStream == null) {
			if (fileName == null) { throw new RuntimeException("Could not initialize input stream"); }
			XSimpleFileAccess simpleFileAccess = serviceManager.createSimpleFileAccessInstance();
			try {
				outputStream = simpleFileAccess.openFileWrite(fileName);
			} catch (Exception e) {
				throw new RuntimeException("File could not be opened for writing", e);
			}
			mustBeClosed = true;
		}
	}
	
	public void writeBytes(byte[] bytes) {
		try {
			outputStream.writeBytes(bytes);
		} catch (Exception e) {
			throw new RuntimeException("Writing output stream failed", e);
		}
	}
	
	public void close() {
		if (outputStream == null) { return; }
		if (mustBeClosed) {
			try {
				outputStream.closeOutput();
			} catch (Exception e) {
				throw new RuntimeException("Closing output stream failed", e);
			}
		}
		mustBeClosed = false;
		outputStream = null;
	}
	
	public static void writeAllBytes(UServiceManager serviceManager, PropertyValue[] propertyValues, byte[] bytes) {
		UOutputStream outputStream = new UOutputStream(serviceManager, propertyValues);
		try {
			outputStream.writeBytes(bytes);
		} finally {
			outputStream.close();
		}
	}
}