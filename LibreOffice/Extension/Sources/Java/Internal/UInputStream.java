/* (C) Stefan John / Stenway / Stenway.com / 2024 */

package com.stenway.libreoffice.rsvextension;

import com.sun.star.beans.PropertyValue;
import com.sun.star.io.XInputStream;
import com.sun.star.ucb.XSimpleFileAccess;
import java.io.ByteArrayOutputStream;

/*
	||	Interface XInputStream:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/io/XInputStream.html
	||
	||	Interface XSimpleFileAccess:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/ucb/XSimpleFileAccess.html
*/

class UInputStream {
	public XInputStream inputStream;
	public boolean mustBeClosed;
	
	public UInputStream(UServiceManager serviceManager, PropertyValue[] propertyValues) {
		String fileName = UPropertyValues.getStringOrNull(propertyValues, "FileName");
		inputStream = UPropertyValues.getObjectOrNull(propertyValues, "InputStream", XInputStream.class);
		if (inputStream == null) {
			if (fileName == null) { throw new RuntimeException("Could not initialize input stream"); }
			XSimpleFileAccess simpleFileAccess = serviceManager.createSimpleFileAccessInstance();
			try {
				inputStream = simpleFileAccess.openFileRead(fileName);
			} catch (Exception e) {
				throw new RuntimeException("File could not be opened for reading", e);
			}
			mustBeClosed = true;
		}
	}
	
	public byte[] readCompletely() {
		byte[] byteArray;
		try {
			ByteArrayOutputStream buffer = new ByteArrayOutputStream();
			
			int maxChunkSize = 8192;
			byte[][] chunkBuffer = new byte[1][];
			while (true) {
				int numReadBytes = inputStream.readBytes(chunkBuffer, maxChunkSize);
				if (numReadBytes == 0) { break; }
				buffer.write(chunkBuffer[0], 0, numReadBytes);
				if (numReadBytes < maxChunkSize) { break; }
			}
			
			byteArray = buffer.toByteArray();
		} catch (Exception e) {
			throw new RuntimeException("Reading input stream failed", e);
		}
		return byteArray;
	}
	
	public void close() {
		if (inputStream == null) { return; }
		if (mustBeClosed) {
			try {
				inputStream.closeInput();
			} catch (Exception e) {
				throw new RuntimeException("Closing input stream failed", e);
			}
		}
		mustBeClosed = false;
		inputStream = null;
	}
	
	public static byte[] readAllBytes(UServiceManager serviceManager, PropertyValue[] propertyValues) {
		UInputStream inputStream = new UInputStream(serviceManager, propertyValues);
		try {
			byte[] bytes = inputStream.readCompletely();
			return bytes;
		} finally {
			inputStream.close();
		}
	}
}