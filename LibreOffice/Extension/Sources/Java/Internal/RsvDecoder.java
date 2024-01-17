/* (C) Stefan John / Stenway / Stenway.com / 2024 */

package com.stenway.libreoffice.rsvextension;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

abstract class RsvDecoder {
	static String[][] decode(byte[] bytes) {
		if (bytes.length > 0 && (bytes[bytes.length-1] & 0xFF) != 0xFD) { throw new RuntimeException("Last byte is not a row terminator byte."); }
		CharsetDecoder decoder = StandardCharsets.UTF_8.newDecoder();
		decoder.onMalformedInput(CodingErrorAction.REPORT);
		decoder.onUnmappableCharacter(CodingErrorAction.REPORT);
		ByteBuffer byteBuffer = ByteBuffer.wrap(bytes);
		ArrayList<String[]> result = new ArrayList<String[]>();
		ArrayList<String> currentRow = new ArrayList<String>();
		int valueStartIndex = 0;
		for (int i=0; i<bytes.length; i++) {
			int currentByte = bytes[i] & 0xFF;
			if (currentByte == 0xFF) {
				int length = i-valueStartIndex;
				if (length == 0) { currentRow.add(""); }
				else if (length == 1 && (bytes[valueStartIndex] & 0xFF) == 0xFE) { currentRow.add(null); }
				else {
					ByteBuffer valueBytes = ((ByteBuffer)(byteBuffer.duplicate().position(valueStartIndex).limit(i))).slice();
					try { currentRow.add(decoder.decode(valueBytes).toString()); }
					catch (Exception e) { throw new RuntimeException("Invalid string value starting at byte index "+valueStartIndex+".", e); }
				}
				valueStartIndex = i+1;
			} else if (currentByte == 0xFD) {
				if (i > 0 && valueStartIndex != i) { throw new RuntimeException("Value starting at byte index "+valueStartIndex+" was not terminated with a value terminator byte."); }
				result.add(currentRow.toArray(new String[0]));
				currentRow.clear();
				valueStartIndex = i+1;
			}
		}
		return result.toArray(new String[0][]);
	}
}