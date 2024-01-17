/* (C) Stefan John / Stenway / Stenway.com / 2024 */

package com.stenway.libreoffice.rsvextension;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

abstract class RsvEncoder {
	static byte[] encode(String[][] rows) {
		ArrayList<byte[]> parts = new ArrayList<byte[]>();
		byte[] valueTerminatorByte = new byte[]{(byte)0xFF};
		byte[] nullValueByte = new byte[]{(byte)0xFE};
		byte[] rowTerminatorByte = new byte[]{(byte)0xFD};
		CharsetEncoder encoder = StandardCharsets.UTF_8.newEncoder();
		int resultLength = 0;
		for (String[] row : rows) {
			for (String value : row) {
				if (value == null) { parts.add(nullValueByte); resultLength++; }
				else if (value.length() > 0) {
					byte[] valueBytes;
					try {
						ByteBuffer byteBuffer = encoder.encode(CharBuffer.wrap(value));
						valueBytes = new byte[byteBuffer.limit()];
						byteBuffer.get(valueBytes);
					} catch (Exception e) { throw new RuntimeException("Invalid string value", e); }
					parts.add(valueBytes); resultLength += valueBytes.length;
				}
				parts.add(valueTerminatorByte); resultLength++;
			}
			parts.add(rowTerminatorByte); resultLength++;
		}
		byte[] result = new byte[resultLength];		
		ByteBuffer resultBuffer = ByteBuffer.wrap(result);
		for (byte[] part : parts) { resultBuffer.put(part); }
		return result;
	}
}