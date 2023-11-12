/* (C) Stefan John / Stenway / Stenway.com / 2023 */

export function encodeRsv(rows: (string | null)[][]): Uint8Array {
	const parts: Uint8Array[] = [];
	const valueTerminatorByte = new Uint8Array([0xFF]);
	const nullValueByte = new Uint8Array([0xFE]);
	const rowTerminatorByte = new Uint8Array([0xFD]);
	const encoder = new TextEncoder();
	for (const row of rows) {
		for (const value of row) {
			if (value === null) { parts.push(nullValueByte); }
			else if (value.length > 0) {
				if (!/\p{Surrogate}/u.test(value) === false) { throw new Error(`Invalid string value`); }
				parts.push(encoder.encode(value));
			}
			parts.push(valueTerminatorByte);
		}
		parts.push(rowTerminatorByte);
	}
	const result = new Uint8Array(parts.reduce((result, bytes) => result + bytes.length, 0));
	let offset = 0;
	for (const bytes of parts) {
		result.set(bytes, offset);
		offset += bytes.length;
	}
	return result;
}

export function decodeRsv(bytes: Uint8Array): (string | null)[][] {
	const decoder = new TextDecoder("utf-8", {fatal: true, ignoreBOM: true});
	const result: (string | null)[][] = [];
	let currentRow: (string | null)[] = [];
	if (bytes.length > 0 && bytes[bytes.length-1] != 0xFD) { throw new Error("Incomplete RSV document"); }
	let valueStartIndex = 0;
	for (let i=0; i<bytes.length; i++) {
		if (bytes[i] == 0xFF) {
			const length = i-valueStartIndex;
			if (length == 0) { currentRow.push(""); }
			else if (length == 1 && bytes[valueStartIndex] == 0xFE) { currentRow.push(null); }
			else {
				const valueBytes = bytes.subarray(valueStartIndex, valueStartIndex + length);
				currentRow.push(decoder.decode(valueBytes));
			}
			valueStartIndex = i+1;
		} else if (bytes[i] == 0xFD) {
			if (i > 0 && valueStartIndex != i) { throw new Error("Incomplete RSV row"); }
			result.push(currentRow);
			currentRow = [];
			valueStartIndex = i+1;
		}
	}
	return result;
}