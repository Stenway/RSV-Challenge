/* (C) Stefan John / Stenway / Stenway.com / 2023 */

export function encodeXRsv(rows: (string | null)[][]): Uint8Array {
	const parts: Uint8Array[] = [];
	const valueTerminatorByte = new Uint8Array([0xFF]);
	const nullValueByte = new Uint8Array([0xFE]);
	const rowTerminatorByte = new Uint8Array([0xFD]);
	const encoder = new TextEncoder();
	for (const row of rows) {
		parts.push(rowTerminatorByte);
		for (const value of row) {
			parts.push(valueTerminatorByte);
			if (value === null) { parts.push(nullValueByte); }
			else if (value.length > 0) {
				if (!/\p{Surrogate}/u.test(value) === false) { throw new Error(`Invalid string value`); }
				parts.push(encoder.encode(value));
			}
		}
	}
	const result = new Uint8Array(parts.reduce((result, bytes) => result + bytes.length, 0));
	let offset = 0;
	for (const bytes of parts) {
		result.set(bytes, offset);
		offset += bytes.length;
	}
	return result;
}

export function decodeXRsv(bytes: Uint8Array): (string | null)[][] {
	if (bytes.length == 0) { return []; }
	if (bytes[0] != 0xFD) { throw new Error("Invalid XRSV document"); }
	const decoder = new TextDecoder("utf-8", {fatal: true, ignoreBOM: true});
	const result: (string | null)[][] = [];
	let currentRow: (string | null)[] = [];
	let valueStartIndex = -1;
	let i = -1;
	while (true) {
		i++;
		let valueWasAdded = false;
		if (valueStartIndex != -1 && (i >= bytes.length || bytes[i] == 0xFD || bytes[i] == 0xFF)) {
			const length = i-valueStartIndex;
			if (length == 0) { currentRow.push(""); }
			else if (length == 1 && bytes[valueStartIndex] == 0xFE) { currentRow.push(null); }
			else {
				const valueBytes = bytes.subarray(valueStartIndex, valueStartIndex + length);
				currentRow.push(decoder.decode(valueBytes));
			}
			valueWasAdded = true;
			valueStartIndex = -1;
		}
		if (i >= bytes.length) {
			break;
		} else if (bytes[i] == 0xFD) {
			currentRow = [];
			result.push(currentRow);
			valueStartIndex = -1;
		} else if (bytes[i] == 0xFF) {
			valueStartIndex = i+1;
		} else if (valueStartIndex == -1 && valueWasAdded === false) {
			throw new Error("Invalid XRSV document"); 
		}
	}
	return result;
}