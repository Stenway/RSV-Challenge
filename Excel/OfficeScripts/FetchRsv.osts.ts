// Example file:
// https://raw.githubusercontent.com/Stenway/RSV-Challenge/main/TestFiles/Valid_001.rsv
//
// See also:
// https://support.microsoft.com/en-gb/office/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3

async function main(workbook: ExcelScript.Workbook, rsvUrl: string) {
	const rsvDocument = await fetchRsv(rsvUrl);
	const worksheet = workbook.getActiveWorksheet();
	setRsv(worksheet, rsvDocument);
}

async function fetchRsv(url: string): Promise<(string | null)[][]> {
	const response = await fetch(url);
	const arrayBuffer = await response.arrayBuffer();
	const bytes = new Uint8Array(arrayBuffer);
	return decodeRsv(bytes);
}

function decodeRsv(bytes: Uint8Array): (string | null)[][] {
	if (bytes.length > 0 && bytes[bytes.length - 1] != 0xFD) { throw new Error("Incomplete RSV document"); }
	const decoder = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });
	const result: (string | null)[][] = [];
	let currentRow: (string | null)[] = [];
	let valueStartIndex = 0;
	for (let i = 0; i < bytes.length; i++) {
		if (bytes[i] == 0xFF) {
			const length = i - valueStartIndex;
			if (length == 0) { currentRow.push(""); }
			else if (length == 1 && bytes[valueStartIndex] == 0xFE) { currentRow.push(null); }
			else {
				const valueBytes = bytes.subarray(valueStartIndex, valueStartIndex + length);
				currentRow.push(decoder.decode(valueBytes));
			}
			valueStartIndex = i + 1;
		} else if (bytes[i] == 0xFD) {
			if (i > 0 && valueStartIndex != i) { throw new Error("Incomplete RSV row"); }
			result.push(currentRow);
			currentRow = [];
			valueStartIndex = i + 1;
		}
	}
	return result;
}

function setRsv(worksheet: ExcelScript.Worksheet, rsvDocument: (string | null)[][]) {
	worksheet.getUsedRange()?.clear();
	for (let rowIndex = 0; rowIndex < rsvDocument.length; rowIndex++) {
		const row = rsvDocument[rowIndex];
		if (row.length >= 16384) { throw new Error("Too many values per row"); }
		for (let columnIndex = 0; columnIndex < row.length; columnIndex++) {
			const value = row[columnIndex];
			const cell = worksheet.getCell(rowIndex, columnIndex);
			if (value === null) {
				cell.setValue("");
				cell.getFormat().getFill().setColor("FFAAAA");
			} else {
				cell.setValue(value);
				cell.getFormat().getFill().setColor("F0F0F0");
			}
		}
	}
}