/* (C) Stefan John / Stenway / Stenway.com / 2023 */

export function rsvToJson(rows: (string | null)[][]): string {
	return "["+(rows.length > 0 ? "\n" : "") +
		rows.map(row => {
			return "  ["+row.map((value) => JSON.stringify(value)).join(", ")+"]"
		}).join(",\n")
		+ "\n]";
}

// ----------------------------------------------------------------------

function stringToXmlElement(str: string): string {
	let needsBase64Encoding = false
	for (let i=0; i<str.length; i++) {
		const c = str.charCodeAt(i)
		if (c >= 0 && c <= 0x1F && c !== 0x09 && c !== 0x0A && c !== 0x0D)  {
			needsBase64Encoding = true
			break
		}
	}
	if (needsBase64Encoding === true) {
		const encoder = new TextEncoder()
		if (!/\p{Surrogate}/u.test(str) === false) { throw new Error(`Invalid string value`); }
		const bytes = encoder.encode(str)
		const binaryString = String.fromCodePoint(...bytes)
  		return "<B>"+btoa(binaryString)+"</B>"
	} else {
		return "<V>"+str.replace("&", "&amp;").replace("<", "&lt;")+"</V>"
	}
}

export function rsvToXml(rows: (string | null)[][]): string {
	return `<?xml version="1.0"?>\n<Rows>`+(rows.length > 0 ? "\n" : "") +
		rows.map(row => {
			return "  <R>"+row.map(
				(value) => {
					if (value === null) { return "<N/>" }
					return stringToXmlElement(value)
				}
			).join("")+"</R>"
		}).join("\n")
		+ "\n</Rows>";
}

// ----------------------------------------------------------------------

function containsSpecialWsvChar(value: string): boolean {
	for (let i=0; i<value.length; i++) {
		const c: number = value.charCodeAt(i)
		switch (c) {
		case 0x0022:
		case 0x0023:
		case 0x000A:
		case 0x0009: case 0x000B: case 0x000C: case 0x000D: case 0x0020: case 0x0085: case 0x00A0: case 0x1680: case 0x2000: case 0x2001: case 0x2002: case 0x2003: case 0x2004: case 0x2005: case 0x2006: case 0x2007: case 0x2008: case 0x2009: case 0x200A: case 0x2028: case 0x2029: case 0x202F: case 0x205F: case 0x3000:
			return true
		}
		if (c >= 0xD800 && c <= 0xDFFF) {
			i++
			if (c >= 0xDC00 || i >= value.length) { throw new Error("Invalid UTF-16 string") }
			const secondCodeUnit: number = value.charCodeAt(i)
			if (!(secondCodeUnit >= 0xDC00 && secondCodeUnit <= 0xDFFF)) { throw new Error("Invalid UTF-16 string") }
		}
	}
	return false
}

function serializeWsvValue(value: string | null): string {
	if (value === null) {
		return "-"
	} else if (value.length === 0) {
		return "\"\""
	} else if (value === "-") {
		return "\"-\""
	} else if (containsSpecialWsvChar(value)) {
		let result = "\""
		for (let i=0; i<value.length; i++) {
			const codeUnit: number = value.charCodeAt(i)
			switch (codeUnit) {
			case 0x000A:
				result += "\"/\""
				break
			case 0x0022:
				result += "\"\""
				break
			default:
				result += value.charAt(i)
			}
		}
		result += "\""
		return result
	} else {
		return value
	}
}

export function rsvToSml(rows: (string | null)[][]): string {
	return "Rows"+(rows.length > 0 ? "\n" : "") +
		rows.map(row => {
			if (row.length == 0) { return "  Empty Row" }
			return "  Row "+row.map((value) => serializeWsvValue(value)).join(" ")
		}).join("\n")
		+ "\nEnd";
}