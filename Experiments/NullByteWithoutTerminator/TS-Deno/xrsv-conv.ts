/* (C) Stefan John / Stenway / Stenway.com / 2023 */

export function xrsvToJson(rows: (string | null)[][]): string {
	return "["+(rows.length > 0 ? "\n" : "") +
		rows.map(row => {
			return "  ["+row.map((value) => JSON.stringify(value)).join(", ")+"]"
		}).join(",\n")
		+ "\n]";
}