/* (C) Stefan John / Stenway / Stenway.com / 2023 */

import { xrsvToJson } from "./xrsv-conv.ts"
import { loadXRsvSync, saveXRsvSync } from "./xrsv-io.ts"

const rows = [
	["Hello", "🌎", null, ""],
	["A\0B\nC", "Test 𝄞"],
	[],
	[""]
]

saveXRsvSync(rows, "Test.xrsv")
console.log("XRSV")
console.log("---")
console.log(xrsvToJson(rows))

const loadedRows = loadXRsvSync("Test.xrsv")
console.log("\nLoaded XRSV")
console.log("----------")
console.log(xrsvToJson(loadedRows))

saveXRsvSync(loadedRows, "TestResaved.xrsv")
/*
console.log("\nXML")
console.log("---")
console.log(rsvToXml(rows))

console.log("\nSML")
console.log("---")
console.log(rsvToSml(rows))
console.log("# Warning: with Chrome console.log replaces the NUL character with a space")
*/
//appendRsvSync([["ABC"]], "Append.rsv", false)

console.log("Done")