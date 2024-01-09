/* (C) Stefan John / Stenway / Stenway.com / 2023 */

import { rsvToJson } from "./rsv-conv.ts"
import { appendRsvSync, loadRsvSync, saveRsvSync } from "./rsv-io.ts"

const rows = [
	["Hello", "🌎", null, ""],
	["A\0B\nC", "Test 𝄞"],
	[],
	[""]
]

saveRsvSync(rows, "Test.rsv")
console.log("RSV")
console.log("---")
console.log(rsvToJson(rows))

const loadedRows = loadRsvSync("Test.rsv")
console.log("\nLoaded RSV")
console.log("----------")
console.log(rsvToJson(loadedRows))

saveRsvSync(loadedRows, "TestResaved.rsv")
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