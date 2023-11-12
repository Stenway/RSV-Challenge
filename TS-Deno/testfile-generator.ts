import { rsvToJson, rsvToSml, rsvToXml } from "./rsv-conv.ts";
import { loadRsvSync, saveRsvSync } from "./rsv-io.ts"
import { getValidTestCases, getInvalidTestCases } from "./rsv-testcases.ts";
import { encodeRsv } from "./rsv.ts"

function getDirectoryPath(): string {
	return "../TestFiles"
}

function areEqual(bytesA: Uint8Array, bytesB: Uint8Array): boolean {
	if (bytesA.length !== bytesB.length) { return false }
	for (let i=0; i<bytesA.length; i++) {
		if (bytesA[i] !== bytesB[i]) { return false }
	}
	return true
}

function saveValidTestCase(i: number, rows: (string | null)[][]) {
	const path = getDirectoryPath() + "/Valid_"+i.toString().padStart(3, "0")
	const bytesA = encodeRsv(rows)
	saveRsvSync(rows, path+".rsv")
	
	const customJsonStr = rsvToJson(rows)
	Deno.writeTextFileSync(path+".json", customJsonStr)
	const readJson = Deno.readTextFileSync(path+".json")
	const parsedJaggedArray = JSON.parse(readJson)
	const bytesB = encodeRsv(parsedJaggedArray)
	if (areEqual(bytesA, bytesB) === false) { throw new Error("Mismatch") }

	const loadedJaggedArray = loadRsvSync(path+".rsv")
	const bytesC = encodeRsv(loadedJaggedArray)
	if (areEqual(bytesA, bytesC) === false) { throw new Error("Mismatch") }
	
	Deno.writeTextFileSync(path+".xml", rsvToXml(rows))
	Deno.writeTextFileSync(path+".sml", "\uFEFF"+rsvToSml(rows))
}

function saveInvalidTestCase(i: number, bytes: number[]) {
	const filePath = getDirectoryPath() + "/Invalid_"+i.toString().padStart(3, "0")+".rsv"
	Deno.writeFileSync(filePath, new Uint8Array(bytes))
	
	try {
		loadRsvSync(filePath)
	} catch(_e) {
		return
	}
	throw new Error("Test case is not invalid")
}

function createTestFiles() {
	try { Deno.mkdirSync(getDirectoryPath()) } catch(_e) {
		//
	}
	
	const validTestCases = getValidTestCases()
	for (let i=0; i<validTestCases.length; i++) {
		saveValidTestCase(i+1, validTestCases[i])
	}
	
	const invalidTestCases = getInvalidTestCases()
	for (let i=0; i<invalidTestCases.length; i++) {
		saveInvalidTestCase(i+1, invalidTestCases[i])
	}
}

createTestFiles()

console.log("TestFiles Generated")