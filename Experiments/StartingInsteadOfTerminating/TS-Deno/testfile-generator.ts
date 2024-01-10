import { xrsvToJson } from "./xrsv-conv.ts"
import { loadXRsvSync, saveXRsvSync } from "./xrsv-io.ts"
import { getValidTestCases, getInvalidTestCases } from "./xrsv-testcases.ts"
import { encodeXRsv } from "./xrsv.ts"

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
	console.log(path)
	const bytesA = encodeXRsv(rows)
	//if (isValidRsv(bytesA) === false) { throw new Error("Validation mismatch") }
	saveXRsvSync(rows, path+".xrsv")
	
	const customJsonStr = xrsvToJson(rows)
	Deno.writeTextFileSync(path+".json", customJsonStr)
	const readJson = Deno.readTextFileSync(path+".json")
	const parsedJaggedArray = JSON.parse(readJson)
	const bytesB = encodeXRsv(parsedJaggedArray)
	if (areEqual(bytesA, bytesB) === false) { throw new Error("Mismatch") }

	const loadedJaggedArray = loadXRsvSync(path+".xrsv")
	const bytesC = encodeXRsv(loadedJaggedArray)
	if (areEqual(bytesA, bytesC) === false) { throw new Error("Mismatch") }
	
	//Deno.writeTextFileSync(path+".xml", rsvToXml(rows))
	//Deno.writeTextFileSync(path+".sml", "\uFEFF"+rsvToSml(rows))
}

function saveInvalidTestCase(i: number, byteArray: number[]) {
	const filePath = getDirectoryPath() + "/Invalid_"+i.toString().padStart(3, "0")+".xrsv"
	console.log(filePath)
	const bytes = new Uint8Array(byteArray)
	Deno.writeFileSync(filePath, bytes)
	
	let wasError = false
	try {
		loadXRsvSync(filePath)
	} catch(_e) {
		wasError = true
	}
	if (wasError === false) { throw new Error("Test case is not invalid") }
	
	/*if (isValidRsv(bytes) === true) { throw new Error("Validation mismatch") }*/
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