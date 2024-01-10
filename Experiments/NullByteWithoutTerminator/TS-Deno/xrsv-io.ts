/* (C) Stefan John / Stenway / Stenway.com / 2023 */
// deno-lint-ignore-file no-inferrable-types
/* eslint-disable @typescript-eslint/no-explicit-any */

import * as fs from 'node:fs'
import * as bf from 'node:buffer'
import { decodeXRsv, encodeXRsv } from './xrsv.ts'

export function saveXRsvSync(rows: (string | null)[][], filePath: string) {
	fs.writeFileSync(filePath, encodeXRsv(rows));
}

export async function saveXRsv(rows: (string | null)[][], filePath: string) {
	await fs.promises.writeFile(filePath, encodeXRsv(rows));
}

export function loadXRsvSync(filePath: string): (string | null)[][] {
	return decodeXRsv(fs.readFileSync(filePath));
}

export async function loadXRsv(filePath: string): Promise<(string | null)[][]> {
	return decodeXRsv(await fs.promises.readFile(filePath));
}

export function appendXRsvSync(rows: (string | null)[][], filePath: string, continueLastRow: boolean = false) {
	let handle: number;
	let existed = false;
	try {
		handle = fs.openSync(filePath, "r+");
		existed = true;
	} catch (error) {
		// deno-lint-ignore no-explicit-any
		if ((error as any).code === "ENOENT") {
			handle = fs.openSync(filePath, "wx");
		}
		else { throw error; }
	}
	try {
		const size = fs.fstatSync(handle).size;
		let position: number;
		if (existed === true && continueLastRow === true && size > 0) {
			position = size - 1;
			const buffer = bf.Buffer.alloc(1)
			if (fs.readSync(handle, buffer, 0, 1, position) !== 1) { throw new Error(`Reading last byte failed`); }
			if (buffer[0] !== 0xFD) { throw new Error("Incomplete XRSV document"); }
		} else {
			position = size;
		}
		const bytes = encodeXRsv(rows);
		if (fs.writeSync(handle, bytes, 0, bytes.length, position) !== bytes.length) { throw new Error(`Data not fully written`); }
	} finally {
		fs.closeSync(handle);
	}
}

export async function appendXRsv(rows: (string | null)[][], filePath: string, continueLastRow: boolean = false) {
	let handle: fs.promises.FileHandle;
	let existed = false;
	try {
		handle = await fs.promises.open(filePath, "r+");
		existed = true;
	} catch (error) {
		// deno-lint-ignore no-explicit-any
		if ((error as any).code === "ENOENT") {
			handle = await fs.promises.open(filePath, "wx");
		}
		else { throw error; }
	}
	try {
		const size = (await handle.stat()).size;
		let position: number;
		if (existed === true && continueLastRow === true && size > 0) {
			position = size - 1;
			const buffer = bf.Buffer.alloc(1)
			if ((await handle.read(buffer, 0, 1, position)).bytesRead !== 1) { throw new Error(`Reading last byte failed`); }
			if (buffer[0] !== 0xFD) { throw new Error("Incomplete XRSV document"); }
		} else {
			position = size;
		}
		const bytes = encodeXRsv(rows);
		if ((await handle.write(bytes, 0, bytes.length, position)).bytesWritten !== bytes.length) { throw new Error(`Data not fully written`); }
	} finally {
		await handle.close(); 
	}
}