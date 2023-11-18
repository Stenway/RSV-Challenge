/* (C) Stefan John / Stenway / Stenway.com / 2023 */

import * as fs from 'node:fs'
import * as bf from 'node:buffer'
import { decodeRsv, encodeRsv } from './rsv.js'

export function saveRsvSync(rows, filePath) {
	fs.writeFileSync(filePath, encodeRsv(rows));
}

export async function saveRsv(rows, filePath) {
	await fs.promises.writeFile(filePath, encodeRsv(rows));
}

export function loadRsvSync(filePath) {
	return decodeRsv(fs.readFileSync(filePath));
}

export async function loadRsv(filePath) {
	return decodeRsv(await fs.promises.readFile(filePath));
}

export function appendRsvSync(rows, filePath, continueLastRow = false) {
	let handle;
	let existed = false;
	try {
		handle = fs.openSync(filePath, "r+");
		existed = true;
	} catch (error) {
		if (error.code === "ENOENT") {
			handle = fs.openSync(filePath, "wx");
		}
		else { throw error; }
	}
	try {
		const size = fs.fstatSync(handle).size;
		let position;
		if (existed === true && continueLastRow === true && size > 0) {
			position = size - 1;
			const buffer = bf.Buffer.alloc(1)
			if (fs.readSync(handle, buffer, 0, 1, position) !== 1) { throw new Error(`Reading last byte failed`); }
			if (buffer[0] !== 0xFD) { throw new Error("Incomplete RSV document"); }
		} else {
			position = size;
		}
		const bytes = encodeRsv(rows);
		if (fs.writeSync(handle, bytes, 0, bytes.length, position) !== bytes.length) { throw new Error(`Data not fully written`); }
	} finally {
		fs.closeSync(handle);
	}
}

export async function appendRsv(rows, filePath, continueLastRow = false) {
	let handle;
	let existed = false;
	try {
		handle = await fs.promises.open(filePath, "r+");
		existed = true;
	} catch (error) {
		if (error.code === "ENOENT") {
			handle = await fs.promises.open(filePath, "wx");
		}
		else { throw error; }
	}
	try {
		const size = (await handle.stat()).size;
		let position;
		if (existed === true && continueLastRow === true && size > 0) {
			position = size - 1;
			const buffer = bf.Buffer.alloc(1)
			if ((await handle.read(buffer, 0, 1, position)).bytesRead !== 1) { throw new Error(`Reading last byte failed`); }
			if (buffer[0] !== 0xFD) { throw new Error("Incomplete RSV document"); }
		} else {
			position = size;
		}
		const bytes = encodeRsv(rows);
		if ((await handle.write(bytes, 0, bytes.length, position)).bytesWritten !== bytes.length) { throw new Error(`Data not fully written`); }
	} finally {
		await handle.close(); 
	}
}