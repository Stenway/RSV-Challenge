/* (C) Stefan John / Stenway / Stenway.com / 2023 */
// deno-lint-ignore-file no-inferrable-types
/* eslint-disable @typescript-eslint/no-explicit-any */

import * as fs from 'node:fs'
import * as bf from 'node:buffer'
import { decodeXRsv, encodeXRsv } from './xrsv.ts'

export function saveXRsvSync(rows: (string | null)[][], filePath: string) {
	fs.writeFileSync(filePath, encodeXRsv(rows));
}

export function loadXRsvSync(filePath: string): (string | null)[][] {
	return decodeXRsv(fs.readFileSync(filePath));
}