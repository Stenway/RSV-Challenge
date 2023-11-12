function getCodepointsTestCase(from: number, to: number): (string | null)[][] {
	const result: (string | null)[][] = []	
	for (let j=from; j<=to; j++) {
		const startCodePoint = j*0x100
		let str = ""
		let count = 0
		for (let i=0; i<=0xFF; i++) {
			const codePoint = startCodePoint + i
			if (codePoint >= 0xD800 && codePoint <= 0xDFFF) { continue }
			str += String.fromCodePoint(codePoint)
			count++
		}
		result.push([
			"U+"+startCodePoint.toString(16).padStart(6, "0").toUpperCase(),
			"U+"+(startCodePoint+0xFF).toString(16).padStart(6, "0").toUpperCase(),
			count.toString(),
			str
		])
	}
	return result
}

export function getValidTestCases(): (string | null)[][][] {
	return [
		[["Hello", "🌎", null, ""],	["A\0B\nC", "Test 𝄞"],	[],	[""]],
		[["a", "U+0061", "61", "0061", "Latin Small Letter A"], ["~", "U+007E", "7E", "007E", "Tilde"], ["¥", "U+00A5", "C2_A5", "00A5", "Yen Sign"], ["»", "U+00BB", "C2_BB", "00BB", "Right-Pointing Double Angle Quotation Mark"], ["½", "U+00BD", "C2_BD", "00BD", "Vulgar Fraction One Half"], ["¿", "U+00BF", "C2_BF", "00BF", "Inverted Question Mark"], ["ß", "U+00DF", "C3_9F", "00DF", "Latin Small Letter Sharp S"], ["ä", "U+00E4", "C3_A4", "00E4", "Latin Small Letter A with Diaeresis"], ["ï", "U+00EF", "C3_AF", "00EF", "Latin Small Letter I with Diaeresis"], ["œ", "U+0153", "C5_93", "0153", "Latin Small Ligature Oe"], ["€", "U+20AC", "E2_82_AC", "20AC", "Euro Sign"], ["東", "U+6771", "E6_9D_B1", "6771", "CJK Unified Ideograph-6771"], ["𝄞", "U+1D11E", "F0_9D_84_9E", "D834_DD1E", "Musical Symbol G Clef"], ["𠀇", "U+20007", "F0_A0_80_87", "D840_DC07", "CJK Unified Ideograph-20007"]],
		
		// -- 0 rows --
		
		[],
		
		// -- 1 row / 0 values --
		
		[[]],
		
		// -- 1 row / 1 value --
		
		[["A"]],
		[["AB"]],
		[["🌎"]],
		[[""]],
		[[null]],
		
		// -- 1 row / 2 values --
		
		[["A", "B"]],
		[["A", ""]],
		[["A", null]],
		
		[["", "B"]],
		[["", ""]],
		[["", null]],
		
		[[null, "B"]],
		[[null, ""]],
		[[null, null]],
		
		// -- 1 row / 3 values --
		
		[["A", "B", "C"]],
		[["", "", ""]],
		[[null, null, null]],
		
		// -- 2 rows --
		
		[[], []],
		[["A"], []],
		[[""], []],
		[[null], []],
		
		[[], ["B"]],
		[["A"], ["B"]],
		[[""], ["B"]],
		[[null], ["B"]],
		
		[[], [""]],
		[["A"], [""]],
		[[""], [""]],
		[[null], [""]],
		
		[[], [null]],
		[["A"], [null]],
		[[""], [null]],
		[[null], [null]],
		
		// -- 3 rows --
		
		[[], [], []],
		
		[["A"], [], []],
		[[], ["A"], []],
		[[], [], ["A"]],
		[[""], [], []],
		[[], [""], []],
		[[], [], [""]],
		[[null], [], []],
		[[], [null], []],
		[[], [], [null]],
		
		[["A", "B"], ["C", "D"], ["E", "F"]],
		[[], ["C", "D"], ["E", "F"]],
		[["A", "B"], [], ["E", "F"]],
		[["A", "B"], ["C", "D"], []],
		[[null], ["C", "D"], ["E", "F"]],
		[["A", "B"], [null], ["E", "F"]],
		[["A", "B"], ["C", "D"], [null]],
		[[""], ["C", "D"], ["E", "F"]],
		[["A", "B"], [""], ["E", "F"]],
		[["A", "B"], ["C", "D"], [""]],
		
		// -- byte-order-mark --
		
		[["\uFEFF"]],
		[["\uFEFF\uFEFF"]],
		[["\uFEFFA"]],
		[["A\uFEFF"]],
		
		[["\uFEFFA", "\uFEFFB"]],
		
		[["\uFEFF"], ["\uFEFF"]],
		[["\uFEFF\uFEFF"], ["\uFEFF"]],
		[["\uFEFFA"], ["\uFEFF"]],
		[["A\uFEFF"], ["\uFEFF"]],
		
		// -- Unicode --
		
		[["\b\t\n\f\r\"\\"]],
		[["\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u000b\u000e\u000f\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001a\u001b\u001c\u001d\u001e\u001f"]],
		[["¥ä€東"]],
		
		getCodepointsTestCase(0, 0),
		getCodepointsTestCase(0x01, 0xFF),
		getCodepointsTestCase(0x100, 0x1FF),
		getCodepointsTestCase(0x200, 0x2FF),
		getCodepointsTestCase(0x300, 0x3FF),
		getCodepointsTestCase(0x400, 0xDFF),
		getCodepointsTestCase(0xE00, 0xEFF),
		getCodepointsTestCase(0xF00, 0x10FF),
		
		// -- misc --
		
		[["A", "B"], ["C"], ["D", "E", "F"]],
		
		[["A".repeat(10000)]],
	]
}

export function getInvalidTestCases(): number[][] {
	const ValueEnd = 0xFF
	const NullValue = 0xFE
	const RowEnd = 0xFD
	const A = 0x41
	
	return [
		// -- incomplete RSV document --
		
		[ValueEnd],
		[A, ValueEnd],
		[NullValue, ValueEnd],
		
		// -- incomplete RSV line --
		
		[A, RowEnd],
		[NullValue, RowEnd],
		
		// -- invalid UTF-8 bytes --
		
		[0xC0, ValueEnd, RowEnd],
		[0xC1, ValueEnd, RowEnd],
		[0xF5, ValueEnd, RowEnd],
		[0xF6, ValueEnd, RowEnd],
		[0xF7, ValueEnd, RowEnd],
		[0xF8, ValueEnd, RowEnd],
		[0xF9, ValueEnd, RowEnd],
		[0xFA, ValueEnd, RowEnd],
		[0xFB, ValueEnd, RowEnd],
		[0xFC, ValueEnd, RowEnd],
		
		// -- invalid surrogates --
		
		[0xED, 0xA0, 0x80, ValueEnd, RowEnd],
		[0xED, 0xAF, 0xBF, ValueEnd, RowEnd],
		[0xED, 0xB0, 0x80, ValueEnd, RowEnd],
		[0xED, 0xBF, 0xBF, ValueEnd, RowEnd],
		[0xED, 0xA0, 0x80, 0xED, 0xB0, 0x80, ValueEnd, RowEnd],
		
		// -- incomplete UTF-8 sequences --
		
		[0xC4, ValueEnd, RowEnd],
		[0xE1, 0x80, ValueEnd, RowEnd],
		[0xF0, 0x9D, 0x84, ValueEnd, RowEnd],
		
		// -- overlong UTF-8 sequences --
		
		[0xC1, 0xBF, ValueEnd, RowEnd],
		[0xE0, 0x81, 0xBF, ValueEnd, RowEnd],
		[0xF0, 0x80, 0x81, 0xBF, ValueEnd, RowEnd],
		
		// -- modified UTF-8 --
		
		[0xC0, 0x80, ValueEnd, RowEnd],
		
		// -- out of range UTF-8 --
		
		[0xF4, 0x90, 0x80, 0x80, ValueEnd, RowEnd],
		
		// -- single continuation byte --
		
		[0x80, ValueEnd, RowEnd],
	]
}