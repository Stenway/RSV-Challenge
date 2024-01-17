/* (C) Stefan John / Stenway / Stenway.com / 2024 */

package com.stenway.libreoffice.rsvextension;

import com.sun.star.sheet.XCellRangeAddressable;
import com.sun.star.table.CellRangeAddress;
import com.sun.star.table.XCellRange;

/*
	||	Interface XCellRange:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/table/XCellRange.html
	||
	||	Interface XCellRangeAddressable:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/sheet/XCellRangeAddressable.html
	||
	||	Struct CellRangeAddress:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/table/CellRangeAddress.html
*/

class UCellRange {
	private XCellRange cellRange;
	
	public UCellRange(XCellRange cellRange) {
		this.cellRange = cellRange;
	}
	
	public void setOptimalHeight(boolean enabled) {
		UPropertySet.setProperty(cellRange, "OptimalHeight", enabled);
	}
	
	public void setOptimalWidth(boolean enabled) {
		UPropertySet.setProperty(cellRange, "OptimalWidth", enabled);
	}
	
	public CellRangeAddress getAddress() {
		XCellRangeAddressable cellRangeAddressable = UObject.asInterface(XCellRangeAddressable.class, cellRange);
		return cellRangeAddressable.getRangeAddress();
	}
}