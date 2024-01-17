/* (C) Stefan John / Stenway / Stenway.com / 2024 */

package com.stenway.libreoffice.rsvextension;

import com.sun.star.container.XIndexAccess;

import com.sun.star.sheet.XSheetCellCursor;
import com.sun.star.sheet.XSpreadsheet;
import com.sun.star.sheet.XUsedAreaCursor;
import com.sun.star.table.CellRangeAddress;
import com.sun.star.table.XCell;
import com.sun.star.table.XCellRange;
import com.sun.star.table.XColumnRowRange;
import com.sun.star.table.XTableColumns;
import com.sun.star.table.XTableRows;
import com.sun.star.text.XTextRange;
import java.util.Objects;

/*
	||	Interface XSpreadsheet:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/sheet/XSpreadsheet.html
	||
	||	Interface XCell:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/table/XCell.html
	||
	||	Interface XTextRange:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/text/XTextRange.html
	||
	||	Interface XColumnRowRange:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/table/XColumnRowRange.html
	||
	||	Interface XTableColumns:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/table/XTableColumns.html
	||
	||	Interface XTableRows:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/table/XTableRows.html
	||
	||	Interface XIndexAccess:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/container/XIndexAccess.html
	||
	||	Interface XSheetCellCursor:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/sheet/XSheetCellCursor.html
	||
	||	Interface XUsedAreaCursor:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/sheet/XUsedAreaCursor.html
*/

class USpreadsheet {
	private XSpreadsheet spreadsheet;
	
	public USpreadsheet(XSpreadsheet spreadsheet) {
		Objects.requireNonNull(spreadsheet);
		this.spreadsheet = spreadsheet;
	}
	
	public String getCellText(int x, int y) {
		try {
			XCell cell = spreadsheet.getCellByPosition(x, y);
			XTextRange textRange = UObject.asInterface(XTextRange.class, cell);
			return textRange.getString();
		} catch (Exception e) {
			throw new RuntimeException("Could not get cell text", e);
		}
	}
	
	public void setCellText(int x, int y, String str) {
		try {
			XCell cell = spreadsheet.getCellByPosition(x, y);
			XTextRange textRange = UObject.asInterface(XTextRange.class, cell);
			textRange.setString(str);
		} catch (Exception e) {
			throw new RuntimeException("Could not set cell text", e);
		}
	}
	
	public UCellRange getColumnCells(int columnIndex) {
		XColumnRowRange columnRowRange = UObject.asInterface(XColumnRowRange.class, spreadsheet);
		XTableColumns tableColumns = columnRowRange.getColumns();
		XIndexAccess indexAccess = UObject.asInterface(XIndexAccess.class, tableColumns);
		try {
			Object obj = indexAccess.getByIndex(columnIndex);
			XCellRange cellRange = UObject.asInterface(XCellRange.class, obj);
			return new UCellRange(cellRange);
		} catch (Exception e) {
			throw new RuntimeException("Could not access column "+columnIndex+"'", e); 
		}
	}
	
	public UCellRange getRowCells(int rowIndex) {
		XColumnRowRange columnRowRange = UObject.asInterface(XColumnRowRange.class, spreadsheet);
		XTableRows tableRows = columnRowRange.getRows();
		XIndexAccess indexAccess = UObject.asInterface(XIndexAccess.class, tableRows);
		try {
			Object obj = indexAccess.getByIndex(rowIndex);
			XCellRange cellRange = UObject.asInterface(XCellRange.class, obj);
			return new UCellRange(cellRange);
		} catch (Exception e) {
			throw new RuntimeException("Could not access row "+rowIndex+"'", e); 
		}
	}
	
	public CellRangeAddress getUsedAreaSize() {
		XSheetCellCursor sheetCellCursor = spreadsheet.createCursor();
		XUsedAreaCursor usedAreaCursor = UObject.asInterface(XUsedAreaCursor.class, sheetCellCursor); 
		usedAreaCursor.gotoStartOfUsedArea(false);
		usedAreaCursor.gotoEndOfUsedArea(true);
		XCellRange usedCellRange = UObject.asInterface(XCellRange.class, usedAreaCursor); 
		return new UCellRange(usedCellRange).getAddress();
	}
}