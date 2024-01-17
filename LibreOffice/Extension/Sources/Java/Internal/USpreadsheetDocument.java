/* (C) Stefan John / Stenway / Stenway.com / 2024 */

package com.stenway.libreoffice.rsvextension;

import com.sun.star.container.XIndexAccess;
import com.sun.star.sheet.XSpreadsheet;
import com.sun.star.sheet.XSpreadsheetDocument;
import com.sun.star.sheet.XSpreadsheets;

/*
	||	Interface XSpreadsheetDocument:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/sheet/XSpreadsheetDocument.html
	||
	||	Interface XSpreadsheets:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/sheet/XSpreadsheets.html
	||
	||	Interface XIndexAccess:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/container/XIndexAccess.html
*/

class USpreadsheetDocument {
	private XSpreadsheetDocument document;
	private XSpreadsheets spreadsheets;
	
	public USpreadsheetDocument(XSpreadsheetDocument document) {
		this.document = document;
		this.spreadsheets = document.getSheets();
	}
	
	public USpreadsheet getSpreadsheet(int index) {
		try {
			XIndexAccess indexAccess = UObject.asInterface(XIndexAccess.class, spreadsheets);
			Object obj = indexAccess.getByIndex(index);
			XSpreadsheet spreadsheet = UObject.asInterface(XSpreadsheet.class, obj);
			return new USpreadsheet(spreadsheet);
		} catch (Exception e) {
			throw new RuntimeException("Could not get sheet at index "+index);
		}
	}
	
	public int getSpreadsheetCount() {
		return spreadsheets.getElementNames().length;
	}
}