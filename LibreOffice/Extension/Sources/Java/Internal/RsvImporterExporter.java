/* (C) Stefan John / Stenway / Stenway.com / 2024 */

package com.stenway.libreoffice.rsvextension;

import com.sun.star.uno.XComponentContext;
import com.sun.star.beans.PropertyValue;
import com.sun.star.sheet.XSpreadsheetDocument;
import com.sun.star.table.CellRangeAddress;

import java.nio.file.Paths;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;


// TODO: Optimize - directly set cells

public class RsvImporterExporter {
	private XComponentContext componentContext;
	private UServiceManager serviceManager;
	
	public RsvImporterExporter(XComponentContext componentContext) {
		this.componentContext = componentContext;
		this.serviceManager = new UServiceManager(componentContext);
	}
	
	public void initialize(PropertyValue[] propertyValues) {
		
	}
	
	public String importSpreadsheet(XSpreadsheetDocument spreadsheetDocument, PropertyValue[] propertyValues) {
		byte[] bytes;
		try {
			bytes = UInputStream.readAllBytes(this.serviceManager, propertyValues);
		} catch (Exception e) {
			return "Could not read the file\n\nDetails:\n" + UMessageBox.getExceptionText(e);
		}
		
		String[][] rsvDocument;
		try {
			rsvDocument = RsvDecoder.decode(bytes);
		} catch (Exception e) {
			return "Document is not a valid RSV document:\n\n" + e.getMessage();
		}
		
		try {
			USpreadsheetDocument uSpreadsheetDocument = new USpreadsheetDocument(spreadsheetDocument);
			USpreadsheet spreadsheet = uSpreadsheetDocument.getSpreadsheet(0);
			setRsvDocument(rsvDocument, spreadsheet);
		} catch (Exception e) {
			return "Could not set RSV document:\n\n" + e.getMessage();
		}
		
		return null;
	}
	
	/*
		||	Maximum number of columns:
		||	->	https://wiki.openoffice.org/wiki/Documentation/FAQ/Calc/Miscellaneous/What%27s_the_maximum_number_of_rows_and_cells_for_a_spreadsheet_file%3F
		||	->	https://wiki.documentfoundation.org/Faq/Calc/022
	*/
	
	private static void setRsvDocument(String[][] rsvDocument, USpreadsheet spreadsheet) {
		int rowIndex = 0;
		int maxColumnIndex = 0;
		int maxAllowedColumns = 1024;
		for (String[] rsvLine : rsvDocument) {
			int columnIndex = 0;
			if (rsvLine.length > maxAllowedColumns) { throw new RuntimeException("RSV row has more than the allowed "+maxAllowedColumns + " values per row"); }
			for (String rsvValue : rsvLine) {
				if (rsvValue != null) {
					spreadsheet.setCellText(columnIndex, rowIndex, rsvValue);
				}
				columnIndex++;
			}
			if (columnIndex > maxColumnIndex) {
				maxColumnIndex = columnIndex;
			}
			spreadsheet.getRowCells(rowIndex).setOptimalHeight(true);
			rowIndex++;
		}
		if (maxColumnIndex > 100) { maxColumnIndex = 100; }
		for (int i=0; i<maxColumnIndex; i++) {
			spreadsheet.getColumnCells(i).setOptimalWidth(true);
		}
	}
	
	public String exportSpreadsheet(XSpreadsheetDocument spreadsheetDocument, PropertyValue[] propertyValues) {
		USpreadsheetDocument uSpreadsheetDocument = new USpreadsheetDocument(spreadsheetDocument);
		if (uSpreadsheetDocument.getSpreadsheetCount() > 1) {
			UMessageBox.showWarning(serviceManager, "RSV Exporter Warning", "The document has more than one sheet. Only the first sheet will be exported.");
			
		}
		USpreadsheet spreadsheet = uSpreadsheetDocument.getSpreadsheet(0);
		
		String[][] rsvDocument = getRsvDocument(spreadsheet);
		
		byte[] bytes;
		try {
			bytes = RsvEncoder.encode(rsvDocument);
		} catch (Exception e) {
			return "Could not encode RSV document:\n\n" + e.getMessage();
		}
		
		try {
			UOutputStream.writeAllBytes(serviceManager, propertyValues, bytes);
		} catch (Exception e) {
			return "RSV document could not be written:\n\n" + e.getMessage();
		}
		
		return null;
	}
	
	private static String[][] getRsvDocument(USpreadsheet spreadsheet) {
		CellRangeAddress usedAreaSize = spreadsheet.getUsedAreaSize();
		int maxColumnCount = usedAreaSize.EndColumn+1;
		int numRows = usedAreaSize.EndRow+1;
		
		ArrayList<String[]> rows = new ArrayList<>();
		for (int rowIndex=0; rowIndex<numRows; rowIndex++) {
			int numColumns = 0;
			for (int i=maxColumnCount-1; i>=0; i--) {
				String value = spreadsheet.getCellText(i, rowIndex);
				if (value.length() > 0) {
					numColumns = i+1;
					break;
				}
			}
			String[] values = new String[numColumns];
			for (int i=0; i<numColumns; i++) {
				String value = spreadsheet.getCellText(i, rowIndex);
				if (value.length() > 0) {
					values[i] = value;
				}
			}
			rows.add(values);
		}
		return rows.toArray(new String[0][]);
	}
}