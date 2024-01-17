/* (C) Stefan John / Stenway / Stenway.com / 2024 */

package com.stenway.libreoffice.rsvextension;

import com.sun.star.beans.PropertyValue;
import com.sun.star.container.XNamed;
import com.sun.star.document.XExporter;
import com.sun.star.document.XExtendedFilterDetection;
import com.sun.star.document.XFilter;
import com.sun.star.document.XImporter;
import com.sun.star.lang.IllegalArgumentException;
import com.sun.star.lang.XComponent;
import com.sun.star.lang.XInitialization;
import com.sun.star.lang.XServiceInfo;
import com.sun.star.lib.uno.helper.WeakBase;
import com.sun.star.sheet.XSpreadsheetDocument;
import com.sun.star.uno.UnoRuntime;
import com.sun.star.uno.XComponentContext;

/*
	RsvImportExportFilter
	
	||	Developer's Guide / Office Development / Common Application Features / Integrating Import and Export Filters:
	||	->	https://wiki.openoffice.org/wiki/Documentation/DevGuide/OfficeDev/Integrating_Import_and_Export_Filters
	||	->	https://wiki.documentfoundation.org/Documentation/DevGuide/Office_Development#Integrating_Import_and_Export_Filters
	||
	||	Service ImportFilter:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/document/ImportFilter.html
	||
	||	Service ExportFilter:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/document/ExportFilter.html
	||
	||	Service ExtendedTypeDetection:
	||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/document/ExtendedTypeDetection.html
*/

public class RsvImportExportFilter extends WeakBase implements 
		XInitialization,
		XNamed,
		XServiceInfo,
		
		XImporter,
		XExporter,
		XFilter,
		
		XExtendedFilterDetection {
			
	/*
		Constructor
		
		||	Class WeakBase:
		||	->	https://www.openoffice.org/api/docs/java/ref/com/sun/star/lib/uno/helper/WeakBase.html
	*/
	
	private XComponentContext componentContext;
	private RsvImporterExporter rsvImporterExporter;
	
	public RsvImportExportFilter(XComponentContext componentContext) {
		try {
			this.componentContext = componentContext;
			this.rsvImporterExporter = new RsvImporterExporter(componentContext);
		} catch(Exception e) {
			e.printStackTrace();
		}
	}
	
	/*
		XInitialization implementation
		
		||	Developer's Guide / Writing UNO Components / Core Interfaces to Implement / XInitialization:
		||	->	https://wiki.openoffice.org/wiki/Documentation/DevGuide/WritingUNO/XInitialization
		||
		||	Interface XInitialization:
		||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/lang/XInitialization.html
	*/
	
	private String name = null;
	
	public void initialize(Object[] arguments) throws com.sun.star.uno.Exception {
		PropertyValue[] propertyValues = null;
		if (arguments.length >= 1) {
			propertyValues = (PropertyValue[])arguments[0];
			this.name = UPropertyValues.getStringOrNull(propertyValues, "Name");
			if (this.name == null) { throw new IllegalArgumentException("Could not get the name of the filter"); }
		}
		this.rsvImporterExporter.initialize(propertyValues);
	}
	
	/*
		XNamed implementation
		
		||	Interface XNamed:
		||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/container/XNamed.html
	*/
	
	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		
	}
	
	/*
		XServiceInfo implementation
		
		||	Developer's Guide / Writing UNO Components / Core Interfaces to Implement / XServiceInfo:
		||	->	https://wiki.openoffice.org/wiki/Documentation/DevGuide/WritingUNO/XServiceInfo
		||
		||	Interface XServiceInfo:
		||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/lang/XServiceInfo.html
	*/
	
	public static final String implementationName = RsvImportExportFilter.class.getName();
	public static final String[] supportedServiceNames = {
		"com.stenway.libreoffice.rsvextension.RsvImportExportFilter",
		"com.sun.star.document.ImportFilter",
		"com.sun.star.document.ExportFilter",
		"com.sun.star.document.ExtendedTypeDetection"
	};
	
	public String[] getSupportedServiceNames() {
		return this.supportedServiceNames;
	}
	
	public boolean supportsService(String serviceName) {
		for (String supportedServiceName : this.supportedServiceNames) {
			if (serviceName.equals(supportedServiceName)) { return true; }
		}
		return false;
	}
	
	public String getImplementationName() {
		return this.implementationName;
	}
	
	/*
		XImporter / XExporter helper
	*/
	
	private XSpreadsheetDocument spreadsheetDocument = null;
	private boolean isImport = true;
	
	private static XSpreadsheetDocument asSpreadsheetDocument(XComponent component) throws IllegalArgumentException {
		if (!UServiceInfo.supportsService(component, "com.sun.star.sheet.SpreadsheetDocument")) {
			throw new IllegalArgumentException("Not a spreadsheet document type");
		}
		return UObject.asInterface(XSpreadsheetDocument.class, component);
	}
	
	/*
		XImporter implementation
		
		||	Interface XImporter:
		||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/document/XImporter.html
	*/
	
	public void setTargetDocument(XComponent component) throws IllegalArgumentException {
		this.isImport = true;
		this.spreadsheetDocument = asSpreadsheetDocument(component);
	}
	
	/*
		XExporter implementation
		
		||	Interface XExporter:
		||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/document/XExporter.html
	*/
	
	public void setSourceDocument(XComponent component) throws IllegalArgumentException {
		this.isImport = false;
		this.spreadsheetDocument = asSpreadsheetDocument(component);
	}
	
	/*
		XFilter implementation
		
		||	Interface XFilter:
		||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/document/XFilter.html
	*/
	
	public boolean filter(PropertyValue[] propertyValues) {
		String errorMessage;
		try {
			if (this.isImport) {
				errorMessage = this.rsvImporterExporter.importSpreadsheet(this.spreadsheetDocument, propertyValues);
			} else {
				errorMessage = this.rsvImporterExporter.exportSpreadsheet(this.spreadsheetDocument, propertyValues);
			}
		} catch (Exception e) {
			errorMessage = "An unexpected error occured:\n\n" + UMessageBox.getExceptionText(e);
		}
		if (errorMessage != null) {
			String errorTitle = "RSV Export Failure";
			if (isImport) errorTitle = "RSV Import Failure";
			UMessageBox.showError(new UServiceManager(this.componentContext), errorTitle, errorMessage);
			return false;
		}
		return true;
	}
	
	public void cancel() {
		// TODO
	}
	
	/*
		XExtendedFilterDetection implementation
		
		||	Interface XExtendedFilterDetection:
		||	->	https://www.openoffice.org/api/docs/common/ref/com/sun/star/document/XExtendedFilterDetection.html
	*/
	
	public String detect(PropertyValue[][] descriptor) {
		PropertyValue[] propertyValues = descriptor[0];
		String typeName = UPropertyValues.getStringOrNull(propertyValues, "TypeName");
		if (typeName != null) {
			return typeName;
		}
		return "";
	}
}