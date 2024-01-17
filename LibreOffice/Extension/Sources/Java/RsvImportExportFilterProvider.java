/* (C) Stefan John / Stenway / Stenway.com / 2024 */

package com.stenway.libreoffice.rsvextension;

import com.sun.star.lang.XSingleComponentFactory;
import com.sun.star.lib.uno.helper.Factory;

/*
	RsvImportExportFilterProvider
	
	||	Developer's Guide / Writing UNO Components / Simple Component in Java / Possible Structures for Java Components:
	||	->	https://wiki.openoffice.org/wiki/Documentation/DevGuide/WritingUNO/Possible_Structures_for_Java_Components
	||	->	https://wiki.documentfoundation.org/Documentation/DevGuide/Writing_UNO_Components#Possible_Structures_for_Java_Components
*/

public class RsvImportExportFilterProvider {
	/*
		__getComponentFactory
		
		||	General UNO Component Project Type:
		||	->	https://wiki.openoffice.org/wiki/General_UNO_Component_Project_Type
	*/
	
	public static XSingleComponentFactory __getComponentFactory(String implementationName) {
		if (implementationName.equals(RsvImportExportFilter.implementationName)) {
			return Factory.createComponentFactory(RsvImportExportFilter.class, RsvImportExportFilter.supportedServiceNames);
		} else {
			return null;
		}
	}
}