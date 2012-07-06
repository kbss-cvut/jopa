/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cz.cvut.kbss.jopa.owlapi;

import cz.cvut.kbss.jopa.model.PersistenceProperties;

public interface OWLAPIPersistenceProperties extends PersistenceProperties {

	public static final String ONTOLOGY_URI_KEY = "cz.cvut.jopa.ontologyURI";
	public static final String ONTOLOGY_DB_CONNECTION = "cz.cvut.jopa.ontologyDBConnection";
	public static final String ONTOLOGY_FILE_KEY = "cz.cvut.jopa.ontologyDocumentURI";
	public static final String ONTOLOGY_PHYSICAL_URI_KEY = "cz.cvut.jopa.ontologyPhysicalURI";
	public static final String MAPPING_FILE_URI_KEY = "cz.cvut.jopa.mappingFileURI";
	public static final String REASONER_FACTORY_CLASS = "cz.cvut.jopa.reasonerFactoryClass";
	public static final String LANG = "cz.cvut.jopa.lang";
}
